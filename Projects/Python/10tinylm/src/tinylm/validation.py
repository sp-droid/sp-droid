"""Full held-out validation and JSON reporting for a trained checkpoint."""

from __future__ import annotations

import argparse
import math
from pathlib import Path
import time
from typing import Any

import numpy as np
import torch
from tqdm import tqdm

from .config import load_config, report_path
from .data import PreparedData, load_prepared_data
from .lm_eval_adapter import run_lm_eval
from .model import GPTLLM, build_model, parameter_counts
from .reporting import provenance, write_json, write_markdown
from .telemetry import peak_memory_gb, reset_peak_memory, synchronize


def choose_device() -> torch.device:
    return torch.device("cuda" if torch.cuda.is_available() else "cpu")


def autocast_context(device: torch.device, enabled: bool):
    return torch.amp.autocast(device_type=device.type, enabled=enabled and device.type == "cuda")


def load_checkpoint(checkpoint_path: str | Path, device: torch.device) -> tuple[dict[str, object], GPTLLM, PreparedData]:
    checkpoint = torch.load(checkpoint_path, map_location=device, weights_only=False)
    config = dict(checkpoint["config"])
    prepared = load_prepared_data(config)
    expected_tokens = checkpoint["tokenizer"]["tokens"]
    if prepared.tokenizer.tokens != expected_tokens:
        raise ValueError("Prepared tokenizer differs from the tokenizer saved in this checkpoint")
    model_config = config["model"]
    assert isinstance(model_config, dict)
    model = build_model(model_config, prepared.tokenizer.vocab_size).to(device)
    model.load_state_dict(checkpoint["model_state"])
    model.eval()
    return checkpoint, model, prepared


@torch.inference_mode()
def full_validation_loss(
    model: GPTLLM,
    prepared: PreparedData,
    config: dict[str, object],
    device: torch.device,
) -> dict[str, object]:
    """Score every held-out token in sequential, non-overlapping BOS windows."""
    model_config = config["model"]
    training = config["training"]
    assert isinstance(model_config, dict) and isinstance(training, dict)
    block_size = int(model_config["block_size"])
    batch_size = int(training["batch_size"])
    tokens = prepared.validation_tokens
    total_loss = 0.0
    scored_tokens = 0
    offset = 0

    reset_peak_memory(device)
    synchronize(device)
    started = time.perf_counter()
    progress = tqdm(total=len(tokens), desc="Full validation", unit="token")
    while offset < len(tokens):
        complete_rows = min(batch_size, (len(tokens) - offset) // block_size)
        if complete_rows:
            count = complete_rows * block_size
            targets_np = np.asarray(tokens[offset : offset + count]).reshape(complete_rows, block_size)
        else:
            count = len(tokens) - offset
            targets_np = np.asarray(tokens[offset : offset + count]).reshape(1, count)
        inputs_np = np.empty_like(targets_np)
        inputs_np[:, 0] = prepared.tokenizer.bos_id
        if targets_np.shape[1] > 1:
            inputs_np[:, 1:] = targets_np[:, :-1]
        inputs = torch.from_numpy(inputs_np.astype(np.int64, copy=False)).to(device)
        targets = torch.from_numpy(targets_np.astype(np.int64, copy=False)).to(device)
        with autocast_context(device, bool(training["amp"])):
            total_loss += float(model.loss(inputs, targets, reduction="sum").item())
        scored_tokens += count
        offset += count
        progress.update(count)
    progress.close()
    synchronize(device)
    elapsed = time.perf_counter() - started
    cross_entropy = total_loss / scored_tokens
    return {
        "cross_entropy_loss_nats": cross_entropy,
        "perplexity": math.exp(cross_entropy),
        "scored_tokens": scored_tokens,
        "scored_tokens_m": scored_tokens / 1_000_000,
        "inference_tokens_per_second": scored_tokens / elapsed if elapsed else None,
        "throughput_scope": "forward-only full validation pass; not comparable to training-step throughput",
        "elapsed_seconds": elapsed,
        "peak_gpu_memory": peak_memory_gb(device),
        "windowing": "sequential non-overlapping windows, each conditioned on BOS",
    }


@torch.inference_mode()
def qualitative_generations(
    model: GPTLLM,
    prepared: PreparedData,
    config: dict[str, object],
    device: torch.device,
) -> list[dict[str, object]]:
    validation = config["validation"]
    assert isinstance(validation, dict)
    output: list[dict[str, object]] = []
    for item in validation["prompts"]:
        prompt = str(item["prompt"])
        prompt_ids, unknown_count = prepared.tokenizer.encode(prompt)
        input_ids = torch.tensor([[prepared.tokenizer.bos_id, *prompt_ids]], dtype=torch.long, device=device)
        generated = model.generate(input_ids, int(validation["generation_max_new_tokens"]), temperature=0.0)
        completion = prepared.tokenizer.decode(generated[0, input_ids.size(1) :].tolist())
        output.append(
            {
                "level": item["level"],
                "prompt": prompt,
                "completion": completion,
                "unknown_prompt_character_count": unknown_count,
                "generation": {"temperature": 0.0, "max_new_tokens": int(validation["generation_max_new_tokens"])},
            }
        )
    return output


def create_report(
    checkpoint_path: str | Path,
    skip_lm_eval: bool = False,
    skip_hellaswag: bool = False,
) -> dict[str, object]:
    device = choose_device()
    checkpoint, model, prepared = load_checkpoint(checkpoint_path, device)
    config = checkpoint["config"]
    assert isinstance(config, dict)
    validation = full_validation_loss(model, prepared, config, device)
    generations = qualitative_generations(model, prepared, config, device)
    if skip_lm_eval:
        harness: dict[str, object] = {"status": "skipped", "reason": "--skip-lm-eval"}
    else:
        harness = run_lm_eval(model, prepared.tokenizer, config, device, include_hellaswag=not skip_hellaswag)

    training_state = checkpoint["training_state"]
    assert isinstance(training_state, dict)
    training_tokens = int(training_state["tokens_seen"])
    dataset_tokens = prepared.train_token_count
    active_seconds = float(training_state["active_training_seconds"])
    timed_tokens = int(training_state["timed_tokens"])
    counts = parameter_counts(model)
    return {
        "schema_version": 1,
        "description": str(config["run"].get("description", "")),
        "provenance": provenance(device),
        "checkpoint": {"path": str(Path(checkpoint_path).resolve()), "completed_steps": training_state["completed_steps"]},
        "model": {
            "config": config["model"],
            "parameter_count_m": counts["total_m"],
            "trainable_parameter_count_m": counts["trainable_m"],
            "parameter_count": counts,
        },
        "data": {
            "dataset_name": config["dataset"]["name"],
            "training_dataset_tokens": dataset_tokens,
            "training_dataset_tokens_m": dataset_tokens / 1_000_000,
            "training_dataset_characters": dataset_tokens,
            "training_dataset_characters_m": dataset_tokens / 1_000_000,
            "validation_dataset_tokens": prepared.validation_token_count,
            "validation_dataset_tokens_m": prepared.validation_token_count / 1_000_000,
            "training_tokens_seen": training_tokens,
            "training_tokens_seen_m": training_tokens / 1_000_000,
            "training_tokens_to_dataset_ratio": training_tokens / dataset_tokens,
            "prepared_data": prepared.metadata,
        },
        "training": {
            "last_loss": training_state["last_training_loss"],
            "last_progress_validation_loss": training_state["last_progress_validation_loss"],
            "last_progress_generations": training_state.get(
                "last_progress_generations",
                [training_state["last_progress_generation"]] if training_state.get("last_progress_generation") else [],
            ),
            "timed_tokens": timed_tokens,
            "active_training_seconds": active_seconds,
            "tokens_per_second": timed_tokens / active_seconds if active_seconds else None,
            "throughput_scope": "optimizer steps after warm-up; excludes validation and checkpoint I/O",
            "peak_gpu_memory": training_state["peak_gpu_memory"],
        },
        "validation": validation,
        "qualitative_generations": generations,
        "lm_eval": harness,
    }


def main() -> None:
    parser = argparse.ArgumentParser(description="Run complete post-training validation and write JSON.")
    parser.add_argument("--checkpoint", required=True, help="Path to the timestamped checkpoint to validate.")
    parser.add_argument("--output", help="Report path; defaults to output/<run-name>/validation.json.")
    parser.add_argument("--skip-lm-eval", action="store_true", help="Only run held-out loss and qualitative prompts.")
    parser.add_argument("--skip-hellaswag", action="store_true", help="Run local lm-eval tasks without downloading HellaSwag.")
    args = parser.parse_args()
    checkpoint = Path(args.checkpoint.strip("'\""))
    report = create_report(checkpoint, args.skip_lm_eval, args.skip_hellaswag)
    run_name = checkpoint.resolve().parent.name
    output = Path(args.output) if args.output else report_path(run_name)
    write_json(output, report)
    write_markdown(output.with_suffix(".md"), report)
    print(f"Wrote validation report: {output}")


if __name__ == "__main__":
    main()
