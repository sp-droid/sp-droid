"""CLI and implementation for resumable TinyLM training."""

from __future__ import annotations

import argparse
import math
from pathlib import Path
import random
import time
from typing import Any

import numpy as np
import torch
from tqdm import tqdm

from .config import checkpoint_path, config_for_checkpoint, load_config, next_run_name
from .data import PreparedData, load_prepared_data, sample_batch
from .model import GPTLLM, build_model, parameter_counts
from .telemetry import peak_memory_gb, reset_peak_memory, synchronize
from .tokenizer import CharacterTokenizer


def choose_device() -> torch.device:
    return torch.device("cuda" if torch.cuda.is_available() else "cpu")


def autocast_context(device: torch.device, enabled: bool):
    return torch.amp.autocast(device_type=device.type, enabled=enabled and device.type == "cuda")


def set_seed(seed: int) -> None:
    random.seed(seed)
    np.random.seed(seed)
    torch.manual_seed(seed)
    if torch.cuda.is_available():
        torch.cuda.manual_seed_all(seed)


def make_scheduler(optimizer: torch.optim.Optimizer, training: dict[str, object]) -> torch.optim.lr_scheduler.LambdaLR:
    max_steps = int(training["max_steps"])
    warmup_steps = min(int(training["warmup_steps"]), max_steps)
    initial_lr = float(training["learning_rate"])
    minimum_lr = float(training["min_learning_rate"])
    floor = minimum_lr / initial_lr

    def multiplier(step: int) -> float:
        if warmup_steps and step < warmup_steps:
            return (step + 1) / warmup_steps
        if max_steps <= warmup_steps:
            return 1.0
        progress = min(1.0, (step - warmup_steps) / (max_steps - warmup_steps))
        return floor + (1.0 - floor) * 0.5 * (1.0 + math.cos(math.pi * progress))

    return torch.optim.lr_scheduler.LambdaLR(optimizer, multiplier)


@torch.inference_mode()
def estimate_loss(
    model: GPTLLM,
    prepared: PreparedData,
    config: dict[str, object],
    device: torch.device,
    generator: torch.Generator,
) -> float:
    training = config["training"]
    model_config = config["model"]
    batch_size = int(training["batch_size"])
    block_size = int(model_config["block_size"])
    eval_iters = int(training["eval_iters"])
    use_amp = bool(training["amp"])

    previous_mode = model.training
    model.eval()
    losses: list[float] = []
    for _ in range(eval_iters):
        inputs, targets = sample_batch(
            prepared.validation_tokens,
            batch_size,
            block_size,
            prepared.tokenizer.bos_id,
            device,
            generator,
        )
        with autocast_context(device, use_amp):
            losses.append(float(model.loss(inputs, targets).item()))
    model.train(previous_mode)
    return sum(losses) / len(losses)


@torch.inference_mode()
def progress_generations(
    model: GPTLLM,
    tokenizer: CharacterTokenizer,
    config: dict[str, object],
    device: torch.device,
) -> list[str]:
    training = config["training"]
    prompt = str(training["progress_prompt"])
    prompt_ids, _ = tokenizer.encode(prompt)
    input_ids = torch.tensor([[tokenizer.bos_id, *prompt_ids]], dtype=torch.long, device=device)
    previous_mode = model.training
    model.eval()
    completions: list[str] = []
    for sample_index in range(int(training["progress_samples"])):
        generator = torch.Generator(device=device.type)
        generator.manual_seed(int(training["seed"]) + 10_000 + sample_index)
        generated = model.generate(
            input_ids,
            int(training["progress_max_new_tokens"]),
            temperature=float(training["progress_temperature"]),
            generator=generator,
        )
        completions.append(tokenizer.decode(generated[0, input_ids.size(1) :].tolist()).replace("\n", "\\n"))
    model.train(previous_mode)
    return completions


def _checkpoint_payload(
    config: dict[str, object],
    model: GPTLLM,
    optimizer: torch.optim.Optimizer,
    scheduler: torch.optim.lr_scheduler.LRScheduler,
    scaler: torch.amp.GradScaler,
    tokenizer: CharacterTokenizer,
    prepared: PreparedData,
    state: dict[str, object],
) -> dict[str, object]:
    return {
        "format_version": 1,
        "config": config_for_checkpoint(config),
        "model_state": model.state_dict(),
        "optimizer_state": optimizer.state_dict(),
        "scheduler_state": scheduler.state_dict(),
        "scaler_state": scaler.state_dict(),
        "tokenizer": tokenizer.to_dict(),
        "data_metadata": prepared.metadata,
        "training_state": state,
        "torch_rng_state": torch.get_rng_state(),
        "cuda_rng_states": torch.cuda.get_rng_state_all() if torch.cuda.is_available() else None,
    }


def save_checkpoint(path: Path, payload: dict[str, object]) -> None:
    path.parent.mkdir(parents=True, exist_ok=True)
    torch.save(payload, path)


def _restore_checkpoint(
    path: Path,
    model: GPTLLM,
    optimizer: torch.optim.Optimizer,
    scheduler: torch.optim.lr_scheduler.LRScheduler,
    scaler: torch.amp.GradScaler,
    device: torch.device,
) -> dict[str, object]:
    payload = torch.load(path, map_location=device, weights_only=False)
    model.load_state_dict(payload["model_state"])
    optimizer.load_state_dict(payload["optimizer_state"])
    scheduler.load_state_dict(payload["scheduler_state"])
    scaler.load_state_dict(payload["scaler_state"])
    torch.set_rng_state(payload["torch_rng_state"])
    if device.type == "cuda" and payload.get("cuda_rng_states") is not None:
        torch.cuda.set_rng_state_all(payload["cuda_rng_states"])
    return dict(payload["training_state"])


def train(config: dict[str, object], resume: str | None = None, run_name: str | None = None) -> Path:
    prepared = load_prepared_data(config)
    model_config = config["model"]
    training = config["training"]
    device = choose_device()
    set_seed(int(training["seed"]))

    if bool(training["amp"]) and device.type == "cuda":
        torch.set_float32_matmul_precision("high")

    model = build_model(model_config, prepared.tokenizer.vocab_size).to(device)
    optimizer = torch.optim.AdamW(
        model.parameters(), lr=float(training["learning_rate"]), weight_decay=float(training["weight_decay"])
    )
    scheduler = make_scheduler(optimizer, training)
    scaler = torch.amp.GradScaler(enabled=bool(training["amp"]) and device.type == "cuda")
    resolved_run_name = Path(resume).resolve().parent.name if resume else (run_name or next_run_name(config))
    target_checkpoint = checkpoint_path(config, resolved_run_name)
    output_dir = target_checkpoint.parent
    if output_dir.exists() and resume is None:
        raise FileExistsError(f"Checkpoint directory already exists: {output_dir}. Change run.name or use --resume.")
    output_dir.mkdir(parents=True, exist_ok=True)

    state: dict[str, object] = {
        "completed_steps": 0,
        "tokens_seen": 0,
        "timed_tokens": 0,
        "active_training_seconds": 0.0,
        "peak_gpu_memory": {"allocated_gb": None, "reserved_gb": None, "reason": "not measured"},
        "last_training_loss": None,
        "last_progress_validation_loss": None,
        "last_progress_generations": [],
        "run_name": resolved_run_name,
    }
    if resume:
        state.update(_restore_checkpoint(Path(resume), model, optimizer, scheduler, scaler, device))

    reset_peak_memory(device)
    batch_size = int(training["batch_size"])
    block_size = int(model_config["block_size"])
    max_steps = int(training["max_steps"])
    throughput_warmup = int(training["throughput_warmup_steps"])
    eval_generator = torch.Generator().manual_seed(int(training["seed"]) + 1)
    timed_since_start = False
    active_started_at: float | None = None
    train_peak = state["peak_gpu_memory"]

    def pause_timer() -> None:
        nonlocal active_started_at, train_peak
        if active_started_at is None:
            return
        synchronize(device)
        state["active_training_seconds"] = float(state["active_training_seconds"]) + time.perf_counter() - active_started_at
        active_started_at = None
        current_peak = peak_memory_gb(device)
        previous = train_peak if isinstance(train_peak, dict) else {}
        train_peak = {
            "allocated_gb": max(filter(lambda x: x is not None, [previous.get("allocated_gb"), current_peak.get("allocated_gb")]), default=None),
            "reserved_gb": max(filter(lambda x: x is not None, [previous.get("reserved_gb"), current_peak.get("reserved_gb")]), default=None),
            "reason": current_peak.get("reason", ""),
        }
        state["peak_gpu_memory"] = train_peak

    def resume_timer() -> None:
        nonlocal active_started_at
        if active_started_at is None and timed_since_start:
            reset_peak_memory(device)
            synchronize(device)
            active_started_at = time.perf_counter()

    starting_step = int(state["completed_steps"])
    progress_interval = max(1, math.ceil(max_steps / 500))
    pending_progress_updates = 0
    progress = tqdm(total=max_steps, initial=starting_step, desc="Training")

    def show_progress_postfix(training_loss: float | None, validation_loss: float, generated: list[str]) -> None:
        progress.set_postfix(
            loss="n/a" if training_loss is None else f"{training_loss:.4f}",
            val=f"{validation_loss:.4f}",
            prompt=str(training["progress_prompt"]),
            answer=" / ".join(generated),
            refresh=True,
        )

    # Establish a visible, pre-training baseline even when max_steps is below eval_interval.
    initial_validation_loss = estimate_loss(model, prepared, config, device, eval_generator)
    initial_generated = progress_generations(model, prepared.tokenizer, config, device)
    state["last_progress_validation_loss"] = initial_validation_loss
    state["last_progress_generations"] = initial_generated
    show_progress_postfix(None, initial_validation_loss, initial_generated)

    for step in range(starting_step, max_steps):
        if step >= throughput_warmup and not timed_since_start:
            timed_since_start = True
            resume_timer()

        inputs, targets = sample_batch(
            prepared.train_tokens,
            batch_size,
            block_size,
            prepared.tokenizer.bos_id,
            device,
        )
        with autocast_context(device, bool(training["amp"])):
            loss = model.loss(inputs, targets)
        optimizer.zero_grad(set_to_none=True)
        scaler.scale(loss).backward()
        scaler.unscale_(optimizer)
        scaler.step(optimizer)
        scaler.update()
        scheduler.step()

        state["completed_steps"] = step + 1
        state["tokens_seen"] = int(state["tokens_seen"]) + batch_size * block_size
        state["last_training_loss"] = float(loss.item())
        if step >= throughput_warmup:
            state["timed_tokens"] = int(state["timed_tokens"]) + batch_size * block_size

        should_eval = (step + 1) % int(training["eval_interval"]) == 0
        should_checkpoint = (step + 1) % int(training["checkpoint_interval"]) == 0
        if should_eval or should_checkpoint:
            pause_timer()

        if should_eval:
            validation_loss = estimate_loss(model, prepared, config, device, eval_generator)
            generated = progress_generations(model, prepared.tokenizer, config, device)
            state["last_progress_validation_loss"] = validation_loss
            state["last_progress_generations"] = generated
            show_progress_postfix(float(loss.item()), validation_loss, generated)

        if should_checkpoint:
            save_checkpoint(
                target_checkpoint,
                _checkpoint_payload(config, model, optimizer, scheduler, scaler, prepared.tokenizer, prepared, state),
            )

        if should_eval or should_checkpoint:
            resume_timer()

        pending_progress_updates += 1
        if pending_progress_updates >= progress_interval or step + 1 == max_steps:
            progress.update(pending_progress_updates)
            pending_progress_updates = 0
            show_progress_postfix(
                float(loss.item()),
                float(state["last_progress_validation_loss"]),
                list(state["last_progress_generations"]),
            )

    pause_timer()
    save_checkpoint(
        target_checkpoint,
        _checkpoint_payload(config, model, optimizer, scheduler, scaler, prepared.tokenizer, prepared, state),
    )
    progress.close()
    print(f"Saved checkpoint: {target_checkpoint}")
    print(f"Parameters: {parameter_counts(model)['total_m']:.3f}M")
    return target_checkpoint


def main() -> None:
    parser = argparse.ArgumentParser(description="Train TinyLM from a TOML config.")
    parser.add_argument("--config", required=True, help="Path to a TOML experiment config.")
    parser.add_argument("--resume", help="Checkpoint to resume from.")
    parser.add_argument("--run-name", help="Explicit run ID; omit to generate the next dated config ID.")
    args = parser.parse_args()
    train(load_config(args.config), args.resume, args.run_name)


if __name__ == "__main__":
    main()
