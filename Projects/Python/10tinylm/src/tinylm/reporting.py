"""JSON-safe reporting and reproducibility metadata."""

from __future__ import annotations

from datetime import datetime, timezone
import json
from pathlib import Path
import subprocess
from typing import Any

import numpy as np
import torch

from .config import project_root


def _git(command: list[str]) -> str | None:
    try:
        completed = subprocess.run(
            ["git", *command],
            cwd=project_root(),
            capture_output=True,
            check=True,
            text=True,
        )
    except (FileNotFoundError, subprocess.CalledProcessError):
        return None
    return completed.stdout.strip()


def provenance(device: torch.device) -> dict[str, object]:
    return {
        "created_at_utc": datetime.now(timezone.utc).isoformat(),
        "git_commit": _git(["rev-parse", "HEAD"]),
        "git_dirty": bool(_git(["status", "--porcelain"])),
        "torch_version": torch.__version__,
        "cuda_available": bool(torch.cuda.is_available()),
        "cuda_version": torch.version.cuda,
        "device": str(device),
        "gpu_name": torch.cuda.get_device_name(device) if device.type == "cuda" and torch.cuda.is_available() else None,
    }


def json_safe(value: Any) -> Any:
    if isinstance(value, Path):
        return str(value)
    if isinstance(value, np.generic):
        return value.item()
    if isinstance(value, np.ndarray):
        return value.tolist()
    if isinstance(value, torch.Tensor):
        return value.detach().cpu().tolist()
    if isinstance(value, dict):
        return {str(key): json_safe(item) for key, item in value.items()}
    if isinstance(value, (list, tuple)):
        return [json_safe(item) for item in value]
    if isinstance(value, (str, int, float, bool)) or value is None:
        return value
    return str(value)


def write_json(path: str | Path, report: dict[str, object]) -> None:
    destination = Path(path)
    destination.parent.mkdir(parents=True, exist_ok=True)
    destination.write_text(json.dumps(json_safe(report), indent=2, ensure_ascii=False, sort_keys=True) + "\n", encoding="utf-8")


def _markdown_cell(value: Any) -> str:
    if value is None:
        return "—"
    if isinstance(value, float):
        return f"{value:.4f}"
    return str(value).replace("|", "\\|").replace("\n", " ")


def _rate(value: Any) -> str:
    return "—" if value is None else f"{float(value):,.0f}"


def _table(headers: list[str], rows: list[list[Any]]) -> list[str]:
    return [
        "| " + " | ".join(headers) + " |",
        "| " + " | ".join("---" for _ in headers) + " |",
        *["| " + " | ".join(_markdown_cell(value) for value in row) + " |" for row in rows],
    ]


def _metric(task: dict[str, Any], name: str) -> Any:
    for key, value in task.items():
        if key == name or key.startswith(f"{name},"):
            return value
    return None


def _example_choice_index(example: dict[str, Any], name: str) -> int | None:
    index = example.get(f"{name}_choice_index")
    if isinstance(index, int):
        return index
    answer = example.get(f"{name}_answer")
    for choice_index, choice in enumerate(example.get("choices", [])):
        if choice == answer:
            return choice_index
    return None


def _answer_row(index: int, example: dict[str, Any]) -> list[Any]:
    """Compact multiple-choice outcome used for both evaluation suites."""
    is_correct = bool(example["correct"]) if "correct" in example else (
        _example_choice_index(example, "model") is not None
        and _example_choice_index(example, "model") == _example_choice_index(example, "expected")
    )
    return [
        index,
        example.get("prompt"),
        f"**{str(example.get('expected_answer', '—')).strip()}**",
        str(example.get("model_answer", "—")).strip(),
        "✅" if is_correct else "❌",
    ]


def _evenly_spaced(items: list[tuple[int, dict[str, Any]]], count: int) -> list[tuple[int, dict[str, Any]]]:
    if count <= 0:
        return []
    if count >= len(items):
        return items
    if count == 1:
        return [items[len(items) // 2]]
    return [items[round(position * (len(items) - 1) / (count - 1))] for position in range(count)]


def _proportional_examples(examples: list[dict[str, Any]], maximum: int = 30) -> list[tuple[int, dict[str, Any]]]:
    """Select a deterministic, evenly spread subset with the observed outcome mix."""
    indexed_examples = list(enumerate(examples, start=1))
    count = min(maximum, len(indexed_examples))
    if not count:
        return []
    correct = [item for item in indexed_examples if item[1].get("correct")]
    errors = [item for item in indexed_examples if not item[1].get("correct")]
    correct_count = round(count * len(correct) / len(indexed_examples))
    selected = [*_evenly_spaced(correct, correct_count), *_evenly_spaced(errors, count - correct_count)]
    return sorted(selected, key=lambda item: item[0])


def render_markdown(report: dict[str, Any]) -> str:
    """Render the readable companion to validation.json."""
    checkpoint = report["checkpoint"]
    run_name = Path(str(checkpoint["path"])).parent.name
    validation = report["validation"]
    training = report["training"]
    data = report["data"]
    model = report["model"]
    provenance_data = report["provenance"]
    lines = ["[toc]", "", f"# Validation report: `{run_name}`", ""]
    description = report.get("description", report.get("comment", ""))
    if description:
        lines.extend([f"> {description}", ""])

    lines.extend(["## At a glance", ""])
    lines.extend(
        _table(
            ["Validation loss", "Perplexity", "Train loss", "Steps", "Parameters"],
            [[
                validation["cross_entropy_loss_nats"],
                validation["perplexity"],
                training["last_loss"],
                checkpoint["completed_steps"],
                f"{model['parameter_count_m']:.3f}M",
            ]],
        )
    )

    lines.extend(["", "## Data and performance", ""])
    training_characters_m = data.get("training_dataset_characters_m", data["training_dataset_tokens_m"])
    lines.extend(
        _table(
            ["Training data characters", "Training data tokens", "Training tokens seen", "Data-equivalent passes", "Train tokens/sec"],
            [[
                f"{training_characters_m:.3f}M",
                f"{data['training_dataset_tokens_m']:.3f}M",
                f"{data['training_tokens_seen_m']:.3f}M",
                data["training_tokens_to_dataset_ratio"],
                _rate(training["tokens_per_second"]),
            ]],
        )
    )
    validation_rate = validation.get("inference_tokens_per_second", validation.get("tokens_per_second"))
    lines.extend(["", "### Full validation performance", ""])
    lines.extend(
        _table(
            ["Scored tokens", "Elapsed seconds", "Validation inference tokens/sec"],
            [[validation["scored_tokens"], validation["elapsed_seconds"], _rate(validation_rate)]],
        )
    )
    lines.extend(
        [
            "",
            "This rate covers the forward-only held-out-loss pass, so it is not comparable to the training rate (which includes backward propagation and optimizer updates).",
        ]
    )
    lines.append("")
    lines.extend(
        _table(
            ["Training peak allocated (GB)", "Training peak reserved (GB)", "Validation peak allocated (GB)", "GPU"],
            [[
                training["peak_gpu_memory"]["allocated_gb"],
                training["peak_gpu_memory"]["reserved_gb"],
                validation["peak_gpu_memory"]["allocated_gb"],
                provenance_data["gpu_name"],
            ]],
        )
    )

    lines.extend(["", "## Deterministic generations", ""])
    for generation in report["qualitative_generations"]:
        lines.extend(
            [
                f"### {str(generation['level']).title()}",
                "",
                "**Prompt**",
                "```text",
                str(generation["prompt"]),
                "```",
                "**Completion**",
                "```text",
                str(generation["completion"]),
                "```",
                "",
            ]
        )

    harness = report["lm_eval"]
    lines.extend(["## Evaluation harness", "", "### Summary", ""])
    if harness.get("status") != "completed":
        lines.append(f"lm-eval: **{harness.get('status')}** — {harness.get('reason', '')}")
    else:
        reasoning = harness["tinystories_reasoning"]
        levels = reasoning["levels"]
        rows = [
            [level.title(), values["samples"], values["accuracy"], values["normalized_accuracy"], values["normalized_accuracy_stderr"]]
            for level, values in levels.items()
        ]
        aggregate = reasoning["aggregate"]
        rows.append(["All", aggregate["samples"], aggregate["accuracy"], aggregate["normalized_accuracy"], aggregate["normalized_accuracy_stderr"]])
        lines.extend(_table(["TinyStories reasoning", "Samples", "Accuracy", "Length-normalized accuracy", "Norm. stderr"], rows))

        raw_results = harness["results"].get("results", {})
        hellaswag = raw_results.get("hellaswag")
        if hellaswag:
            lines.extend(["", "#### HellaSwag", ""])
            lines.extend(
                _table(
                    ["Samples", "Accuracy", "Length-normalized accuracy", "OOV characters"],
                    [[hellaswag.get("sample_len"), _metric(hellaswag, "acc"), _metric(hellaswag, "acc_norm"), harness["counters"]["oov_character_count"]]],
                )
            )
            hellaswag_details = harness.get("hellaswag", {})
            hellaswag_analysis = hellaswag_details.get("analysis", {})
            matrix = hellaswag_analysis.get("choice_confusion_matrix", [])
            if matrix:
                lines.extend(["", "##### Answer-position confusion", ""])
                lines.extend(
                    _table(
                        ["Gold \\ Model", "A", "B", "C", "D"],
                        [[chr(ord("A") + row_index), *row] for row_index, row in enumerate(matrix)],
                    )
                )
                lines.extend(
                    [
                        "",
                        "Each cell is a count: its row is the correct answer position and its column is the position selected by the model.",
                    ]
                )
            hellaswag_examples = hellaswag_details.get("examples", [])
            if hellaswag_examples:
                selected_examples = _proportional_examples(hellaswag_examples, maximum=30)
                correct_count = sum(1 for _, example in selected_examples if example.get("correct"))
                error_count = len(selected_examples) - correct_count
                lines.extend(["", "##### Representative examples", ""])
                lines.append(
                    f"These {len(selected_examples)} deterministically selected examples preserve the observed result mix: "
                    f"{correct_count} correct and {error_count} incorrect. All 256 compact item results, including every option, are retained in `validation.json`."
                )
                lines.append("")
                rows = [_answer_row(index, example) for index, example in selected_examples]
                lines.extend(_table(["#", "Prompt", "Correct ending", "Model ending", "Result"], rows))

        lines.extend(["", "### TinyStories reasoning details", ""])
        examples = reasoning.get("examples", {})
        if not examples:
            lines.append("Detailed examples were not recorded in this report. Run `validate` again to generate them.")
        else:
            for level in ("easy", "medium", "hard"):
                level_examples = examples.get(level, [])
                if not level_examples:
                    continue
                lines.extend(["", f"#### {level.title()}", ""])
                rows = [_answer_row(index, example) for index, example in enumerate(level_examples, start=1)]
                lines.extend(_table(["#", "Prompt", "Correct answer", "Model answer", "Result"], rows))
    return "\n".join(lines) + "\n"


def write_markdown(path: str | Path, report: dict[str, Any]) -> None:
    destination = Path(path)
    destination.parent.mkdir(parents=True, exist_ok=True)
    destination.write_text(render_markdown(json_safe(report)), encoding="utf-8")
