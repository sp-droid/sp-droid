"""Configuration loading for repeatable TinyLM experiments."""

from __future__ import annotations

from copy import deepcopy
from datetime import datetime
from pathlib import Path
import re
from typing import Any
import tomllib


REQUIRED_SECTIONS = ("run", "dataset", "model", "training", "validation")


def project_root() -> Path:
    return Path(__file__).resolve().parents[2]


def _resolve_path(root: Path, value: str | None) -> str | None:
    if not value:
        return None
    path = Path(value)
    return str((root / path).resolve()) if not path.is_absolute() else str(path.resolve())


def load_config(path: str | Path) -> dict[str, Any]:
    """Load a TOML experiment configuration and resolve project-relative paths."""
    config_path = Path(path).resolve()
    with config_path.open("rb") as file:
        config: dict[str, Any] = tomllib.load(file)

    missing = [section for section in REQUIRED_SECTIONS if section not in config]
    if missing:
        raise ValueError(f"Missing configuration sections: {', '.join(missing)}")

    root = project_root()
    config = deepcopy(config)
    dataset = config["dataset"]
    for key in ("train_path", "validation_path"):
        dataset[key] = _resolve_path(root, dataset.get(key))
    dataset.setdefault("holdout_fraction", 0.1)
    dataset.setdefault("chunk_chars", 1_048_576)

    run = config["run"]
    checkpoint_name = str(run.setdefault("checkpoint", "checkpoint_last.pt"))
    if Path(checkpoint_name).name != checkpoint_name:
        raise ValueError("run.checkpoint must be a filename, not a path")
    run.setdefault("description", "")

    config["training"].setdefault("throughput_warmup_steps", 10)
    config["training"].setdefault("checkpoint_interval", 1_000)
    config["training"].setdefault("eval_interval", 100)
    config["training"].setdefault("eval_iters", 20)
    config["training"].setdefault("amp", True)
    config["training"].setdefault("weight_decay", 0.1)
    config["training"].setdefault("warmup_steps", 200)
    config["training"].setdefault("min_learning_rate", 3e-5)
    config["training"].setdefault("progress_max_new_tokens", 10)
    config["training"].setdefault("progress_samples", 3)
    config["training"].setdefault("progress_temperature", 1.0)

    validation = config["validation"]
    validation.setdefault("lm_eval_batch_size", 8)
    validation.setdefault("hellaswag_limit", 256)
    validation.setdefault("generation_max_new_tokens", 128)
    validation.setdefault("prompts", [])

    model = config["model"]
    if model["n_embed"] % model["n_heads"]:
        raise ValueError("model.n_embed must be divisible by model.n_heads")
    if (model["n_embed"] // model["n_heads"]) % 2:
        raise ValueError("The attention head size must be even for RoPE")

    config["_meta"] = {
        "config_path": str(config_path),
        "project_root": str(root),
    }
    return config


def config_for_checkpoint(config: dict[str, Any]) -> dict[str, Any]:
    """Return a serializable config without process-specific metadata."""
    copied = deepcopy(config)
    copied.pop("_meta", None)
    return copied


def cache_dir(config: dict[str, Any]) -> Path:
    dataset_name = config["dataset"]["name"]
    return project_root() / "intermediate" / "data" / dataset_name


def next_run_name(config: dict[str, Any]) -> str:
    """Return YYYY-MM-DD_N-configname, incrementing N for matching runs."""
    config_path = Path(str(config["_meta"]["config_path"]))
    config_name = config_path.stem
    today = datetime.now().astimezone().strftime("%Y-%m-%d")
    pattern = re.compile(rf"^{re.escape(today)}_(\d+)_{re.escape(config_name)}$")
    checkpoint_root = project_root() / "intermediate" / "checkpoints"
    existing_indices = [
        int(match.group(1))
        for path in checkpoint_root.iterdir()
        if path.is_dir() and (match := pattern.fullmatch(path.name))
    ] if checkpoint_root.exists() else []
    return f"{today}_{max(existing_indices, default=-1) + 1}_{config_name}"


def checkpoint_path(config: dict[str, Any], run_name: str) -> Path:
    if not run_name or Path(run_name).name != run_name:
        raise ValueError("run name must be a simple, non-empty directory name")
    run = config["run"]
    return project_root() / "intermediate" / "checkpoints" / run_name / run["checkpoint"]


def report_path(run_name: str) -> Path:
    if not run_name or Path(run_name).name != run_name:
        raise ValueError("run name must be a simple, non-empty directory name")
    return project_root() / "output" / run_name / "validation.json"
