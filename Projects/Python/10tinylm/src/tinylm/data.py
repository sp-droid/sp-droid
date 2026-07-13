"""Streaming preparation and memory-mapped token datasets."""

from __future__ import annotations

from dataclasses import dataclass
from hashlib import sha256
import json
from pathlib import Path
from typing import Iterator

import numpy as np
import torch

from .config import cache_dir
from .tokenizer import CharacterTokenizer


METADATA_FILE = "metadata.json"
TOKENIZER_FILE = "tokenizer.json"


def _read_chunks(path: Path, chunk_chars: int) -> Iterator[str]:
    with path.open("r", encoding="utf-8", newline="") as file:
        while chunk := file.read(chunk_chars):
            yield chunk


def _source_identity(path: Path) -> dict[str, int | str]:
    stat = path.stat()
    return {"path": str(path.resolve()), "size_bytes": stat.st_size, "mtime_ns": stat.st_mtime_ns}


def _matches_source(metadata: dict[str, object], path: Path | None, key: str) -> bool:
    if path is None:
        return metadata.get(key) is None
    return metadata.get(key) == _source_identity(path)


def _write_token_file(
    source: Path,
    destination: Path,
    tokenizer: CharacterTokenizer,
    chunk_chars: int,
) -> dict[str, object]:
    destination.parent.mkdir(parents=True, exist_ok=True)
    unknown_count = 0
    token_count = 0
    digest = sha256()
    with destination.open("wb") as output:
        for chunk in _read_chunks(source, chunk_chars):
            digest.update(chunk.encode("utf-8"))
            ids, chunk_unknown_count = tokenizer.encode(chunk)
            unknown_count += chunk_unknown_count
            token_count += len(ids)
            np.asarray(ids, dtype=np.uint8).tofile(output)
    return {
        "token_count": token_count,
        "unknown_character_count": unknown_count,
        "content_sha256": digest.hexdigest(),
    }


def prepare_data(config: dict[str, object], force: bool = False) -> Path:
    """Create reusable uint8 token files without holding a corpus in RAM."""
    dataset = config["dataset"]
    assert isinstance(dataset, dict)
    train_path = Path(str(dataset["train_path"]))
    validation_value = dataset.get("validation_path")
    validation_path = Path(str(validation_value)) if validation_value else None
    chunk_chars = int(dataset["chunk_chars"])
    destination = cache_dir(config)
    metadata_path = destination / METADATA_FILE

    if not force and metadata_path.exists() and (destination / TOKENIZER_FILE).exists():
        metadata = json.loads(metadata_path.read_text(encoding="utf-8"))
        if (
            metadata.get("format_version") == 1
            and _matches_source(metadata, train_path, "train_source")
            and _matches_source(metadata, validation_path, "validation_source")
        ):
            return destination

    tokenizer = CharacterTokenizer.fit_chunks(_read_chunks(train_path, chunk_chars))
    if tokenizer.vocab_size > np.iinfo(np.uint8).max + 1:
        raise ValueError("Character vocabulary is too large for uint8 token storage")

    destination.mkdir(parents=True, exist_ok=True)
    tokenizer.save(destination / TOKENIZER_FILE)
    train_info = _write_token_file(train_path, destination / "train.bin", tokenizer, chunk_chars)
    validation_info: dict[str, object] | None = None
    if validation_path is not None:
        validation_info = _write_token_file(
            validation_path, destination / "validation.bin", tokenizer, chunk_chars
        )

    metadata = {
        "format_version": 1,
        "dataset_name": dataset["name"],
        "train_source": _source_identity(train_path),
        "validation_source": _source_identity(validation_path) if validation_path else None,
        "train": train_info,
        "validation": validation_info,
        "holdout_fraction": float(dataset["holdout_fraction"]),
        "tokenizer_vocab_size": tokenizer.vocab_size,
    }
    metadata_path.write_text(json.dumps(metadata, indent=2, sort_keys=True), encoding="utf-8")
    return destination


@dataclass(frozen=True)
class PreparedData:
    tokenizer: CharacterTokenizer
    train_tokens: np.memmap | np.ndarray
    validation_tokens: np.memmap | np.ndarray
    metadata: dict[str, object]

    @property
    def train_token_count(self) -> int:
        return len(self.train_tokens)

    @property
    def validation_token_count(self) -> int:
        return len(self.validation_tokens)


def load_prepared_data(config: dict[str, object], prepare_if_missing: bool = False) -> PreparedData:
    location = cache_dir(config)
    metadata_path = location / METADATA_FILE
    if not metadata_path.exists():
        if not prepare_if_missing:
            raise FileNotFoundError(
                f"Prepared data is missing at {location}. Run `python -m tinylm.prepare` first."
            )
        prepare_data(config)

    metadata = json.loads(metadata_path.read_text(encoding="utf-8"))
    tokenizer = CharacterTokenizer.load(location / TOKENIZER_FILE)
    all_train = np.memmap(location / "train.bin", dtype=np.uint8, mode="r")
    if metadata.get("validation") is not None:
        validation = np.memmap(location / "validation.bin", dtype=np.uint8, mode="r")
        train = all_train
    else:
        split = int((1.0 - float(metadata["holdout_fraction"])) * len(all_train))
        train = all_train[:split]
        validation = all_train[split:]

    return PreparedData(tokenizer=tokenizer, train_tokens=train, validation_tokens=validation, metadata=metadata)


def sample_batch(
    tokens: np.memmap | np.ndarray,
    batch_size: int,
    block_size: int,
    bos_id: int,
    device: torch.device,
    generator: torch.Generator | None = None,
) -> tuple[torch.Tensor, torch.Tensor]:
    """Sample blocks whose first prediction is conditioned on a trained BOS token."""
    if len(tokens) < block_size:
        raise ValueError(f"Need at least {block_size} validation tokens, found {len(tokens)}")
    maximum_start = len(tokens) - block_size
    starts = torch.randint(0, maximum_start + 1, (batch_size,), generator=generator).numpy()
    targets_np = np.stack([np.asarray(tokens[start : start + block_size]) for start in starts])
    inputs_np = np.empty_like(targets_np)
    inputs_np[:, 0] = bos_id
    inputs_np[:, 1:] = targets_np[:, :-1]
    return (
        torch.from_numpy(inputs_np.astype(np.int64, copy=False)).to(device),
        torch.from_numpy(targets_np.astype(np.int64, copy=False)).to(device),
    )
