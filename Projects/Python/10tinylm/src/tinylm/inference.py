"""Stateful character-by-character inference from a saved TinyLM checkpoint."""

from __future__ import annotations

from collections import deque
from collections.abc import Mapping
from dataclasses import dataclass
from pathlib import Path
import sys
from typing import TextIO

import torch

from .config import project_root
from .model import GPTLLM, build_model
from .tokenizer import CharacterTokenizer


EOS_TEXT = "<EOS>"
MAX_NEW_TOKENS = 1_000
TEMPERATURE = 1.0
ERASE_EOS = "\b \b" * len(EOS_TEXT)


def choose_device() -> torch.device:
    return torch.device("cuda" if torch.cuda.is_available() else "cpu")


def discover_checkpoints(checkpoint_root: str | Path | None = None) -> list[Path]:
    """Return checkpoint files from newest to oldest across all training runs."""
    root = (
        Path(checkpoint_root)
        if checkpoint_root is not None
        else project_root() / "intermediate" / "checkpoints"
    )
    if not root.exists():
        return []
    checkpoints = [path for path in root.glob("*/*") if path.is_file()]
    return sorted(
        checkpoints,
        key=lambda path: (path.stat().st_mtime_ns, path.parent.name, path.name),
        reverse=True,
    )


def _require_mapping(value: object, description: str) -> Mapping[str, object]:
    if not isinstance(value, Mapping):
        raise ValueError(f"Checkpoint {description} is missing or invalid")
    return value


def load_inference_model(
    checkpoint_path: str | Path,
    device: torch.device | None = None,
) -> tuple[GPTLLM, CharacterTokenizer, torch.device]:
    """Reconstruct a model and tokenizer without loading prepared dataset files."""
    path = Path(checkpoint_path)
    resolved_device = device or choose_device()
    try:
        # Checkpoints also contain optimizer state. Keep that training-only data on
        # CPU so selecting CUDA does not waste device memory before model loading.
        checkpoint = torch.load(path, map_location="cpu", weights_only=False)
    except Exception as error:
        raise ValueError(f"Could not read TinyLM checkpoint {path}: {error}") from error

    checkpoint = _require_mapping(checkpoint, "payload")
    if checkpoint.get("format_version") != 1:
        raise ValueError(f"Unsupported checkpoint format in {path}")

    config = _require_mapping(checkpoint.get("config"), "configuration")
    model_config = _require_mapping(config.get("model"), "model configuration")
    tokenizer_data = _require_mapping(checkpoint.get("tokenizer"), "tokenizer")
    if tokenizer_data.get("version") != 1:
        raise ValueError(f"Unsupported tokenizer format in {path}")
    tokens = tokenizer_data.get("tokens")
    if not isinstance(tokens, list) or not all(isinstance(token, str) for token in tokens):
        raise ValueError(f"Checkpoint tokenizer tokens are missing or invalid in {path}")
    model_state = _require_mapping(checkpoint.get("model_state"), "model state")

    try:
        tokenizer = CharacterTokenizer(list(tokens))
        model = build_model(dict(model_config), tokenizer.vocab_size).to(resolved_device)
        model.load_state_dict(model_state)
    except (KeyError, TypeError, ValueError, RuntimeError) as error:
        raise ValueError(f"Checkpoint model is incompatible or incomplete in {path}: {error}") from error
    model.eval()
    return model, tokenizer, resolved_device


@dataclass
class InferenceSession:
    """Retain the usable model context across interactive generation turns."""

    model: GPTLLM
    tokenizer: CharacterTokenizer
    device: torch.device
    context_ids: torch.Tensor

    @classmethod
    def create(
        cls,
        model: GPTLLM,
        tokenizer: CharacterTokenizer,
        device: torch.device,
    ) -> "InferenceSession":
        context_ids = torch.tensor([[tokenizer.bos_id]], dtype=torch.long, device=device)
        return cls(model=model, tokenizer=tokenizer, device=device, context_ids=context_ids)

    def _crop_context(self) -> None:
        self.context_ids = self.context_ids[:, -self.model.block_size :]

    def append_text(self, text: str) -> int:
        """Append user text exactly and return its unknown-character count."""
        token_ids, unknown_count = self.tokenizer.encode(text)
        if token_ids:
            appended = torch.tensor([token_ids], dtype=torch.long, device=self.device)
            self.context_ids = torch.cat((self.context_ids, appended), dim=1)
            self._crop_context()
        return unknown_count

    def _remove_recent_tokens(self, count: int) -> None:
        remaining = self.context_ids.size(1) - count
        if remaining > 0:
            self.context_ids = self.context_ids[:, :remaining]
        else:
            self.context_ids = torch.tensor(
                [[self.tokenizer.bos_id]],
                dtype=torch.long,
                device=self.device,
            )

    @torch.inference_mode()
    def generate(self, output: TextIO | None = None) -> bool:
        """Stream one turn, returning whether the literal EOS marker was generated."""
        stream = sys.stdout if output is None else output
        eos_ids, eos_unknown_count = self.tokenizer.encode(EOS_TEXT)
        eos_sequence = tuple(eos_ids) if eos_unknown_count == 0 else None
        recent_ids: deque[int] = deque(maxlen=len(EOS_TEXT))
        found_eos = False

        for _ in range(MAX_NEW_TOKENS):
            next_token = self.model.sample_next_token(self.context_ids, temperature=TEMPERATURE)
            token_id = int(next_token[0, 0].item())
            self.context_ids = torch.cat((self.context_ids, next_token), dim=1)

            stream.write(self.tokenizer.decode([token_id]))
            stream.flush()
            recent_ids.append(token_id)
            if eos_sequence is not None and tuple(recent_ids) == eos_sequence:
                stream.write(ERASE_EOS)
                stream.flush()
                self._remove_recent_tokens(len(eos_ids))
                found_eos = True
                break

        # sample_next_token crops every forward pass. Cropping the retained
        # context only after EOS removal avoids losing older usable tokens to a
        # marker that is not meant to remain in the session.
        self._crop_context()
        stream.write("\n")
        stream.flush()
        return found_eos
