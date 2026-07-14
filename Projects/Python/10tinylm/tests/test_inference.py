from __future__ import annotations

from io import StringIO
import os
from pathlib import Path
import sys
from tempfile import TemporaryDirectory
import unittest

import torch


PROJECT_ROOT = Path(__file__).resolve().parents[1]
sys.path.insert(0, str(PROJECT_ROOT / "src"))

from tinylm.inference import (  # noqa: E402
    ERASE_EOS,
    MAX_NEW_TOKENS,
    TEMPERATURE,
    InferenceSession,
    discover_checkpoints,
    load_inference_model,
)
from tinylm.model import build_model  # noqa: E402
from tinylm.tokenizer import CharacterTokenizer  # noqa: E402


def make_tokenizer(characters: str) -> CharacterTokenizer:
    return CharacterTokenizer(
        [
            *CharacterTokenizer.SPECIAL_TOKENS,
            *sorted(set(characters)),
        ]
    )


class FakeModel:
    def __init__(self, token_ids: list[int], block_size: int = 64):
        self.block_size = block_size
        self._token_ids = iter(token_ids)
        self.call_count = 0
        self.temperatures: list[float] = []

    def sample_next_token(
        self,
        input_ids: torch.Tensor,
        temperature: float = 0.0,
        generator: torch.Generator | None = None,
    ) -> torch.Tensor:
        del generator
        self.call_count += 1
        self.temperatures.append(temperature)
        return torch.tensor([[next(self._token_ids)]], dtype=torch.long, device=input_ids.device)


class RecordingStream(StringIO):
    def __init__(self) -> None:
        super().__init__()
        self.flush_count = 0

    def flush(self) -> None:
        self.flush_count += 1


class CheckpointTests(unittest.TestCase):
    def test_discover_checkpoints_returns_files_newest_first(self) -> None:
        with TemporaryDirectory() as temporary_directory:
            root = Path(temporary_directory)
            old = root / "old-run" / "checkpoint.bin"
            new = root / "new-run" / "custom-name"
            old.parent.mkdir()
            new.parent.mkdir()
            old.write_bytes(b"old")
            new.write_bytes(b"new")
            os.utime(old, ns=(1_000_000_000, 1_000_000_000))
            os.utime(new, ns=(2_000_000_000, 2_000_000_000))

            self.assertEqual(discover_checkpoints(root), [new, old])

    def test_discover_checkpoints_handles_missing_directory(self) -> None:
        with TemporaryDirectory() as temporary_directory:
            missing = Path(temporary_directory) / "missing"
            self.assertEqual(discover_checkpoints(missing), [])

    def test_checkpoint_load_is_self_contained(self) -> None:
        tokenizer = make_tokenizer("a<EOS>")
        model_config: dict[str, object] = {
            "block_size": 8,
            "n_embed": 4,
            "n_heads": 1,
            "n_layers": 1,
        }
        model = build_model(model_config, tokenizer.vocab_size)
        payload = {
            "format_version": 1,
            "config": {"model": model_config},
            "tokenizer": tokenizer.to_dict(),
            "model_state": model.state_dict(),
        }

        with TemporaryDirectory() as temporary_directory:
            checkpoint = Path(temporary_directory) / "checkpoint.pt"
            torch.save(payload, checkpoint)
            loaded_model, loaded_tokenizer, device = load_inference_model(
                checkpoint,
                device=torch.device("cpu"),
            )

        self.assertEqual(device, torch.device("cpu"))
        self.assertEqual(loaded_tokenizer.tokens, tokenizer.tokens)
        self.assertEqual(loaded_model.block_size, 8)
        self.assertFalse(loaded_model.training)

    def test_incompatible_checkpoint_has_actionable_error(self) -> None:
        with TemporaryDirectory() as temporary_directory:
            checkpoint = Path(temporary_directory) / "checkpoint.pt"
            torch.save({"format_version": 999}, checkpoint)
            with self.assertRaisesRegex(ValueError, "Unsupported checkpoint format"):
                load_inference_model(checkpoint, device=torch.device("cpu"))


class ModelGenerationTests(unittest.TestCase):
    def test_generate_preserves_greedy_context_cropping_behavior(self) -> None:
        torch.manual_seed(42)
        tokenizer = make_tokenizer("abcdef")
        model = build_model(
            {
                "block_size": 4,
                "n_embed": 4,
                "n_heads": 1,
                "n_layers": 1,
            },
            tokenizer.vocab_size,
        )
        input_ids = torch.tensor([[2, 3, 4, 5, 6, 7]], dtype=torch.long)
        expected = input_ids
        for _ in range(3):
            logits = model(expected[:, -model.block_size :])[:, -1, :]
            next_token = torch.argmax(logits, dim=-1, keepdim=True)
            expected = torch.cat((expected, next_token), dim=1)

        generated = model.generate(input_ids, max_new_tokens=3, temperature=0.0)

        self.assertTrue(torch.equal(generated, expected))


class InferenceSessionTests(unittest.TestCase):
    def test_eos_is_streamed_erased_and_removed_from_context(self) -> None:
        tokenizer = make_tokenizer("promptabc<EOS>")
        generated_ids, unknown_count = tokenizer.encode("abc<EOS>")
        self.assertEqual(unknown_count, 0)
        model = FakeModel(generated_ids)
        session = InferenceSession.create(model, tokenizer, torch.device("cpu"))
        session.append_text("prompt")
        output = RecordingStream()

        found_eos = session.generate(output)

        self.assertTrue(found_eos)
        self.assertEqual(output.getvalue(), f"abc<EOS>{ERASE_EOS}\n")
        self.assertEqual(tokenizer.decode(session.context_ids[0].tolist()), "promptabc")
        self.assertEqual(model.call_count, len("abc<EOS>"))
        self.assertEqual(model.temperatures, [TEMPERATURE] * model.call_count)
        self.assertGreaterEqual(output.flush_count, model.call_count)

    def test_generation_falls_back_after_exactly_one_thousand_tokens(self) -> None:
        tokenizer = make_tokenizer("a<EOS>")
        a_id = tokenizer.stoi["a"]
        model = FakeModel([a_id] * MAX_NEW_TOKENS, block_size=16)
        session = InferenceSession.create(model, tokenizer, torch.device("cpu"))
        output = RecordingStream()

        found_eos = session.generate(output)

        self.assertFalse(found_eos)
        self.assertEqual(model.call_count, 1_000)
        self.assertEqual(output.getvalue(), "a" * 1_000 + "\n")
        self.assertEqual(session.context_ids.size(1), model.block_size)

    def test_context_is_reused_across_turns_without_eos(self) -> None:
        tokenizer = make_tokenizer("abcxy<EOS>")
        generated_ids, _ = tokenizer.encode("x<EOS>y<EOS>")
        model = FakeModel(generated_ids)
        session = InferenceSession.create(model, tokenizer, torch.device("cpu"))

        session.append_text("a")
        self.assertTrue(session.generate(StringIO()))
        session.append_text("b")
        self.assertTrue(session.generate(StringIO()))

        self.assertEqual(tokenizer.decode(session.context_ids[0].tolist()), "axby")

    def test_erased_eos_does_not_displace_older_full_context(self) -> None:
        tokenizer = make_tokenizer("abcdefx<EOS>")
        generated_ids, _ = tokenizer.encode("x<EOS>")
        model = FakeModel(generated_ids, block_size=6)
        session = InferenceSession.create(model, tokenizer, torch.device("cpu"))
        session.append_text("abcdef")

        self.assertTrue(session.generate(StringIO()))

        self.assertEqual(tokenizer.decode(session.context_ids[0].tolist()), "bcdefx")

    def test_context_crops_and_empty_input_adds_nothing(self) -> None:
        tokenizer = make_tokenizer("abcdef")
        model = FakeModel([], block_size=4)
        session = InferenceSession.create(model, tokenizer, torch.device("cpu"))

        session.append_text("abcdef")
        context_before_empty = session.context_ids.clone()
        unknown_count = session.append_text("")

        self.assertEqual(unknown_count, 0)
        self.assertTrue(torch.equal(session.context_ids, context_before_empty))
        self.assertEqual(tokenizer.decode(session.context_ids[0].tolist()), "cdef")

    def test_user_typed_eos_is_not_a_stop_condition(self) -> None:
        tokenizer = make_tokenizer("a<EOS>")
        generated_ids, _ = tokenizer.encode("a<EOS>")
        model = FakeModel(generated_ids)
        session = InferenceSession.create(model, tokenizer, torch.device("cpu"))

        session.append_text("<EOS>")
        self.assertTrue(session.generate(StringIO()))

        self.assertEqual(tokenizer.decode(session.context_ids[0].tolist()), "<EOS>a")


if __name__ == "__main__":
    unittest.main()
