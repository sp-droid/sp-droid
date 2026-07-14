from __future__ import annotations

from contextlib import redirect_stdout
from io import StringIO
from pathlib import Path
import sys
from tempfile import TemporaryDirectory
import unittest
from unittest.mock import patch

import torch


PROJECT_ROOT = Path(__file__).resolve().parents[1]
sys.path.insert(0, str(PROJECT_ROOT / "src"))

from tinylm import cli  # noqa: E402
from tinylm.inference import InferenceSession  # noqa: E402
from tinylm.tokenizer import CharacterTokenizer  # noqa: E402


class AnswerPrompt:
    def __init__(self, answer: object):
        self.answer = answer

    def execute(self) -> object:
        return self.answer


class FakeModel:
    def __init__(self, token_ids: list[int], block_size: int = 64):
        self.block_size = block_size
        self._token_ids = iter(token_ids)
        self.call_count = 0

    def sample_next_token(
        self,
        input_ids: torch.Tensor,
        temperature: float = 0.0,
        generator: torch.Generator | None = None,
    ) -> torch.Tensor:
        del temperature, generator
        self.call_count += 1
        return torch.tensor([[next(self._token_ids)]], dtype=torch.long, device=input_ids.device)


def make_tokenizer(characters: str) -> CharacterTokenizer:
    return CharacterTokenizer(
        [
            *CharacterTokenizer.SPECIAL_TOKENS,
            *sorted(set(characters)),
        ]
    )


class CliTests(unittest.TestCase):
    def test_inference_is_first_and_skips_config_selection(self) -> None:
        action_prompt = AnswerPrompt("inference")
        checkpoint = Path("intermediate/checkpoints/run/checkpoint.pt")

        with (
            patch.object(cli.inquirer, "select", return_value=action_prompt) as select,
            patch.object(cli, "_choose_inference_checkpoint", return_value=checkpoint),
            patch.object(cli, "_run_inference") as run_inference,
            patch.object(cli, "_choose_config") as choose_config,
        ):
            cli.main()

        choices = select.call_args.kwargs["choices"]
        self.assertEqual(choices[0], {"name": "Inference", "value": "inference"})
        choose_config.assert_not_called()
        run_inference.assert_called_once_with(checkpoint)

    def test_missing_checkpoints_prints_actionable_message(self) -> None:
        output = StringIO()
        with (
            patch.object(cli.inquirer, "select", return_value=AnswerPrompt("inference")),
            patch.object(
                cli,
                "_choose_inference_checkpoint",
                side_effect=FileNotFoundError("Run Prepare and Train first."),
            ),
            patch.object(cli, "_run_inference") as run_inference,
            redirect_stdout(output),
        ):
            cli.main()

        run_inference.assert_not_called()
        self.assertIn("Inference unavailable: Run Prepare and Train first.", output.getvalue())

    def test_checkpoint_choice_preserves_newest_first_discovery_order(self) -> None:
        with TemporaryDirectory() as temporary_directory:
            root = Path(temporary_directory)
            newest = root / "new-run" / "checkpoint.pt"
            older = root / "old-run" / "other-name"
            selected_prompt = AnswerPrompt(newest)
            with (
                patch.object(cli, "project_root", return_value=root),
                patch.object(cli, "discover_checkpoints", return_value=[newest, older]),
                patch.object(cli.inquirer, "select", return_value=selected_prompt) as select,
            ):
                selected = cli._choose_inference_checkpoint()

        self.assertEqual(selected, newest)
        self.assertEqual(
            select.call_args.kwargs["choices"],
            [
                {"name": "new-run (checkpoint.pt)", "value": newest},
                {"name": "old-run (other-name)", "value": older},
            ],
        )

    def test_interactive_session_reuses_context_until_case_insensitive_exit(self) -> None:
        tokenizer = make_tokenizer("abxy<EOS>")
        generated_ids, _ = tokenizer.encode("x<EOS>y<EOS>")
        model = FakeModel(generated_ids)
        session = InferenceSession.create(model, tokenizer, torch.device("cpu"))
        checkpoint = Path("intermediate/checkpoints/run/checkpoint.pt")
        output = StringIO()

        with (
            patch.object(
                cli,
                "load_inference_model",
                return_value=(model, tokenizer, torch.device("cpu")),
            ) as load_model,
            patch.object(cli.InferenceSession, "create", return_value=session) as create_session,
            patch.object(
                cli.inquirer,
                "text",
                side_effect=[AnswerPrompt("a"), AnswerPrompt("b"), AnswerPrompt(" ExIt ")],
            ) as text_prompt,
            redirect_stdout(output),
        ):
            cli._run_inference(checkpoint)

        load_model.assert_called_once_with(checkpoint)
        create_session.assert_called_once_with(model, tokenizer, torch.device("cpu"))
        self.assertEqual(text_prompt.call_count, 3)
        self.assertEqual(
            text_prompt.call_args_list[0].kwargs,
            {"message": 'Continue (type "exit" to quit):', "mandatory": False},
        )
        self.assertEqual(tokenizer.decode(session.context_ids[0].tolist()), "axby")
        self.assertEqual(model.call_count, len("x<EOS>y<EOS>"))
        self.assertEqual(output.getvalue().count("Generation:"), 2)
        self.assertIn("Inference session ended.", output.getvalue())


if __name__ == "__main__":
    unittest.main()
