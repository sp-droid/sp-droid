"""lm-eval adapter for the local character transformer."""

from __future__ import annotations

from dataclasses import dataclass
from importlib import metadata
from pathlib import Path
from typing import Any, Iterable

import torch
from torch.nn import functional as F

from .config import project_root
from .eval_assets import ensure_eval_suite
from .model import GPTLLM
from .reporting import json_safe
from .tokenizer import CharacterTokenizer

try:  # Keep normal TinyLM commands importable before lm-eval is installed.
    from lm_eval.api.model import LM

    LMEVAL_IMPORT_ERROR: Exception | None = None
except ImportError as error:  # pragma: no cover - covered by installation behavior
    LM = object  # type: ignore[assignment,misc]
    LMEVAL_IMPORT_ERROR = error


@dataclass
class ScoreItem:
    index: int
    prefix: list[int]
    continuation: list[int]


class CharacterLM(LM):  # type: ignore[misc]
    """Batching adapter whose probabilities are character-token probabilities."""

    def __init__(self, model: GPTLLM, tokenizer: CharacterTokenizer, batch_size: int, device: torch.device):
        if LMEVAL_IMPORT_ERROR is not None:
            raise RuntimeError("lm-eval is required; install project dependencies before running validation.") from LMEVAL_IMPORT_ERROR
        super().__init__()
        self.model = model
        self.tokenizer = tokenizer
        self._batch_size = batch_size
        self._device = device
        self._oov_characters = 0
        self._truncated_requests = 0

    @property
    def eot_token_id(self) -> int:
        return self.tokenizer.bos_id

    @property
    def max_length(self) -> int:
        return self.model.block_size

    @property
    def max_gen_toks(self) -> int:
        return self.model.block_size - 1

    @property
    def batch_size(self) -> int:
        return self._batch_size

    @property
    def device(self) -> torch.device:
        return self._device

    @property
    def tokenizer_name(self) -> str:
        return "tinylm-character-v1"

    @property
    def counters(self) -> dict[str, int]:
        return {"oov_character_count": self._oov_characters, "truncated_request_count": self._truncated_requests}

    def _encode(self, text: str) -> list[int]:
        ids, unknown_count = self.tokenizer.encode(text)
        self._oov_characters += unknown_count
        return ids

    def _score_short_items(self, items: list[ScoreItem]) -> dict[int, tuple[float, bool]]:
        output: dict[int, tuple[float, bool]] = {}
        for start in range(0, len(items), self.batch_size):
            batch = items[start : start + self.batch_size]
            input_rows = [item.prefix + item.continuation[:-1] for item in batch]
            maximum_length = max(len(row) for row in input_rows)
            inputs = torch.full(
                (len(batch), maximum_length), self.tokenizer.bos_id, dtype=torch.long, device=self.device
            )
            for row_index, row in enumerate(input_rows):
                inputs[row_index, : len(row)] = torch.tensor(row, dtype=torch.long, device=self.device)
            with torch.inference_mode():
                logits = self.model(inputs)
            for row_index, item in enumerate(batch):
                prediction_start = len(item.prefix) - 1
                target = torch.tensor(item.continuation, dtype=torch.long, device=self.device)
                selected_logits = logits[row_index, prediction_start : prediction_start + len(target)]
                log_probabilities = F.log_softmax(selected_logits.float(), dim=-1)
                score = float(log_probabilities.gather(1, target[:, None]).sum().item())
                greedy = bool(torch.equal(selected_logits.argmax(dim=-1), target))
                output[item.index] = (score, greedy)
        return output

    def _score_long(self, prefix: list[int], continuation: list[int]) -> tuple[float, bool]:
        """Score a rare over-context continuation in bounded chunks."""
        total = 0.0
        greedy = True
        cursor = 0
        current_prefix = prefix
        while cursor < len(continuation):
            room = max(1, self.max_length - len(current_prefix) + 1)
            piece = continuation[cursor : cursor + room]
            if len(current_prefix) + len(piece) - 1 > self.max_length:
                current_prefix = [self.tokenizer.bos_id, *current_prefix[-(self.max_length - 1) :]]
                self._truncated_requests += 1
                room = max(1, self.max_length - len(current_prefix) + 1)
                piece = continuation[cursor : cursor + room]
            score, was_greedy = self._score_short_items([ScoreItem(0, current_prefix, piece)])[0]
            total += score
            greedy = greedy and was_greedy
            cursor += len(piece)
            current_prefix = [self.tokenizer.bos_id, *(current_prefix[1:] + piece)[-(self.max_length - 1) :]]
        return total, greedy

    def loglikelihood(self, requests: list[Any]) -> list[tuple[float, bool]]:
        results: list[tuple[float, bool] | None] = [None] * len(requests)
        short_items: list[ScoreItem] = []
        for index, request in enumerate(requests):
            context, continuation = request.args
            continuation_ids = self._encode(str(continuation))
            if not continuation_ids:
                results[index] = (0.0, True)
                continue
            context_ids = self._encode(str(context))
            maximum_prefix_length = self.max_length - len(continuation_ids) + 1
            if maximum_prefix_length < 1:
                results[index] = self._score_long([self.tokenizer.bos_id, *context_ids], continuation_ids)
                continue
            if len(context_ids) + 1 > maximum_prefix_length:
                context_ids = context_ids[-(maximum_prefix_length - 1) :]
                self._truncated_requests += 1
            prefix = [self.tokenizer.bos_id, *context_ids]
            short_items.append(ScoreItem(index, prefix, continuation_ids))

        if short_items:
            scored = self._score_short_items(short_items)
            for item in short_items:
                results[item.index] = scored[item.index]
        return [item if item is not None else (0.0, False) for item in results]

    def loglikelihood_rolling(self, requests: list[Any]) -> list[tuple[float]]:
        result: list[tuple[float]] = []
        for request in requests:
            text = str(request.args[0])
            score, _ = self.loglikelihood([type("Request", (), {"args": ("", text)})()])[0]
            result.append((score,))
        return result

    def generate_until(self, requests: list[Any]) -> list[str]:
        generated_text: list[str] = []
        for request in requests:
            context, generation_kwargs = request.args
            context_ids = self._encode(str(context))
            max_new_tokens = min(int(generation_kwargs.get("max_gen_toks", self.max_gen_toks)), self.max_gen_toks)
            input_ids = torch.tensor([[self.tokenizer.bos_id, *context_ids]], dtype=torch.long, device=self.device)
            with torch.inference_mode():
                output = self.model.generate(input_ids, max_new_tokens=max_new_tokens, temperature=0.0)
            completion = self.tokenizer.decode(output[0, input_ids.size(1) :].tolist())
            until = generation_kwargs.get("until", [])
            if isinstance(until, str):
                until = [until]
            positions = [completion.find(marker) for marker in until if marker and completion.find(marker) >= 0]
            if positions:
                completion = completion[: min(positions)]
            generated_text.append(completion)
        return generated_text


def _response_score(response: Any) -> float | None:
    """Extract the log-likelihood from lm-eval's nested sample response."""
    while isinstance(response, list) and len(response) == 1:
        response = response[0]
    if isinstance(response, (list, tuple)) and response and isinstance(response[0], (int, float)):
        return float(response[0])
    return None


def _choice_index(choices: list[Any], target: Any) -> int | None:
    """Resolve lm-eval's text or integer answer target to a choice index."""
    if isinstance(target, int) and 0 <= target < len(choices):
        return target
    for index, choice in enumerate(choices):
        if choice == target:
            return index
    if isinstance(target, str) and target.strip().isdigit():
        numeric_target = int(target.strip())
        if 0 <= numeric_target < len(choices):
            return numeric_target
    return None


def _multiple_choice_examples(
    raw_results: dict[str, Any],
    task_names: list[str],
    prompt_fields: tuple[str, ...],
    answer_fields: tuple[str, ...],
) -> dict[str, list[dict[str, object]]]:
    """Keep compact question-level outcomes from lm-eval's logged samples."""
    logged_samples = raw_results.get("samples", {})
    examples: dict[str, list[dict[str, object]]] = {}
    for task_name in task_names:
        task_examples: list[dict[str, object]] = []
        for sample in logged_samples.get(task_name, []):
            document = sample.get("doc", {})
            choices = list(document.get("choices", []))
            scores = [_response_score(response) for response in sample.get("filtered_resps", [])]
            if not choices or len(scores) != len(choices) or any(score is None for score in scores):
                continue
            selected_index = max(range(len(choices)), key=lambda index: scores[index])
            expected = next((document[field] for field in answer_fields if field in document), sample.get("target"))
            expected_index = _choice_index(choices, expected)
            if expected_index is None:
                continue
            prompt = next((document[field] for field in prompt_fields if field in document), "")
            task_examples.append(
                {
                    "prompt": prompt,
                    "choices": choices,
                    "expected_choice_index": expected_index,
                    "model_choice_index": selected_index,
                    "expected_answer": choices[expected_index],
                    "model_answer": choices[selected_index],
                    "correct": selected_index == expected_index,
                    "choice_log_likelihoods": scores,
                }
            )
        examples[task_name] = task_examples
    return examples


def _hellaswag_analysis(examples: list[dict[str, object]]) -> dict[str, object]:
    """Summarize answer-position bias without dumping 256 long examples into Markdown."""
    matrix = [[0 for _ in range(4)] for _ in range(4)]
    for example in examples:
        expected = example.get("expected_choice_index")
        selected = example.get("model_choice_index")
        if isinstance(expected, int) and isinstance(selected, int) and expected < 4 and selected < 4:
            matrix[expected][selected] += 1
    correct = sum(1 for example in examples if example["correct"])
    return {
        "samples": len(examples),
        "correct": correct,
        "incorrect": len(examples) - correct,
        "choice_confusion_matrix": matrix,
        "matrix_rows": "gold choice A-D",
        "matrix_columns": "model choice A-D",
    }


def run_lm_eval(
    model: GPTLLM,
    tokenizer: CharacterTokenizer,
    config: dict[str, object],
    device: torch.device,
    include_hellaswag: bool = True,
) -> dict[str, object]:
    """Run local TinyStories reasoning tasks plus the fixed HellaSwag slice."""
    if LMEVAL_IMPORT_ERROR is not None:
        raise RuntimeError("lm-eval is required for final validation.") from LMEVAL_IMPORT_ERROR
    import lm_eval
    from lm_eval.tasks import TaskManager

    validation = config["validation"]
    assert isinstance(validation, dict)
    adapter = CharacterLM(model, tokenizer, int(validation["lm_eval_batch_size"]), device)
    ensure_eval_suite()
    tasks = ["tinystories_reasoning_easy", "tinystories_reasoning_medium", "tinystories_reasoning_hard"]
    if include_hellaswag:
        tasks.append("hellaswag")
    task_manager = TaskManager(include_path=str(project_root() / "intermediate" / "evals"))
    raw_results = lm_eval.simple_evaluate(
        model=adapter,
        tasks=tasks,
        task_manager=task_manager,
        num_fewshot=0,
        batch_size=int(validation["lm_eval_batch_size"]),
        limit=int(validation["hellaswag_limit"]),
        log_samples=True,
    )

    def metric(result: dict[str, object], name: str) -> float | None:
        for key, value in result.items():
            if key == name or key.startswith(f"{name},"):
                return float(value)
        return None

    raw_task_results = raw_results.get("results", {})
    raw_samples = raw_results.get("n-samples", {})
    custom_names = ["tinystories_reasoning_easy", "tinystories_reasoning_medium", "tinystories_reasoning_hard"]
    local_examples_by_task = _multiple_choice_examples(
        raw_results,
        custom_names,
        prompt_fields=("prompt",),
        answer_fields=("answer",),
    )
    examples = {
        task_name.removeprefix("tinystories_reasoning_"): local_examples_by_task[task_name]
        for task_name in custom_names
    }
    hellaswag_examples = _multiple_choice_examples(
        raw_results,
        ["hellaswag"] if include_hellaswag else [],
        prompt_fields=("query", "prompt"),
        answer_fields=("gold", "label", "answer"),
    ).get("hellaswag", [])
    # The JSON retains compact per-question results. Raw lm-eval samples are
    # substantially larger and contain duplicate task metadata.
    raw_results.pop("samples", None)
    levels: dict[str, object] = {}
    total_samples = 0
    weighted_accuracy = 0.0
    weighted_normalized_accuracy = 0.0
    for task_name in custom_names:
        result = raw_task_results.get(task_name, {})
        sample_metadata = raw_samples.get(task_name, 0)
        sample_count = int(sample_metadata.get("effective", 0) if isinstance(sample_metadata, dict) else sample_metadata)
        accuracy = metric(result, "acc")
        normalized_accuracy = metric(result, "acc_norm")
        levels[task_name.removeprefix("tinystories_reasoning_")] = {
            "samples": sample_count,
            "accuracy": accuracy,
            "accuracy_stderr": metric(result, "acc_stderr"),
            "normalized_accuracy": normalized_accuracy,
            "normalized_accuracy_stderr": metric(result, "acc_norm_stderr"),
        }
        if sample_count and accuracy is not None and normalized_accuracy is not None:
            total_samples += sample_count
            weighted_accuracy += sample_count * accuracy
            weighted_normalized_accuracy += sample_count * normalized_accuracy
    aggregate = {
        "samples": total_samples,
        "accuracy": weighted_accuracy / total_samples if total_samples else None,
        "normalized_accuracy": weighted_normalized_accuracy / total_samples if total_samples else None,
    }
    if total_samples and aggregate["accuracy"] is not None and aggregate["normalized_accuracy"] is not None:
        aggregate["accuracy_stderr"] = (aggregate["accuracy"] * (1 - aggregate["accuracy"]) / total_samples) ** 0.5
        aggregate["normalized_accuracy_stderr"] = (
            aggregate["normalized_accuracy"] * (1 - aggregate["normalized_accuracy"]) / total_samples
        ) ** 0.5
    try:
        package_version = metadata.version("lm-eval")
    except metadata.PackageNotFoundError:  # pragma: no cover - only happens with nonstandard installs
        package_version = None
    return {
        "status": "completed",
        "package_version": package_version,
        "settings": {
            "num_fewshot": 0,
            "hellaswag_limit": int(validation["hellaswag_limit"]),
            "include_hellaswag": include_hellaswag,
        },
        "counters": adapter.counters,
        "tinystories_reasoning": {"levels": levels, "aggregate": aggregate, "examples": examples},
        "hellaswag": {"examples": hellaswag_examples, "analysis": _hellaswag_analysis(hellaswag_examples)},
        "results": json_safe(raw_results),
    }
