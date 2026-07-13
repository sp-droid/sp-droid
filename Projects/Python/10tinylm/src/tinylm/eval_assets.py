"""Generate the fixed local lm-eval task suite under intermediate/evals."""

from __future__ import annotations

import json
from pathlib import Path

from .config import project_root


def _choices(answer: str, distractors: list[str], index: int) -> list[str]:
    values = [answer, *distractors]
    shift = index % len(values)
    return values[shift:] + values[:shift]


def _record(prompt: str, answer: str, distractors: list[str], index: int) -> dict[str, object]:
    return {"prompt": prompt, "choices": _choices(answer, distractors, index), "answer": answer}


def _easy_questions() -> list[dict[str, object]]:
    facts = [
        ("Emma", "an apple", "was hungry"), ("Sam", "a coat", "was cold"),
        ("Lily", "a red crayon", "wanted to draw"), ("Noah", "his bed", "was sleepy"),
        ("Mia", "a puppy", "saw an animal"), ("Ben", "hot soup", "was hungry"),
        ("Ava", "a seed", "wanted a flower"), ("Tom", "the moon", "looked at the sky"),
        ("Ella", "soap", "washed her hands"), ("Jack", "a puddle", "walked outside"),
        ("Grace", "an umbrella", "saw rain"), ("Finn", "a shell", "walked by the sea"),
        ("Ruby", "a scarf", "felt cold"), ("Leo", "a flower", "wanted a gift"),
        ("Sofia", "music", "wanted to dance"), ("Henry", "cookies", "baked with his mother"),
        ("Owen", "a bird", "looked up"), ("Zoe", "a carrot", "fed her rabbit"),
        ("Kai", "a kite", "played in the wind"), ("Maya", "a book", "wanted to read"),
    ]
    return [
        _record(
            f"{name} {reason}. {name} used or found {answer}. What did {name} use or find?",
            f" {answer}",
            [" a spoon", " a shoe", " a cloud"],
            index,
        )
        for index, (name, answer, reason) in enumerate(facts)
    ]


def _medium_questions() -> list[dict[str, object]]:
    names = ["Mia", "Ben", "Ana", "Leo", "Nora", "Ivy", "Tom", "Ella", "Kai", "Liam"]
    objects = ["blue ball", "small key", "cookie", "toy boat", "red hat", "yellow flower", "cup", "book", "coin", "paper star"]
    recipients = ["Tom", "Mia", "Sam", "Nora", "Leo", "Ava", "Finn", "Zoe", "Owen", "Ruby"]
    questions: list[dict[str, object]] = []
    for index in range(20):
        owner = names[index % len(names)]
        recipient = recipients[index % len(recipients)]
        item = objects[index % len(objects)]
        questions.append(
            _record(
                f"{owner} put a {item} in a box. {owner} gave the box to {recipient}. Where is the {item}?",
                " in the box",
                [" on the road", " under a bed", " in the sky"],
                index,
            )
        )
    return questions


def _hard_questions() -> list[dict[str, object]]:
    questions: list[dict[str, object]] = []
    for index in range(10):
        total = 3 + (index % 4)
        given = 1 + (index % 2)
        answer = total - given - 1
        distractors = [f" {value}" for value in range(5) if value != answer][:3]
        questions.append(
            _record(
                f"A child had {total} apples. The child gave away {given} apples and ate one apple. How many apples remain?",
                f" {answer}",
                distractors,
                index,
            )
        )
    for index in range(10, 20):
        giver = ["Ben", "Ana", "Sam", "Mia", "Leo"][index % 5]
        middle = ["Ana", "Sam", "Mia", "Leo", "Ben"][index % 5]
        final = ["Sam", "Mia", "Leo", "Ben", "Ana"][index % 5]
        questions.append(
            _record(
                f"{giver} gave a red ball to {middle}. {middle} then gave the red ball to {final}. Who has the red ball now?",
                f" {final}",
                [f" {giver}", f" {middle}", " nobody"],
                index,
            )
        )
    return questions


def _yaml(task_name: str, filename: str) -> str:
    return f"""task: {task_name}
dataset_path: json
dataset_kwargs:
  data_files:
    test: intermediate/evals/tinystories_reasoning/{filename}.jsonl
test_split: test
output_type: multiple_choice
doc_to_text: \"{{{{prompt}}}}\\nAnswer:\"
doc_to_choice: \"{{{{choices}}}}\"
doc_to_target: \"{{{{answer}}}}\"
metric_list:
  - metric: acc
    aggregation: mean
    higher_is_better: true
  - metric: acc_norm
    aggregation: mean
    higher_is_better: true
metadata:
  version: 2.0
"""


def ensure_eval_suite() -> Path:
    """Materialize a deterministic 60-question task suite for lm-eval."""
    suite = project_root() / "intermediate" / "evals" / "tinystories_reasoning"
    suite.mkdir(parents=True, exist_ok=True)
    task_sets = {
        "easy": _easy_questions(),
        "medium": _medium_questions(),
        "hard": _hard_questions(),
    }
    for level, questions in task_sets.items():
        jsonl = "\n".join(json.dumps(question, ensure_ascii=False) for question in questions) + "\n"
        (suite / f"{level}.jsonl").write_text(jsonl, encoding="utf-8")
        (suite / f"{level}.yaml").write_text(_yaml(f"tinystories_reasoning_{level}", level), encoding="utf-8")
    return suite
