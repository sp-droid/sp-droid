# TinyLM experiment workflow

This project trains a character-level RoPE/SwiGLU transformer on TinyStories or
Shakespeare. The supported workflow is interactive and produces a stable,
machine-readable final validation report.

## Run an experiment

Run the menu and select **Inference**, **Prepare**, **Train**, **Validate**, or
**All**. Experiment actions then let you choose a TOML file from
`input/configs/`.

```powershell
uv run tinylm
```

Each training invocation gets an ID in `YYYY-MM-DD_N-configname` format, where
`N` starts at `0` and increments for runs made with that config on the same day.
Configs only own the checkpoint filename.
Prepared memory-mapped tokens go under `intermediate/data/`; checkpoints go to
`intermediate/checkpoints/<run-name>/`; validation reports go to
`output/<run-name>/validation.json`.

## Interactive inference

Select **Inference** to choose any existing run under
`intermediate/checkpoints/`. The checkpoint supplies its own model
configuration, weights, and character tokenizer, so inference does not need a
TOML configuration or prepared dataset files.

Enter text at `Continue (type "exit" to quit):`. Each entry is appended exactly
to the previous prompt and generated text, and the selected model remains
loaded for the whole session. Empty input asks the model to continue without
adding text. Enter `exit` in any capitalization to leave the session.

Generation samples at temperature `1.0` and flushes each character to the
terminal as soon as it is produced. If the model generates the literal,
case-sensitive text `<EOS>`, the marker is erased from the terminal and removed
from the retained context. A turn otherwise stops after 1,000 generated tokens.
Context is stateful but bounded by the model's `block_size`; once it fills, only
the most recent characters remain available to the model.

Training performs only its quick periodic cross-entropy and prompt check. Final
validation scans every held-out token, generates the fixed prompt suite, runs
the local TinyStories lm-eval tasks, and evaluates the fixed HellaSwag slice.
HellaSwag downloads its data on first use.

## Configuration

Edit a TOML file in `input/configs/` to change model, training, checkpoint
name, data, prompts, and harness settings. TinyStories uses its
separate validation file. Shakespeare has no separate validation file, so its
config uses a deterministic final 10% holdout.

Set `[run].description` in the config to annotate both `validation.json` and
`validation.md`; the interactive menu does not ask for a comment.

Each checkpoint saves resolved configuration, data/tokenizer fingerprints,
optimizer state, scheduler state, throughput counters, and peak training GPU
memory. This lets `tinylm.validation` report training telemetry without
retraining.

## What is `intermediate/evals`?

This is version-controlled evaluation reference material: the 60 labelled
TinyStories-style reasoning questions used by lm-eval. They are not generated
model results. Keeping them in `intermediate/evals/` makes the exact prompts
and correct answers easy to inspect without mixing them with generated
validation JSON files; changing one changes the evaluation, so compare runs
only when the suite is unchanged.
