# TinyLM experiment workflow

This project trains a character-level RoPE/SwiGLU transformer on TinyStories or
Shakespeare. The supported workflow is interactive and produces a stable,
machine-readable final validation report.

## Run an experiment

Run the menu and select **Prepare**, **Train**, **Validate**, or **All**. It
then lets you choose a TOML file from `input/configs/`.

```powershell
uv run tinylm
```

Each training invocation gets an ID in `YYYY-MM-DD_N-configname` format, where
`N` starts at `0` and increments for runs made with that config on the same day.
Configs only own the checkpoint filename.
Prepared memory-mapped tokens go under `intermediate/data/`; checkpoints go to
`intermediate/checkpoints/<run-name>/`; validation reports go to
`output/<run-name>/validation.json`.

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
