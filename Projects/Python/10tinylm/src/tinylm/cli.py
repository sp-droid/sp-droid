"""Interactive console workflow for TinyLM experiments."""

from __future__ import annotations

from pathlib import Path

from InquirerPy import inquirer

from .config import checkpoint_path, load_config, next_run_name, project_root, report_path
from .data import prepare_data
from .reporting import write_json, write_markdown
from .training import train
from .validation import create_report


def _choose_config() -> Path:
    config_directory = project_root() / "input" / "configs"
    configs = sorted(config_directory.glob("*.toml"))
    if not configs:
        raise FileNotFoundError(f"No TOML configurations found in {config_directory}")
    choices = [{"name": path.stem, "value": path} for path in configs]
    return Path(inquirer.select(message="Choose an experiment configuration:", choices=choices).execute())


def _windows_quote(value: str | Path) -> str:
    """Quote commands for both PowerShell and cmd.exe."""
    return '"' + str(value).replace('"', '\\"') + '"'


def _relative_to_project(path: Path) -> Path:
    try:
        return path.resolve().relative_to(project_root())
    except ValueError:
        return path.resolve()


def _print_commands(commands: list[str]) -> None:
    print("\nEquivalent non-interactive command(s):")
    for command in commands:
        print(command)
    print()


def _choose_checkpoint(config: dict[str, object]) -> Path:
    checkpoint_name = str(config["run"]["checkpoint"])
    checkpoint_root = project_root() / "intermediate" / "checkpoints"
    checkpoints = sorted(checkpoint_root.glob(f"*/{checkpoint_name}"), reverse=True)
    if not checkpoints:
        raise FileNotFoundError(f"No {checkpoint_name} files found below {checkpoint_root}")
    choices = [{"name": path.parent.name, "value": path} for path in checkpoints]
    return Path(inquirer.select(message="Choose a checkpoint:", choices=choices).execute())


def _validation_command(checkpoint: Path) -> str:
    return "uv run python -m tinylm.validation" f" --checkpoint {_windows_quote(_relative_to_project(checkpoint))}"


def _run_validation(checkpoint: Path) -> None:
    selected_checkpoint = checkpoint
    report = create_report(selected_checkpoint)
    destination = report_path(selected_checkpoint.parent.name)
    write_json(destination, report)
    write_markdown(destination.with_suffix(".md"), report)
    print(f"Wrote validation report: {destination}")


def main() -> None:
    action = inquirer.select(
        message="What would you like to do?",
        choices=[
            {"name": "Prepare token data", "value": "prepare"},
            {"name": "Train", "value": "train"},
            {"name": "Validate a checkpoint", "value": "validate"},
            {"name": "All: prepare, train, then validate", "value": "all"},
        ],
    ).execute()
    config_path = _choose_config()
    config = load_config(config_path)
    relative_config = _relative_to_project(config_path)

    if action == "prepare":
        _print_commands([f"uv run python -m tinylm.prepare --config {_windows_quote(relative_config)}"])
        location = prepare_data(config)
        print(f"Prepared token data at {location}")
        return

    if action == "validate":
        selected_checkpoint = _choose_checkpoint(config)
        _print_commands([_validation_command(selected_checkpoint)])
        _run_validation(selected_checkpoint)
        return

    run_name = next_run_name(config)
    target_checkpoint = checkpoint_path(config, run_name)
    train_command = (
        "uv run python -m tinylm.training"
        f" --config {_windows_quote(relative_config)}"
        f" --run-name {_windows_quote(run_name)}"
    )
    if action == "train":
        _print_commands([train_command])
        train(config, run_name=run_name)
        return

    _print_commands(
        [
            f"uv run python -m tinylm.prepare --config {_windows_quote(relative_config)}",
            train_command,
            _validation_command(target_checkpoint),
        ]
    )
    location = prepare_data(config)
    print(f"Prepared token data at {location}")
    trained_checkpoint = train(config, run_name=run_name)
    _run_validation(trained_checkpoint)


if __name__ == "__main__":
    main()
