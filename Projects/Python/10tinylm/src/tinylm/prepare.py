"""CLI for streaming token preparation."""

from __future__ import annotations

import argparse

from .config import load_config
from .data import prepare_data


def main() -> None:
    parser = argparse.ArgumentParser(description="Stream a dataset into TinyLM token files.")
    parser.add_argument("--config", required=True, help="Path to a TOML experiment config.")
    parser.add_argument("--force", action="store_true", help="Rebuild even when the source metadata matches.")
    args = parser.parse_args()
    location = prepare_data(load_config(args.config), force=args.force)
    print(f"Prepared token data at {location}")


if __name__ == "__main__":
    main()
