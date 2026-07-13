"""Timing and CUDA-memory helpers used by training and validation."""

from __future__ import annotations

from contextlib import contextmanager
import time
from typing import Iterator

import torch


def cuda_available(device: torch.device) -> bool:
    return device.type == "cuda" and torch.cuda.is_available()


def synchronize(device: torch.device) -> None:
    if cuda_available(device):
        torch.cuda.synchronize(device)


def reset_peak_memory(device: torch.device) -> None:
    if cuda_available(device):
        torch.cuda.reset_peak_memory_stats(device)


def peak_memory_gb(device: torch.device) -> dict[str, float | None | str]:
    if not cuda_available(device):
        return {"allocated_gb": None, "reserved_gb": None, "reason": "CUDA is not available"}
    gib = 1024**3
    return {
        "allocated_gb": torch.cuda.max_memory_allocated(device) / gib,
        "reserved_gb": torch.cuda.max_memory_reserved(device) / gib,
        "reason": "",
    }


@contextmanager
def synchronized_timer(device: torch.device) -> Iterator[callable]:
    """Yield a callable returning elapsed wall time after CUDA synchronization."""
    synchronize(device)
    start = time.perf_counter()
    yield lambda: time.perf_counter() - start
    synchronize(device)
