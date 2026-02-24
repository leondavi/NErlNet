#!/usr/bin/env python3
"""Compare PTD POC loss summaries between expected and actual logs."""

from __future__ import annotations

import argparse
import json
import re
import sys
from pathlib import Path
from typing import Dict


LOSS_PATTERN = re.compile(r"^\[\d+\]\s+(GPipe|1F1B|Interleaved) loss = ([0-9]+(?:\.[0-9]+)?)\s*$")
LOSS_KEYS = ("GPipe", "1F1B", "Interleaved")


def parse_losses(log_path: Path) -> Dict[str, float]:
    losses: Dict[str, float] = {}
    for line in log_path.read_text(encoding="utf-8").splitlines():
        match = LOSS_PATTERN.match(line.strip())
        if not match:
            continue
        losses[match.group(1)] = float(match.group(2))
    missing = [key for key in LOSS_KEYS if key not in losses]
    if missing:
        raise ValueError(
            f"Could not find loss summaries {missing} in {log_path}"
        )
    return losses


def main() -> int:
    parser = argparse.ArgumentParser(
        description="Compare PTD_P_Tests loss summaries between logs."
    )
    parser.add_argument("--expected", required=True, type=Path, help="Reference log path")
    parser.add_argument("--actual", required=True, type=Path, help="Current log path")
    parser.add_argument(
        "--tolerance",
        type=float,
        default=0.0,
        help="Absolute tolerance for numeric comparison",
    )
    args = parser.parse_args()

    expected_losses = parse_losses(args.expected)
    actual_losses = parse_losses(args.actual)

    deltas = {
        key: abs(expected_losses[key] - actual_losses[key])
        for key in LOSS_KEYS
    }
    matched = all(delta <= args.tolerance for delta in deltas.values())

    report = {
        "expected_log": str(args.expected),
        "actual_log": str(args.actual),
        "expected_losses": expected_losses,
        "actual_losses": actual_losses,
        "abs_deltas": deltas,
        "tolerance": args.tolerance,
        "matched": matched,
    }
    print(json.dumps(report, indent=2, sort_keys=True))

    if not matched:
        print(
            "[compare_ptd_poc_losses] Loss mismatch detected.",
            file=sys.stderr,
        )
        return 1
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
