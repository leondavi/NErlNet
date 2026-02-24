#!/usr/bin/env python3
"""Deterministic assertions over PTD_P_POC scheduler trace output.

This test converts the exploratory simulator log into an executable contract:
- schedule ordering for GPipe / 1F1B / Interleaved
- tensor-parallel collective evidence (all-gather + all-reduce)
- pre-update loss consistency across schedules
"""

from __future__ import annotations

import re
import unittest
from pathlib import Path


REPO_ROOT = Path(__file__).resolve().parents[2]
PRIMARY_LOG_PATH = REPO_ROOT / "PTD_P_POC" / "PTD_P_Tests.log"
FALLBACK_LOG_PATH = REPO_ROOT / "tests" / "parallelism" / "fixtures" / "ptd_p_schedule_excerpt.log"


def _extract_microbatch_indices(lines: list[str], schedule: str, direction: str) -> list[int]:
    pattern = re.compile(rf"{re.escape(schedule)}: {direction} microbatch (\d+)")
    indices: list[int] = []
    for line in lines:
        match = pattern.search(line)
        if match:
            indices.append(int(match.group(1)))
    return indices


def _first_line_index(lines: list[str], needle: str) -> int:
    for idx, line in enumerate(lines):
        if needle in line:
            return idx
    raise AssertionError(f"'{needle}' not found in log")


class PTDPOCTraceTest(unittest.TestCase):
    @classmethod
    def setUpClass(cls) -> None:
        log_path = PRIMARY_LOG_PATH if PRIMARY_LOG_PATH.is_file() else FALLBACK_LOG_PATH
        if not log_path.is_file():
            raise FileNotFoundError(f"Missing POC log. Checked: {PRIMARY_LOG_PATH} and {FALLBACK_LOG_PATH}")
        cls.lines = log_path.read_text(encoding="utf-8", errors="replace").splitlines()

        microbatch_match = re.search(
            r"Microbatch count = (\d+)", "\n".join(cls.lines)
        )
        if not microbatch_match:
            raise AssertionError("Microbatch count marker not found in POC log")
        cls.microbatch_count = int(microbatch_match.group(1))

    def test_gpipe_schedule_order(self) -> None:
        expected_forward = list(range(self.microbatch_count))
        expected_backward = list(range(self.microbatch_count - 1, -1, -1))

        forwards = _extract_microbatch_indices(self.lines, "GPipe", "FORWARD")
        backwards = _extract_microbatch_indices(self.lines, "GPipe", "BACKWARD")

        self.assertEqual(forwards, expected_forward)
        self.assertEqual(backwards, expected_backward)

        first_backward_idx = _first_line_index(self.lines, "GPipe: BACKWARD microbatch")
        last_forward_idx = _first_line_index(
            self.lines[::-1], "GPipe: FORWARD microbatch"
        )
        # last_forward_idx is reversed index
        last_forward_idx = len(self.lines) - 1 - last_forward_idx
        self.assertGreater(first_backward_idx, last_forward_idx)

    def test_1f1b_schedule_order(self) -> None:
        expected = list(range(self.microbatch_count))
        forwards = _extract_microbatch_indices(self.lines, "1F1B", "FORWARD")
        backwards = _extract_microbatch_indices(self.lines, "1F1B", "BACKWARD")

        self.assertEqual(forwards, expected)
        self.assertEqual(backwards, expected)

        first_backward_idx = _first_line_index(self.lines, "1F1B: BACKWARD microbatch")
        first_forward_idx = _first_line_index(self.lines, "1F1B: FORWARD microbatch")
        self.assertGreater(first_backward_idx, first_forward_idx)

    def test_interleaved_schedule_order(self) -> None:
        expected = list(range(self.microbatch_count))
        forwards = _extract_microbatch_indices(self.lines, "Interleaved", "FORWARD")
        backwards = _extract_microbatch_indices(self.lines, "Interleaved", "BACKWARD")

        self.assertEqual(forwards, expected)
        self.assertEqual(backwards, expected)

    def test_parallel_collectives_present(self) -> None:
        text = "\n".join(self.lines)
        self.assertIn("all-gather local outputs from TP ranks", text)
        self.assertIn("all-reduce(sum) local partial outputs across TP ranks", text)

    def test_total_loss_consistency(self) -> None:
        text = "\n".join(self.lines)
        pattern = re.compile(r"(GPipe|1F1B|Interleaved): total loss before update = (\d+)")
        schedule_losses = {match.group(1): int(match.group(2)) for match in pattern.finditer(text)}

        self.assertEqual(schedule_losses.get("GPipe"), 2404)
        self.assertEqual(schedule_losses.get("1F1B"), 2404)
        self.assertEqual(schedule_losses.get("Interleaved"), 2404)

        summary_pattern = re.compile(r"(GPipe|1F1B|Interleaved) loss = (\d+)")
        summary_losses = {match.group(1): int(match.group(2)) for match in summary_pattern.finditer(text)}
        self.assertEqual(summary_losses, {"GPipe": 2404, "1F1B": 2404, "Interleaved": 2404})


if __name__ == "__main__":
    unittest.main()
