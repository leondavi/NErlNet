#!/usr/bin/env python3
"""Soak-style scheduler stress checks for repeated high-microbatch runs."""

from __future__ import annotations

import hashlib
import sys
import unittest
from pathlib import Path


REPO_ROOT = Path(__file__).resolve().parents[2]
sys.path.insert(0, str(REPO_ROOT / "src_py" / "apiServer"))

from parallel_scheduler import (  # noqa: E402
    assert_scheduler_trace_integrity,
    build_scheduler_schedule,
    summarize_microbatch_lifecycle,
)


def _trace_fingerprint(trace) -> str:
    payload = "|".join(f"{e.direction}:{e.microbatch_id}:{e.stage_id}" for e in trace)
    return hashlib.sha256(payload.encode("utf-8")).hexdigest()


class ParallelSchedulerSoakTests(unittest.TestCase):
    def test_repeated_scheduler_generation_integrity(self) -> None:
        configs = [
            ("gpipe", 4, 64, 1),
            ("1f1b", 4, 64, 1),
            ("interleaved", 4, 64, 2),
        ]
        for scheduler, stage_count, num_microbatches, virtual_stages in configs:
            for _ in range(75):
                trace = build_scheduler_schedule(
                    scheduler,
                    stage_count,
                    num_microbatches,
                    virtual_stages=virtual_stages,
                )
                effective_stages = (
                    stage_count * virtual_stages if scheduler == "interleaved" else stage_count
                )
                assert_scheduler_trace_integrity(trace, effective_stages, num_microbatches)

    def test_scheduler_fingerprint_is_deterministic_under_repetition(self) -> None:
        trace = build_scheduler_schedule("1f1b", 5, 48)
        expected_fingerprint = _trace_fingerprint(trace)
        for _ in range(100):
            candidate = build_scheduler_schedule("1f1b", 5, 48)
            self.assertEqual(_trace_fingerprint(candidate), expected_fingerprint)

    def test_large_microbatch_lifecycle_remains_complete(self) -> None:
        stage_count = 6
        num_microbatches = 96
        trace = build_scheduler_schedule("gpipe", stage_count, num_microbatches)
        lifecycle = summarize_microbatch_lifecycle(trace)
        self.assertEqual(len(trace), stage_count * num_microbatches * 2)
        self.assertEqual(len(lifecycle), num_microbatches)
        for microbatch_id in range(num_microbatches):
            forward_count, backward_count = lifecycle[microbatch_id]
            self.assertEqual(forward_count, stage_count)
            self.assertEqual(backward_count, stage_count)


if __name__ == "__main__":
    unittest.main()
