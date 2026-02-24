#!/usr/bin/env python3
"""Conformance tests for deterministic pipeline scheduler traces."""

from __future__ import annotations

import sys
import unittest
from pathlib import Path


REPO_ROOT = Path(__file__).resolve().parents[2]
sys.path.insert(0, str(REPO_ROOT / "src_py" / "apiServer"))

from parallel_scheduler import (  # noqa: E402
    ScheduleEvent,
    assert_scheduler_trace_integrity,
    build_1f1b_schedule,
    build_gpipe_schedule,
    build_interleaved_schedule,
    build_scheduler_schedule,
    summarize_microbatch_lifecycle,
)


class SchedulerConformanceTests(unittest.TestCase):
    def test_gpipe_order_matches_reference(self) -> None:
        stage_count = 3
        num_microbatches = 4
        trace = build_gpipe_schedule(stage_count, num_microbatches)

        forward = [event for event in trace if event.direction == "forward"]
        backward = [event for event in trace if event.direction == "backward"]

        expected_forward = [
            (microbatch, stage)
            for microbatch in range(num_microbatches)
            for stage in range(stage_count)
        ]
        expected_backward = [
            (microbatch, stage)
            for microbatch in range(num_microbatches - 1, -1, -1)
            for stage in range(stage_count - 1, -1, -1)
        ]

        self.assertEqual([(e.microbatch_id, e.stage_id) for e in forward], expected_forward)
        self.assertEqual([(e.microbatch_id, e.stage_id) for e in backward], expected_backward)

        assert_scheduler_trace_integrity(trace, stage_count, num_microbatches)

    def test_1f1b_warmup_steady_cooldown_order(self) -> None:
        stage_count = 3
        num_microbatches = 5
        trace = build_1f1b_schedule(stage_count, num_microbatches)

        first_backward_idx = next(
            idx for idx, event in enumerate(trace) if event.direction == "backward"
        )
        warmup = stage_count - 1
        expected_first_backward_idx = (warmup + 1) * stage_count
        self.assertEqual(first_backward_idx, expected_first_backward_idx)

        assert_scheduler_trace_integrity(trace, stage_count, num_microbatches)

    def test_interleaved_trace_integrity(self) -> None:
        stage_count = 2
        num_microbatches = 6
        virtual_stages = 2
        trace = build_interleaved_schedule(stage_count, num_microbatches, virtual_stages)

        assert_scheduler_trace_integrity(
            trace,
            stage_count * virtual_stages,
            num_microbatches,
        )

    def test_lifecycle_has_no_orphan_microbatches(self) -> None:
        stage_count = 4
        num_microbatches = 3
        trace = build_scheduler_schedule("gpipe", stage_count, num_microbatches)
        lifecycle = summarize_microbatch_lifecycle(trace)

        for microbatch_id in range(num_microbatches):
            self.assertIn(microbatch_id, lifecycle)
            forward_count, backward_count = lifecycle[microbatch_id]
            self.assertEqual(forward_count, stage_count)
            self.assertEqual(backward_count, stage_count)

    def test_duplicate_transition_rejected(self) -> None:
        stage_count = 2
        num_microbatches = 2
        trace = build_gpipe_schedule(stage_count, num_microbatches)
        tampered = [trace[0], trace[0]] + trace[1:]

        with self.assertRaises(AssertionError):
            assert_scheduler_trace_integrity(tampered, stage_count, num_microbatches)

    def test_invalid_scheduler_name_rejected(self) -> None:
        with self.assertRaises(ValueError):
            build_scheduler_schedule("unknown", 2, 2)

    def test_invalid_interleaved_virtual_stages_rejected(self) -> None:
        with self.assertRaises(ValueError):
            build_interleaved_schedule(2, 2, 0)


if __name__ == "__main__":
    unittest.main()
