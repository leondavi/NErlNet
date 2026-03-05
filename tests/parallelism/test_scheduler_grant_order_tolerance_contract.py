#!/usr/bin/env python3
"""Contract checks for worker scheduler-grant matching semantics."""

from __future__ import annotations

import unittest
from pathlib import Path


REPO_ROOT = Path(__file__).resolve().parents[2]
WORKER_GENERIC = (
    REPO_ROOT
    / "src_erl"
    / "NerlnetApp"
    / "src"
    / "Bridge"
    / "onnWorkers"
    / "workerGeneric.erl"
)


class SchedulerGrantOrderToleranceContractTests(unittest.TestCase):
    def test_worker_runtime_uses_match_based_grant_lookup(self) -> None:
        content = WORKER_GENERIC.read_text(encoding="utf-8")
        self.assertIn("pop_matching_scheduler_grant", content)
        self.assertIn("has_matching_scheduler_grant", content)

    def test_pipeline_stage0_grants_allow_batch_decoupling_only_for_pipeline(self) -> None:
        content = WORKER_GENERIC.read_text(encoding="utf-8")
        # Stage-0 forward events are grant-authoritative on batch-id so the
        # scheduler does not deadlock when source ingress batch ids drift.
        # This applies to pure pipeline mode only; pipeline_tensor keeps strict
        # batch matching to preserve TP collective token alignment.
        self.assertIn("expected_parallel_scheduler_batch_id", content)
        self.assertIn("{pipeline, forward, 0}", content)
        self.assertIn("batch_id_matches(any, _GrantBatchID)", content)

    def test_backward_dispatch_uses_grant_batch_id(self) -> None:
        content = WORKER_GENERIC.read_text(encoding="utf-8")
        self.assertIn("{ok, {backward, GrantBatchID, MicrobatchID, StageID}}", content)
        self.assertIn(
            "pop_pending_parallel_backward_event(PendingEvents, GrantBatchID, MicrobatchID, StageID, [])",
            content,
        )


if __name__ == "__main__":
    unittest.main()
