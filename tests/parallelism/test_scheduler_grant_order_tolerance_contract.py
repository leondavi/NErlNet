#!/usr/bin/env python3
"""Contract checks for scheduler-grant head-order gating in worker runtime."""

from __future__ import annotations

import unittest
from pathlib import Path


REPO_ROOT = Path(__file__).resolve().parents[2]
WORKER_GENERIC = REPO_ROOT / "src_erl" / "NerlnetApp" / "src" / "Bridge" / "onnWorkers" / "workerGeneric.erl"


class SchedulerGrantOrderToleranceContractTests(unittest.TestCase):
    def test_worker_runtime_uses_head_grant_matching(self) -> None:
        content = WORKER_GENERIC.read_text(encoding="utf-8")
        self.assertIn("scheduler_grant_mismatch", content)
        self.assertIn("waiting for matching scheduler grant expected=~p head=~p", content)
        self.assertIn("[HeadGrant | RestGrants]", content)
        self.assertNotIn("pop_matching_scheduler_grant", content)
        self.assertNotIn("has_matching_scheduler_grant", content)

    def test_worker_consumes_only_head_matching_grant(self) -> None:
        content = WORKER_GENERIC.read_text(encoding="utf-8")
        self.assertIn("case Grants of", content)
        self.assertIn("[] ->", content)
        self.assertIn("{error, no_scheduler_grant}", content)
        self.assertIn("{ok, NormalizedHead", content)


if __name__ == "__main__":
    unittest.main()
