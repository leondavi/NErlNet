#!/usr/bin/env python3
"""Contract checks for out-of-order scheduler grant handling in worker runtime."""

from __future__ import annotations

import unittest
from pathlib import Path


REPO_ROOT = Path(__file__).resolve().parents[2]
WORKER_GENERIC = REPO_ROOT / "src_erl" / "NerlnetApp" / "src" / "Bridge" / "onnWorkers" / "workerGeneric.erl"


class SchedulerGrantOrderToleranceContractTests(unittest.TestCase):
    def test_worker_runtime_has_matching_grant_search_helpers(self) -> None:
        content = WORKER_GENERIC.read_text(encoding="utf-8")
        self.assertIn("has_matching_scheduler_grant(Grants, ExpectedGrant)", content)
        self.assertIn("pop_matching_scheduler_grant(Grants, ExpectedGrant)", content)
        self.assertIn("pop_matching_scheduler_grant([Grant | Rest], ExpectedGrant, Acc)", content)

    def test_worker_consumes_matching_grant_not_only_head(self) -> None:
        content = WORKER_GENERIC.read_text(encoding="utf-8")
        self.assertIn("case pop_matching_scheduler_grant(Grants, ExpectedGrant) of", content)
        self.assertIn("{ok, _MatchedGrant, RemainingGrants} ->", content)
        self.assertIn("has_matching_scheduler_grant(", content)
        self.assertIn("not_found ->", content)
        self.assertNotIn("scheduler_grant_mismatch", content)


if __name__ == "__main__":
    unittest.main()

