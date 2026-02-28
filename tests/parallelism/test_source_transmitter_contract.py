#!/usr/bin/env python3
"""Contract checks for source transmitter stream-signal liveness guards."""

from __future__ import annotations

import unittest
from pathlib import Path


REPO_ROOT = Path(__file__).resolve().parents[2]
SOURCE_STATEM_FILE = REPO_ROOT / "src_erl" / "NerlnetApp" / "src" / "Source" / "sourceStatem.erl"


class SourceTransmitterContractTest(unittest.TestCase):
    def test_stream_signal_requests_are_timeout_guarded(self) -> None:
        content = SOURCE_STATEM_FILE.read_text(encoding="utf-8")
        self.assertIn("STREAM_SIGNAL_TIMEOUT_MS", content)
        self.assertIn("safe_stream_signal_request(", content)
        self.assertIn("stream signal ~p timed out", content)
        self.assertIn("stream signal ~p failed", content)
        self.assertIn("gen_statem:cast(SourcePid,{finishedCasting,BatchesSent})", content)


if __name__ == "__main__":
    unittest.main()
