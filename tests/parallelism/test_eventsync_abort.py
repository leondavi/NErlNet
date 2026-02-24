#!/usr/bin/env python3
"""EventSync fail-fast behavior for deterministic parallel aborts."""

from __future__ import annotations

import sys
import unittest
from pathlib import Path


REPO_ROOT = Path(__file__).resolve().parents[2]
sys.path.insert(0, str(REPO_ROOT / "src_py" / "apiServer"))

from events_sync import EventSync  # noqa: E402


class EventSyncAbortTests(unittest.TestCase):
    def test_parallel_abort_maps_to_main_server_error(self) -> None:
        sync = EventSync()
        self.assertIn("parallel_abort", sync.done_actions_dict)
        self.assertEqual(sync.done_actions_dict["parallel_abort"], sync.MAIN_SERVER_ERROR)

    def test_parallel_abort_triggers_error_status(self) -> None:
        sync = EventSync()
        self.assertFalse(sync.get_error_status())
        sync.set_event_done(sync.MAIN_SERVER_ERROR)
        self.assertTrue(sync.get_error_status())


if __name__ == "__main__":
    unittest.main()
