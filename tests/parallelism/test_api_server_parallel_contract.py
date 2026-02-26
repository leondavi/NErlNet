#!/usr/bin/env python3
"""Contract checks for API-server preflight validation in parallel rollout."""

from __future__ import annotations

import unittest
from pathlib import Path


REPO_ROOT = Path(__file__).resolve().parents[2]
API_SERVER_FILE = REPO_ROOT / "src_py" / "apiServer" / "apiServer.py"


class ApiServerParallelContractTests(unittest.TestCase):
    def test_initialization_validates_connection_map_with_runtime_entities(self) -> None:
        content = API_SERVER_FILE.read_text(encoding="utf-8")
        self.assertIn("globe.components.validate_connection_map(connData.get(\"connectionsMap\", {}))", content)


if __name__ == "__main__":
    unittest.main()
