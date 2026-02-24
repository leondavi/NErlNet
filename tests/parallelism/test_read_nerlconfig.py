#!/usr/bin/env python3
"""Regression tests for nerlconfig path loading behavior."""

from __future__ import annotations

import sys
import tempfile
import types
import unittest
from pathlib import Path


REPO_ROOT = Path(__file__).resolve().parents[2]
sys.path.insert(0, str(REPO_ROOT / "src_py" / "apiServer"))

# Keep this test lightweight in environments without pandas installed.
if "pandas" not in sys.modules:
    pandas_stub = types.SimpleNamespace(read_csv=lambda *_args, **_kwargs: None)
    sys.modules["pandas"] = pandas_stub

from definitions import read_nerlconfig  # noqa: E402


class ReadNerlConfigTests(unittest.TestCase):
    def test_missing_json_dir_is_created(self) -> None:
        with tempfile.TemporaryDirectory(prefix="nerlconfig-tests-") as tmp:
            tmp_path = Path(tmp)
            target_jsons_dir = tmp_path / "missing" / "jsons"
            config_path = tmp_path / "jsonsDir.nerlconfig"
            config_path.write_text(str(target_jsons_dir), encoding="utf-8")

            resolved = read_nerlconfig(str(config_path))
            self.assertEqual(resolved, str(target_jsons_dir))
            self.assertTrue(target_jsons_dir.is_dir())

    def test_json_dir_path_pointing_to_file_is_rejected(self) -> None:
        with tempfile.TemporaryDirectory(prefix="nerlconfig-tests-") as tmp:
            tmp_path = Path(tmp)
            file_target = tmp_path / "not_a_dir"
            file_target.write_text("x", encoding="utf-8")
            config_path = tmp_path / "jsonsDir.nerlconfig"
            config_path.write_text(str(file_target), encoding="utf-8")

            with self.assertRaises(NotADirectoryError):
                read_nerlconfig(str(config_path))


if __name__ == "__main__":
    unittest.main()
