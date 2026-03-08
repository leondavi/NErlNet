#!/usr/bin/env python3
from __future__ import annotations

import json
import sys
import tempfile
import types
import unittest
from pathlib import Path

REPO_ROOT = Path(__file__).resolve().parents[2]
sys.path.insert(0, str(REPO_ROOT / "JupyterLabDir" / "report_pipeline"))
sys.path.insert(0, str(REPO_ROOT / "src_py" / "apiServer"))

if "comm" not in sys.modules:
    sys.modules["comm"] = types.ModuleType("comm")

from apiServer import ApiServer  # noqa: E402
from generate_jsons import build_report_profile  # noqa: E402


class ReportPipelineGenerationTests(unittest.TestCase):
    def setUp(self) -> None:
        self.temp_dir = tempfile.TemporaryDirectory(prefix="nerlnet_report_pipeline_")
        self.temp_path = Path(self.temp_dir.name)
        self.manifest_path = self.temp_path / "manifest.json"
        self.matrix_path = REPO_ROOT / "JupyterLabDir" / "report_pipeline" / "experiment_matrix.report_core.json"
        self.out_root = self.temp_path / "generated"
        manifest = {
            "controller": "controller",
            "nodes": [
                {"id": "controller", "name": "controller", "ip": "127.0.0.1", "user": "admin", "ssh_key": "~/.ssh/id_rsa", "nerl_dir": "~/workspace/NErlNet", "local": True},
                {"id": "node_a", "name": "node_a", "ip": "127.0.0.1", "user": "admin", "ssh_key": "~/.ssh/id_rsa", "nerl_dir": "~/workspace/NErlNet", "local": True},
                {"id": "node_b", "name": "node_b", "ip": "127.0.0.1", "user": "admin", "ssh_key": "~/.ssh/id_rsa", "nerl_dir": "~/workspace/NErlNet", "local": True},
                {"id": "node_c", "name": "node_c", "ip": "127.0.0.1", "user": "admin", "ssh_key": "~/.ssh/id_rsa", "nerl_dir": "~/workspace/NErlNet", "local": True},
            ],
        }
        self.manifest_path.write_text(json.dumps(manifest, indent=2), encoding="utf-8")

    def tearDown(self) -> None:
        self.temp_dir.cleanup()

    def test_generated_jsons_are_api_server_initializable_for_representative_topologies(self) -> None:
        manifest_out = build_report_profile("report_core", self.manifest_path, self.matrix_path, self.out_root)
        generated = json.loads(manifest_out.read_text(encoding="utf-8"))
        experiments = {entry["label"]: entry for entry in generated["experiments"]}
        required_labels = [
            "baseline_p1_t1_legacy",
            "sweep_pp2_gpipe_m01",
            "topo_tp2_1f1b_m8",
            "topo_pp4_1f1b_m8",
            "topo_p2_t2_1f1b_m8",
        ]
        for label in required_labels:
            self.assertIn(label, experiments)
            entry = experiments[label]
            api = ApiServer()
            api.reset()
            api.initialization(f"test_{label}", entry["dc"], entry["conn"], entry["exp"])
            current_phase = api.current_exp.get_current_experiment_phase()
            self.assertEqual(current_phase.get_experiment_flow_name(), f"synthetic_experiment_{label}")

    def test_non_legacy_generated_experiments_enable_trace_instrumentation(self) -> None:
        manifest_out = build_report_profile("report_core", self.manifest_path, self.matrix_path, self.out_root)
        generated = json.loads(manifest_out.read_text(encoding="utf-8"))
        for entry in generated["experiments"]:
            exp_payload = json.loads(Path(entry["exp"]).read_text(encoding="utf-8"))
            for phase in exp_payload["Phases"]:
                parallel = phase.get("parallelExecution", {})
                mode = parallel.get("mode", "legacy")
                if mode == "legacy":
                    continue
                instrumentation = parallel.get("instrumentation", {})
                self.assertTrue(instrumentation.get("enabled"))
                self.assertEqual(instrumentation.get("traceGranularity"), "microbatch")

    def test_interleaved_report_sweep_matches_supported_runtime_contract(self) -> None:
        manifest_out = build_report_profile("report_core", self.manifest_path, self.matrix_path, self.out_root)
        generated = json.loads(manifest_out.read_text(encoding="utf-8"))
        experiments = {entry["label"]: entry for entry in generated["experiments"]}
        exp_payload = json.loads(Path(experiments["sweep_pp2_interleaved_m01"]["exp"]).read_text(encoding="utf-8"))
        for phase in exp_payload["Phases"]:
            parallel = phase["parallelExecution"]
            self.assertEqual(parallel["scheduler"], "interleaved")
            self.assertEqual(parallel["virtualStages"], 1)


if __name__ == "__main__":
    unittest.main()
