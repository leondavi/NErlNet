#!/usr/bin/env python3
"""Schema-level tests for Super Node + parallel execution parsing."""

from __future__ import annotations

import copy
import json
import sys
import types
import unittest
from pathlib import Path


REPO_ROOT = Path(__file__).resolve().parents[2]
sys.path.insert(0, str(REPO_ROOT / "src_py" / "apiServer"))
sys.path.insert(0, str(REPO_ROOT / "src_py" / "nerlPlanner"))

# Keep tests lightweight in environments without pandas installed.
if "pandas" not in sys.modules:
    pandas_stub = types.SimpleNamespace()
    sys.modules["pandas"] = pandas_stub

if "numpy" not in sys.modules:
    numpy_stub = types.ModuleType("numpy")
    sys.modules["numpy"] = numpy_stub

# Avoid pulling heavy plotting/science stacks in this focused schema test.
if "stats" not in sys.modules:
    stats_stub = types.ModuleType("stats")
    class _Stats:  # pylint: disable=too-few-public-methods
        pass
    stats_stub.Stats = _Stats
    sys.modules["stats"] = stats_stub

if "statsTiles" not in sys.modules:
    stats_tiles_stub = types.ModuleType("statsTiles")
    class _StatsTiles:  # pylint: disable=too-few-public-methods
        pass
    stats_tiles_stub.StatsTiles = _StatsTiles
    sys.modules["statsTiles"] = stats_tiles_stub

if "stats_aec" not in sys.modules:
    stats_aec_stub = types.ModuleType("stats_aec")
    class _StatsAEC:  # pylint: disable=too-few-public-methods
        pass
    stats_aec_stub.StatsAEC = _StatsAEC
    sys.modules["stats_aec"] = stats_aec_stub

from experiment_flow import ExperimentFlow  # noqa: E402
from experiment_flow_defs import (  # noqa: E402
    EXPFLOW_PARALLEL_EXECUTION_MICRO_BATCH_SIZE_FIELD,
    EXPFLOW_PARALLEL_EXECUTION_MODE_FIELD,
    EXPFLOW_PARALLEL_EXECUTION_NUM_MICRO_BATCHES_FIELD,
    EXPFLOW_PARALLEL_EXECUTION_SCHEDULER_FIELD,
    EXPFLOW_PARALLEL_EXECUTION_SUPER_NODE_FIELD,
    EXPFLOW_PHASES_PARALLEL_EXECUTION_FIELD,
    EXPFLOW_PHASES_PHASE_NAME_FIELD,
)
from networkComponents import NetworkComponents  # noqa: E402


def _base_dc() -> dict:
    return {
        "nerlnetSettings": {"frequency": "1", "batchSize": "4"},
        "mainServer": {"port": "9991", "args": ""},
        "apiServer": {"port": "9992", "args": ""},
        "devices": [
            {
                "name": "device0",
                "ipv4": "127.0.0.1",
                "entities": "mainServer,apiServer,router_a,source_a,client_a,client_b,super_0",
            }
        ],
        "routers": [{"name": "router_a", "port": "7000", "policy": "0"}],
        "sources": [
            {
                "name": "source_a",
                "port": "7100",
                "frequency": "1",
                "policy": "0",
                "epochs": "1",
                "type": "0",
            }
        ],
        "clients": [
            {"name": "client_a", "port": "7200", "workers": "worker_a0", "superNode": "super_0"},
            {"name": "client_b", "port": "7201", "workers": "worker_b0", "superNode": "super_0"},
        ],
        "superNodes": [
            {
                "name": "super_0",
                "port": "6601",
                "managedClients": ["client_a", "client_b"],
                "heartbeatMs": 1000,
                "maxInflightMicrobatches": 16,
            }
        ],
        "workers": [
            {
                "name": "worker_a0",
                "model_sha": "sha_x",
                "parallel": {
                    "pipelineStage": 0,
                    "pipelineWorldSize": 2,
                    "tpGroup": "tp_g0",
                    "tpRank": 0,
                    "tpWorldSize": 1,
                },
            },
            {
                "name": "worker_b0",
                "model_sha": "sha_x",
                "parallel": {
                    "pipelineStage": 1,
                    "pipelineWorldSize": 2,
                    "tpGroup": "tp_g1",
                    "tpRank": 0,
                    "tpWorldSize": 1,
                },
            }
        ],
        "model_sha": {
            "sha_x": {
                "infraType": "0",
                "tpPlan": [
                    {"layer": "fc1", "mode": "column", "shardAxis": 0, "group": "tp_g0"},
                    {"layer": "fc2", "mode": "row", "shardAxis": 1, "group": "tp_g0"},
                ],
            }
        },
    }


class ParallelSchemaTests(unittest.TestCase):
    def test_valid_super_node_and_parallel_parse(self) -> None:
        components = NetworkComponents(_base_dc())
        self.assertTrue(components.has_super_nodes())
        self.assertEqual(components.get_super_nodes_list(), ["super_0"])
        self.assertEqual(components.get_client_super_node("client_a"), "super_0")
        self.assertIn("worker_a0", components.get_worker_parallel_map())
        self.assertIn("sha_x", components.get_model_tp_plan_map())

    def test_duplicate_client_super_assignment_rejected(self) -> None:
        dc = _base_dc()
        dc["devices"][0]["entities"] += ",super_1"
        dc["superNodes"].append(
            {
                "name": "super_1",
                "port": "6602",
                "managedClients": ["client_a"],
                "heartbeatMs": 1000,
                "maxInflightMicrobatches": 16,
            }
        )
        with self.assertRaises(ValueError):
            NetworkComponents(dc)

    def test_client_super_assignment_requires_managed_clients_membership(self) -> None:
        dc = _base_dc()
        dc["superNodes"][0]["managedClients"] = ["client_b"]
        with self.assertRaises(ValueError):
            NetworkComponents(dc)

    def test_invalid_tp_rank_rejected(self) -> None:
        dc = _base_dc()
        dc["workers"][0]["parallel"]["tpRank"] = 2
        dc["workers"][0]["parallel"]["tpWorldSize"] = 2
        with self.assertRaises(ValueError):
            NetworkComponents(dc)

    def test_invalid_tp_plan_mode_rejected(self) -> None:
        dc = _base_dc()
        dc["model_sha"]["sha_x"]["tpPlan"][0]["mode"] = "diag"
        with self.assertRaises(ValueError):
            NetworkComponents(dc)

    def test_parallel_execution_phase_validation(self) -> None:
        components = NetworkComponents(_base_dc())
        exp = ExperimentFlow("exp", 4, components, temp_data_path="/tmp/nerlnet_parallel_schema_tests")

        phase = {
            EXPFLOW_PHASES_PHASE_NAME_FIELD: "train_1",
            EXPFLOW_PHASES_PARALLEL_EXECUTION_FIELD: {
                EXPFLOW_PARALLEL_EXECUTION_MODE_FIELD: "pipeline",
                EXPFLOW_PARALLEL_EXECUTION_SUPER_NODE_FIELD: "super_0",
                EXPFLOW_PARALLEL_EXECUTION_SCHEDULER_FIELD: "gpipe",
                EXPFLOW_PARALLEL_EXECUTION_MICRO_BATCH_SIZE_FIELD: 2,
                EXPFLOW_PARALLEL_EXECUTION_NUM_MICRO_BATCHES_FIELD: 2,
            },
        }
        normalized = exp._parse_parallel_execution(copy.deepcopy(phase))
        self.assertEqual(normalized.get("mode"), "pipeline")
        self.assertEqual(normalized.get("superNode"), "super_0")

    def test_parallel_execution_unknown_super_node_rejected(self) -> None:
        components = NetworkComponents(_base_dc())
        exp = ExperimentFlow("exp", 4, components, temp_data_path="/tmp/nerlnet_parallel_schema_tests")
        phase = {
            EXPFLOW_PHASES_PHASE_NAME_FIELD: "train_1",
            EXPFLOW_PHASES_PARALLEL_EXECUTION_FIELD: {
                EXPFLOW_PARALLEL_EXECUTION_MODE_FIELD: "pipeline",
                EXPFLOW_PARALLEL_EXECUTION_SUPER_NODE_FIELD: "missing_super",
                EXPFLOW_PARALLEL_EXECUTION_SCHEDULER_FIELD: "gpipe",
                EXPFLOW_PARALLEL_EXECUTION_MICRO_BATCH_SIZE_FIELD: 2,
                EXPFLOW_PARALLEL_EXECUTION_NUM_MICRO_BATCHES_FIELD: 2,
            },
        }
        with self.assertRaises(ValueError):
            exp._parse_parallel_execution(phase)

    def test_pipeline_mode_rejects_multi_worker_stage_layout(self) -> None:
        dc = _base_dc()
        dc["clients"][0]["workers"] = "worker_a0,worker_a1"
        dc["clients"][1]["workers"] = "worker_b0,worker_b1"
        dc["devices"][0]["entities"] = "mainServer,apiServer,router_a,source_a,client_a,client_b,super_0"
        dc["workers"].extend(
            [
                {
                    "name": "worker_a1",
                    "model_sha": "sha_x",
                    "parallel": {
                        "pipelineStage": 0,
                        "pipelineWorldSize": 2,
                        "tpGroup": "tp_g2",
                        "tpRank": 0,
                        "tpWorldSize": 1,
                    },
                },
                {
                    "name": "worker_b1",
                    "model_sha": "sha_x",
                    "parallel": {
                        "pipelineStage": 1,
                        "pipelineWorldSize": 2,
                        "tpGroup": "tp_g3",
                        "tpRank": 0,
                        "tpWorldSize": 1,
                    },
                },
            ]
        )

        components = NetworkComponents(dc)
        exp = ExperimentFlow("exp", 4, components, temp_data_path="/tmp/nerlnet_parallel_schema_tests")
        phase = {
            EXPFLOW_PHASES_PHASE_NAME_FIELD: "train_replicated_stage",
            "sourcePieces": [{"workers": "worker_a0,worker_a1,worker_b0,worker_b1"}],
            EXPFLOW_PHASES_PARALLEL_EXECUTION_FIELD: {
                EXPFLOW_PARALLEL_EXECUTION_MODE_FIELD: "pipeline",
                EXPFLOW_PARALLEL_EXECUTION_SUPER_NODE_FIELD: "super_0",
                EXPFLOW_PARALLEL_EXECUTION_SCHEDULER_FIELD: "gpipe",
                EXPFLOW_PARALLEL_EXECUTION_MICRO_BATCH_SIZE_FIELD: 2,
                EXPFLOW_PARALLEL_EXECUTION_NUM_MICRO_BATCHES_FIELD: 2,
            },
        }

        with self.assertRaisesRegex(ValueError, "exactly one worker per pipeline stage"):
            exp._parse_parallel_execution(phase)

    def test_pipeline_mode_rejects_non_stage0_source_targets(self) -> None:
        components = NetworkComponents(_base_dc())
        exp = ExperimentFlow("exp", 4, components, temp_data_path="/tmp/nerlnet_parallel_schema_tests")
        source_pieces = [
            {
                "sourceName": "source_a",
                "workers": "worker_b0",
            }
        ]
        with self.assertRaisesRegex(ValueError, "only stage 0 workers can receive source batches"):
            exp._validate_pipeline_source_piece_workers("train_1", "pipeline", source_pieces)

    def test_pipeline_mode_accepts_stage0_source_targets(self) -> None:
        components = NetworkComponents(_base_dc())
        exp = ExperimentFlow("exp", 4, components, temp_data_path="/tmp/nerlnet_parallel_schema_tests")
        source_pieces = [
            {
                "sourceName": "source_a",
                "workers": "worker_a0",
            }
        ]
        exp._validate_pipeline_source_piece_workers("train_1", "pipeline", source_pieces)

    def test_pipeline_stage_coverage_mismatch_rejected(self) -> None:
        dc = _base_dc()
        dc["workers"][1]["parallel"]["pipelineStage"] = 2
        with self.assertRaises(ValueError):
            NetworkComponents(dc)

    def test_tp_group_spanning_multiple_stages_rejected(self) -> None:
        dc = _base_dc()
        dc["workers"][1]["parallel"]["tpGroup"] = "tp_g0"
        with self.assertRaises(ValueError):
            NetworkComponents(dc)

    def test_tensor_phase_requires_tp_metadata(self) -> None:
        dc = _base_dc()
        for worker in dc["workers"]:
            worker["parallel"].pop("tpGroup", None)
            worker["parallel"].pop("tpRank", None)
            worker["parallel"].pop("tpWorldSize", None)
        components = NetworkComponents(dc)
        exp = ExperimentFlow("exp", 4, components, temp_data_path="/tmp/nerlnet_parallel_schema_tests")
        phase = {
            EXPFLOW_PHASES_PHASE_NAME_FIELD: "train_tensor",
            EXPFLOW_PHASES_PARALLEL_EXECUTION_FIELD: {
                EXPFLOW_PARALLEL_EXECUTION_MODE_FIELD: "tensor",
                EXPFLOW_PARALLEL_EXECUTION_SUPER_NODE_FIELD: "super_0",
            },
        }
        with self.assertRaises(ValueError):
            exp._parse_parallel_execution(phase)

    def test_connection_map_with_super_node_is_validated(self) -> None:
        components = NetworkComponents(_base_dc())
        valid_conn_map = {
            "router_a": ["mainServer", "source_a", "client_a", "client_b", "super_0"]
        }
        components.validate_connection_map(valid_conn_map)

    def test_connection_map_missing_super_node_is_rejected(self) -> None:
        components = NetworkComponents(_base_dc())
        invalid_conn_map = {
            "router_a": ["mainServer", "source_a", "client_a", "client_b"]
        }
        with self.assertRaises(ValueError):
            components.validate_connection_map(invalid_conn_map)

    def test_legacy_fixture_parses_without_parallel_fields(self) -> None:
        dc_path = REPO_ROOT / "tests" / "inputJsonsFiles" / "dc_test_synt_1d_2c_1s_4r_4w.json.noip"
        with dc_path.open("r", encoding="utf-8") as dc_file_obj:
            dc = json.load(dc_file_obj)
        components = NetworkComponents(dc)
        self.assertFalse(components.has_super_nodes())
        self.assertEqual(components.get_worker_parallel_map(), {})


if __name__ == "__main__":
    unittest.main()
