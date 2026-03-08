#!/usr/bin/env python3
from __future__ import annotations

import importlib
import sys
import tempfile
import types
import unittest
from collections import OrderedDict
from pathlib import Path

import numpy as np
import pandas as pd

REPO_ROOT = Path(__file__).resolve().parents[2]
sys.path.insert(0, str(REPO_ROOT / "src_py" / "apiServer"))
sys.path.insert(0, str(REPO_ROOT / "JupyterLabDir" / "report_pipeline"))

if "comm" not in sys.modules:
    sys.modules["comm"] = types.ModuleType("comm")

if "IPython" not in sys.modules:
    ipython_mod = types.ModuleType("IPython")
    ipython_mod.get_ipython = lambda: None
    display_mod = types.ModuleType("IPython.display")
    display_mod.display = lambda _value: None
    ipython_mod.display = display_mod
    sys.modules["IPython"] = ipython_mod
    sys.modules["IPython.display"] = display_mod

if "matplotlib" not in sys.modules:
    matplotlib_mod = types.ModuleType("matplotlib")
    pyplot_mod = types.ModuleType("matplotlib.pyplot")
    matplotlib_mod.pyplot = pyplot_mod
    sys.modules["matplotlib"] = matplotlib_mod
    sys.modules["matplotlib.pyplot"] = pyplot_mod

if "seaborn" not in sys.modules:
    seaborn_mod = types.ModuleType("seaborn")
    seaborn_mod.set_theme = lambda *args, **kwargs: None
    seaborn_mod.set = lambda *args, **kwargs: None
    sys.modules["seaborn"] = seaborn_mod

sys.modules.pop("stats", None)
from stats import Stats  # noqa: E402
import run_matrix  # noqa: E402


class _FakeEntityStats:
    def __init__(self, payload):
        self._payload = payload

    def get_as_dict(self):
        return dict(self._payload)


class _FakeCommDb:
    def __init__(self):
        self._workers = OrderedDict(
            [
                (
                    "w1",
                    _FakeEntityStats(
                        {
                            "batches_dropped_train": 2,
                            "batches_dropped_predict": 1,
                            "bytes_received": 0,
                            "bytes_sent": 0,
                            "bad_messages": 0,
                            "batches_received_train": 0,
                            "batches_received_predict": 0,
                            "batches_sent_train": 0,
                            "batches_sent_predict": 0,
                            "empty_batches": 0,
                            "average_time_training": 0,
                            "average_time_prediction": 0,
                            "acc_time_training": 0,
                            "acc_time_prediction": 0,
                            "nan_loss_count": 0,
                            "tp_collective_count": 0,
                            "tp_collective_latency_us": 0,
                        }
                    ),
                )
            ]
        )

    def get_workers(self):
        return self._workers

    def get_sources(self):
        return OrderedDict()

    def get_clients(self):
        return OrderedDict()

    def get_routers(self):
        return OrderedDict()

    def get_super_nodes(self):
        return OrderedDict()

    def get_main_server(self):
        return _FakeEntityStats({"messages_received": 0, "messages_sent": 0, "messages_dropped": 0, "bytes_received": 0, "bytes_sent": 0, "bad_messages": 0})


class _FakePerfDb:
    def get_clients(self):
        return {"client_1": _FakeEntityStats({"time_train_total": 2_000_000, "time_predict_total": 1_000_000})}


class _FakeModelDb:
    def get_workers_model_db_list(self):
        return []


class _FakeSourcePiece:
    def __init__(self, source_name: str, num_batches: int):
        self._source_name = source_name
        self._num_batches = num_batches

    def get_source_name(self):
        return self._source_name

    def get_num_of_batches(self):
        return self._num_batches


class _FakeNetworkComponents:
    def __init__(self):
        self.worker_parallel_map = {"w1": {"pipeline_stage": 0, "tp_group": "tp_g0", "tp_rank": 0}}
        self.map_worker_to_client = {"w1": "client_1"}
        self.map_entity_to_device = {"client_1": "dev0", "w1": "dev0"}
        self.map_device_to_ip = {"dev0": "127.0.0.1"}

    def get_workers_list(self):
        return ["w1"]

    def get_freq(self):
        return 5

    def get_batch_size(self):
        return 64

    def get_num_of_sources(self):
        return 1

    def get_source_epochs_dict(self):
        return {"s1": 1}

    def get_client_name_by_worker_name(self, worker_name):
        return self.map_worker_to_client[worker_name]


class _FakePhase:
    def __init__(self):
        self._network = _FakeNetworkComponents()
        self._trace = [
            {
                "worker": "w1",
                "client": "client_1",
                "device_ip": "127.0.0.1",
                "pipeline_stage": 0,
                "tp_group": "tp_g0",
                "tp_rank": 0,
                "batch_id": 0,
                "microbatch_id": 0,
                "stage_id": 0,
                "fwd_compute_us": 100,
                "bwd_compute_us": 50,
                "predict_compute_us": 0,
                "act_send_us": 10,
                "act_recv_wait_us": 20,
                "grad_send_us": 5,
                "grad_recv_wait_us": 15,
                "tp_collective_us": 25,
                "optimizer_barrier_us": 7,
                "wait_for_grant_us": 8,
                "wait_for_input_us": 12,
                "wait_for_output_slot_us": 0,
                "wait_other_us": 0,
                "bytes_act_sent": 1024,
                "bytes_grad_sent": 256,
                "bytes_tp_collective": 2048,
                "status": "completed",
                "skip_reason": "",
            },
            {
                "worker": "w1",
                "client": "client_1",
                "device_ip": "127.0.0.1",
                "pipeline_stage": 0,
                "tp_group": "tp_g0",
                "tp_rank": 0,
                "batch_id": 0,
                "microbatch_id": 1,
                "stage_id": 0,
                "fwd_compute_us": 90,
                "bwd_compute_us": 40,
                "predict_compute_us": 0,
                "act_send_us": 10,
                "act_recv_wait_us": 10,
                "grad_send_us": 5,
                "grad_recv_wait_us": 10,
                "tp_collective_us": 20,
                "optimizer_barrier_us": 0,
                "wait_for_grant_us": 10,
                "wait_for_input_us": 5,
                "wait_for_output_slot_us": 0,
                "wait_other_us": 0,
                "bytes_act_sent": 1000,
                "bytes_grad_sent": 200,
                "bytes_tp_collective": 2000,
                "status": "skipped",
                "skip_reason": "skip_completion_timeout",
            },
        ]

    def get_nerl_model_db(self):
        return _FakeModelDb()

    def get_nerl_comm_db(self):
        return _FakeCommDb()

    def get_nerl_perf_db(self):
        return _FakePerfDb()

    def get_phase_type(self):
        return "training"

    def get_experiment_name(self):
        return "report_contract"

    def get_network_components(self):
        return self._network

    def get_experiment_flow_type(self):
        return "classification"

    def get_sources_pieces(self):
        return [_FakeSourcePiece("s1", 2)]

    def get_experiment_flow_name(self):
        return "report_contract"

    def get_name(self):
        return "Training1"

    def get_parallel_trace_records(self):
        return list(self._trace)

    def get_parallel_execution(self):
        return {"mode": "pipeline_tensor", "scheduler": "1f1b", "numMicroBatches": 2}


class _FakeLegacyCommDb(_FakeCommDb):
    def __init__(self):
        self._workers = OrderedDict(
            [
                (
                    "w1",
                    _FakeEntityStats(
                        {
                            "batches_dropped_train": 1,
                            "batches_dropped_predict": 0,
                            "bytes_received": 0,
                            "bytes_sent": 0,
                            "bad_messages": 0,
                            "batches_received_train": 199,
                            "batches_received_predict": 200,
                            "batches_sent_train": 199,
                            "batches_sent_predict": 200,
                            "empty_batches": 0,
                            "average_time_training": 27837,
                            "average_time_prediction": 6579,
                            "acc_time_training": 5_539_633,
                            "acc_time_prediction": 1_315_793,
                            "nan_loss_count": 0,
                            "tp_collective_count": 0,
                            "tp_collective_latency_us": 0,
                            "batches_completed_train": 199,
                            "batches_completed_predict": 200,
                            "skip_grant_accept_timeout": 0,
                            "skip_payload_delivery_timeout": 0,
                            "skip_completion_timeout": 0,
                            "skip_phase_close_drain": 0,
                            "stale_event_after_skip": 0,
                        }
                    ),
                )
            ]
        )


class _FakeLegacyPhase(_FakePhase):
    def __init__(self):
        self._network = _FakeNetworkComponents()
        self._trace = []

    def get_nerl_comm_db(self):
        return _FakeLegacyCommDb()

    def get_parallel_execution(self):
        return {"mode": "legacy"}


class _FakeCharlistPhase(_FakePhase):
    def __init__(self):
        super().__init__()
        for record in self._trace:
            record["worker"] = [119, 49]
            record["client"] = [99, 108, 105, 101, 110, 116, 95, 49]
            record["device_ip"] = [49, 50, 55, 46, 48, 46, 48, 46, 49]
            record["tp_group"] = [116, 112, 95, 103, 48]
            record["status"] = [ord(ch) for ch in record["status"]]
            record["skip_reason"] = [ord(ch) for ch in record["skip_reason"]]


class ReportPipelineStatsContractTests(unittest.TestCase):
    def test_trace_and_report_dataframes_are_non_empty_and_bounded(self) -> None:
        stats_obj = Stats(_FakePhase())
        trace_df = stats_obj.get_parallel_trace_df()
        step_df = stats_obj.get_step_summary_df()
        report_df = stats_obj.get_report_metrics_df()

        self.assertFalse(trace_df.empty)
        self.assertFalse(step_df.empty)
        self.assertFalse(report_df.empty)
        self.assertTrue((step_df["bubble_fraction"] >= 0).all())
        self.assertTrue((step_df["bubble_fraction"] <= 1).all())
        self.assertTrue((step_df["comm_fraction"] >= 0).all())
        self.assertTrue((step_df["comm_fraction"] <= 1).all())
        self.assertEqual(int(report_df.iloc[0]["expected_microbatches"]), 4)
        self.assertEqual(int(report_df.iloc[0]["completed_microbatches"]), 1)
        self.assertEqual(int(report_df.iloc[0]["skipped_microbatches"]), 1)

    def test_legacy_report_fallback_emits_non_empty_phase_metrics_without_trace(self) -> None:
        stats_obj = Stats(_FakeLegacyPhase())
        trace_df = stats_obj.get_parallel_trace_df()
        step_df = stats_obj.get_step_summary_df()
        report_df = stats_obj.get_report_metrics_df()

        self.assertTrue(trace_df.empty)
        self.assertTrue(step_df.empty)
        self.assertFalse(report_df.empty)
        self.assertEqual(str(report_df.iloc[0]["parallel_mode"]), "legacy")
        self.assertEqual(int(report_df.iloc[0]["expected_microbatches"]), 2)
        self.assertEqual(int(report_df.iloc[0]["completed_microbatches"]), 199)
        self.assertEqual(int(report_df.iloc[0]["source_dropped_batches"]), 1)
        self.assertGreater(float(report_df.iloc[0]["throughput_sps"]), 0.0)

    def test_write_run_artifacts_normalizes_run_label_to_report_label(self) -> None:
        stats_obj = Stats(_FakePhase())
        entry = {
            "label": "expected_report_label",
            "kind": "contract",
            "deviceCount": 2,
            "mode": "pipeline_tensor",
            "scheduler": "1f1b",
            "numMicroBatches": 2,
            "microBatchSize": 32,
        }
        with tempfile.TemporaryDirectory(prefix="nerlnet_report_artifacts_") as tmpdir:
            artifacts = run_matrix.write_run_artifacts(Path(tmpdir), [stats_obj], entry, 1)
            self.assertFalse(artifacts["phase"].empty)
            self.assertTrue((artifacts["phase"]["run_label"] == entry["label"]).all())
            self.assertTrue((artifacts["trace"]["run_label"] == entry["label"]).all())
            self.assertTrue((artifacts["step"]["run_label"] == entry["label"]).all())

    def test_parallel_trace_decodes_erlang_charlists_and_uses_microbatch_size_for_throughput(self) -> None:
        stats_obj = Stats(_FakeCharlistPhase())
        trace_df = stats_obj.get_parallel_trace_df()
        report_df = stats_obj.get_report_metrics_df()

        self.assertEqual(trace_df.iloc[0]["worker"], "w1")
        self.assertEqual(trace_df.iloc[0]["client"], "client_1")
        self.assertEqual(trace_df.iloc[0]["status"], "completed")
        self.assertEqual(trace_df.iloc[1]["status"], "skipped")
        self.assertEqual(int(report_df.iloc[0]["completed_microbatches"]), 1)
        self.assertAlmostEqual(float(report_df.iloc[0]["throughput_sps"]), 32.0, places=6)


if __name__ == "__main__":
    unittest.main()
