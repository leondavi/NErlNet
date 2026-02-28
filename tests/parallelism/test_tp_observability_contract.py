#!/usr/bin/env python3
"""Contract tests for TP observability across worker stats, Stats API, and summary CSV."""

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

if not hasattr(np, "ndarray"):
    sys.modules.pop("numpy", None)
    np = importlib.import_module("numpy")

if not hasattr(pd, "DataFrame"):
    sys.modules.pop("pandas", None)
    pd = importlib.import_module("pandas")

REPO_ROOT = Path(__file__).resolve().parents[2]
sys.path.insert(0, str(REPO_ROOT / "src_py" / "apiServer"))

# Optional runtime modules used by apiServer stats are not required for this unit test.
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

# Ensure we import real modules (other tests may stub them).
sys.modules.pop("stats", None)

from NerlComDB import WorkerComDB  # noqa: E402
from definitions import PHASE_PREDICTION_STR, PHASE_TRAINING_STR  # noqa: E402
from exp_summary import ExperimentSummary  # noqa: E402
from stats import Stats  # noqa: E402


def _base_worker_payload() -> dict:
    return {
        "bytes_received": 1,
        "bytes_sent": 2,
        "bad_messages": 0,
        "batches_received_train": 3,
        "batches_received_predict": 4,
        "batches_dropped_train": 0,
        "batches_dropped_predict": 0,
        "batches_sent_train": 3,
        "empty_batches": 0,
        "batches_sent_predict": 4,
        "average_time_training": 10,
        "average_time_prediction": 5,
        "acc_time_training": 100,
        "acc_time_prediction": 40,
        "nan_loss_count": 0,
    }


class _FakeEntityStats:
    def __init__(self, payload):
        self._payload = payload

    def get_as_dict(self):
        return dict(self._payload)


class _FakeCommDb:
    def __init__(self, workers_payload: OrderedDict[str, dict]):
        self._workers_payload = workers_payload

    def get_workers(self):
        return OrderedDict(
            (worker_name, _FakeEntityStats(payload))
            for worker_name, payload in self._workers_payload.items()
        )

    def get_sources(self):
        return {}

    def get_clients(self):
        return {}

    def get_routers(self):
        return {}

    def get_main_server(self):
        return _FakeEntityStats({"bytes_received": 0, "bytes_sent": 0})


class _FakePerfDb:
    def get_clients(self):
        return {}


class _FakeModelDb:
    def get_workers_model_db_list(self):
        return []


class _FakeNetworkComponents:
    def __init__(self, workers):
        self._workers = workers

    def get_workers_list(self):
        return list(self._workers)

    def get_freq(self):
        return 1

    def get_batch_size(self):
        return 2

    def get_num_of_sources(self):
        return 1

    def get_source_epochs_dict(self):
        return {}


class _FakeStatsPhase:
    def __init__(self, workers_payload: OrderedDict[str, dict]):
        self._comm_db = _FakeCommDb(workers_payload)
        self._perf_db = _FakePerfDb()
        self._model_db = _FakeModelDb()
        self._network = _FakeNetworkComponents(workers_payload.keys())

    def get_nerl_model_db(self):
        return self._model_db

    def get_nerl_comm_db(self):
        return self._comm_db

    def get_nerl_perf_db(self):
        return self._perf_db

    def get_phase_type(self):
        return PHASE_TRAINING_STR

    def get_experiment_name(self):
        return "tp_observability_contract"

    def get_network_components(self):
        return self._network

    def get_experiment_flow_type(self):
        return "classification"

    def get_sources_pieces(self):
        return []

    def get_experiment_flow_name(self):
        return "tp_observability_contract"

    def get_name(self):
        return "Training1"


class _FakeSummaryStats:
    def __init__(self, experiment_name: str, tp_count: int, tp_latency_us: int):
        self.workers_list = ["w1", "w2"]
        self.batch_size = 5
        self.freq = 2
        self.num_of_sources = 1
        self._experiment_name = experiment_name
        self._tp_count = tp_count
        self._tp_latency_us = tp_latency_us

    def get_phase(self):
        return PHASE_PREDICTION_STR

    def get_name(self):
        return self._experiment_name

    def get_confusion_matrices(self):
        confusion = np.array([[8, 1], [1, 10]])
        return {("s1", "w1", "c0"): confusion}, {("w1", "c0"): confusion}

    def get_model_performence_stats(self, _confusion_matrix_worker_dict):
        return pd.DataFrame(
            [
                {
                    "Worker": "w1",
                    "Class": "c0",
                    "TN": 8,
                    "FP": 1,
                    "FN": 1,
                    "TP": 10,
                    "Accuracy": 0.9,
                    "Precision": 0.91,
                    "F1": 0.9,
                }
            ]
        )

    def get_communication_stats_workers(self):
        return {
            "w1": {
                "batches_received_train": 100,
                "batches_dropped_train": 0,
                "batches_received_predict": 20,
                "batches_dropped_predict": 1,
                "tp_collective_count": self._tp_count,
                "tp_collective_latency_us": self._tp_latency_us,
            },
            "w2": {
                "batches_received_train": 100,
                "batches_dropped_train": 0,
                "batches_received_predict": 20,
                "batches_dropped_predict": 1,
            },
        }

    def get_performance_stats_clients(self):
        payload = {
            "time_predict_active": 1.0,
            "time_predict_total": 2.0,
            "memory_predict_ema_usage": 10.0,
            "memory_predict_peak_usage": 12.0,
            "num_of_cores": 1,
            "cpu_predict_util_per_core": {0: 0.2},
        }
        return {"w1": payload, "w2": payload}


class TpObservabilityContractTests(unittest.TestCase):
    def test_worker_com_db_roundtrip_includes_tp_fields(self):
        payload = _base_worker_payload()
        payload["tp_collective_count"] = 5
        payload["tp_collective_latency_us"] = 500
        db = WorkerComDB()
        db.update_stats(payload)
        exported = db.get_as_dict()

        self.assertEqual(exported["tp_collective_count"], 5)
        self.assertEqual(exported["tp_collective_latency_us"], 500)
        self.assertAlmostEqual(exported["tp_collective_avg_latency_us"], 100.0)

    def test_worker_com_db_missing_tp_payload_defaults_to_zero(self):
        db = WorkerComDB()
        db.update_stats(_base_worker_payload())
        exported = db.get_as_dict()

        self.assertEqual(exported["tp_collective_count"], 0)
        self.assertEqual(exported["tp_collective_latency_us"], 0)
        self.assertEqual(exported["tp_collective_avg_latency_us"], 0.0)

    def test_stats_tensor_parallel_dataframe_contract(self):
        workers_payload = OrderedDict(
            {
                "w1": {
                    "batches_received_predict": 10,
                    "tp_collective_count": 20,
                    "tp_collective_latency_us": 2000,
                },
                "w2": {
                    "batches_received_predict": 5,
                },
            }
        )
        stats_obj = Stats(_FakeStatsPhase(workers_payload))
        tp_df = stats_obj.get_tensor_parallel_stats()

        self.assertListEqual(
            list(tp_df.columns),
            [
                "tp_collective_count",
                "tp_collective_latency_us",
                "tp_collective_avg_latency_us",
                "tp_collective_per_predict_batch",
            ],
        )
        self.assertEqual(tp_df.loc["w1", "tp_collective_count"], 20)
        self.assertEqual(tp_df.loc["w1", "tp_collective_latency_us"], 2000)
        self.assertAlmostEqual(tp_df.loc["w1", "tp_collective_avg_latency_us"], 100.0)
        self.assertAlmostEqual(tp_df.loc["w1", "tp_collective_per_predict_batch"], 2.0)
        self.assertEqual(tp_df.loc["w2", "tp_collective_count"], 0)
        self.assertEqual(tp_df.loc["w2", "tp_collective_latency_us"], 0)
        self.assertEqual(tp_df.loc["w2", "tp_collective_avg_latency_us"], 0.0)
        self.assertEqual(tp_df.loc["w2", "tp_collective_per_predict_batch"], 0.0)

    def test_stats_tensor_parallel_dataframe_empty_workers(self):
        stats_obj = Stats(_FakeStatsPhase(OrderedDict()))
        tp_df = stats_obj.get_tensor_parallel_stats()
        self.assertListEqual(
            list(tp_df.columns),
            [
                "tp_collective_count",
                "tp_collective_latency_us",
                "tp_collective_avg_latency_us",
            ],
        )
        self.assertTrue(tp_df.empty)

    def test_experiment_summary_includes_tp_columns_and_preserves_dedupe(self):
        with tempfile.TemporaryDirectory() as tmp_dir:
            output_path = Path(tmp_dir) / "summary.csv"

            stats_v1 = _FakeSummaryStats("PTD_EXP", tp_count=30, tp_latency_us=3000)
            summary_v1 = ExperimentSummary([stats_v1])
            row_v1 = summary_v1.generate_summary_row(stats_v1)

            self.assertIn("w1 TP Collective Count", row_v1)
            self.assertIn("w1 TP Collective Latency (us)", row_v1)
            self.assertIn("w1 TP Avg Collective Latency (us)", row_v1)
            self.assertGreaterEqual(row_v1["w1 TP Collective Count"], 0)
            self.assertGreaterEqual(row_v1["w1 TP Collective Latency (us)"], 0)
            self.assertGreaterEqual(row_v1["w1 TP Avg Collective Latency (us)"], 0.0)
            self.assertEqual(row_v1["w2 TP Collective Count"], 0)
            self.assertEqual(row_v1["w2 TP Collective Latency (us)"], 0)
            self.assertEqual(row_v1["w2 TP Avg Collective Latency (us)"], 0.0)

            df1 = summary_v1.generate_summary_csv(str(output_path), force_append=False)
            self.assertIn("w1 TP Collective Count", df1.columns)
            self.assertEqual(len(df1), 1)

            stats_v2 = _FakeSummaryStats("PTD_EXP", tp_count=50, tp_latency_us=5000)
            summary_v2 = ExperimentSummary([stats_v2])
            summary_v2.generate_summary_csv(str(output_path), force_append=False)

            persisted = pd.read_csv(output_path)
            self.assertEqual(len(persisted), 1)
            self.assertEqual(int(persisted.iloc[0]["w1 TP Collective Count"]), 50)
            self.assertEqual(int(persisted.iloc[0]["w1 TP Collective Latency (us)"]), 5000)


if __name__ == "__main__":
    unittest.main()
