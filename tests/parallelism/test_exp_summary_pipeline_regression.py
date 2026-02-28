#!/usr/bin/env python3
"""Regression checks for ExperimentSummary + prediction stats aggregation."""

from __future__ import annotations

import sys
import tempfile
import unittest
from pathlib import Path
import types
import importlib

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

# Ensure we import the real apiServer stats module (other tests may stub it).
sys.modules.pop("stats", None)

from definitions import PHASE_PREDICTION_STR  # noqa: E402
from exp_summary import ExperimentSummary  # noqa: E402
from nerl_model_db import NerlModelDB  # noqa: E402
from stats import Stats  # noqa: E402


class _FakeCsvDatasetParent:
    def __init__(self, labels_path: str, headers: list[str]):
        self._labels_path = labels_path
        self._headers = headers

    def genrate_source_piece_ds_csv_file_labels(self, _source_piece, _phase_type):
        return self._labels_path

    def get_headers_row(self):
        return self._headers


class _FakeSourcePiece:
    def __init__(self, source_name: str, batch_size: int, num_batches: int, csv_parent):
        self._source_name = source_name
        self._batch_size = batch_size
        self._num_batches = num_batches
        self._target_workers = "w1"
        self._csv_parent = csv_parent
        self._labels_pointer = ""

    def get_nerltensor_type(self):
        return "float"

    def get_source_name(self):
        return self._source_name

    def get_target_workers(self):
        return self._target_workers

    def get_batch_size(self):
        return self._batch_size

    def get_num_of_batches(self):
        return self._num_batches

    def get_csv_dataset_parent(self):
        return self._csv_parent

    def set_pointer_to_sourcePiece_CsvDataSet_labels(self, labels_path: str):
        self._labels_pointer = labels_path

    def get_pointer_to_sourcePiece_CsvDataSet_labels(self):
        return self._labels_pointer


class _FakeNetworkComponents:
    def __init__(self):
        self.sourceEpochs = {"s1": 1}
        self.sources_policy_dict = {"s1": "0"}

    def get_workers_list(self):
        return ["w1", "w2"]

    def get_freq(self):
        return 1

    def get_batch_size(self):
        return 2

    def get_num_of_sources(self):
        return 1

    def get_source_epochs_dict(self):
        return dict(self.sourceEpochs)

    def get_client_name_by_worker_name(self, _worker_name):
        return "client_1"


class _FakeEntityStats:
    def __init__(self, payload):
        self._payload = payload

    def get_as_dict(self):
        return dict(self._payload)


class _FakeCommDb:
    def get_workers(self):
        payload = {
            "batches_received_train": 0,
            "batches_dropped_train": 0,
            "batches_received_predict": 2,
            "batches_dropped_predict": 0,
        }
        return {
            "w1": _FakeEntityStats(payload),
            "w2": _FakeEntityStats(payload),
        }

    def get_sources(self):
        return {}

    def get_clients(self):
        return {}

    def get_routers(self):
        return {}

    def get_main_server(self):
        return _FakeEntityStats({})


class _FakePerfDb:
    def get_clients(self):
        payload = {
            "time_predict_active": 1.0,
            "time_predict_total": 1.0,
            "memory_predict_ema_usage": 1.0,
            "memory_predict_peak_usage": 1.0,
            "num_of_cores": 1,
            "cpu_predict_util_per_core": {0: 0.1},
        }
        return {"client_1": _FakeEntityStats(payload)}


class _FakeExperimentPhase:
    def __init__(self, model_db, source_piece):
        self._model_db = model_db
        self._source_piece = source_piece
        self._network = _FakeNetworkComponents()
        self._comm_db = _FakeCommDb()
        self._perf_db = _FakePerfDb()

    def get_nerl_model_db(self):
        return self._model_db

    def get_nerl_comm_db(self):
        return self._comm_db

    def get_nerl_perf_db(self):
        return self._perf_db

    def get_phase_type(self):
        return PHASE_PREDICTION_STR

    def get_experiment_name(self):
        return "synthetic_pipeline_regression"

    def get_network_components(self):
        return self._network

    def get_experiment_flow_type(self):
        return "classification"

    def get_sources_pieces(self):
        return [self._source_piece]

    def get_experiment_flow_name(self):
        return "synthetic_pipeline_regression"

    def get_name(self):
        return "Prediction1"


class ExperimentSummaryPipelineRegressionTests(unittest.TestCase):
    def test_stage0_targeting_does_not_force_zero_metrics(self) -> None:
        with tempfile.TemporaryDirectory() as tmp_dir:
            labels = pd.DataFrame(
                [
                    [1, 0, 0],
                    [0, 1, 0],
                    [0, 0, 1],
                    [1, 0, 0],
                ]
            )
            labels_path = Path(tmp_dir) / "labels.csv"
            labels.to_csv(labels_path, index=False)

            csv_parent = _FakeCsvDatasetParent(str(labels_path), ["c0", "c1", "c2"])
            source_piece = _FakeSourcePiece("s1", batch_size=2, num_batches=2, csv_parent=csv_parent)

            model_db = NerlModelDB(PHASE_PREDICTION_STR)
            client_db = model_db.get_client("client_1")
            w1 = client_db.get_worker("w1")
            w2 = client_db.get_worker("w2")

            # Stage-0 worker carries intermediate activations (shape mismatch with labels).
            w1.create_batch("0", "s1", np.zeros((2, 4), dtype=np.float32), 0, "none", 0)
            w1.create_batch("1", "s1", np.zeros((2, 4), dtype=np.float32), 0, "none", 1)

            # Last-stage worker carries final logits/labels-compatible predictions.
            w2.create_batch("0", "s1", labels.iloc[0:2].to_numpy(dtype=np.float32), 0, "none", 2)
            w2.create_batch("1", "s1", labels.iloc[2:4].to_numpy(dtype=np.float32), 0, "none", 3)

            phase = _FakeExperimentPhase(model_db, source_piece)
            stats_obj = Stats(phase)
            summary = ExperimentSummary([stats_obj])

            confusion_by_source, confusion_by_worker = stats_obj.get_confusion_matrices()
            worker_names = {worker for (worker, _class_name) in confusion_by_worker.keys()}

            self.assertTrue(confusion_by_source)
            self.assertTrue(confusion_by_worker)
            self.assertEqual(worker_names, {"w2"})

            perf = summary.get_model_performance_aggregates(stats_obj)
            self.assertGreater(perf["min_accuracy"], 0.99)
            self.assertGreater(perf["min_f1"], 0.99)


if __name__ == "__main__":
    unittest.main()
