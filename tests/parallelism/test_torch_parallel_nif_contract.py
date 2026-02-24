#!/usr/bin/env python3
"""Contract checks for Torch microbatch/barrier runtime surfaces."""

from __future__ import annotations

import unittest
from pathlib import Path


REPO_ROOT = Path(__file__).resolve().parents[2]
WORKER_GENERIC = REPO_ROOT / "src_erl" / "NerlnetApp" / "src" / "Bridge" / "onnWorkers" / "workerGeneric.erl"
TORCH_ERL = REPO_ROOT / "src_erl" / "NerlnetApp" / "src" / "Bridge" / "torchWorkers" / "nerlTorchNIF.erl"
ONNN_ERL = REPO_ROOT / "src_erl" / "NerlnetApp" / "src" / "Bridge" / "onnWorkers" / "nerlNIF.erl"
JSON_PARSER = REPO_ROOT / "src_erl" / "NerlnetApp" / "src" / "Init" / "jsonParser.erl"
TORCH_NIF_H = REPO_ROOT / "src_cpp" / "torchBridge" / "torchNIF.h"
TORCH_NIF_CPP = REPO_ROOT / "src_cpp" / "torchBridge" / "torchNIF.cpp"
TORCH_WORKER_H = REPO_ROOT / "src_cpp" / "torchBridge" / "NerlWorkerTorch.h"
TORCH_WORKER_CPP = REPO_ROOT / "src_cpp" / "torchBridge" / "NerlWorkerTorch.cpp"


class TorchParallelNifContractTests(unittest.TestCase):
    def test_worker_microbatch_path_is_torch_gated(self) -> None:
        content = WORKER_GENERIC.read_text(encoding="utf-8")
        self.assertIn("NifModule =:= nerlTorchNIF", content)
        self.assertIn("call_to_train_microbatch", content)
        self.assertIn("super_node_authority_required", content)
        self.assertIn("maybe_consume_parallel_scheduler_grant", content)
        self.assertIn("maybe_apply_tensor_parallel_collectives", content)
        self.assertIn("execute_tp_column_collective", content)
        self.assertIn("execute_tp_row_collective", content)
        self.assertIn("tp_collective_payload", content)
        self.assertIn("parallel_deferred_samples", content)
        self.assertIn("queue_deferred_parallel_sample", content)
        self.assertIn("maybe_dispatch_deferred_parallel_sample", content)
        self.assertNotIn("Fallback to legacy semantics if no pending parallel accumulation context exists.", content)
        self.assertNotIn("tensor_parallel_runtime_not_implemented", content)

    def test_torch_erlang_bridge_exports_parallel_training_apis(self) -> None:
        content = TORCH_ERL.read_text(encoding="utf-8")
        self.assertIn("train_microbatch_nif/4", content)
        self.assertIn("optimizer_barrier_nif/1", content)
        self.assertIn("call_to_train_microbatch/5", content)
        self.assertIn("call_to_optimizer_barrier/1", content)

    def test_opennn_bridge_exposes_compatibility_stubs(self) -> None:
        content = ONNN_ERL.read_text(encoding="utf-8")
        self.assertIn("train_microbatch_nif/4", content)
        self.assertIn("call_to_train_microbatch/5", content)
        self.assertIn("call_to_optimizer_barrier/1", content)

    def test_torch_native_nif_registers_parallel_symbols(self) -> None:
        content = TORCH_NIF_H.read_text(encoding="utf-8")
        self.assertIn("train_microbatch_nif", content)
        self.assertIn("optimizer_barrier_nif", content)
        self.assertIn('{"train_microbatch_nif", 4, train_microbatch_nif}', content)
        self.assertIn('{"optimizer_barrier_nif", 1, optimizer_barrier_nif}', content)

    def test_torch_train_thread_emits_microbatch_aware_tuple(self) -> None:
        content = TORCH_NIF_CPP.read_text(encoding="utf-8")
        self.assertIn("thread_args_ptr->is_microbatch", content)
        self.assertIn("enif_make_tuple(env,", content)
        self.assertIn("5,", content)
        self.assertIn("thread_args_ptr->microbatch_id", content)

    def test_torch_worker_supports_deferred_optimizer_barrier(self) -> None:
        h_content = TORCH_WORKER_H.read_text(encoding="utf-8")
        cpp_content = TORCH_WORKER_CPP.read_text(encoding="utf-8")

        self.assertIn("train_microbatch", h_content)
        self.assertIn("optimizer_barrier", h_content)
        self.assertIn("_has_deferred_gradients", h_content)

        self.assertIn("train_batch_impl", cpp_content)
        self.assertIn("_deferred_microbatch_count", cpp_content)
        self.assertIn("optimizer_barrier", cpp_content)
        self.assertIn("defer_optimizer_step", cpp_content)

    def test_json_parser_stores_model_tp_plan_map_for_runtime(self) -> None:
        content = JSON_PARSER.read_text(encoding="utf-8")
        self.assertIn("extract_tp_plan(ModelParams)", content)
        self.assertIn("model_tp_plan_map", content)
        self.assertIn("normalize_tp_plan_entry", content)


if __name__ == "__main__":
    unittest.main()
