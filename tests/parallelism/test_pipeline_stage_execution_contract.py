#!/usr/bin/env python3
"""Contract checks for stage-sliced pipeline runtime execution paths."""

from __future__ import annotations

import unittest
from pathlib import Path


REPO_ROOT = Path(__file__).resolve().parents[2]
WORKER_GENERIC = REPO_ROOT / "src_erl" / "NerlnetApp" / "src" / "Bridge" / "onnWorkers" / "workerGeneric.erl"
W2W_COM = REPO_ROOT / "src_erl" / "NerlnetApp" / "src" / "Bridge" / "Common" / "w2wCom.erl"
TORCH_WORKER_CPP = REPO_ROOT / "src_cpp" / "torchBridge" / "NerlWorkerTorch.cpp"


class PipelineStageExecutionContractTests(unittest.TestCase):
    def test_worker_runtime_has_stage_payload_buffers_and_dispatch(self) -> None:
        content = WORKER_GENERIC.read_text(encoding="utf-8")
        self.assertIn("parallel_pipeline_forward_buffer", content)
        self.assertIn("parallel_pipeline_backward_buffer", content)
        self.assertIn("parallel_pipeline_predict_buffer", content)
        self.assertIn("dispatch_pipeline_stage0_forward_microbatch_loop", content)
        self.assertIn("dispatch_pipeline_stage0_predict_microbatch_loop", content)
        self.assertIn("maybe_process_pipeline_forward_payload", content)
        self.assertIn("maybe_process_pipeline_backward_payload", content)
        self.assertIn("maybe_process_pipeline_predict_payload", content)
        self.assertIn("resolve_pipeline_adjacent_worker", content)
        self.assertIn("route_pipeline_payload_to_worker", content)
        self.assertIn("stage0 pipeline batch=", content)
        self.assertIn("pipeline last-stage completed batch", content)

    def test_worker_runtime_handles_backward_ack_and_stage0_predict_turnover(self) -> None:
        content = WORKER_GENERIC.read_text(encoding="utf-8")
        self.assertIn("pipeline_last_stage_backward_event_rejected", content)
        self.assertIn("maybe_dispatch_pending_parallel_backward_events(GenWorkerEts)", content)
        self.assertIn("dispatch_pipeline_backward_buffer_by_grant", content)
        self.assertIn("peek_pipeline_backward_grant", content)
        self.assertIn("pop_pipeline_backward_payload_for_microbatch", content)
        self.assertIn("parallel_forward_event_meta(pipeline_predict)", content)
        self.assertIn("maps:get(forward_completed, Ctx, maps:get(forward_dispatched, Ctx, 0))", content)
        self.assertIn("pipeline predict stage ~p completed local batch=", content)
        self.assertIn("maybe_dispatch_deferred_parallel_sample(GenWorkerEts, predict)", content)
        self.assertIn("should_replay_parallel_sample_immediately", content)
        self.assertIn("replaying deferred parallel sample immediately by transitioning wait->~p", content)
        self.assertIn("normalize_parallel_next_state(NextState, LastPhase)", content)

    def test_w2w_bridge_notifies_worker_pipeline_inbox(self) -> None:
        content = W2W_COM.read_text(encoding="utf-8")
        self.assertIn("maybe_notify_pipeline_inbox", content)
        self.assertIn("{parallel_pipeline_inbox, FromWorkerName, Data}", content)
        self.assertIn("pipeline_forward_payload", content)
        self.assertIn("pipeline_backward_payload", content)
        self.assertIn("pipeline_predict_payload", content)

    def test_torch_cpp_worker_has_stage_partition_execution(self) -> None:
        content = TORCH_WORKER_CPP.read_text(encoding="utf-8")
        self.assertIn("initialize_pipeline_partition()", content)
        self.assertIn("run_pipeline_stage_layers", content)
        self.assertIn("pipeline_stage0_forward", content)
        self.assertIn("pipeline_stage_forward", content)
        self.assertIn("pipeline_stage_last_forward_backward", content)
        self.assertIn("pipeline_stage_backward", content)
        self.assertIn("pipeline_predict_stage0_forward", content)
        self.assertIn("pipeline_predict_stage_forward", content)
        self.assertIn("Torch pipeline partition stage=", content)


if __name__ == "__main__":
    unittest.main()
