#!/usr/bin/env python3
"""Contract checks for non-legacy communication cutover through Super Node."""

from __future__ import annotations

import unittest
from pathlib import Path


REPO_ROOT = Path(__file__).resolve().parents[2]
SUPER_NODE_FILE = REPO_ROOT / "src_erl" / "NerlnetApp" / "src" / "SuperNode" / "superNodeGenserver.erl"
CLIENT_STATEM_FILE = REPO_ROOT / "src_erl" / "NerlnetApp" / "src" / "Client" / "clientStatem.erl"
CLIENT_HANDLER_FILE = REPO_ROOT / "src_erl" / "NerlnetApp" / "src" / "Client" / "clientStateHandler.erl"
APP_FILE = REPO_ROOT / "src_erl" / "NerlnetApp" / "src" / "nerlnetApp_app.erl"
ACTION_HANDLER_FILE = REPO_ROOT / "src_erl" / "NerlnetApp" / "src" / "MainServer" / "actionHandler.erl"
MAIN_GENSERVER_FILE = REPO_ROOT / "src_erl" / "NerlnetApp" / "src" / "MainServer" / "mainGenserver.erl"


class SuperNodeCommContractTest(unittest.TestCase):
    def test_super_node_delivers_to_parallel_deliver_endpoint(self) -> None:
        content = SUPER_NODE_FILE.read_text(encoding="utf-8")
        self.assertIn("atom_to_list(parallelDeliver)", content)
        self.assertIn("{parallel_deliver, FromWorker, ToWorker, Data}", content)
        self.assertIn("atom_to_list(parallelSuperCommand)", content)
        self.assertIn("configure_parallel", content)
        self.assertIn("grant_scheduler_event", content)
        self.assertIn("pipeline_tensor ->", content)
        self.assertIn("pending_grant = #{", content)
        self.assertIn("direction => Direction", content)
        self.assertIn("microbatch_id => MicrobatchId", content)
        self.assertIn("stage_id => StageId", content)
        self.assertIn("workers => TargetWorkers", content)
        self.assertIn("acked_workers => []", content)
        self.assertIn("scheduler_duplicate_worker_event", content)
        self.assertIn("normalize_phase_name(PhaseName)", content)
        self.assertIn("prediction -> forward_only_trace(BaseTrace)", content)
        self.assertIn("maybe_check_pending_grant_timeout", content)
        self.assertIn("parallel_phase_close", content)
        self.assertIn("phase_close_granted", content)
        self.assertIn("scheduler_grant_rejected", content)
        self.assertIn("phase_close_requested = []", content)
        self.assertIn("phase_epoch = 0", content)
        self.assertIn("extract_event_epoch_and_meta", content)
        self.assertIn("atom_to_list(parallelPhaseDone)", content)

    def test_client_has_parallel_deliver_path(self) -> None:
        handler_content = CLIENT_HANDLER_FILE.read_text(encoding="utf-8")
        statem_content = CLIENT_STATEM_FILE.read_text(encoding="utf-8")
        app_content = APP_FILE.read_text(encoding="utf-8")

        self.assertIn("parallel_deliver", handler_content)
        self.assertIn("{parallel_deliver, FromWorker, ToWorker, Data}", statem_content)
        self.assertIn("deliver_parallel_msg(EtsRef, FromWorker, ToWorker, Data)", statem_content)
        self.assertIn("\"/parallelDeliver\"", app_content)
        self.assertIn("\"/parallelSuperCommand\"", app_content)
        self.assertIn("parallel_super_command", handler_content)
        self.assertIn("apply_parallel_super_command(EtsRef, SuperCommand, waitforWorkers)", statem_content)
        self.assertIn("{parallel_scheduler_grant, normalize_parallel_direction(Direction), BatchID, MicrobatchID, StageID}", statem_content)
        self.assertIn("Client ~p received scheduler grant direction=~p batch=~p microbatch=~p stage=~p target=~p epoch=~p", statem_content)
        self.assertIn("parallelPhaseClose", statem_content)
        self.assertIn("schedulerGrantRejected", statem_content)
        self.assertIn("phase_close_granted", statem_content)
        self.assertIn("parallel_meta", statem_content)
        self.assertIn("\"/parallelPhaseClose\"", app_content)
        self.assertIn("\"/schedulerGrantRejected\"", app_content)

    def test_non_legacy_path_still_routes_outbound_via_super_node(self) -> None:
        statem_content = CLIENT_STATEM_FILE.read_text(encoding="utf-8")
        self.assertIn("atom_to_list(parallelWorkerMessage)", statem_content)
        self.assertIn("should_accept_parallel_update(super_node, main_server, _Mode)", statem_content)

    def test_super_node_registration_and_heartbeat_handshake_paths_exist(self) -> None:
        statem_content = CLIENT_STATEM_FILE.read_text(encoding="utf-8")
        app_content = APP_FILE.read_text(encoding="utf-8")
        super_content = SUPER_NODE_FILE.read_text(encoding="utf-8")
        self.assertIn("maybe_register_super_node", statem_content)
        self.assertIn("atom_to_list(registerClient)", statem_content)
        self.assertIn("maybe_start_super_node_heartbeat", statem_content)
        self.assertIn("atom_to_list(superHeartbeat)", statem_content)
        self.assertIn("\"/registerClient\"", app_content)
        self.assertIn("\"/superHeartbeat\"", app_content)
        self.assertIn("check_heartbeats", super_content)
        self.assertIn("Super node heartbeat received from ~p ts_ms=~p", super_content)

    def test_main_server_accepts_super_node_phase_done_signal(self) -> None:
        app_content = APP_FILE.read_text(encoding="utf-8")
        action_content = ACTION_HANDLER_FILE.read_text(encoding="utf-8")
        main_content = MAIN_GENSERVER_FILE.read_text(encoding="utf-8")
        self.assertIn("\"/parallelPhaseDone\"", app_content)
        self.assertIn("parallelPhaseDone ->", action_content)
        self.assertIn("{parallelPhaseDone, Body}", main_content)
        self.assertIn("[Main-Server] parallel phase completion reported by Super Node", main_content)
        self.assertIn("awaiting client idle acknowledgements", main_content)
        self.assertIn("treating as idempotent completion", main_content)

    def test_main_server_non_legacy_source_done_waits_for_super_node(self) -> None:
        main_content = MAIN_GENSERVER_FILE.read_text(encoding="utf-8")
        self.assertIn("all sources finished casting in mode=~p; waiting for Super Node parallel phase completion", main_content)

    def test_client_idle_unrecognized_message_does_not_switch_to_training(self) -> None:
        statem_content = CLIENT_STATEM_FILE.read_text(encoding="utf-8")
        self.assertIn("idle(cast, EventContent", statem_content)
        self.assertIn("{next_state, idle, State#client_statem_state", statem_content)
        self.assertIn("ets:insert(EtsRef, {parallel_idle_requested, true})", statem_content)


if __name__ == "__main__":
    unittest.main()
