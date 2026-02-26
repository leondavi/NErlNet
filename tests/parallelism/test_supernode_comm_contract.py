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


class SuperNodeCommContractTest(unittest.TestCase):
    def test_super_node_delivers_to_parallel_deliver_endpoint(self) -> None:
        content = SUPER_NODE_FILE.read_text(encoding="utf-8")
        self.assertIn("atom_to_list(parallelDeliver)", content)
        self.assertIn("{parallel_deliver, FromWorker, ToWorker, Data}", content)
        self.assertIn("atom_to_list(parallelSuperCommand)", content)
        self.assertIn("configure_parallel", content)
        self.assertIn("grant_scheduler_event", content)
        self.assertIn("pipeline_tensor ->", content)
        self.assertIn("pending_grant = {Direction, MicrobatchId, StageId, TargetWorkers, []}", content)
        self.assertIn("scheduler_duplicate_worker_event", content)
        self.assertIn("normalize_phase_name(PhaseName)", content)
        self.assertIn("prediction -> forward_only_trace(BaseTrace)", content)

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
        self.assertIn("apply_parallel_super_command(EtsRef, SuperCommand)", statem_content)
        self.assertIn("{parallel_scheduler_grant, normalize_parallel_direction(Direction), MicrobatchID, StageID}", statem_content)
        self.assertIn("Client ~p received scheduler grant direction=~p microbatch=~p stage=~p target=~p", statem_content)

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


if __name__ == "__main__":
    unittest.main()
