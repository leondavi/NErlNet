#!/usr/bin/env python3
"""Contract checks for fail-fast abort/reset behavior in parallel mode."""

from __future__ import annotations

import unittest
from pathlib import Path


REPO_ROOT = Path(__file__).resolve().parents[2]
MAIN_SERVER_FILE = REPO_ROOT / "src_erl" / "NerlnetApp" / "src" / "MainServer" / "mainGenserver.erl"
SUPER_NODE_FILE = REPO_ROOT / "src_erl" / "NerlnetApp" / "src" / "SuperNode" / "superNodeGenserver.erl"
CLIENT_STATEM_FILE = REPO_ROOT / "src_erl" / "NerlnetApp" / "src" / "Client" / "clientStatem.erl"


class FailureAbortContractTests(unittest.TestCase):
    def test_main_server_abort_decode_is_safe(self) -> None:
        content = MAIN_SERVER_FILE.read_text(encoding="utf-8")
        self.assertIn("binary_to_term(Body, [safe])", content)

    def test_main_server_parallel_abort_resets_parallel_state(self) -> None:
        content = MAIN_SERVER_FILE.read_text(encoding="utf-8")
        self.assertIn("update_clients_parallel_execution(#{}),", content)
        self.assertIn("update_clients_parallel_mode(legacy),", content)
        self.assertIn("clean_phase_result_data_to_send_ets()", content)
        self.assertIn("parallel_mode = legacy", content)
        self.assertIn("parallel_super_node = none", content)
        self.assertIn("apply_parallel_phase_routing(PhaseAtom, ParallelMode, SuperNode, ParallelExecution)", content)
        self.assertIn("Non-legacy mode settings are propagated to clients only by Super Node commands.", content)
        self.assertIn("Main server delegating non-legacy parallel routing to super node ~p phase=~p mode=~p", content)

    def test_phase_payload_parsing_uses_whitelist_normalization(self) -> None:
        content = MAIN_SERVER_FILE.read_text(encoding="utf-8")
        self.assertIn("normalize_phase_atom(PhaseValue)", content)
        self.assertIn("resolve_super_node(SuperNodeValue)", content)
        self.assertNotIn("{binary_to_atom(PhaseBin)", content)
        self.assertNotIn("{list_to_atom(Trimmed), legacy, none, #{}}", content)

    def test_main_server_client_ack_handles_cleared_active_phase(self) -> None:
        content = MAIN_SERVER_FILE.read_text(encoding="utf-8")
        self.assertIn("skipping phase result upload because active_phase is", content)
        self.assertIn("UnexpectedPhase ->", content)
        self.assertIn("clean_phase_result_data_to_send_ets()", content)
        self.assertIn("ignoring stale/duplicate clientAck from", content)
        self.assertIn("case lists:member(ClientName, WaitingList) of", content)

    def test_super_node_heartbeat_checks_only_active_parallel_phase(self) -> None:
        content = SUPER_NODE_FILE.read_text(encoding="utf-8")
        self.assertIn("parallel_active = false", content)
        self.assertIn("phase_start_ms = 0", content)
        self.assertIn("case ParallelActive of", content)
        self.assertIn("maybe_check_client_heartbeat(", content)

    def test_super_node_abort_is_deduped(self) -> None:
        content = SUPER_NODE_FILE.read_text(encoding="utf-8")
        self.assertIn("last_abort = {none, 0}", content)
        self.assertIn("SuppressWindow = erlang:max(200, HeartbeatMs)", content)
        self.assertIn("State#super_node_state{last_abort = {Reason, NowMs}}", content)
        self.assertIn("scheduler gate: parallel_active=false", content)
        self.assertIn("scheduler gate: pending grant still open", content)

    def test_client_legacy_mode_forces_parallel_execution_clear(self) -> None:
        content = CLIENT_STATEM_FILE.read_text(encoding="utf-8")
        self.assertIn("apply_parallel_mode(EtsRef, Mode, SourceRaw)", content)
        self.assertIn("apply_parallel_execution(EtsRef, ParallelExecution, SourceRaw)", content)
        self.assertIn("cast_message_to_workers(EtsRef, {set_parallel_execution, #{}})", content)
        self.assertIn("reset_parallel_phase_close_state(EtsRef)", content)

    def test_phase_close_barrier_paths_exist(self) -> None:
        super_content = SUPER_NODE_FILE.read_text(encoding="utf-8")
        client_content = CLIENT_STATEM_FILE.read_text(encoding="utf-8")
        self.assertIn("handle_parallel_phase_close_request", super_content)
        self.assertIn("finalize_parallel_phase_close", super_content)
        self.assertIn("broadcast_phase_close_granted", super_content)
        self.assertIn("handle_scheduler_grant_rejected", super_content)
        self.assertIn("is_phase_close_reject_reason", super_content)
        self.assertIn("close-related grant rejection is entering close-drain mode", super_content)
        self.assertIn("phase_close_deadline_expired", super_content)
        self.assertIn("scheduler_grant_rejection_storm", super_content)
        self.assertIn("maybe_request_super_phase_close(EtsRef, training)", client_content)
        self.assertIn("maybe_request_super_phase_close(EtsRef, prediction)", client_content)
        self.assertIn("scheduler_grant_reject_reason", client_content)
        self.assertIn("parallel_phase_close_last_request_ms", client_content)
        self.assertIn("PHASE_CLOSE_RETRY_MS", client_content)


if __name__ == "__main__":
    unittest.main()
