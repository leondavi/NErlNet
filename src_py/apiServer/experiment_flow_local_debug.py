#!/usr/bin/env python3
"""Local debug runner for NErlNet experiment flows.

Runs a full pipeline (init -> send JSONs -> run phases) without baseline checks.
Intended for local Torch pipeline validation with verbose logging.
"""

import argparse
import logging
import os
import sys
import traceback

from apiServer import ApiServer
from definitions import pretty_dict
from logger import LOG_INFO, LOG_ERROR


def parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser(description="Run a local NErlNet experiment flow without baseline checks.")
    parser.add_argument("--dc", required=True, help="Path to dc_*.json")
    parser.add_argument("--conn", required=True, help="Path to conn_*.json")
    parser.add_argument("--exp", required=True, help="Path to exp_*.json")
    parser.add_argument("--csv", default="", help="Optional override CSV path")
    parser.add_argument("--experiment-name", default="local_debug", help="Experiment name")
    parser.add_argument("--run-all", action="store_true", help="Run all phases (default)")
    return parser.parse_args()


def validate_paths(args: argparse.Namespace) -> None:
    for label, path in (("dc", args.dc), ("conn", args.conn), ("exp", args.exp)):
        if not os.path.isfile(path):
            raise FileNotFoundError(f"{label} json not found: {path}")
    if args.csv and not os.path.isfile(args.csv):
        raise FileNotFoundError(f"csv not found: {args.csv}")


def log_phase_stats(stats, index: int) -> None:
    LOG_INFO(f"Phase {index}: {stats.get_name()} ({stats.get_phase()})")
    try:
        LOG_INFO("Performance stats (clients):")
        print(pretty_dict(stats.get_performance_stats_clients()))
    except Exception as exc:
        LOG_ERROR(f"Failed to collect performance stats: {exc}")
    try:
        LOG_INFO("Communication stats (main server):")
        print(pretty_dict(stats.get_communication_stats_main_server()))
    except Exception as exc:
        LOG_ERROR(f"Failed to collect main server comm stats: {exc}")
    try:
        missed = stats.get_missed_batches()
        if missed:
            LOG_INFO("Missed batches:")
            print(pretty_dict(missed))
    except Exception as exc:
        LOG_ERROR(f"Failed to collect missed batches: {exc}")


def main() -> int:
    args = parse_args()
    logging.getLogger().setLevel(logging.DEBUG)
    try:
        validate_paths(args)
    except Exception as exc:
        LOG_ERROR(str(exc))
        return 2

    LOG_INFO("=== Local experiment flow debug ===")
    LOG_INFO(f"DC:   {args.dc}")
    LOG_INFO(f"Conn: {args.conn}")
    LOG_INFO(f"Exp:  {args.exp}")
    if args.csv:
        LOG_INFO(f"CSV override: {args.csv}")

    api_server = ApiServer()
    try:
        api_server.initialization(args.experiment_name, args.dc, args.conn, args.exp, args.csv)
        api_server.send_jsons_to_devices()
        stats_list = api_server.run_all_experiment_phases()
        if not stats_list:
            LOG_ERROR("No phases executed. Check experiment flow JSON.")
            return 1
        for idx, stats in enumerate(stats_list, start=1):
            log_phase_stats(stats, idx)
        LOG_INFO("=== Local experiment flow debug completed ===")
        return 0
    except Exception:
        LOG_ERROR("Experiment flow failed with exception:")
        traceback.print_exc()
        return 1


if __name__ == "__main__":
    sys.exit(main())
