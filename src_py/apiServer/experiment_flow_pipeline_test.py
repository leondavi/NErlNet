#!/usr/bin/env python3

import math
import os
import sys
import time
import traceback

from apiServer import ApiServer
from definitions import pretty_dict, format_performance_stats, format_communication_stats
from logger import LOG_ERROR, LOG_INFO
from runCommand import RunCommand


def parse_int_env(var_name: str, default: int) -> int:
    raw_val = os.getenv(var_name, str(default))
    try:
        return int(raw_val)
    except (TypeError, ValueError):
        return default


TEST_VARIANT = os.getenv("TEST_VARIANT", "unknown")
TEST_TARGET_DC_JSON = os.getenv("TEST_TARGET_DC_JSON", "dc_test.json")
TEST_TARGET_CONN_JSON = os.getenv("TEST_TARGET_CONN_JSON", "conn_test.json")
TEST_TARGET_EXP_JSON = os.getenv("TEST_TARGET_EXP_JSON", "exp_test.json")
TEST_EXPECT_MODE = os.getenv("TEST_EXPECT_MODE", "").strip()
TEST_EXPECT_SCHEDULER = os.getenv("TEST_EXPECT_SCHEDULER", "").strip().lower()
TEST_EXPECT_DATASET_TOKEN = os.getenv(
    "TEST_EXPECT_DATASET_TOKEN",
    "synthetic_norm/synthetic_full.csv",
).strip()
TEST_EXPECT_MIN_WORKERS = max(parse_int_env("TEST_EXPECT_MIN_WORKERS", 0), 0)

NERLNET_PATH = os.getenv("NERLNET_PATH")
TESTS_PATH = os.getenv("TESTS_PATH")
NERLNET_RUN_SCRIPT = "./NerlnetRun.sh --run-mode release > /tmp/nerlnet_run_log.txt 2>&1"
NERLNET_RUN_STOP_SCRIPT = "./NerlnetRun.sh --run-mode stop"
NERLNET_RUNNING_TIMEOUT_SEC = max(parse_int_env("NERLNET_RUNNING_TIMEOUT_SEC", 5), 1)
NERLNET_RUN_BOOT_WAIT_SEC = max(parse_int_env("NERLNET_RUN_BOOT_WAIT_SEC", 5), 1)
TEST_DATASET_IDX = 2
MANUAL_START_MODE = os.getenv("NERLNET_MANUAL_START", "0").lower() in ("1", "true", "yes", "on")
NERLNET_RUN_LOG_PATH = "/tmp/nerlnet_run_log.txt"


def print_test(in_str: str, enable: bool = True):
    prefix = f"[NERLNET-PARALLEL-TEST][{TEST_VARIANT}]"
    if enable:
        LOG_INFO(f"{prefix} {in_str}")


def tail_file(path: str, max_lines: int = 40) -> str:
    try:
        with open(path, "r", encoding="utf-8", errors="ignore") as handle:
            return "".join(handle.readlines()[-max_lines:]).strip()
    except Exception:
        return ""


def ensure_nerlnet_started(nerlnet_run_cmd: RunCommand) -> None:
    if nerlnet_run_cmd is None:
        return
    rc = nerlnet_run_cmd.process.poll()
    if rc is None:
        return
    run_log_tail = tail_file(NERLNET_RUN_LOG_PATH, max_lines=60)
    raise RuntimeError(
        "NerlnetRun exited before test startup completed "
        f"(rc={rc}). Log tail:\n{run_log_tail}"
    )


def find_json_index_or_default(files_list, target_filename: str, default_index: int = 0) -> int:
    for idx, elem in enumerate(files_list):
        try:
            if elem.get_filename() == target_filename:
                return idx
        except Exception:
            continue
    return default_index


def count_missed_batch_entries(payload) -> int:
    if payload is None:
        return 0
    if isinstance(payload, dict):
        return sum(count_missed_batch_entries(value) for value in payload.values())
    if isinstance(payload, (list, tuple, set)):
        return sum(count_missed_batch_entries(value) for value in payload)
    return 1


def validate_json_contracts(json_parser, dc_json_path: str, exp_json_path: str):
    dc_data = json_parser.json_from_path(dc_json_path)
    exp_data = json_parser.json_from_path(exp_json_path)

    workers = dc_data.get("workers", [])
    if TEST_EXPECT_MIN_WORKERS > 0 and len(workers) < TEST_EXPECT_MIN_WORKERS:
        raise AssertionError(
            f"Expected at least {TEST_EXPECT_MIN_WORKERS} workers, found {len(workers)} in {dc_json_path}"
        )

    csv_path = str(exp_data.get("csvFilePath", ""))
    if TEST_EXPECT_DATASET_TOKEN and TEST_EXPECT_DATASET_TOKEN not in csv_path:
        raise AssertionError(
            f"Expected dataset token '{TEST_EXPECT_DATASET_TOKEN}' in csvFilePath, got '{csv_path}'"
        )

    phases = exp_data.get("Phases", [])
    if not isinstance(phases, list) or not phases:
        raise AssertionError("Experiment flow has no phases")

    for phase in phases:
        phase_name = phase.get("phaseName", "<unknown>")
        parallel_execution = phase.get("parallelExecution", {})
        mode = str(parallel_execution.get("mode", "")).strip()
        scheduler = str(parallel_execution.get("scheduler", "")).strip().lower()

        if TEST_EXPECT_MODE and mode != TEST_EXPECT_MODE:
            raise AssertionError(
                f"Phase '{phase_name}' mode mismatch: expected '{TEST_EXPECT_MODE}', got '{mode}'"
            )

        if TEST_EXPECT_SCHEDULER and scheduler != TEST_EXPECT_SCHEDULER:
            raise AssertionError(
                f"Phase '{phase_name}' scheduler mismatch: expected '{TEST_EXPECT_SCHEDULER}', got '{scheduler}'"
            )


def validate_loss_min(stats_train) -> bool:
    loss_min_dict = stats_train.get_min_loss()
    LOG_INFO(f"Loss min dict: {loss_min_dict}")
    if not loss_min_dict:
        LOG_ERROR("No loss data collected from training phase")
        return False

    is_valid = True
    for worker_name, raw_loss in loss_min_dict.items():
        try:
            loss_value = float(raw_loss)
        except (TypeError, ValueError):
            LOG_ERROR(f"Worker {worker_name}: loss is not numeric ({raw_loss})")
            is_valid = False
            continue

        if not math.isfinite(loss_value):
            LOG_ERROR(f"Worker {worker_name}: loss is not finite ({loss_value})")
            is_valid = False
            continue

        if loss_value < 0:
            LOG_ERROR(f"Worker {worker_name}: loss is negative ({loss_value})")
            is_valid = False
            continue

        print_test(f"Worker {worker_name}: min loss = {loss_value:.6f}")

    return is_valid


def validate_prediction_quality(stats_predict) -> bool:
    try:
        _, confusion_matrix_worker_dict = stats_predict.get_confusion_matrices()
        performance_df = stats_predict.get_model_performence_stats(confusion_matrix_worker_dict)
    except Exception as exc:
        LOG_ERROR(f"Failed collecting prediction performance metrics: {exc}")
        return False

    if performance_df is None or performance_df.empty:
        LOG_ERROR("Prediction performance DataFrame is empty")
        return False

    required_columns = ["TN", "FP", "FN", "TP", "Accuracy", "F1"]
    missing_columns = [column for column in required_columns if column not in performance_df.columns]
    if missing_columns:
        LOG_ERROR(f"Prediction performance is missing required columns: {missing_columns}")
        return False

    valid_rows = 0
    sum_f1 = 0.0
    sum_accuracy = 0.0
    min_f1 = float("inf")
    min_accuracy = float("inf")

    for _, row in performance_df.iterrows():
        try:
            tn = float(row["TN"])
            fp = float(row["FP"])
            fn = float(row["FN"])
            tp = float(row["TP"])
            accuracy = float(row["Accuracy"])
            f1_score = float(row["F1"])
        except (TypeError, ValueError):
            LOG_ERROR(f"Encountered non-numeric prediction row: {row.to_dict()}")
            return False

        support = tn + fp + fn + tp
        if support <= 0:
            continue
        if not math.isfinite(accuracy) or not math.isfinite(f1_score):
            LOG_ERROR(f"Encountered non-finite prediction metrics row: {row.to_dict()}")
            return False

        valid_rows += 1
        sum_f1 += f1_score
        sum_accuracy += accuracy
        min_f1 = min(min_f1, f1_score)
        min_accuracy = min(min_accuracy, accuracy)

    if valid_rows == 0:
        LOG_ERROR("No valid prediction rows with non-zero confusion-matrix support")
        return False

    avg_f1 = sum_f1 / valid_rows
    avg_accuracy = sum_accuracy / valid_rows
    print_test(
        "Prediction quality summary: "
        f"rows={valid_rows}, avg_f1={avg_f1:.4f}, min_f1={min_f1:.4f}, "
        f"avg_accuracy={avg_accuracy:.4f}, min_accuracy={min_accuracy:.4f}"
    )
    return True


def stop_nerlnet(nerlnet_run_cmd):
    if MANUAL_START_MODE or nerlnet_run_cmd is None:
        return
    print_test("Stopping NerlnetApp")

    try:
        nerlnet_stop_cmd = RunCommand(NERLNET_RUN_STOP_SCRIPT, NERLNET_PATH)
        stdout, stderr, rc = nerlnet_stop_cmd.sync(NERLNET_RUNNING_TIMEOUT_SEC)
        print_test(f"stop rc: {rc}")
        if stderr:
            LOG_ERROR(stderr)
        elif stdout:
            print_test(stdout, enable=False)
    except Exception as exc:
        LOG_ERROR(f"Failed to run stop script: {exc}")

    try:
        stdout, stderr, rc = nerlnet_run_cmd.sync(NERLNET_RUNNING_TIMEOUT_SEC)
        print_test(f"release rc: {rc}")
        if stderr:
            LOG_ERROR(stderr)
        elif stdout:
            print_test(stdout, enable=False)
    except Exception as exc:
        LOG_ERROR(f"Failed waiting on release command: {exc}")


def main() -> int:
    exit_value = 0
    nerlnet_run_cmd = None
    stats_train = None
    stats_predict = None
    perf_stats_train = {}
    perf_stats_predict = {}

    print_test(f"$NERLNET_PATH: {NERLNET_PATH}")
    print_test(f"$TESTS_PATH: {TESTS_PATH}")
    print_test(f"$NERLNET_RUNNING_TIMEOUT_SEC: {NERLNET_RUNNING_TIMEOUT_SEC}")

    try:
        if MANUAL_START_MODE:
            print_test("Manual start mode - assuming NerlnetApp is already running")
        else:
            print_test("NerlnetApp Start")
            nerlnet_run_cmd = RunCommand(NERLNET_RUN_SCRIPT, NERLNET_PATH)
            time.sleep(NERLNET_RUN_BOOT_WAIT_SEC)
            ensure_nerlnet_started(nerlnet_run_cmd)

        api_server_instance = ApiServer()
        api_server_instance.download_dataset(TEST_DATASET_IDX)

        num_dc = len(api_server_instance.json_dir_parser.dc_list)
        num_conn = len(api_server_instance.json_dir_parser.conn_map_list)
        num_exp = len(api_server_instance.json_dir_parser.experiments_list)
        print_test(f"Found {num_dc} dc, {num_conn} conn, {num_exp} exp JSON files")
        assert num_dc >= 1, "No dc_*.json found in JSON directory"
        assert num_conn >= 1, "No conn_*.json found in JSON directory"
        assert num_exp >= 1, "No exp_*.json found in JSON directory"

        dc_idx = find_json_index_or_default(
            api_server_instance.json_dir_parser.dc_list,
            TEST_TARGET_DC_JSON,
            0,
        )
        conn_idx = find_json_index_or_default(
            api_server_instance.json_dir_parser.conn_map_list,
            TEST_TARGET_CONN_JSON,
            0,
        )
        exp_idx = find_json_index_or_default(
            api_server_instance.json_dir_parser.experiments_list,
            TEST_TARGET_EXP_JSON,
            0,
        )

        print_test(
            "Selected JSON indices -> "
            f"dc:{dc_idx} conn:{conn_idx} exp:{exp_idx} "
            f"(targets: {TEST_TARGET_DC_JSON}, {TEST_TARGET_CONN_JSON}, {TEST_TARGET_EXP_JSON})"
        )

        api_server_instance.setJsons(dc_idx, conn_idx, exp_idx)
        dc_json, connmap_json, exp_flow_json = api_server_instance.getUserJsons()
        validate_json_contracts(api_server_instance.json_dir_parser, dc_json, exp_flow_json)

        experiment_name = f"{TEST_VARIANT}_test_exp"
        api_server_instance.initialization(experiment_name, dc_json, connmap_json, exp_flow_json)
        api_server_instance.send_jsons_to_devices()

        curr_experiment_phase_exists = api_server_instance.experiment_phase_is_valid()
        assert curr_experiment_phase_exists, "No experiment phase found"

        print_test("Starting training phase")
        api_server_instance.run_current_experiment_phase()
        stats_train = api_server_instance.get_experiment_flow(experiment_name).generate_stats()
        perf_stats_train = stats_train.get_performance_stats_clients()

        api_server_instance.next_experiment_phase()
        assert api_server_instance.next_expertiment_phase_exist, "No next experiment phase found"
        print_test("Starting prediction phase")
        api_server_instance.run_current_experiment_phase()
        stats_predict = api_server_instance.get_experiment_flow(experiment_name).generate_stats()
        perf_stats_predict = stats_predict.get_performance_stats_clients()
        print_test("Experiment phases completed")

        if not validate_loss_min(stats_train):
            exit_value = 1
        if not validate_prediction_quality(stats_predict):
            exit_value = 1

        missed_batches = stats_predict.get_missed_batches()
        missed_batches_count = count_missed_batch_entries(missed_batches)
        if missed_batches_count > 0:
            LOG_INFO(f"Missed batches prediction (non-fatal): count={missed_batches_count}")
            LOG_INFO(missed_batches)

    except Exception:
        LOG_ERROR("Parallel full-flow test failed with exception")
        traceback.print_exc()
        exit_value = 1
    finally:
        if stats_train is not None:
            print(
                format_communication_stats(
                    "training",
                    main_server_stats=stats_train.get_communication_stats_main_server(),
                    workers_stats=stats_train.get_communication_stats_workers(),
                    sources_stats=stats_train.get_communication_stats_sources(),
                    clients_stats=stats_train.get_communication_stats_clients(),
                    super_nodes_stats=stats_train.get_communication_stats_super_nodes(),
                    routers_stats=stats_train.get_communication_stats_routers(),
                    actual_frequencies=stats_train.get_actual_frequencies_of_sources(),
                )
            )

        if stats_predict is not None:
            print(
                format_communication_stats(
                    "prediction",
                    main_server_stats=stats_predict.get_communication_stats_main_server(),
                    workers_stats=stats_predict.get_communication_stats_workers(),
                    sources_stats=stats_predict.get_communication_stats_sources(),
                    clients_stats=stats_predict.get_communication_stats_clients(),
                    super_nodes_stats=stats_predict.get_communication_stats_super_nodes(),
                    routers_stats=stats_predict.get_communication_stats_routers(),
                    actual_frequencies=stats_predict.get_actual_frequencies_of_sources(),
                )
            )

        comm_train = stats_train.get_communication_stats_workers() if stats_train else {}
        comm_predict = stats_predict.get_communication_stats_workers() if stats_predict else {}
        w2c = stats_train.net_comps.get_map_worker_to_client() if stats_train else {}
        print(format_performance_stats(perf_stats_train, perf_stats_predict,
                                       workers_comm_train=comm_train,
                                       workers_comm_predict=comm_predict,
                                       worker_to_client=w2c))
        stop_nerlnet(nerlnet_run_cmd)

    if exit_value == 0:
        print_test("Test PASSED")
    else:
        print_test("Test FAILED")
    return exit_value


if __name__ == "__main__":
    sys.exit(main())
