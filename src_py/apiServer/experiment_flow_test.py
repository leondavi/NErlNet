
import os
import socket
import time
from apiServer import *
from runCommand import RunCommand
from logger import *
from stats import Stats
from definitions import format_performance_stats, format_communication_stats

ExitValue = 0

TEST_ACCEPTABLE_MARGIN_OF_ERROR = 0.01 # distance from loss value to baseline loss value
TEST_ACCEPTABLE_F1_DIFF = 0.02 # distance from F1 value to baseline F1 value
TEST_STRICT_BASELINE_CHECK = os.getenv("NERLNET_STRICT_BASELINE_CHECK", "0").lower() in ("1", "true", "yes", "on")


def parse_float_env(var_name: str, default: float) -> float:
    raw_val = os.getenv(var_name, str(default))
    try:
        return float(raw_val)
    except (TypeError, ValueError):
        return default


TEST_MIN_AVG_F1 = parse_float_env("NERLNET_TEST_MIN_AVG_F1", 0.75)
TEST_MIN_AVG_ACCURACY = parse_float_env("NERLNET_TEST_MIN_AVG_ACCURACY", 0.85)
TEST_MAX_AVG_LOSS = parse_float_env("NERLNET_TEST_MAX_AVG_LOSS", 0.10)

def print_test(in_str : str , enable = True):
    PREFIX = "[NERLNET-TEST] "
    if enable:
        LOG_INFO(f"{PREFIX} {in_str}")


def parse_int_env(var_name: str, default: int) -> int:
    raw_val = os.getenv(var_name, str(default))
    try:
        return int(raw_val)
    except (TypeError, ValueError):
        return default

NERLNET_PATH = os.getenv('NERLNET_PATH')
TESTS_PATH = os.getenv('TESTS_PATH')
TESTS_BASELINE_MODEL_STATS = os.getenv('TEST_BASELINE_MODEL_STATS')
TEST_BASELINE_LOSS_MIN = os.getenv('TEST_BASELINE_LOSS_MIN')
NERLNET_RUN_SCRIPT = "./NerlnetRun.sh --run-mode release > /tmp/nerlnet_run_log.txt 2>&1"
NERLNET_RUN_STOP_SCRIPT = "./NerlnetRun.sh --run-mode stop"
NERLNET_RUNNING_TIMEOUT_SEC = int(os.getenv('NERLNET_RUNNING_TIMEOUT_SEC'))
TEST_DATASET_IDX = 2

WAIT_TIME_FOR_NERLNET_RUN_BOOT = max(parse_int_env("NERLNET_RUN_BOOT_WAIT_SEC", 5), 1) # secs
NERLNET_RUN_READY_TIMEOUT_SEC = max(
    parse_int_env("NERLNET_RUN_READY_TIMEOUT_SEC", 30),
    WAIT_TIME_FOR_NERLNET_RUN_BOOT,
)
MANUAL_START_MODE = os.getenv('NERLNET_MANUAL_START', '0').lower() in ('1', 'true', 'yes', 'on')
TEST_VARIANT = os.getenv("TEST_VARIANT", "full_flow")
TEST_TARGET_DC_JSON = os.getenv("TEST_TARGET_DC_JSON", "").strip()
TEST_TARGET_CONN_JSON = os.getenv("TEST_TARGET_CONN_JSON", "").strip()
TEST_TARGET_EXP_JSON = os.getenv("TEST_TARGET_EXP_JSON", "").strip()
NERLNET_RUN_LOG_PATH = "/tmp/nerlnet_run_log.txt"


def tail_file(path: str, max_lines: int = 40) -> str:
    try:
        with open(path, "r", encoding="utf-8", errors="ignore") as handle:
            return "".join(handle.readlines()[-max_lines:]).strip()
    except Exception:
        return ""


def stop_stale_nerlnet() -> None:
    # Best-effort cleanup to avoid attaching tests to stale nodes.
    try:
        nerlnet_stop_cmd = RunCommand(NERLNET_RUN_STOP_SCRIPT, NERLNET_PATH)
        nerlnet_stop_cmd.sync(NERLNET_RUNNING_TIMEOUT_SEC)
    except Exception:
        pass

    for pattern in ("beam.smp", "erlexec", "nerlnetApp"):
        os.system(f"pkill -9 -f '{pattern}' >/dev/null 2>&1")
    time.sleep(2)


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


def resolve_local_probe_hosts() -> list[str]:
    hosts = []
    preferred = None
    try:
        with socket.socket(socket.AF_INET, socket.SOCK_DGRAM) as sock:
            sock.connect(("1.1.1.1", 80))
            preferred = sock.getsockname()[0]
    except OSError:
        preferred = None

    for candidate in (preferred, "127.0.0.1", "localhost"):
        if candidate and candidate not in hosts:
            hosts.append(candidate)
    return hosts


def is_initiator_ready(host: str, port: int = 8484) -> bool:
    try:
        with socket.create_connection((host, port), timeout=0.5):
            return True
    except OSError:
        return False


def wait_for_nerlnet_ready(nerlnet_run_cmd: RunCommand) -> None:
    deadline = time.time() + NERLNET_RUN_READY_TIMEOUT_SEC
    probe_hosts = resolve_local_probe_hosts()
    while time.time() < deadline:
        ensure_nerlnet_started(nerlnet_run_cmd)
        for host in probe_hosts:
            if is_initiator_ready(host):
                print_test(f"Nerlnet initiator ready on {host}:8484")
                return
        time.sleep(0.5)

    run_log_tail = tail_file(NERLNET_RUN_LOG_PATH, max_lines=80)
    raise RuntimeError(
        "NerlnetRun did not expose the initiator listener on :8484 "
        f"within {NERLNET_RUN_READY_TIMEOUT_SEC}s. Log tail:\n{run_log_tail}"
    )


def resolve_target_filename(files_list, explicit_target: str, preferred_targets, kind: str) -> str:
    available = []
    for elem in files_list:
        try:
            available.append(elem.get_filename())
        except Exception:
            continue

    if not available:
        raise AssertionError(f"No {kind} JSON files found")

    if explicit_target:
        if explicit_target in available:
            return explicit_target
        raise AssertionError(
            f"Requested {kind} target '{explicit_target}' not found. Available: {available}"
        )

    for preferred_target in preferred_targets:
        if preferred_target in available:
            return preferred_target

    return available[0]


def find_json_index(files_list, target_filename: str) -> int:
    for idx, elem in enumerate(files_list):
        try:
            if elem.get_filename() == target_filename:
                return idx
        except Exception:
            continue
    raise AssertionError(f"Target JSON '{target_filename}' not found")

# TODO JUST FOR DEBUG
print_test(f"$NERLNET_PATH: {NERLNET_PATH}")
print_test(f"$TESTS_PATH: {TESTS_PATH}")
print_test(f"$NERLNET_RUN_SCRIPT: {NERLNET_RUN_SCRIPT}")
print_test(f"$NERLNET_RUNNING_TIMEOUT_SEC: {NERLNET_RUNNING_TIMEOUT_SEC}")
print_test(f"$NERLNET_RUN_BOOT_WAIT_SEC: {WAIT_TIME_FOR_NERLNET_RUN_BOOT}")
print_test(f"$NERLNET_RUN_READY_TIMEOUT_SEC: {NERLNET_RUN_READY_TIMEOUT_SEC}")

if MANUAL_START_MODE:
    print_test("Manual start mode enabled - assuming NerlnetApp is already running")
    nerlnet_run_cmd = None
else:
    print_test("Ensuring no stale NerlnetApp process is running")
    stop_stale_nerlnet()
    print_test("NerlnetApp Start")
    nerlnet_run_cmd = RunCommand(NERLNET_RUN_SCRIPT, NERLNET_PATH)
    time.sleep(WAIT_TIME_FOR_NERLNET_RUN_BOOT)
    wait_for_nerlnet_ready(nerlnet_run_cmd)

api_server_instance = ApiServer()
api_server_instance.download_dataset(TEST_DATASET_IDX)
#api_server_instance.help()
#api_server_instance.showJsons()
selected_dc_name = resolve_target_filename(
    api_server_instance.json_dir_parser.dc_list,
    TEST_TARGET_DC_JSON,
    [
        "dc_test_synt_1d_2c_1s_4r_4w.json",
        "dc_torch_synt_1d_2c_1s_4r_4w.json",
    ],
    "dc",
)
selected_conn_name = resolve_target_filename(
    api_server_instance.json_dir_parser.conn_map_list,
    TEST_TARGET_CONN_JSON,
    [
        "conn_test_synt_1d_2c_1s_4r_4w.json",
        "conn_torch_synt_1d_2c_1s_4r_4w.json",
    ],
    "conn",
)
selected_exp_name = resolve_target_filename(
    api_server_instance.json_dir_parser.experiments_list,
    TEST_TARGET_EXP_JSON,
    [
        "exp_test_synt_1d_2c_1s_4r_4w.json",
        "exp_torch_synt_1d_2c_1s_4r_4w.json",
    ],
    "exp",
)

dc_idx = find_json_index(api_server_instance.json_dir_parser.dc_list, selected_dc_name)
conn_idx = find_json_index(api_server_instance.json_dir_parser.conn_map_list, selected_conn_name)
exp_idx = find_json_index(api_server_instance.json_dir_parser.experiments_list, selected_exp_name)
print_test(
    f"Selected JSON indices -> dc:{dc_idx} conn:{conn_idx} exp:{exp_idx} "
    f"(targets: {selected_dc_name}, {selected_conn_name}, {selected_exp_name})"
)
api_server_instance.setJsons(dc_idx, conn_idx, exp_idx)

dc_json , connmap_json, exp_flow_json = api_server_instance.getUserJsons()

experiment_name = "test_exp"
api_server_instance.initialization(experiment_name, dc_json , connmap_json, exp_flow_json)
api_server_instance.send_jsons_to_devices()

curr_experiment_phase_exists = api_server_instance.experiment_phase_is_valid()
assert curr_experiment_phase_exists, "No experiment phase found"

api_server_instance.run_current_experiment_phase() # blocking until phase is completed
stats_train = api_server_instance.get_experiment_flow(experiment_name).generate_stats()
perf_stats_train = stats_train.get_performance_stats_clients()
api_server_instance.next_experiment_phase()
assert api_server_instance.next_expertiment_phase_exist, "No next experiment phase found"
api_server_instance.run_current_experiment_phase() # blocking until phase is completed
stats_predict = api_server_instance.get_experiment_flow(experiment_name).generate_stats()
perf_stats_predict = stats_predict.get_performance_stats_clients()
print_test("Experiment phases completed")

if MANUAL_START_MODE:
    print_test("Manual start mode enabled - skipping automatic stop")
else:
    print_test("Stopping NerlnetApp")
    nerlnet_stop_cmd = RunCommand(NERLNET_RUN_STOP_SCRIPT, NERLNET_PATH)
    stdout, stderr, rc = nerlnet_stop_cmd.sync(NERLNET_RUNNING_TIMEOUT_SEC)
    print_test(f'rc stop: {rc}')
    if stderr: 
        LOG_ERROR(stderr)
    else:
        print_test(stdout, False)
    stdout, stderr, rc = nerlnet_run_cmd.sync(NERLNET_RUNNING_TIMEOUT_SEC)
    print_test(f'rc: {rc}')
    if stderr: 
        LOG_ERROR(stderr)
    else:
        print_test(stdout)


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

LOG_INFO("Missed Batches training:")
#LOG_INFO(stats_train.get_missed_batches())

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

missed_batches = stats_predict.get_missed_batches()
if missed_batches:
    LOG_INFO("Missed Batches prediction:")
    LOG_INFO(missed_batches)

generate_baseline_files = True

loss_min_dict = stats_train.get_min_loss(saveToFile=generate_baseline_files)
LOG_INFO(loss_min_dict)
_ , confusion_matrix_worker_dict = stats_predict.get_confusion_matrices()
performence_stats = stats_predict.get_model_performence_stats(confusion_matrix_worker_dict, saveToFile=generate_baseline_files) # Now a pandas DataFrame

loss_values = [float(value) for value in loss_min_dict.values()]
avg_loss = average_list(loss_values) if loss_values else float("inf")

perf_f1_values = [float(value) for value in performence_stats["F1"]]
perf_accuracy_values = [float(value) for value in performence_stats["Accuracy"]]
avg_f1 = average_list(perf_f1_values) if perf_f1_values else 0.0
avg_accuracy = average_list(perf_accuracy_values) if perf_accuracy_values else 0.0

LOG_INFO(
    "Prediction quality summary: "
    f"rows={len(performence_stats)}, avg_f1={avg_f1:.4f}, min_f1={min(perf_f1_values):.4f}, "
    f"avg_accuracy={avg_accuracy:.4f}, min_accuracy={min(perf_accuracy_values):.4f}"
)
LOG_INFO(
    "Training loss summary: "
    f"workers={len(loss_values)}, avg_loss={avg_loss:.6f}, "
    f"min_loss={min(loss_values):.6f}, max_loss={max(loss_values):.6f}"
)

if TEST_STRICT_BASELINE_CHECK:
    baseline_loss_min = import_dict_json(TEST_BASELINE_LOSS_MIN)
    baseline_performance_stats = import_csv_df(TESTS_BASELINE_MODEL_STATS)

    baseline_loss_min_avg = average_list(list(baseline_loss_min.values()))

    for worker in loss_min_dict.keys():
        dist_from_avg_anomaly = abs(loss_min_dict[worker] - baseline_loss_min_avg)
        if dist_from_avg_anomaly > TEST_ACCEPTABLE_MARGIN_OF_ERROR:
            LOG_INFO(f"Anomaly: {dist_from_avg_anomaly}, error: {loss_min_dict[worker]} , baseline mean: {baseline_loss_min_avg}, Acceptable error range: {TEST_ACCEPTABLE_MARGIN_OF_ERROR}")
            LOG_ERROR(f"Anomaly failure detected")
            ExitValue = 1

    DIFF_MEASURE_METHOD = "F1"

    for f1_score_exp , f1_score_baseline in zip(performence_stats[DIFF_MEASURE_METHOD], baseline_performance_stats[DIFF_MEASURE_METHOD]):
        diff = abs(f1_score_exp - f1_score_baseline)
        error = diff/f1_score_baseline
        if error > TEST_ACCEPTABLE_F1_DIFF:
            LOG_INFO(f"Anomaly: {error}, Diff: {diff}, F1: {f1_score_exp} , F1 baseline: {f1_score_baseline}, Acceptable error range: {TEST_ACCEPTABLE_F1_DIFF}")
            LOG_ERROR("Anomaly failure detected")
            LOG_ERROR(f"diff_from_baseline: {diff}")
            ExitValue = 1
else:
    if avg_loss > TEST_MAX_AVG_LOSS:
        LOG_ERROR(
            f"Average training loss too high: {avg_loss:.6f} > {TEST_MAX_AVG_LOSS:.6f}"
        )
        ExitValue = 1

    if avg_f1 < TEST_MIN_AVG_F1:
        LOG_ERROR(
            f"Average F1 too low: {avg_f1:.4f} < {TEST_MIN_AVG_F1:.4f}"
        )
        ExitValue = 1

    if avg_accuracy < TEST_MIN_AVG_ACCURACY:
        LOG_ERROR(
            f"Average accuracy too low: {avg_accuracy:.4f} < {TEST_MIN_AVG_ACCURACY:.4f}"
        )
        ExitValue = 1

comm_train = stats_train.get_communication_stats_workers() if stats_train else {}
comm_predict = stats_predict.get_communication_stats_workers() if stats_predict else {}
w2c = stats_train.net_comps.get_map_worker_to_client() if stats_train else {}
print(format_performance_stats(perf_stats_train, perf_stats_predict,
                               workers_comm_train=comm_train,
                               workers_comm_predict=comm_predict,
                               worker_to_client=w2c))

exit(ExitValue)
