#!/bin/bash

DEFAULT_NERLNET_PATH="/usr/local/lib/nerlnet-lib/NErlNet"
if [ -z "$NERLNET_PATH" ]; then
    if [ -d "$PWD/src_erl" ]; then
        export NERLNET_PATH="$PWD"
    else
        export NERLNET_PATH="$DEFAULT_NERLNET_PATH"
    fi
fi

export TESTS_PATH="$NERLNET_PATH/tests"
export NERLNET_RUNNING_TIMEOUT_SEC="5"
export NERLNET_RUN_BOOT_WAIT_SEC="${NERLNET_RUN_BOOT_WAIT_SEC:-5}"
export NERLNET_START_CASTING_TIMEOUT_SEC="${NERLNET_START_CASTING_TIMEOUT_SEC:-1800}"
export NERLNET_EVENT_WAIT_PROGRESS_SEC="${NERLNET_EVENT_WAIT_PROGRESS_SEC:-15}"

NERLNET_CONFIG_DIR=$NERLNET_PATH/config
NERLNET_CONFIG_JSONS_DIR=$NERLNET_CONFIG_DIR/jsonsDir.nerlconfig
NERLNET_CONFIG_JSONS_DIR_BACKUP=$NERLNET_CONFIG_DIR/jsonsDir.nerlconfig.bac
NERLNET_CONFIG_SUBNETS_DIR=$NERLNET_CONFIG_DIR/subnets.nerlconfig
NERLNET_CONFIG_SUBNETS_BACKUP=$NERLNET_CONFIG_DIR/subnets.nerlconfig.bac

TORCH_ENV_FILE="$NERLNET_PATH/build/torch_env.sh"
TEST_INPUT_TORCH_DIR="$TESTS_PATH/inputTorchJsonsFiles"
export TEST_BASELINE_DIR="$TEST_INPUT_TORCH_DIR/baseline"
export TEST_BASELINE_MODEL_FILENAME="model_perf_synt_1d_2c_4r_4w.csv"
export TEST_BASELINE_MODEL_STATS="$TEST_BASELINE_DIR/$TEST_BASELINE_MODEL_FILENAME"
export TEST_BASELINE_LOSS_MIN_FILENAME="min_loss_dict_synt_1d_2c_4r_4w.json"
export TEST_BASELINE_LOSS_MIN="$TEST_BASELINE_DIR/$TEST_BASELINE_LOSS_MIN_FILENAME"
export TEST_BASELINE_MODEL_PICKLE="model_perf_synt_1d_2c_4r_4w.pickle"
TEST_DC_JSON_NOIP=$TEST_INPUT_TORCH_DIR/dc_torch_synt_1d_2c_1s_4r_4w.json.noip
TEST_DC_JSON=$TEST_INPUT_TORCH_DIR/dc_torch_synt_1d_2c_1s_4r_4w.json

function replace_ip_in_json()
{
    IN_JSON_NOIP=$1
    OUT_JSON=$2
    NEW_IP=$3
    cp $IN_JSON_NOIP $OUT_JSON
    sed -i -e "s/x.x.x.x/$NEW_IP/g" $OUT_JSON
}

function port_is_listening()
{
    local port="$1"
    ss -ltn "( sport = :$port )" 2>/dev/null | tail -n +2 | grep -q .
}

function pick_api_receiver_port()
{
    local preferred_port="${NERLNET_TEST_API_SERVER_PORT:-8082}"
    if ! port_is_listening "$preferred_port"; then
        echo "$preferred_port"
        return 0
    fi

    if [ -n "${NERLNET_TEST_API_SERVER_PORT:-}" ]; then
        print "Requested NERLNET_TEST_API_SERVER_PORT=$preferred_port is already in use"
        return 1
    fi

    local candidate
    for candidate in $(seq 18082 18182); do
        if ! port_is_listening "$candidate"; then
            echo "$candidate"
            return 0
        fi
    done

    print "Failed to find a free API receiver port in range 18082-18182"
    return 1
}

function set_dc_api_port()
{
    local dc_json_path="$1"
    local api_port="$2"
    python3 - "$dc_json_path" "$api_port" <<'PY'
import json
import sys

dc_path = sys.argv[1]
api_port = str(int(sys.argv[2]))
with open(dc_path, "r", encoding="utf-8") as dc_file:
    dc_data = json.load(dc_file)
dc_data.setdefault("apiServer", {})["port"] = api_port
with open(dc_path, "w", encoding="utf-8") as dc_file:
    json.dump(dc_data, dc_file, indent=4)
    dc_file.write("\n")
PY
}

function stop_nerlnet()
{
    (cd "$NERLNET_PATH" && ./NerlnetRun.sh --run-mode stop >/dev/null 2>&1) || true
    pkill -9 -f "beam.smp" 2>/dev/null || true
    pkill -9 -f "erlexec" 2>/dev/null || true
    pkill -9 -f "nerlnetApp" 2>/dev/null || true
    sleep 2
}

function print()
{
    echo "[NERLNET-FULL-FLOW-TORCH-TEST] $1"
}

MANUAL_START=false

while [[ $# -gt 0 ]]; do
    case "$1" in
        --manual-start)
            MANUAL_START=true
            shift
            ;;
        *)
            print "Unknown argument: $1"
            exit 1
            ;;
    esac
done

if $MANUAL_START; then
    export NERLNET_MANUAL_START=1
    print "Manual start mode enabled - make sure NerlnetApp is already running"
else
    unset NERLNET_MANUAL_START 2>/dev/null
fi

if [ ! -f "$TORCH_ENV_FILE" ]; then
    print "Missing $TORCH_ENV_FILE. Run NerlnetInstall.sh --torch first."
    exit 1
fi

source $TORCH_ENV_FILE

DEFAULT_TEST_PYTHON="/tmp/nerlnet/virtualenv/bin/python"
if [ -n "$NERLNET_TEST_PYTHON" ]; then
    PYTHON_BIN="$NERLNET_TEST_PYTHON"
elif [ -x "$DEFAULT_TEST_PYTHON" ]; then
    PYTHON_BIN="$DEFAULT_TEST_PYTHON"
else
    PYTHON_BIN="python3"
fi

if ! command -v "$PYTHON_BIN" >/dev/null 2>&1; then
    print "Python executable $PYTHON_BIN not found. Run tests/set_env.sh --torch first or set NERLNET_TEST_PYTHON."
    exit 1
fi

MODEL_GENERATOR="$NERLNET_PATH/tests/scripts/generate_torch_test_model.py"
MODEL_PERCEPTRON_OUTPUT="$NERLNET_PATH/tests/inputTorchJsonsFiles/models/placeholder_perceptron.pt"
if [ -x "$MODEL_GENERATOR" ] || [ -f "$MODEL_GENERATOR" ]; then
    print "Generating TorchScript test model at $MODEL_PERCEPTRON_OUTPUT"
    # Ensure the Python wheel-provided libtorch is used instead of the system libtorch
    # by clearing LD_LIBRARY_PATH for this invocation only.
    if ! env LD_LIBRARY_PATH= "$PYTHON_BIN" "$MODEL_GENERATOR" --output "$MODEL_PERCEPTRON_OUTPUT"; then
        print "Failed to generate TorchScript model"
        exit 1
    fi
else
    print "Model generator script not found at $MODEL_GENERATOR"
fi

cp $NERLNET_CONFIG_SUBNETS_DIR $NERLNET_CONFIG_SUBNETS_BACKUP
CURRENT_MACHINE_IPV4_ADD="$(ip -4 -o addr show scope global | awk '{print $4}' | cut -d/ -f1 | grep -v '^127\.' | grep -v '^0\.0\.0\.0$' | head -n 1)"
if [ -z "$CURRENT_MACHINE_IPV4_ADD" ]; then
    CURRENT_MACHINE_IPV4_ADD="$(hostname -I | tr ' ' '\n' | grep -E '^[0-9]+\.[0-9]+\.[0-9]+\.[0-9]+$' | grep -v '^127\.' | grep -v '^0\.0\.0\.0$' | head -n 1)"
fi
if [ -z "$CURRENT_MACHINE_IPV4_ADD" ]; then
    print "Failed to detect a usable local IPv4 address"
    exit 1
fi
print "This machine ipv4 is: $CURRENT_MACHINE_IPV4_ADD"

TEST_API_SERVER_PORT="$(pick_api_receiver_port)" || exit 1
export NERLNET_TEST_API_SERVER_PORT="$TEST_API_SERVER_PORT"
print "Using API receiver port: $NERLNET_TEST_API_SERVER_PORT"

sed -i '$a\' $NERLNET_CONFIG_SUBNETS_DIR
echo "$CURRENT_MACHINE_IPV4_ADD" >> $NERLNET_CONFIG_SUBNETS_DIR
replace_ip_in_json $TEST_DC_JSON_NOIP $TEST_DC_JSON $CURRENT_MACHINE_IPV4_ADD
if ! set_dc_api_port "$TEST_DC_JSON" "$NERLNET_TEST_API_SERVER_PORT"; then
    print "Failed to set API server port in runtime DC JSON: $TEST_DC_JSON"
    exit 1
fi

source $TESTS_PATH/set_env.sh
print "Loaded Virtual Environment: $VIRTUAL_ENV"

cd $NERLNET_PATH

print "Create backup of $NERLNET_CONFIG_JSONS_DIR"
mv $NERLNET_CONFIG_JSONS_DIR $NERLNET_CONFIG_JSONS_DIR_BACKUP
echo $TEST_INPUT_TORCH_DIR > $NERLNET_CONFIG_JSONS_DIR

print "Execute Python - experiment_flow_test.py"
if ! $MANUAL_START; then
    stop_nerlnet
fi
python3 src_py/apiServer/experiment_flow_test.py
env_rc=$?
if ! $MANUAL_START; then
    stop_nerlnet
fi

rm $NERLNET_CONFIG_JSONS_DIR
print "Restore backup of $NERLNET_CONFIG_JSONS_DIR"
cp $NERLNET_CONFIG_JSONS_DIR_BACKUP $NERLNET_CONFIG_JSONS_DIR
rm $NERLNET_CONFIG_JSONS_DIR_BACKUP
print "Restore backup of $NERLNET_CONFIG_SUBNETS_DIR"
cp $NERLNET_CONFIG_SUBNETS_BACKUP $NERLNET_CONFIG_SUBNETS_DIR
rm $NERLNET_CONFIG_SUBNETS_BACKUP
rm $TEST_DC_JSON

if [[ -z "$RUNNING_IN_DOCKER" ]]; then
    deactivate
fi

print "Torch full flow test finished with status: $env_rc"
exit $env_rc
