#!/bin/bash

set -o pipefail

DEFAULT_NERLNET_PATH="/usr/local/lib/nerlnet-lib/NErlNet"
if [ -z "${NERLNET_PATH:-}" ]; then
    if [ -d "$PWD/src_erl" ]; then
        export NERLNET_PATH="$PWD"
    else
        export NERLNET_PATH="$DEFAULT_NERLNET_PATH"
    fi
fi

export TESTS_PATH="$NERLNET_PATH/tests"
export NERLNET_RUNNING_TIMEOUT_SEC="30"

NERLNET_CONFIG_DIR=$NERLNET_PATH/config
NERLNET_CONFIG_JSONS_DIR=$NERLNET_CONFIG_DIR/jsonsDir.nerlconfig
NERLNET_CONFIG_JSONS_DIR_BACKUP=$NERLNET_CONFIG_DIR/jsonsDir.nerlconfig.bac
NERLNET_CONFIG_SUBNETS_DIR=$NERLNET_CONFIG_DIR/subnets.nerlconfig
NERLNET_CONFIG_SUBNETS_BACKUP=$NERLNET_CONFIG_DIR/subnets.nerlconfig.bac

TEST_INPUT_JSONS_FILES_DIR="$TESTS_PATH/inputJsonsFiles"
export TEST_BASELINE_DIR="$TEST_INPUT_JSONS_FILES_DIR/baseline"
export TEST_BASELINE_MODEL_FILENAME="model_perf_synt_1d_2c_4r_4w.csv"
export TEST_BASELINE_MODEL_STATS="$TEST_BASELINE_DIR/$TEST_BASELINE_MODEL_FILENAME"
export TEST_BASELINE_LOSS_MIN_FILENAME="min_loss_dict_synt_1d_2c_4r_4w.json"
export TEST_BASELINE_LOSS_MIN="$TEST_BASELINE_DIR/$TEST_BASELINE_LOSS_MIN_FILENAME"

TEST_DC_JSON_NOIP=$TEST_INPUT_JSONS_FILES_DIR/dc_test_synt_1d_2c_1s_4r_4w.json.noip
TEST_DC_JSON=$TEST_INPUT_JSONS_FILES_DIR/dc_test_synt_1d_2c_1s_4r_4w.json
export TEST_VARIANT="full_flow_legacy"
export TEST_TARGET_DC_JSON="$(basename "$TEST_DC_JSON")"
export TEST_TARGET_CONN_JSON="conn_test_synt_1d_2c_1s_4r_4w.json"
export TEST_TARGET_EXP_JSON="exp_test_synt_1d_2c_1s_4r_4w.json"
CONFIGS_BACKED_UP=false
VENV_LOADED=false

function replace_ip_in_json()
{
    IN_JSON_NOIP=$1
    OUT_JSON=$2
    NEW_IP=$3
    cp $IN_JSON_NOIP $OUT_JSON
    sed -i -e "s/x.x.x.x/$NEW_IP/g" $OUT_JSON
}

function print()
{
    echo "[NERLNET-FULL-FLOW-TEST] $1"
}

function usage()
{
    cat <<EOF
Usage: ${0##*/} [options]

Options:
  --manual-start    NerlnetApp is already running; skip auto start logic.
  -h, --help        Show this help message and exit.
EOF
}

MANUAL_START=false

function cleanup()
{
    if [ "$CONFIGS_BACKED_UP" = true ]; then
        if [ -f "$NERLNET_CONFIG_JSONS_DIR_BACKUP" ]; then
            cp "$NERLNET_CONFIG_JSONS_DIR_BACKUP" "$NERLNET_CONFIG_JSONS_DIR"
            rm -f "$NERLNET_CONFIG_JSONS_DIR_BACKUP"
        fi
        if [ -f "$NERLNET_CONFIG_SUBNETS_BACKUP" ]; then
            cp "$NERLNET_CONFIG_SUBNETS_BACKUP" "$NERLNET_CONFIG_SUBNETS_DIR"
            rm -f "$NERLNET_CONFIG_SUBNETS_BACKUP"
        fi
    fi
    rm -f "$TEST_DC_JSON"

    if [ "$VENV_LOADED" = true ] && [[ -z "$RUNNING_IN_DOCKER" ]]; then
        deactivate 2>/dev/null || true
    fi
}

trap cleanup EXIT

while [[ $# -gt 0 ]]; do
    case "$1" in
        --manual-start)
            MANUAL_START=true
            shift
            ;;
        -h|--help)
            usage
            exit 0
            ;;
        *)
            print "Unknown argument: $1"
            usage
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

# add this host ip to subnets and backup subnets.nerlconfig
cp "$NERLNET_CONFIG_SUBNETS_DIR" "$NERLNET_CONFIG_SUBNETS_BACKUP"
cp "$NERLNET_CONFIG_JSONS_DIR" "$NERLNET_CONFIG_JSONS_DIR_BACKUP"
CONFIGS_BACKED_UP=true
CURRENT_MACHINE_IPV4_ADD="$(ip addr | grep -m 2 -E -o "([0-9]{1,3}[\.]){3}[0-9]{1,3}" | head -n 2 | grep -v "127.0.0.1")"
print "This machine ipv4 is: $CURRENT_MACHINE_IPV4_ADD"
sed -i '$a\' "$NERLNET_CONFIG_SUBNETS_DIR"
echo "$CURRENT_MACHINE_IPV4_ADD" >> "$NERLNET_CONFIG_SUBNETS_DIR"
replace_ip_in_json "$TEST_DC_JSON_NOIP" "$TEST_DC_JSON" "$CURRENT_MACHINE_IPV4_ADD"

source "$TESTS_PATH/set_env.sh"
VENV_LOADED=true
print "Loaded Virtual Environment: $VIRTUAL_ENV"

cd $NERLNET_PATH

print "Create backup of $NERLNET_CONFIG_JSONS_DIR" 
echo "$TEST_INPUT_JSONS_FILES_DIR" > "$NERLNET_CONFIG_JSONS_DIR"

print "Execute Python - experiment_flow_test.py" 
python3 src_py/apiServer/experiment_flow_test.py
rc=$(echo $?)


exit $rc
