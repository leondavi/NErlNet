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
export NERLNET_RUNNING_TIMEOUT_SEC="${NERLNET_RUNNING_TIMEOUT_SEC:-5}"

NERLNET_CONFIG_DIR="$NERLNET_PATH/config"
NERLNET_CONFIG_JSONS_DIR="$NERLNET_CONFIG_DIR/jsonsDir.nerlconfig"
NERLNET_CONFIG_JSONS_DIR_BACKUP="$NERLNET_CONFIG_DIR/jsonsDir.nerlconfig.pipeline_suite.bac"
NERLNET_CONFIG_SUBNETS_DIR="$NERLNET_CONFIG_DIR/subnets.nerlconfig"
NERLNET_CONFIG_SUBNETS_BACKUP="$NERLNET_CONFIG_DIR/subnets.nerlconfig.pipeline_suite.bac"

TORCH_ENV_FILE="$NERLNET_PATH/build/torch_env.sh"
TEST_BASE_DIR="$TESTS_PATH/inputTorchPipelineJsonsFiles"
MODEL_GENERATOR="$NERLNET_PATH/tests/scripts/generate_torch_pipeline_test_model.py"
MODEL_PIPELINE_OUTPUT="$NERLNET_PATH/tests/inputTorchJsonsFiles/models/placeholder_pipeline.pt"

ALL_VARIANTS=("pipeline_gpipe" "pipeline_1f1b" "pipeline_interleaved" "tensor_only" "pipeline_tensor")
TEST_VARIANTS=("${ALL_VARIANTS[@]}")

MANUAL_START=false
SELECTED_VARIANTS=()
CONFIGS_BACKED_UP=false
VENV_LOADED=false
RUN_ID="$(date +%s)_$$"

function print()
{
    echo "[NERLNET-PARALLEL-SUITE] $1"
}

function usage()
{
    cat <<EOF
Usage: ${0##*/} [options]

Options:
  --manual-start       NerlnetApp is already running; skip start/stop in test runner.
  --variant <name>     Run only a specific variant (can be provided multiple times).
  -h, --help           Show this help message.

Valid variants:
  pipeline_gpipe | pipeline_1f1b | pipeline_interleaved | tensor_only | pipeline_tensor
EOF
}

function replace_ip_in_json()
{
    local in_json_noip="$1"
    local out_json="$2"
    local new_ip="$3"
    cp "$in_json_noip" "$out_json"
    sed -i -e "s/x.x.x.x/$new_ip/g" "$out_json"
}

function variant_is_valid()
{
    local candidate="$1"
    local known
    for known in "${ALL_VARIANTS[@]}"; do
        if [ "$candidate" = "$known" ]; then
            return 0
        fi
    done
    return 1
}

function stop_nerlnet()
{
    (cd "$NERLNET_PATH" && ./NerlnetRun.sh --run-mode stop >/dev/null 2>&1) || true
    pkill -9 -f "beam.smp" 2>/dev/null || true
    pkill -9 -f "erlexec" 2>/dev/null || true
    pkill -9 -f "nerlnetApp" 2>/dev/null || true
    sleep 2
}

function cleanup()
{
    find "$TEST_BASE_DIR" -maxdepth 2 -type f -name "dc_test*.json" -delete 2>/dev/null || true
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
    if [ "$VENV_LOADED" = true ] && [[ -z "${RUNNING_IN_DOCKER:-}" ]]; then
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
        --variant)
            if [ -z "${2:-}" ]; then
                print "Missing value for --variant"
                usage
                exit 1
            fi
            SELECTED_VARIANTS+=("$2")
            shift 2
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
    print "Manual start mode enabled - test runner will not stop/start NerlnetApp."
else
    unset NERLNET_MANUAL_START 2>/dev/null || true
fi

if [ ${#SELECTED_VARIANTS[@]} -gt 0 ]; then
    TEST_VARIANTS=()
    for requested in "${SELECTED_VARIANTS[@]}"; do
        if ! variant_is_valid "$requested"; then
            print "Unknown variant: $requested"
            usage
            exit 1
        fi
        TEST_VARIANTS+=("$requested")
    done
fi

if [ ! -f "$TORCH_ENV_FILE" ]; then
    print "Missing $TORCH_ENV_FILE. Run NerlnetInstall.sh --torch first."
    exit 1
fi
source "$TORCH_ENV_FILE"

DEFAULT_TEST_PYTHON="/tmp/nerlnet/virtualenv/bin/python"
if [ -n "${NERLNET_TEST_PYTHON:-}" ]; then
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

if [ ! -f "$MODEL_GENERATOR" ]; then
    print "Pipeline model generator script not found at $MODEL_GENERATOR"
    exit 1
fi
print "Generating pipeline TorchScript test model at $MODEL_PIPELINE_OUTPUT"
if ! env LD_LIBRARY_PATH= "$PYTHON_BIN" "$MODEL_GENERATOR" --output "$MODEL_PIPELINE_OUTPUT"; then
    print "Failed to generate pipeline TorchScript model"
    exit 1
fi

if [ ! -f "$NERLNET_CONFIG_JSONS_DIR" ] || [ ! -f "$NERLNET_CONFIG_SUBNETS_DIR" ]; then
    print "Missing NErlNet runtime config files under $NERLNET_CONFIG_DIR"
    exit 1
fi
cp "$NERLNET_CONFIG_JSONS_DIR" "$NERLNET_CONFIG_JSONS_DIR_BACKUP"
cp "$NERLNET_CONFIG_SUBNETS_DIR" "$NERLNET_CONFIG_SUBNETS_BACKUP"
CONFIGS_BACKED_UP=true

CURRENT_MACHINE_IPV4_ADD="$(ip -4 -o addr show scope global | awk '{print $4}' | cut -d/ -f1 | grep -v '^127\.' | grep -v '^0\.0\.0\.0$' | head -n 1)"
if [ -z "$CURRENT_MACHINE_IPV4_ADD" ]; then
    CURRENT_MACHINE_IPV4_ADD="$(hostname -I | tr ' ' '\n' | grep -E '^[0-9]+\.[0-9]+\.[0-9]+\.[0-9]+$' | grep -v '^127\.' | grep -v '^0\.0\.0\.0$' | head -n 1)"
fi
if [ -z "$CURRENT_MACHINE_IPV4_ADD" ]; then
    print "Failed to detect a usable local IPv4 address"
    exit 1
fi
print "This machine ipv4 is: $CURRENT_MACHINE_IPV4_ADD"
sed -i '$a\' "$NERLNET_CONFIG_SUBNETS_DIR"
echo "$CURRENT_MACHINE_IPV4_ADD" >> "$NERLNET_CONFIG_SUBNETS_DIR"

export RUNNING_IN_DOCKER="${RUNNING_IN_DOCKER:-}"
source "$TESTS_PATH/set_env.sh"
# tests/set_env.sh defines its own `print` helper; restore suite logger prefix.
function print()
{
    echo "[NERLNET-PARALLEL-SUITE] $1"
}
VENV_LOADED=true
print "Loaded Virtual Environment: ${VIRTUAL_ENV:-unknown}"

cd "$NERLNET_PATH"

declare -A RESULTS
OVERALL_RC=0

print "========================================"
print "Running ${#TEST_VARIANTS[@]} variant(s): ${TEST_VARIANTS[*]}"
print "========================================"

for VARIANT in "${TEST_VARIANTS[@]}"; do
    VARIANT_DIR="$TEST_BASE_DIR/$VARIANT"
    DC_NOIP="$VARIANT_DIR/dc_test.json.noip"
    DC_JSON="$VARIANT_DIR/dc_test_${RUN_ID}.json"
    CONN_JSON="$VARIANT_DIR/conn_test.json"
    EXP_JSON="$VARIANT_DIR/exp_test.json"

    print "========================================"
    print "[$VARIANT] Starting test"
    print "========================================"

    if [ ! -d "$VARIANT_DIR" ]; then
        print "[$VARIANT] Missing directory: $VARIANT_DIR"
        RESULTS[$VARIANT]="FAILED (missing_variant_dir)"
        OVERALL_RC=1
        continue
    fi
    if [ ! -f "$DC_NOIP" ] || [ ! -f "$CONN_JSON" ] || [ ! -f "$EXP_JSON" ]; then
        print "[$VARIANT] Missing required files (dc_test.json.noip/conn_test.json/exp_test.json)"
        RESULTS[$VARIANT]="FAILED (missing_variant_json)"
        OVERALL_RC=1
        continue
    fi

    if ! replace_ip_in_json "$DC_NOIP" "$DC_JSON" "$CURRENT_MACHINE_IPV4_ADD"; then
        print "[$VARIANT] Failed to prepare runtime DC JSON: $DC_JSON"
        RESULTS[$VARIANT]="FAILED (dc_prepare_failed)"
        OVERALL_RC=1
        continue
    fi
    if [ ! -f "$DC_JSON" ]; then
        print "[$VARIANT] Runtime DC JSON missing after preparation: $DC_JSON"
        RESULTS[$VARIANT]="FAILED (dc_missing)"
        OVERALL_RC=1
        continue
    fi
    echo "$VARIANT_DIR" > "$NERLNET_CONFIG_JSONS_DIR"

    export TEST_VARIANT="$VARIANT"
    export TEST_TARGET_DC_JSON="$(basename "$DC_JSON")"
    export TEST_TARGET_CONN_JSON="conn_test.json"
    export TEST_TARGET_EXP_JSON="exp_test.json"
    export TEST_EXPECT_DATASET_TOKEN="${TEST_EXPECT_DATASET_TOKEN:-synthetic_norm/synthetic_full.csv}"
    export TEST_MIN_AVG_F1="${TEST_MIN_AVG_F1:-0.50}"
    export TEST_MIN_AVG_ACCURACY="${TEST_MIN_AVG_ACCURACY:-0.50}"
    case "$VARIANT" in
        pipeline_gpipe)
            export TEST_EXPECT_MODE="pipeline"
            export TEST_EXPECT_SCHEDULER="gpipe"
            export TEST_EXPECT_MIN_WORKERS=2
            ;;
        pipeline_1f1b)
            export TEST_EXPECT_MODE="pipeline"
            export TEST_EXPECT_SCHEDULER="1f1b"
            export TEST_EXPECT_MIN_WORKERS=2
            ;;
        pipeline_interleaved)
            export TEST_EXPECT_MODE="pipeline"
            export TEST_EXPECT_SCHEDULER="interleaved"
            export TEST_EXPECT_MIN_WORKERS=2
            ;;
        tensor_only)
            export TEST_EXPECT_MODE="tensor"
            export TEST_EXPECT_SCHEDULER=""
            export TEST_EXPECT_MIN_WORKERS=2
            ;;
        pipeline_tensor)
            export TEST_EXPECT_MODE="pipeline_tensor"
            export TEST_EXPECT_SCHEDULER="1f1b"
            export TEST_EXPECT_MIN_WORKERS=4
            ;;
        *)
            export TEST_EXPECT_MODE=""
            export TEST_EXPECT_SCHEDULER=""
            export TEST_EXPECT_MIN_WORKERS=0
            ;;
    esac

    if ! $MANUAL_START; then
        stop_nerlnet
    fi

    "$PYTHON_BIN" src_py/apiServer/experiment_flow_pipeline_test.py
    TEST_RC=$?

    rm -f "$DC_JSON"
    if ! $MANUAL_START; then
        stop_nerlnet
    fi

    if [ $TEST_RC -eq 0 ]; then
        RESULTS[$VARIANT]="PASSED"
        print "[$VARIANT] PASSED"
    else
        RESULTS[$VARIANT]="FAILED (exit=$TEST_RC)"
        print "[$VARIANT] FAILED (exit code $TEST_RC)"
        OVERALL_RC=1
    fi
    print ""
done

print "========================================"
print "TEST SUITE RESULTS"
print "========================================"
for VARIANT in "${TEST_VARIANTS[@]}"; do
    print "  $VARIANT: ${RESULTS[$VARIANT]:-NOT_RUN}"
done
print "========================================"

if [ $OVERALL_RC -eq 0 ]; then
    print "ALL SELECTED TESTS PASSED"
else
    print "SOME TESTS FAILED"
fi

exit $OVERALL_RC
