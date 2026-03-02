#!/bin/bash

set -u -o pipefail

function print()
{
    echo "[NERLNET-TORCH-PIPELINE-NIF-TEST] $1"
}

function usage()
{
    cat <<EOF
Usage: ${0##*/} [options]

Options:
  -h, --help    Show this help message.
EOF
}

while [[ $# -gt 0 ]]; do
    case "$1" in
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

DEFAULT_NERLNET_PATH="/usr/local/lib/nerlnet-lib/NErlNet"
if [ -z "${NERLNET_PATH:-}" ]; then
    if [ -d "$PWD/src_erl" ]; then
        NERLNET_PATH="$PWD"
    else
        NERLNET_PATH="$DEFAULT_NERLNET_PATH"
    fi
fi
export NERLNET_PATH

NOW=$(date +"%Y-%m-%d_%H_%M_%S")
LOG_FILE="nerlnet_torch_pipeline_test-$NOW.log"
ERL_BRIDGE_SOURCE_PATH="$NERLNET_PATH/src_erl/NerlnetApp/src/Bridge"
NERLNET_BUILD_DIR="$NERLNET_PATH/build"
NERLNET_TEST_DIR="$NERLNET_BUILD_DIR/test/torchPipelineNifTest"
TORCH_WORKER_DIR="torchWorkers"
TORCH_WORKER_PATH_FULL="$ERL_BRIDGE_SOURCE_PATH/$TORCH_WORKER_DIR"
TORCH_ENV_FILE="$NERLNET_PATH/build/torch_env.sh"

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
if ! command -v erl >/dev/null 2>&1; then
    print "Erlang runtime (erl) was not found in PATH."
    exit 1
fi

print "Using python executable: $PYTHON_BIN"

PIPELINE_MODEL_GENERATOR="$NERLNET_PATH/tests/scripts/generate_torch_pipeline_test_model.py"
MODEL_OUTPUT_PIPELINE="$NERLNET_PATH/tests/inputTorchJsonsFiles/models/placeholder_pipeline.pt"
if [ ! -f "$PIPELINE_MODEL_GENERATOR" ]; then
    print "Pipeline model generator script not found at $PIPELINE_MODEL_GENERATOR"
    exit 1
fi

print "Generating pipeline TorchScript test model at $MODEL_OUTPUT_PIPELINE"
if ! env LD_LIBRARY_PATH= "$PYTHON_BIN" "$PIPELINE_MODEL_GENERATOR" --output "$MODEL_OUTPUT_PIPELINE"; then
    print "Failed to generate pipeline TorchScript model"
    exit 1
fi

REQUIRED_SOURCE_FILES=(
    "$TORCH_WORKER_PATH_FULL/nerlTorchNIF.erl"
    "$TORCH_WORKER_PATH_FULL/torchDefs.hrl"
    "$TORCH_WORKER_PATH_FULL/torchPipelineTests.erl"
    "$TORCH_WORKER_PATH_FULL/torchPipelineTestsDefs.hrl"
    "$ERL_BRIDGE_SOURCE_PATH/nerl.erl"
    "$ERL_BRIDGE_SOURCE_PATH/nerlTensor.hrl"
    "$ERL_BRIDGE_SOURCE_PATH/nerlTensor.erl"
    "$ERL_BRIDGE_SOURCE_PATH/layers_types_ag.hrl"
    "$ERL_BRIDGE_SOURCE_PATH/models_types_ag.hrl"
)
for required_file in "${REQUIRED_SOURCE_FILES[@]}"; do
    if [ ! -f "$required_file" ]; then
        print "Missing required source file: $required_file"
        exit 1
    fi
done

print "Nerlnet pipeline NIF testing script initiated"
print "Copy files to $NERLNET_TEST_DIR"
rm -rf "$NERLNET_TEST_DIR"
mkdir -p "$NERLNET_TEST_DIR/$TORCH_WORKER_DIR"

cp "$TORCH_WORKER_PATH_FULL/nerlTorchNIF.erl" "$NERLNET_TEST_DIR/$TORCH_WORKER_DIR/nerlTorchNIF.erl"
cp "$TORCH_WORKER_PATH_FULL/torchDefs.hrl" "$NERLNET_TEST_DIR/$TORCH_WORKER_DIR/torchDefs.hrl"
cp "$TORCH_WORKER_PATH_FULL/torchPipelineTests.erl" "$NERLNET_TEST_DIR/$TORCH_WORKER_DIR/torchPipelineTests.erl"
cp "$TORCH_WORKER_PATH_FULL/torchPipelineTestsDefs.hrl" "$NERLNET_TEST_DIR/$TORCH_WORKER_DIR/torchPipelineTestsDefs.hrl"
cp "$ERL_BRIDGE_SOURCE_PATH/nerl.erl" "$NERLNET_TEST_DIR/nerl.erl"
cp "$ERL_BRIDGE_SOURCE_PATH/nerlTensor.hrl" "$NERLNET_TEST_DIR/nerlTensor.hrl"
cp "$ERL_BRIDGE_SOURCE_PATH/nerlTensor.erl" "$NERLNET_TEST_DIR/nerlTensor.erl"
cp "$ERL_BRIDGE_SOURCE_PATH/layers_types_ag.hrl" "$NERLNET_TEST_DIR/layers_types_ag.hrl"
cp "$ERL_BRIDGE_SOURCE_PATH/models_types_ag.hrl" "$NERLNET_TEST_DIR/models_types_ag.hrl"

print "Starting compilation"
cd "$NERLNET_TEST_DIR" || exit 1

COMPILE_NERL="compile:file(\"nerl.erl\")"
COMPILE_NERLTENSOR="compile:file(\"nerlTensor.erl\")"
COMPILE_TORCHNIF="compile:file(\"$TORCH_WORKER_DIR/nerlTorchNIF.erl\")"
COMPILE_PIPELINE_TESTS="compile:file(\"$TORCH_WORKER_DIR/torchPipelineTests.erl\")"
COMPILE_FILES="$COMPILE_NERL,$COMPILE_NERLTENSOR,$COMPILE_TORCHNIF,$COMPILE_PIPELINE_TESTS"

erl -noshell -pa "$NERLNET_PATH/_build/default/lib" -eval "$COMPILE_FILES, torchPipelineTests:run_tests()." -s init stop > "$NERLNET_TEST_DIR/$LOG_FILE"
RC=$?
cat "$NERLNET_TEST_DIR/$LOG_FILE"
cd - >/dev/null || true

print "Test returned: $RC"
exit $RC
