#!/bin/bash

function print()
{
    echo "[NERLNET-TORCH-NIF-TEST] $1"
}

DEFAULT_NERLNET_PATH="/usr/local/lib/nerlnet-lib/NErlNet"
if [ -z "$NERLNET_PATH" ]; then
    if [ -d "$PWD/src_erl" ]; then
        NERLNET_PATH="$PWD"
    else
        NERLNET_PATH="$DEFAULT_NERLNET_PATH"
    fi
fi
export NERLNET_PATH
NOW=$(date +"%Y-%m-%d_%H_%M_%S")
LOG_FILE="nerlnet_torch_test-$NOW.log"
TEST_LOG_PATH="/usr/local/lib/nerlnet-lib/log"
TEST_LOG_FILE_PATH="$TEST_LOG_PATH/$LOG_FILE"
ERL_BRIDGE_SOURCE_PATH="$NERLNET_PATH/src_erl/NerlnetApp/src/Bridge"
NERLNET_BUILD_DIR="$NERLNET_PATH/build"
NERLNET_TEST_DIR="$NERLNET_BUILD_DIR/test/torchNifTest"
TORCH_WORKER_DIR="torchWorkers"
TORCH_WORKER_PATH_FULL="$ERL_BRIDGE_SOURCE_PATH/$TORCH_WORKER_DIR"
TORCH_ENV_FILE="$NERLNET_PATH/build/torch_env.sh"

if [ ! -f "$TORCH_ENV_FILE" ]; then
    print "Missing $TORCH_ENV_FILE. Run NerlnetInstall.sh --torch first."
    exit 1
fi

source "$TORCH_ENV_FILE"

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

print "Using python executable: $PYTHON_BIN"

MODEL_GENERATOR="$NERLNET_PATH/tests/scripts/generate_torch_test_model.py"
MODEL_PERCEPTRON_RELATIVE="tests/inputTorchJsonsFiles/models/placeholder_perceptron.pt"
MODEL_AUTOENCODER_RELATIVE="tests/inputTorchJsonsFiles/models/placeholder_autoencoder.pt"
MODEL_CNN_RELATIVE="tests/inputTorchJsonsFiles/models/placeholder_cnn.pt"
MODEL_OUTPUT_PERCEPTRON="$NERLNET_PATH/$MODEL_PERCEPTRON_RELATIVE"
MODEL_OUTPUT_AUTOENCODER="$NERLNET_PATH/$MODEL_AUTOENCODER_RELATIVE"
MODEL_OUTPUT_CNN="$NERLNET_PATH/$MODEL_CNN_RELATIVE"

generate_torch_model() {
    local output="$1"
    shift
    print "Generating TorchScript test model at $output"
    if ! "$PYTHON_BIN" "$MODEL_GENERATOR" --output "$output" "$@"; then
        print "Failed to generate TorchScript model"
        exit 1
    fi
}

if [ -x "$MODEL_GENERATOR" ] || [ -f "$MODEL_GENERATOR" ]; then
    generate_torch_model "$MODEL_OUTPUT_PERCEPTRON"
    generate_torch_model "$MODEL_OUTPUT_AUTOENCODER" --input-size 32 --hidden-sizes 16 8 4 8 16 --labels 32
    generate_torch_model "$MODEL_OUTPUT_CNN" --input-size 784 --hidden-sizes 128 64 --labels 10
else
    print "Model generator script not found at $MODEL_GENERATOR"
fi

print "Nerlnet testing script initiated for Torch-NIF"
print "Copy files to $NERLNET_TEST_DIR"

rm -rf "$NERLNET_TEST_DIR"
mkdir -p "$NERLNET_TEST_DIR/$TORCH_WORKER_DIR"

cd "$NERLNET_PATH"
cp "$TORCH_WORKER_PATH_FULL/nerlTorchNIF.erl" "$NERLNET_TEST_DIR/$TORCH_WORKER_DIR/nerlTorchNIF.erl"
cp "$TORCH_WORKER_PATH_FULL/torchDefs.hrl" "$NERLNET_TEST_DIR/$TORCH_WORKER_DIR/torchDefs.hrl"
cp "$TORCH_WORKER_PATH_FULL/torchTests.erl" "$NERLNET_TEST_DIR/$TORCH_WORKER_DIR/torchTests.erl"
cp "$TORCH_WORKER_PATH_FULL/torchTestsDefs.hrl" "$NERLNET_TEST_DIR/$TORCH_WORKER_DIR/torchTestsDefs.hrl"
cp "$TORCH_WORKER_PATH_FULL/torchTestsModels.hrl" "$NERLNET_TEST_DIR/$TORCH_WORKER_DIR/torchTestsModels.hrl"
cp "$ERL_BRIDGE_SOURCE_PATH/nerl.erl" "$NERLNET_TEST_DIR/nerl.erl"
cp "$ERL_BRIDGE_SOURCE_PATH/nerlTensor.hrl" "$NERLNET_TEST_DIR/nerlTensor.hrl"
cp "$ERL_BRIDGE_SOURCE_PATH/nerlTensor.erl" "$NERLNET_TEST_DIR/nerlTensor.erl"
cp "$ERL_BRIDGE_SOURCE_PATH/layers_types_ag.hrl" "$NERLNET_TEST_DIR/layers_types_ag.hrl"
cp "$ERL_BRIDGE_SOURCE_PATH/models_types_ag.hrl" "$NERLNET_TEST_DIR/models_types_ag.hrl"

print "Starting compilation:"
cd "$NERLNET_TEST_DIR"

COMPILE_NERL="compile:file(\"nerl.erl\")"
COMPILE_NERLTENSOR="compile:file(\"nerlTensor.erl\")"
COMPILE_TORCHNIF="compile:file(\"$TORCH_WORKER_DIR/nerlTorchNIF.erl\")"
COMPILE_TORCHTESTS="compile:file(\"$TORCH_WORKER_DIR/torchTests.erl\")"

COMPILE_FILES="$COMPILE_NERL,$COMPILE_NERLTENSOR,$COMPILE_TORCHNIF,$COMPILE_TORCHTESTS"

erl -noshell -pa "$NERLNET_PATH/_build/default/lib" -eval "$COMPILE_FILES, torchTests:run_tests()." -s init stop > "$NERLNET_TEST_DIR/$LOG_FILE"
rc=$(echo $?)
cat "$NERLNET_TEST_DIR/$LOG_FILE"
cd -

print "Test returned: $rc"
exit $rc
