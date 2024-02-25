#!/bin/bash

function print()
{
    echo "[NERLNET-NIF-TEST] $1"
}
# Global Vars:
NERLNET_PATH="/usr/local/lib/nerlnet-lib/NErlNet"
NOW=$(date +"%Y-%m-%d_%H_%M_%S")
LOG_FILE="nerlnet_test-$NOW.log"
TEST_LOG_PATH="/usr/local/lib/nerlnet-lib/log"
TEST_LOG_FILE_PATH="$TEST_LOG_PATH/$LOG_FILE"
ERL_BRIDGE_SOURCE_PATH="$NERLNET_PATH/src_erl/NerlnetApp/src/Bridge"
NERLNET_BUILD_DIR="$NERLNET_PATH/build"
NERLNET_TEST_DIR="$NERLNET_BUILD_DIR/test"

print "Nerlnet testing script initiated"
print "Copy files to $NERLNET_TEST_DIR"

if [ -d "$NERLNET_TEST_DIR" ];
then
    print "Removing old test directory"
    rm -rf $NERLNET_TEST_DIR
    print "Creating test directory"
    mkdir -p $NERLNET_TEST_DIR
else
	print "Creating test directory"
    mkdir -p $NERLNET_TEST_DIR
fi

cd $NERLNET_PATH
cp $ERL_BRIDGE_SOURCE_PATH/nerlTests.erl $NERLNET_TEST_DIR/nerlTests.erl
cp $ERL_BRIDGE_SOURCE_PATH/nerlNIF.erl $NERLNET_TEST_DIR/nerlNIF.erl
cp $ERL_BRIDGE_SOURCE_PATH/nerl.erl $NERLNET_TEST_DIR/nerl.erl
cp $ERL_BRIDGE_SOURCE_PATH/nerlTensor.hrl $NERLNET_TEST_DIR/nerlTensor.hrl
cp $ERL_BRIDGE_SOURCE_PATH/nerlTensor.erl $NERLNET_TEST_DIR/nerlTensor.erl
cp $ERL_BRIDGE_SOURCE_PATH/neural_networks_testing_models.hrl $NERLNET_TEST_DIR/neural_networks_testing_models.hrl
cp $ERL_BRIDGE_SOURCE_PATH/layers_types_ag.hrl $NERLNET_TEST_DIR/layers_types_ag.hrl

print "Starting compilation: "
# only for raspberry
is_rasp="$(grep -c raspbian /etc/os-release)"
if [ $is_rasp -gt "0" ]; then 
    export LD_PRELOAD=/usr/lib/arm-linux-gnueabihf/libatomic.so.1.2.0 
fi
cmake -S . -B build/release -DCMAKE_BUILD_TYPE=RELEASE
cd build/release 
make . 
cd -

print "Change directory to $NERLNET_TEST_DIR:"
cd $NERLNET_TEST_DIR

print "Running nerlTests.erl"
COMPILE_NERLNIF="compile:file(\"nerlNIF.erl\")"
COMPILE_NERLTEST="compile:file(\"nerlTests.erl\")"
COMPILE_NERL="compile:file(\"nerl.erl\")"
COMPILE_NERLTENSOR="compile:file(\"nerlTensor.erl\")"

COMPILE_FILES="$COMPILE_NERL,$COMPILE_NERLNIF,$COMPILE_NERLTEST,$COMPILE_NERLTENSOR"

erl -noshell -eval "$COMPILE_FILES, nerlTests:run_tests()." -s init stop > "$NERLNET_TEST_DIR/$LOG_FILE" #TODO fix 
rc=$(echo $?)
cat "$NERLNET_TEST_DIR/$LOG_FILE"
cd -

exit $rc
