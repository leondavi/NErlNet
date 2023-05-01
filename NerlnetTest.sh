#!/bin/bash

function print()
{
    echo "[NERLNET-TEST] $1"
}

# Global Vars:
NERLNET_PATH="/usr/local/lib/nerlnet-lib/NErlNet"
NOW=$(date +"%Y-%m-%d_%H_%M_%S")
LOG_FILE="nerlnet_test-$NOW.log"
TEST_LOG_PATH="/usr/local/lib/nerlnet-lib/log"
TEST_LOG_FILE_PATH="$TEST_LOG_PATH/$LOG_FILE"

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


cp src_erl/erlBridge/nerlTests.erl $NERLNET_TEST_DIR/nerlTests.erl
cp src_erl/erlBridge/nerlNIF.erl $NERLNET_TEST_DIR/nerlNIF.erl
cp src_erl/erlBridge/nerl.erl $NERLNET_TEST_DIR/nerl.erl
cp src_erl/erlBridge/nerlTensor.hrl $NERLNET_TEST_DIR/nerlTensor.hrl

print "Starting compilation: "
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

COMPILE_FILES="$COMPILE_NERL,$COMPILE_NERLNIF,$COMPILE_NERLTEST"

erl -noshell -eval "$COMPILE_FILES, nerlTests:run_tests()." -s init stop > "$NERLNET_TEST_DIR/$LOG_FILE"
cd -
