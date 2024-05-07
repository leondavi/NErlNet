#!/bin/bash

function print()
{
    echo "[SOURCE-NIF-TEST] $1"
}
# Global Vars:
NERLNET_PATH="/usr/local/lib/nerlnet-lib/NErlNet"
NOW=$(date +"%Y-%m-%d_%H_%M_%S")
LOG_FILE="nerlnet_sourcenif_test-$NOW.log"
TEST_LOG_PATH="/usr/local/lib/nerlnet-lib/log"
TEST_LOG_FILE_PATH="$TEST_LOG_PATH/$LOG_FILE"
ERL_SOURCE_PATH="$NERLNET_PATH/src_erl/NerlnetApp/src/Source"
NERLNET_BUILD_DIR="$NERLNET_PATH/build"
NERLNET_TEST_DIR_SOURCENIF="$NERLNET_BUILD_DIR/test/sourcenif"

print "Nerlnet testing script initiated"
print "Copy files to $NERLNET_TEST_DIR_SOURCENIF"

if [ -d "$NERLNET_TEST_DIR_SOURCENIF" ];
then
    print "Removing old test directory"
    rm -rf $NERLNET_TEST_DIR_SOURCENIF
    print "Creating test directory"
    mkdir -p $NERLNET_TEST_DIR_SOURCENIF
else
	print "Creating test directory"
    mkdir -p $NERLNET_TEST_DIR_SOURCENIF
fi

cd $NERLNET_PATH
cp $ERL_SOURCE_PATH/sourceNIF.erl $NERLNET_TEST_DIR_SOURCENIF/sourceNIF.erl
cp $ERL_SOURCE_PATH/sourceNIFdefs.hrl $NERLNET_TEST_DIR_SOURCENIF/sourceNIFdefs.hrl
cp $ERL_SOURCE_PATH/testSourceNIF.erl $NERLNET_TEST_DIR_SOURCENIF/testSourceNIF.erl


print "Starting compilation: "
# only for raspberry
is_rasp="$(grep -c raspbian /etc/os-release)"
if [ $is_rasp -gt "0" ]; then 
    export LD_PRELOAD=/usr/lib/arm-linux-gnueabihf/libatomic.so.1.2.0 
fi

print "Change directory to $NERLNET_TEST_DIR_SOURCENIF:"
cd $NERLNET_TEST_DIR_SOURCENIF

print "Running nerlTests.erl"

erl -noshell -eval "cover:compile_directory(), testSourceNIF:run_tests()." -s init stop > "$NERLNET_TEST_DIR_SOURCENIF/$LOG_FILE" #TODO fix 
rc=$(echo $?)
cat "$NERLNET_TEST_DIR_SOURCENIF/$LOG_FILE"
cd -

exit $rc
