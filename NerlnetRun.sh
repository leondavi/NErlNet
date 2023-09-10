#!/bin/bash

NERLNET_PREFIX="[NERLNET_RUN]"
INPUT_DATA_DIR="inputDataDir"
BUILD_DIRECTORY="build/release"
buildNerlnetLibrary=0
TMP_DIR_RUN=/tmp/nerlnet/run
REMOTE_JSONS_DIR=/tmp/nerlnet/jsons
NERLNET_APP_BUILD_DIR=build/rebar/default/lib
NERLNET_APP_DIR=src_erl/NerlnetApp

#--------------------------------------------#
JobsNum=4
if [ "$ARCH_TYPE" = "armv7l" ]; then
        JobsNum=1
fi
#--------------------------------------------#
if [ -d "$INPUT_DATA_DIR" ]; then
        echo "$NERLNET_PREFIX Input data directory of nerlnet is: $INPUT_DATA_DIR"
else
        echo "$NERLNET_PREFIX $INPUT_DATA_DIR is generated and is empty!"
        mkdir $INPUT_DATA_DIR
        echo "$NERLNET_PREFIX Add input data to $INPUT_DATA_DIR"
fi
#--------------------------------------------#

if [ -d "$BUILD_DIRECTORY" ]; then
	echo "$NERLNET_PREFIX Build Directory exists"
else
	echo "$NERLNET_PREFIX Build Directory is missing - first time library build is on"
	buildNerlnetLibrary=1
fi

if [ -d "$TMP_DIR_RUN" ]; then
	echo "$NERLNET_PREFIX Delete temp directory content"
    cd $TMP_DIR_RUN
    rm -rf *
    cd -
else
	echo "$NERLNET_PREFIX Create temp directory"
	mkdir -p $TMP_DIR_RUN
fi

if [ -d "$REMOTE_JSONS_DIR" ]; then
	echo "$NERLNET_PREFIX Delete $REMOTE_JSONS_DIR directory content"
    cd $REMOTE_JSONS_DIR
    rm -rf *
    cd -
else
	echo "$NERLNET_PREFIX Create $REMOTE_JSONS_DIR directory"
	mkdir -p $REMOTE_JSONS_DIR
fi

while getopts "b" flag; do
    case "${flag}" in
        "b") buildNerlnetLibrary=1;;
    esac
done

if [[ "$buildNerlnetLibrary" -eq 1 ]] ; then
    echo "$NERLNET_PREFIX build script starts"
    ./NerlnetBuild.sh -j $JobsNum
fi

# only for raspberry
is_rasp="$(grep -c raspbian /etc/os-release)"
if [ $is_rasp -gt "0" ]; then 
    export LD_PRELOAD=/usr/lib/arm-linux-gnueabihf/libatomic.so.1.2.0 
fi

# Original Nerlnet Run
# cd src_erl/NerlnetApp
# echo "$NERLNET_PREFIX Script CWD: $PWD"
# rebar3 shell 
# cd -

# only for CI debug
cd src_erl/NerlnetApp
echo "$NERLNET_PREFIX Compile NerlnetApp"
rebar3 compile 
cd -

NERLNET_TEST_DIR=/home/parallels/workspace/NErlNet/build/test #tmp
mkdir -p $NERLNET_TEST_DIR
LOG_FILE="log.txt" #tmp 

REBAR3_COMPILED_EBIN_DIRS="jsx/ebin ranch/ebin cowlib/ebin cowboy/ebin nerlnetApp/ebin"
cd $NERLNET_APP_BUILD_DIR
erl -pa $REBAR3_COMPILED_EBIN_DIRS -eval "nerlnetApp_app:start(a,b)." -s init stop > "$NERLNET_TEST_DIR/$LOG_FILE"
cat $NERLNET_TEST_DIR/$LOG_FILE