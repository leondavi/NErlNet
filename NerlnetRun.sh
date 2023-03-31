#!/bin/bash

NERLNET_PREFIX="[NERLNET_RUN]"
INPUT_DATA_DIR="inputDataDir"
BUILD_DIRECTORY="build/release"
buildNerlnetLibrary=0

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

while getopts "b" flag; do
    case "${flag}" in
        "b") buildNerlnetLibrary=1;;
    esac
done

if [[ "$buildNerlnetLibrary" -eq 1 ]] ; then
    echo "$NERLNET_PREFIX build script starts"
    ./NerlnetBuild.sh -j $JobsNum
fi


cd src_erl/Communication_Layer/http_Nerlserver
echo "$NERLNET_PREFIX Script CWD: $PWD"
rebar3 shell 
cd ../../../
