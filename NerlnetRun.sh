#!/bin/bash

BUILD_DIRECTORY="build/release"
buildNerlnetLibrary=0

if [ -d "$BUILD_DIRECTORY" ]; then
	echo "Build Directory exists"
else
	echo "Build Directory is missing - first time library build is on"
	buildNerlnetLibrary=1
fi

while getopts "b" flag; do
    case "${flag}" in
        "b") buildNerlnetLibrary=1;;
    esac
done

if [[ "$buildNerlnetLibrary" -eq 1 ]] ; then
    echo "build script starts"
    ./buildNerlnet.sh
fi


cd src_erl/Communication_Layer/http_Nerlserver
echo "Script CWD: $PWD"
../../rebar3/rebar3 shell 
cd ../../../
