#!/bin/bash

echo "Building Nerlnet Library"
echo "Cmake command of Nerlnet NIFPP"
cmake -S . -B build/release -DCMAKE_BUILD_TYPE=RELEASE
cd build/release
echo "Script CWD: $PWD"
echo "Build Nerlnet"
make -j4 
cd ../../
echo "Script CWD: $PWD"

REBAR3_FILE=src_erl/rebar3/rebar3

if [ -f "$REBAR3_FILE" ]; then
	echo "rebar3 is installed, location: $REBAR3_FILE"
	echo "Please run the following command from Nerlnet library root folder (or install rebar3 to usr/local/bin):"
	echo " sudo ln -s `pwd`/src_erl/rebar3/rebar3 /usr/local/bin/rebar3"
else 
	echo "rebar3 Builder"
	cd src_erl/rebar3
	./bootstrap
	cd ../../	
fi

cd src_erl/Communication_Layer/http_Nerlserver
rebar3 shell