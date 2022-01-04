#!/bin/bash

NERLNET_BUILD_PREFIX="[Nerlnet Build] "

echo "$NERLNET_BUILD_PREFIX Building Nerlnet Library"
echo "$NERLNET_BUILD_PREFIX Cmake command of Nerlnet NIFPP"
cmake -S . -B build/release -DCMAKE_BUILD_TYPE=RELEASE
cd build/release
echo "$NERLNET_BUILD_PREFIX Script CWD: $PWD"
echo "$NERLNET_BUILD_PREFIX Build Nerlnet"
make -j4 
cd ../../
echo "$NERLNET_BUILD_PREFIX Script CWD: $PWD"

REBAR3_FILE=src_erl/rebar3/rebar3
REBAR3_SYMLINK=/usr/local/bin/rebar3

if [ -f "$REBAR3_FILE" ]; then
	echo "$NERLNET_BUILD_PREFIX rebar3 is installed, location: $REBAR3_FILE"
	if [ -f "$REBAR3_SYMLINK" ]; then
		echo "$NERLNET_BUILD_PREFIX rebar3 Synlink exists in /usr/local/bin"
	else
		echo "$NERLNET_BUILD_PREFIX $(tput setaf 1) Please run the following command from Nerlnet library root folder (or install rebar3 to usr/local/bin): $(tput sgr 0)"
		echo "$NERLNET_BUILD_PREFIX $(tput setaf 1) sudo ln -s `pwd`/src_erl/rebar3/rebar3 /usr/local/bin/rebar3 $(tput sgr 0)"
		echo "$NERLNET_BUILD_PREFIX "
	fi
else 
	echo "$NERLNET_BUILD_PREFIX rebar3 Builder Start"
	cd src_erl/rebar3
	./bootstrap
	cd ../../	
	echo "$NERLNET_BUILD_PREFIX rebar3 is Built at $REBAR3_FILE"
fi

echo "$NERLNET_BUILD_PREFIX Starting rebar3 Shell"
cd src_erl/Communication_Layer/http_Nerlserver
rebar3 shell