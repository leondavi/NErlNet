#!/bin/bash

# Run this script with sudo priviledges 

if ! command -v erl &> /dev/null ; then
    echo "erlang could not be found"
    echo "Installing Erlang OTP" 
    wget -O- https://packages.erlang-solutions.com/ubuntu/erlang_solutions.asc | sudo apt-key add -
    echo "deb https://packages.erlang-solutions.com/ubuntu focal contrib" | sudo tee /etc/apt/sources.list.d/rabbitmq.list
    apt update
    apt install erlang
else
    echo "erlang is installed"
    erl -noshell -eval 'erlang:display(erlang:system_info(system_version))' -eval 'init:stop()'
fi


REBAR3_FILE=src_erl/rebar3/rebar3

if [ -f "$REBAR3_FILE" ]; then
	echo "rebar3 is installed, location: $REBAR3_FILE"
else 
	echo "rebar3 Builder"
	cd src_erl/rebar3
	./bootstrap
	cd ../../	
fi

