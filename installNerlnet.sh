#!/bin/bash

# Run this script with sudo priviledges 

apt-get -y install cmake

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

