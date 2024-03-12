#!/bin/bash

MONITOR_PATH="src_erl/NerlMonitor"
GUI_PATH="src_erl/NerlMonitor/src"

echo "NerlnetMonitor Activated"


cd $MONITOR_PATH
rebar3 shell --name erl@127.0.0.1 --setcookie COOKIE 

cd ../../
