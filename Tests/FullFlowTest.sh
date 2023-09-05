#!/bin/bash

NERLNET_PATH="/usr/local/lib/nerlnet-lib/NErlNet"
TESTS_PATH="$NERLNET_PATH/Tests"

function print()
{
    echo "[NERLNET-FULL-FLOW-TEST] $1"
}

source $TESTS_PATH/set_env.sh
print "Loaded Virtual Environment: $VIRTUAL_ENV"

exit 0 #TODO change