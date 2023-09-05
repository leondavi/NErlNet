#!/bin/bash

function print()
{
    echo "[NERLNET-FULL-FLOW-TEST] $1"
}

source set_env.sh
print("Loaded Virtual Environment: $VIRTUAL_ENV)")

exit 0 #TODO change