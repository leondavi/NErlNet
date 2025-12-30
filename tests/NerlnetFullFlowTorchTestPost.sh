#!/bin/bash

LOG_FILE="/tmp/nerlnet_run_log.txt"
TAG="[NERLNET-FULL-FLOW-TORCH-POST]"

echo "$TAG Printing Nerlnet run log from $LOG_FILE"

if [ ! -f "$LOG_FILE" ]; then
    echo "$TAG Log file not found"
    exit 1
fi

cat "$LOG_FILE"
