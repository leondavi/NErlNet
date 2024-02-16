#!/bin/bash

AUTORUN_BRANCH="autorun"
startupRun=true
NERLNET_LIB_DIR="/usr/local/lib/nerlnet-lib"
DATE=date=$(date '+%Y-%m-%d_%H_%M_%S')
NERLNET_LOG_DIR="/usr/local/lib/nerlnet-lib/log"
NERLNET_LOG_FILE="/usr/local/lib/nerlnet-lib/log/log-Nerlnet-Startup-$DATE.txt"
JobsNum=4

startupRunFlow()
{
    cd "$NERLNET_LIB_DIR/NErlNet"

    if [ "$ARCH_TYPE" = "armv7l" ]; then
        JobsNum=1
    fi
    git checkout -f $AUTORUN_BRANCH > "$NERLNET_LOG_FILE"
    /bin/bash NerlnetBuild.sh -p $AUTORUN_BRANCH -j $JobsNum > "$NERLNET_LOG_FILE"
    /bin/bash NerlnetRun.sh --run-mode release
}

if [ "$startupRun" = true ] ; then
    echo "Nerlnet startup run script is activated" > "$NERLNET_LOG_FILE"
    startupRunFlow
else
    echo "Nerlnet startup run script is deactivated" > "$NERLNET_LOG_FILE"
fi
