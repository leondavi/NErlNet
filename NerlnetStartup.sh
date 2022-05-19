#!/bin/bash

startupRun=false
NERLNET_LIB_DIR="/usr/local/lib/nerlnet-lib"
JobsNum=4

startupRunFlow()
{
    cd "$NERLNET_LIB_DIR/NErlNet"

    if [ "$ARCH_TYPE" = "armv7l" ]; then
        JobsNum=1
    fi
    /bin/bash NerlnetBuild.sh -p master -j $JobsNum 
    /bin/bash NerlnetRun.sh
}

if [ "$startupRun" = true ] ; then
    echo "Nerlnet startup run script is activated"
    startupRunFlow
else
    echo "Nerlnet startup run script is deactivated"
fi