#!/bin/bash

NERLNET_PATH="/usr/local/lib/nerlnet-lib/NErlNet"
NERLNET_CONFIG_DIR=$NERLNET_PATH/config
NERLNET_CONFIG_JSONS_DIR=$NERLNET_CONFIG_DIR/jsonsDir.nerlconfig
NERLNET_CONFIG_JSONS_DIR_BACKUP=$NERLNET_CONFIG_DIR/jsonsDir.nerlconfig.bac
TESTS_PATH="$NERLNET_PATH/Tests"

function print()
{
    echo "[NERLNET-FULL-FLOW-TEST] $1"
}

source $TESTS_PATH/set_env.sh
print "Loaded Virtual Environment: $VIRTUAL_ENV"



cd $NERLNET_PATH

print "Create backup of $NERLNET_CONFIG_JSONS_DIR" 
mv $NERLNET_CONFIG_JSONS_DIR $NERLNET_CONFIG_JSONS_DIR_BACKUP
echo "$TESTS_PATH/InputJsonFiles" > $NERLNET_CONFIG_JSONS_DIR

print "Execute NerlnetGetData.sh" 
./NerlnetGetData.sh
print "Execute Python - experiment_flow_test.py" 
python3 src_py/apiServer/experiment_flow_test.py


rm $NERLNET_CONFIG_JSONS_DIR
mv $NERLNET_CONFIG_JSONS_DIR_BACKUP $NERLNET_CONFIG_JSONS_DIR
print "Restore backup of $NERLNET_CONFIG_JSONS_DIR" 

deactivate

exit 0 #TODO change