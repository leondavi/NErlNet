#!/bin/bash

NERLNET_PATH="/usr/local/lib/nerlnet-lib/NErlNet"
NERLNET_CONFIG_DIR=$NERLNET_PATH/config
NERLNET_CONFIG_JSONS_DIR=$NERLNET_CONFIG_DIR/jsonsDir.nerlconfig
NERLNET_CONFIG_JSONS_DIR_BACKUP=$NERLNET_CONFIG_DIR/jsonsDir.nerlconfig.bac
TESTS_PATH="$NERLNET_PATH/Tests"

TEST_INPUT_JSONS_FILES_DIR="$TESTS_PATH/InputJsonsFiles"

TEST_ARCH_JSON_NOIP_0=$TEST_INPUT_JSONS_FILES_DIR/arch_test_synt_d1_2c_1s_4r_4w.json.noip
TEST_ARCH_JSON_0=$TEST_INPUT_JSONS_FILES_DIR/arch_test_synt_d1_2c_1s_4r_4w.json

function replace_ip_in_json()
{
    IN_JSON_NOIP=$1
    OUT_JSON=$2
    NEW_IP=$3
    cp $IN_JSON_NOIP $OUT_JSON
    sed -i -e "s/x.x.x.x/$NEW_IP/g" $OUT_JSON
}

function print()
{
    echo "[NERLNET-FULL-FLOW-TEST] $1"
}


CURRENT_MACHINE_IPV4_ADD="$(ip r | grep -m 1 -E -o "([0-9]{1,3}[\.]){3}[0-9]{1,3}" | head -n 1)"
print "This machine ipv4 is: $CURRENT_MACHINE_IPV4_ADD"

replace_ip_in_json $TEST_ARCH_JSON_NOIP_0 $TEST_ARCH_JSON_0 $CURRENT_MACHINE_IPV4_ADD

source $TESTS_PATH/set_env.sh
print "Loaded Virtual Environment: $VIRTUAL_ENV"

cd $NERLNET_PATH

print "Create backup of $NERLNET_CONFIG_JSONS_DIR" 
mv $NERLNET_CONFIG_JSONS_DIR $NERLNET_CONFIG_JSONS_DIR_BACKUP
echo $TEST_INPUT_JSONS_FILES_DIR > $NERLNET_CONFIG_JSONS_DIR

print "Execute NerlnetGetData.sh" 
./NerlnetGetData.sh
print "Execute Python - experiment_flow_test.py" 
python3 src_py/apiServer/experiment_flow_test.py


rm $NERLNET_CONFIG_JSONS_DIR
mv $NERLNET_CONFIG_JSONS_DIR_BACKUP $NERLNET_CONFIG_JSONS_DIR
print "Restore backup of $NERLNET_CONFIG_JSONS_DIR" 

deactivate

exit 0 #TODO change