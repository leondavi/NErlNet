#!/bin/bash

export NERLNET_PATH="/usr/local/lib/nerlnet-lib/NErlNet"
export TESTS_PATH="$NERLNET_PATH/tests"
export NERLNET_RUNNING_TIMEOUT_SEC="30"

NERLNET_CONFIG_DIR=$NERLNET_PATH/config
NERLNET_CONFIG_JSONS_DIR=$NERLNET_CONFIG_DIR/jsonsDir.nerlconfig
NERLNET_CONFIG_JSONS_DIR_BACKUP=$NERLNET_CONFIG_DIR/jsonsDir.nerlconfig.bac
NERLNET_CONFIG_SUBNETS_DIR=$NERLNET_CONFIG_DIR/subnets.nerlconfig
NERLNET_CONFIG_SUBNETS_BACKUP=$NERLNET_CONFIG_DIR/subnets.nerlconfig.bac

TEST_INPUT_JSONS_FILES_DIR="$TESTS_PATH/inputJsonsFiles"
export TEST_BASELINE_DIR="$TEST_INPUT_JSONS_FILES_DIR/baseline"
export TEST_BASELINE_MODEL_FILENAME="model_perf_synt_1d_2c_4r_4w.csv"
export TEST_BASELINE_MODEL_STATS="$TEST_BASELINE_DIR/$TEST_BASELINE_MODEL_FILENAME"
export TEST_BASELINE_LOSS_MIN_FILENAME="min_loss_dict_synt_1d_2c_4r_4w.json"
export TEST_BASELINE_LOSS_MIN="$TEST_BASELINE_DIR/$TEST_BASELINE_LOSS_MIN_FILENAME"

TEST_DC_JSON_NOIP=$TEST_INPUT_JSONS_FILES_DIR/dc_test_synt_1d_2c_1s_4r_4w.json.noip
TEST_DC_JSON=$TEST_INPUT_JSONS_FILES_DIR/dc_test_synt_1d_2c_1s_4r_4w.json

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

# add this host ip to subnets and backup subnets.nerlconfig
cp $NERLNET_CONFIG_SUBNETS_DIR $NERLNET_CONFIG_SUBNETS_BACKUP
CURRENT_MACHINE_IPV4_ADD="$(ip addr | grep -m 2 -E -o "([0-9]{1,3}[\.]){3}[0-9]{1,3}" | head -n 2 | grep -v "127.0.0.1")"
print "This machine ipv4 is: $CURRENT_MACHINE_IPV4_ADD"
sed -i '$a\' $NERLNET_CONFIG_SUBNETS_DIR
echo "$CURRENT_MACHINE_IPV4_ADD" >> $NERLNET_CONFIG_SUBNETS_DIR
replace_ip_in_json $TEST_DC_JSON_NOIP $TEST_DC_JSON $CURRENT_MACHINE_IPV4_ADD

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
rc=$(echo $?)

rm $NERLNET_CONFIG_JSONS_DIR
print "Restore backup of $NERLNET_CONFIG_JSONS_DIR"
cp $NERLNET_CONFIG_JSONS_DIR_BACKUP $NERLNET_CONFIG_JSONS_DIR
rm $NERLNET_CONFIG_JSONS_DIR_BACKUP
print "Restore backup of $NERLNET_CONFIG_SUBNETS_DIR"
cp $NERLNET_CONFIG_SUBNETS_BACKUP $NERLNET_CONFIG_SUBNETS_DIR
rm $NERLNET_CONFIG_SUBNETS_BACKUP
rm $TEST_DC_JSON

deactivate

exit $rc