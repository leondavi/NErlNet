#!/bin/bash

NERLNET_VENV_PATH="/tmp/nerlnet/venv"

function print()
{
    echo "[NERLNET-TEST][SET-ENV] $1"
}
# set python environment to run Nerlnet Flow
print("Starts")

python3 -m $NERLNET_VENV_PATH

cat src_py/requirements.txt

source $NERLNET_VENV_PATH/bin/activate
pip3 install -r src_py/requirements.txt