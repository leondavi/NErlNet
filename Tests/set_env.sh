#!/bin/bash

NERLNET_VENV_PATH="/tmp/nerlnet/virtualenv"

function print()
{
    echo "[NERLNET-TEST][SET-ENV] $1"
}
# set python environment to run Nerlnet Flow
print "Starts"

pip3 install virtualenv
python3 -m virtualenv $NERLNET_VENV_PATH

cat src_py/requirements.txt

source $NERLNET_VENV_PATH/bin/activate
pip3 install -r src_py/requirements.txt