#!/bin/bash

print()
{
    echo "[NERLPLANNER] $1"
}

NERLNET_DIR="/usr/local/lib/nerlnet-lib/NErlNet"
NERLPLANNER_DIR=$NERLNET_DIR/src_py/nerlPlanner
NERLPLANNER_MAIN_PY=$NERLPLANNER_DIR/main.py
NERLPLANNER_VENV_PATH="/tmp/nerlnet/nerlplanner/python/virtualenv"


# install and validate prerequisites

DOT_PACKAGE="python3-pydot"
DOT_EXIST=$(dpkg-query -W --showformat='${Status}\n' $DOT_PACKAGE | grep "install ok installed")
print "Checking for $DOT_PACKAGE"
if [ "" = "$DOT_EXIST" ]; then
  print "Please run: sudo apt-get install $DOT_PACKAGE"
else
  print "installed"
fi


GRAPHVIZ_PACKAGE="graphviz"
GRAPHVIZ_EXIST=$(dpkg-query -W --showformat='${Status}\n' $GRAPHVIZ_PACKAGE|grep "install ok installed")
print "Checking for $GRAPHVIZ_PACKAGE"
if [ "" = "$GRAPHVIZ_EXIST" ]; then
  print "Please run: sudo apt-get install $GRAPHVIZ_PACKAGE"
else
  print "installed"
fi

# set python environment to run Nerlnet Flow
print "install virtualenv to $NERLPLANNER_VENV_PATH"
pip3 install virtualenv > $NERLPLANNER_VENV_PATH/set_env.log
python3 -m virtualenv $NERLPLANNER_VENV_PATH
print "virtualenv is loaded from $NERLPLANNER_VENV_PATH/bin/activate"
source $NERLPLANNER_VENV_PATH/bin/activate

print "pip3 runs in quiet mode"
pip3 -q install -r src_py/nerlPlanner/requirements.txt

python3 $NERLPLANNER_MAIN_PY