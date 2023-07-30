#!/bin/bash

print()
{
    echo "[NERLPLANNER] $1"
}

NERLNET_DIR="/usr/local/lib/nerlnet-lib/NErlNet"
NERLPLANNER_DIR=$NERLNET_DIR/src_py/nerlPlanner
NERLPLANNER_MAIN_PY=$NERLPLANNER_DIR/main.py


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

python3 $NERLPLANNER_MAIN_PY