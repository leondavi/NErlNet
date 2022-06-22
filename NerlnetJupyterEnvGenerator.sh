#!/bin/bash

SHORT_OPTIONS_LIST=j:,h
LONG_OPTIONS_LIST=jdir:,help

jdir="JupyterLabDir"

help()
{
    echo "-------------------------------------" && echo "Jupyter Environment Generator Script" && echo "-------------------------------------"
    echo "Usage:"
    echo "--j or --jdir followed by Jupyter intendended workspace directory (Default directory is $jdir)"
    exit 2
}

OPTS=$(getopt -a -n jupyterEnv --options $SHORT_OPTIONS_LIST --longoptions $LONG_OPTIONS_LIST -- "$@")
#echo $OPTS

eval set -- "$OPTS"

while :
do
  case "$1" in
    -j | --jdir )
      jdir="$2"
      shift 2
      ;;
    -h | --help)
      help
      ;;
    --)
      shift;
      break
      ;;
    *)
      echo "Unexpected option: $1"
      help
      ;;
  esac
done

echo "Jupyter Project Environment chosen directory: $jdir"
echo "Generating path to $jdir"
mkdir -p $jdir

echo "generate symbloic link: src_py/apiServer.py --> $jdir/apiServer.py"
ln -s `pwd`/src_py/apiServer/apiServer.py $jdir/apiServer.py
echo "generate symbloic link: src_py/globalVars.py --> $jdir/globalVars.py"
ln -s `pwd`/src_py/apiServer/globalVars.py $jdir/globalVars.py
echo "generate symbloic link: src_py/networkComponents.py --> $jdir/networkComponents.py"
ln -s `pwd`/src_py/apiServer/networkComponents.py $jdir/networkComponents.py
echo "generate symbloic link: src_py/receiver.py --> $jdir/receiver.py"
ln -s `pwd`/src_py/apiServer/receiver.py $jdir/receiver.py
echo "generate symbloic link: src_py/transmitter.py --> $jdir/transmitter.py"
ln -s `pwd`/src_py/apiServer/transmitter.py $jdir/transmitter.py
#echo "generate symbloic link: jsonPath --> $jdir/jsonPath"
#ln -s `pwd`/jsonPath $jdir/jsonPath
echo "generate symbloic link: src_py/experiment.py --> $jdir/experiment.py"
ln -s `pwd`/src_py/apiServer/experiment.py $jdir/experiment.py
echo "generate symbloic link: src_py/csvResult.py --> $jdir/csvResult.py"
ln -s `pwd`/src_py/apiServer/csvResult.py $jdir/csvResult.py
echo "generate symbloic link: src_py/workerResult.py --> $jdir/workerResult.py"
ln -s `pwd`/src_py/apiServer/workerResult.py $jdir/workerResult.py
echo "generate symbloic link: src_py/predictBatch.py --> $jdir/predictBatch.py"
ln -s `pwd`/src_py/apiServer/predictBatch.py $jdir/predictBatch.py
echo "generate symbloic link: inputDataDir --> $jdir/inputDataDir"
ln -s `pwd`/inputDataDir $jdir/inputDataDir
echo "generate symbloic link: NErlNet Directory --> $jdir/NErlNet"
ln -s `pwd` $jdir/NErlNet

# to be Deprecated
echo "generate symbloic link: inputJsonFiles --> $jdir/inputJsonFiles"
ln -s `pwd`/inputJsonFiles $jdir/inputJsonFiles



