#!/bin/bash

NERLNET_PREFIX="[NERLNET_SCRIPT]"
INPUT_DATA_DIR="inputDataDir"
SHORT_OPTIONS_LIST=p:,j:,c:,h
LONG_OPTIONS_LIST=pull:,jobs:,clean:,help

Branch="master"
JobsNum=4

help()
{
    echo "-------------------------------------" && echo "Nerlnet Build" && echo "-------------------------------------"
    echo "Usage:"
    echo "--p or --pull Warning! this uses checkout -f! and branch name checkout to branch $Branch and pull the latest"
    echo "--j or --jobs number of jobs to cmake build"
    echo "--c or --clean with any value remove build directory"
    exit 2
}

gitOperations()
{
    echo "$NERLNET_PREFIX Warning! git checkout -f is about to be executed"
    sleep 5
    echo "$NERLNET_PREFIX Interrupt is possible in the next 10 seconds"
    sleep 10
    git checkout -f $1
    git pull origin $1
    git submodule update --init --recursive
}

OPTS=$(getopt -a -n jupyterEnv --options $SHORT_OPTIONS_LIST --longoptions $LONG_OPTIONS_LIST -- "$@")
#echo $OPTS

eval set -- "$OPTS"

while :
do
  case "$1" in
    -p | --pull )
      Branch="$2"
      gitOperations $Branch
      shift 2
      ;;
     -j | --jobs )
      JobsNum="$2"
      shift 2
      ;;
      -c | --clean )
      echo "Are you sure that you want to remove build directory?"
      sleep 1
      echo "Intterupt this process is possible with ctrl+c"
      echo "Remove build directory in 10 seconds"
      sleep 10
      rm -rf build
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

NERLNET_BUILD_PREFIX="[Nerlnet Build] "

echo "$NERLNET_BUILD_PREFIX Building Nerlnet Library"
echo "$NERLNET_BUILD_PREFIX Cmake command of Nerlnet NIFPP"
cmake -S . -B build/release -DCMAKE_BUILD_TYPE=RELEASE
cd build/release
echo "$NERLNET_BUILD_PREFIX Script CWD: $PWD"
echo "$NERLNET_BUILD_PREFIX Build Nerlnet"
echo "Jobs Number: $JobsNum"
make -j$JobsNum 
cd ../../
echo "$NERLNET_BUILD_PREFIX Script CWD: $PWD"

REBAR3_FILE=src_erl/rebar3/rebar3
REBAR3_SYMLINK=/usr/local/bin/rebar3

if [ -f "$REBAR3_FILE" ]; then
	echo "$NERLNET_BUILD_PREFIX rebar3 is installed, location: $REBAR3_FILE"
else 
	echo "$NERLNET_BUILD_PREFIX rebar3 Builder Start"
	cd src_erl/rebar3
	./bootstrap
	cd ../../	
	echo "$NERLNET_BUILD_PREFIX rebar3 is Built at $REBAR3_FILE"
fi

if [ -f "$REBAR3_SYMLINK" ]; then
        echo "$NERLNET_BUILD_PREFIX rebar3 Synlink exists in /usr/local/bin"
else
        echo "$NERLNET_BUILD_PREFIX $(tput setaf 1) Please run the following command from Nerlnet library root folder (or install rebar3 to usr/local/bin): $(tput sgr 0)"
        echo "$NERLNET_BUILD_PREFIX $(tput setaf 1) sudo ln -s `pwd`/src_erl/rebar3/rebar3 /usr/local/bin/rebar3 $(tput sgr 0)"
        echo "$NERLNET_BUILD_PREFIX "
fi

if [ -d "$INPUT_DATA_DIR" ]; then
        echo "$NERLNET_BUILD_PREFIX Input data directory of nerlnet is: $INPUT_DATA_DIR"
else
        echo "$NERLNET_BUILD_PREFIX Generating $INPUT_DATA_DIR"
        mkdir $INPUT_DATA_DIR
        echo "$NERLNET_BUILD_PREFIX Add input data to $INPUT_DATA_DIR"
fi
