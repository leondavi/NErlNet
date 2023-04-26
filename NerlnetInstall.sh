#!/bin/bash

# Global definitions
NERLNET_LIB_DIR="/usr/local/lib/nerlnet-lib"
NERLNET_DIR=$NERLNET_LIB_DIR/NErlNet
NERLNET_LOG_DIR="/usr/local/lib/nerlnet-lib/log"
NumJobs=4
InstallAll=false
REBAR3_FILE=src_erl/rebar3/rebar3
REBAR3_SYMLINK=/usr/local/bin/rebar3

if [[ "$RUNNING_IN_DOCKER" = true ]]; then
  # Not running inside a docker container
  LOGGED_IN_USER=$(logname)
else
  # Running inside a docker container
  LOGGED_IN_USER="$(whoami)" # Probably root
fi

# Arguments handling

SHORT_OPTIONS_LIST=i:,j:,h
LONG_OPTIONS_LIST=install:,jobs:,help

help()
{
    echo "-------------------------------------" && echo "Nerlnet Install" && echo "-------------------------------------"
    echo "Runnig this script only with sudo priviledges!"
    echo "Usage:"
    echo "-i or --install installs all required utilities to run Nerlnet "
    echo "-j or --jobs number of jobs to build of libraries (erlang and cmake)"
    exit 2
}


OPTS=$(getopt -a -n jupyterEnv --options $SHORT_OPTIONS_LIST --longoptions $LONG_OPTIONS_LIST -- "$@")
#echo $OPTS

eval set -- "$OPTS"

while :
do
  case "$1" in
    -i | --install )
      InstallAll=true
      gitOperations $Branch
      shift 2
      ;;
     -j | --jobs )
      NumJobs="$2"
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

# Script startds here

function print()
{
    echo "[NERLNET] $1"
}

function install_erlang()
{
    print "Compile and install erlang from source"
    apt update
    apt install -y make gcc libncurses-dev libssl-dev
    git clone https://github.com/erlang/otp.git
    cd otp
    git checkout maint-25
    ./configure
    make -j$NumJobs
    make install
    cd -
    rm -rf otp
}

function install_cmake()
{
   print "Compile and install CMake version 3.26.3 from source"
   apt update
   apt install -y make gcc g++
   wget https://github.com/Kitware/CMake/releases/download/v3.26.3/cmake-3.26.3.tar.gz
   tar -zxvf cmake-3.26.3.tar.gz
   cd cmake-3.26.3
   ./bootstrap
   make -j$NumJobs
   make install

   cd -
   rm -rf cmake-3.26.3
   rm cmake-3.26.3.tar.gz
}

function build_rebar3()
{
	print "Installing rebar3"
	cd src_erl/rebar3
	./bootstrap
	cd -
	chmod 755 $REBAR3_FILE
	print "Create symbolic link: $REBAR3_SYMLINK--> `pwd`/$REBAR3_FILE"

	if [ -L ${REBAR3_SYMLINK} ] ; then
   		rm $REBAR3_SYMLINK # remove symlink if exists
   		ln -s `pwd`/$REBAR3_FILE $REBAR3_SYMLINK
	elif [ -e ${REBAR3_SYMLINK} ] ; then
	   # Not a link
	   rm $REBAR3_SYMLINK # remove symlink if exists
	   ln -s `pwd`/$REBAR3_FILE $REBAR3_SYMLINK
	else
	   ln -s `pwd`/$REBAR3_FILE $REBAR3_SYMLINK
	fi
}

ARCH_TYPE=`uname -m`

print "Execute this script within NErlNet directory with super user privileges!"
sleep 5
echo "Following commands will be executed with super user privileges:"

if [ "$ARCH_TYPE" = "x86_64" ]; then
    print "Arch type: x86_64"
elif [ "$ARCH_TYPE" = "armv7l" ]; then
    print "Arch type: armv7l."
    NumJobs=1
fi

if [ "$InstallAll" = true ] ; then
    install_erlang
    install_cmake
fi

build_rebar3

print "Creating NErlNet-lib directory in $NERLNET_LIB_DIR"
mkdir -p $NERLNET_LIB_DIR
mkdir -p $NERLNET_LOG_DIR

print "Adding a sym-link to NErlNet directory"
if [ -L ${NERLNET_DIR} ] ; then
   rm $NERLNET_DIR # remove symlink if exists
   ln -s `pwd` $NERLNET_DIR
elif [ -e ${NERLNET_DIR} ] ; then
   # Not a link
   rm $NERLNET_DIR # remove symlink if exists
   ln -s `pwd` $NERLNET_DIR
else
   ln -s `pwd` $NERLNET_DIR
fi

print "$NERLNET_DIR symbolic link is linked to `pwd`"

echo -e "
[Unit]
After=network.service

[Service]
ExecStart=$NERLNET_DIR/NerlnetStartup.sh
User=$LOGGED_IN_USER

[Install]
WantedBy=default.target" > /etc/systemd/system/nerlnet.service

chmod 744 $NERLNET_DIR/NerlnetRun.sh
chmod 664 /etc/systemd/system/nerlnet.service

if [[ -z "$RUNNING_IN_DOCKER" ]]; then
  # Not running in docker
  chown -R $LOGGED_IN_USER $NERLNET_LOG_DIR
  chown -R $LOGGED_IN_USER $NERLNET_DIR
  chown -R $LOGGED_IN_USER $NERLNET_DIR/build
fi

echo "enable and start nerlnet.service"
systemctl enable nerlnet.service
#systemctl start nerlnet.service

