#!/bin/bash

# Run this script with sudo priviledges 

NERLNET_LIB_DIR="/usr/local/lib/nerlnet-lib"
NERLNET_DIR=$NERLNET_LIB_DIR/NErlNet
NumJobs=4

function print()
{
    echo "[NERLNET] $1"
}

ARCH_TYPE=`uname -m`

print "Execute this script within NErlNet directory with super user privilidges!"
sleep 5
echo "Following commands will be executed with super user privilidges:"
echo "installing Erlang and cmake"
wget https://packages.erlang-solutions.com/erlang-solutions_2.0_all.deb
sudo dpkg -i erlang-solutions_2.0_all.deb
apt-get -y update
apt-get -y install erlang cmake


if [ "$ARCH_TYPE" = "x86_64" ]; then
    print "Arch type: x86_64"
elif [ "$ARCH_TYPE" = "armv7l" ]; then
    print "Arch type: armv7l."
    NumJobs=1
fi

print "Creating NErlNet-lib directory in $NERLNET_LIB_DIR"
mkdir -p $NERLNET_LIB_DIR

print "Adding a sym-link to NErlNet directory"
ln -s `pwd` $NERLNET_DIR

/bin/bash NerlnetBuild.sh --j $NumJobs
ln -s $NERLNET_DIR/src_erl/rebar3/rebar3 /usr/local/bin/rebar3
/bin/bash NerlnetBuild.sh --j $NumJobs


echo -e "
[Unit]
After=network.service

[Service]
ExecStart=$NERLNET_DIR/NerlnetRun.sh
User=$(logname)

[Install]
WantedBy=default.target" > /etc/systemd/system/nerlnet.service

sudo chmod 744 $NERLNET_DIR/NerlnetRun.sh
sudo chmod 664 /etc/systemd/system/nerlnet.service