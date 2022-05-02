#!/bin/bash

# Run this script with sudo priviledges 

NERLNET_LIB_DIR="/usr/local/lib/nerlnet-lib"

function print()
{
    echo "[NERLNET] $1"
}

ARCH_TYPE=`uname -m`

echo "installing Erlang and cmake"
echo "Following commands should be executed with super user privilidges:"

wget https://packages.erlang-solutions.com/erlang-solutions_2.0_all.deb
sudo dpkg -i erlang-solutions_2.0_all.deb
apt-get -y update
apt-get -y install erlang cmake


if [ "$ARCH_TYPE" = "x86_64" ]; then
    print "Arch type: x86_64"
elif [ "$ARCH_TYPE" = "armv7l" ]; then
    print "Arch type: armv7l."
fi

print "Creating NErlNet-lib directory in $NERLNET_LIB_DIR"
mkdir -p $NERLNET_LIB_DIR

print "Adding a sym-link to NErlNet directory"
ln -s `pwd` $NERLNET_LIB_DIR/NErlNet
