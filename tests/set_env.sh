#!/bin/bash

NERLNET_VENV_PATH="/tmp/nerlnet/virtualenv"

REQUIRED_PACKAGES="python3-pip virtualenv unzip"

function print()
{
    echo "[NERLNET-TEST][SET-ENV] $1"
}

function print_required_packages_install()
{
    print "Please install required packages:"
    print "sudo apt update"
    print "sudo apt install $REQUIRED_PACKAGES"
}

function pip3_installation()
{
if ! command -v pip3 &> /dev/null
then
    print "pip3 could not be found"
    print_required_packages_install
    exit 1
else
    print "pip3 is already installed."
fi
}

function virtualenv_installation()
{
pip3 install virtualenv
if ! command -v virtualenv &> /dev/null
then
    print "virtualenv could not be found"
    print_required_packages_install
    exit 1
else
    print "virtualenv is already installed."
fi
}

function unzip_installation()
{
if ! command -v unzip &> /dev/null
then
    print "unzip could not be found"
    print_required_packages_install
    exit 1
else
    print "unzip is already installed."
fi
}

pip3_installation
virtualenv_installation # must be installed after pip3
unzip_installation
# set python environment to run Nerlnet Flow
print "install virtualenv to $NERLNET_VENV_PATH"
python3 -m virtualenv $NERLNET_VENV_PATH
print "virtualenv is loaded from $NERLNET_VENV_PATH/bin/activate"
source $NERLNET_VENV_PATH/bin/activate

print "pip3 runs in quiet mode"
pip3 -q install -r src_py/requirements.txt
