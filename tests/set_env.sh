#!/bin/bash

NERLNET_VENV_PATH="/tmp/nerlnet/virtualenv"
REQUIRED_PACKAGES="python3-pip virtualenv unzip"
TORCH_PIP_SPEC_DEFAULT="torch==2.1.2+cpu"
TORCH_INDEX_URL_DEFAULT="https://download.pytorch.org/whl/cpu"

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO_ROOT="$(cd "$SCRIPT_DIR/.." && pwd)"
cd "$REPO_ROOT" || exit 1

INSTALL_TORCH=false
TORCH_PIP_SPEC="$TORCH_PIP_SPEC_DEFAULT"
TORCH_INDEX_URL="$TORCH_INDEX_URL_DEFAULT"

function print()
{
    echo "[NERLNET-TEST][SET-ENV] $1"
}

function usage()
{
    cat <<EOF
Usage: source tests/set_env.sh [OPTIONS]

Options:
  --torch                Install PyTorch inside the virtualenv.
  --torch-version VER    Override torch version (implies torch==VER).
  --torch-spec SPEC      Override full pip spec (e.g. torch==2.1.2+cpu).
  --torch-index-url URL  Custom pip index for torch wheels.
  -h, --help             Show this help message.
EOF
}

while [[ $# -gt 0 ]]; do
    case "$1" in
        --torch)
            INSTALL_TORCH=true
            ;;
        --torch-version)
            if [[ -z "${2:-}" ]]; then
                print "Missing value for --torch-version"
                usage
                exit 1
            fi
            TORCH_PIP_SPEC="torch==${2}"
            shift
            ;;
        --torch-spec)
            if [[ -z "${2:-}" ]]; then
                print "Missing value for --torch-spec"
                usage
                exit 1
            fi
            TORCH_PIP_SPEC="$2"
            shift
            ;;
        --torch-index-url)
            if [[ -z "${2:-}" ]]; then
                print "Missing value for --torch-index-url"
                usage
                exit 1
            fi
            TORCH_INDEX_URL="$2"
            shift
            ;;
        -h|--help)
            usage
            return 0 2>/dev/null || exit 0
            ;;
        *)
            print "Unknown argument: $1"
            usage
            exit 1
            ;;
    esac
    shift
done

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
if ! command -v virtualenv &> /dev/null
then
    print "virtualenv could not be found"
    print "consider using apt to install it: sudo apt install python3-venv"
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

if [[ -z "$RUNNING_IN_DOCKER" ]]; then
    pip3_installation
    virtualenv_installation # must be installed after pip3
    unzip_installation
    if [[ -d "$NERLNET_VENV_PATH" ]]; then
        print "virtualenv already exists at $NERLNET_VENV_PATH"
    else
        print "install virtualenv to $NERLNET_VENV_PATH"
        python3 -m virtualenv "$NERLNET_VENV_PATH"
    fi
    print "virtualenv is loaded from $NERLNET_VENV_PATH/bin/activate"
    # shellcheck source=/tmp/nerlnet/virtualenv/bin/activate
    source "$NERLNET_VENV_PATH/bin/activate"
else
    print "Skip venv installation when running in docker"
fi

print "pip3 runs in quiet mode"
pip3 -q install -r "$REPO_ROOT/src_py/requirements.txt"

if [[ "$INSTALL_TORCH" == true ]]; then
    print "Installing PyTorch package ($TORCH_PIP_SPEC)"
    INSTALL_CMD=(pip3 -q install "$TORCH_PIP_SPEC")
    if [[ -n "$TORCH_INDEX_URL" ]]; then
        INSTALL_CMD+=(--index-url "$TORCH_INDEX_URL")
    fi
    "${INSTALL_CMD[@]}"
fi
