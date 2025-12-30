#!/bin/bash

# Global definitions
NERLNET_LIB_DIR="/usr/local/lib/nerlnet-lib"
NERLNET_DIR=$NERLNET_LIB_DIR/NErlNet
NERLNET_LOG_DIR="/usr/local/lib/nerlnet-lib/log"
REBAR3_FILE=src_erl/rebar3/rebar3
REBAR3_SYMLINK=/usr/local/bin/rebar3

if [[ -z "$RUNNING_IN_DOCKER" ]]; then
  # Not running inside a docker container
  LOGGED_IN_USER=$(logname)
else
  # Running inside a docker container
  LOGGED_IN_USER="$(whoami)" # Probably root
fi

# arguments parsing 
# Implemented by https://github.com/matejak/argbash

# args defaults
InstallAll=false
InstallErlang=false
InstallCmake=false
InstallTorch=false
NumJobs=4

TorchVersion="${NERLNET_TORCH_VERSION:-2.1.2}"
TorchVariant="${NERLNET_TORCH_VARIANT:-cpu}"
TorchArchive="${NERLNET_TORCH_ARCHIVE:-libtorch-cxx11-abi-shared-with-deps-${TorchVersion}+${TorchVariant}.zip}"
if [[ -n "$NERLNET_TORCH_URL" ]]; then
	TorchUrl="$NERLNET_TORCH_URL"
else
	TorchArchiveEncoded="${TorchArchive//+/%2B}"
	TorchUrl="https://download.pytorch.org/libtorch/${TorchVariant}/${TorchArchiveEncoded}"
fi
TorchAbiFlag="${NERLNET_TORCH_ABI_FLAG:--D_GLIBCXX_USE_CXX11_ABI=1}"
TORCH_INSTALL_ROOT="$NERLNET_LIB_DIR/libtorch"

help()
{
    echo "-------------------------------------" && echo "Nerlnet Install" && echo "-------------------------------------"
	echo "Run this script only with sudo privileges!"
	echo "Usage:"
	echo "-a or --all installs Erlang and CMake from source (asks for confirmation)"
	echo "-e or --erlang installs Erlang from source"
	echo "-c or --cmake installs CMake from source"
	echo "-t or --torch downloads libtorch and prepares build/torch_env.sh"
	echo "-j or --jobs number of jobs to build of libraries (erlang and cmake)"
    exit 2
}

die()
{
	local _ret="${2:-1}"
	test "${_PRINT_HELP:-no}" = yes && print_help >&2
	echo "$1" >&2
	exit "${_ret}"
}


begins_with_short_option()
{
	local first_option all_short_options='aectjh'
	first_option="${1:0:1}"
	test "$all_short_options" = "${all_short_options/$first_option/}" && return 1 || return 0
}

# THE DEFAULTS INITIALIZATION - OPTIONALS

print_help()
{
	printf 'Usage: %s [-a|--all] [-e|--erlang] [-c|--cmake] [-t|--torch] [-h|--help] [-j|--jobs <arg>]\n' "$0"
	printf '\t%s\n' "-a, --all: install Erlang and CMake from source (will prompt for confirmation)"
	printf '\t%s\n' "-e, --erlang: install Erlang from source"
	printf '\t%s\n' "-c, --cmake: install CMake from source"
	printf '\t%s\n' "-t, --torch: download libtorch and emit build/torch_env.sh"
	printf '\t%s\n' "-j, --jobs: number of jobs (default: '4')"
}


parse_commandline()
{
	while test $# -gt 0
	do
		_key="$1"
		case "$_key" in
			-a|--all)
				InstallAll=true
				;;
			-a*)
				InstallAll=true
				;;
			-e|--erlang)
				InstallErlang=true
				;;
			-e*)
				InstallErlang=true
				;;
			-c|--cmake)
				InstallCmake=true
				;;
			-c*)
				InstallCmake=true
				;;
			-t|--torch)
				InstallTorch=true
				;;
			-t*)
				InstallTorch=true
				;;
			-h|--help)
				help
				exit 0
				;;
			-h*)
				help
				exit 0
				;;
			-j|--jobs)
				test $# -lt 2 && die "Missing value for the optional argument '$_key'." 1
				NumJobs="$2"
				shift
				;;
			--jobs=*)
				NumJobs="${_key##--jobs=}"
				;;
			-j*)
				NumJobs="${_key##-j}"
				;;
			*)
				_PRINT_HELP=yes die "FATAL ERROR: Got an unexpected argument '$1'" 1
				;;
		esac
		shift
	done
}

parse_commandline "$@"

if [[ $EUID -ne 0 ]]; then
	echo "This installer requires sudo privileges. Please run: sudo $0 $*"
	exit 1
fi

if [ "$InstallAll" = true ]; then
	read -r -p "Install Erlang and CMake from source? (y/N): " _confirm_all
	case "$_confirm_all" in
		[Yy]|[Yy][Ee][Ss])
			InstallErlang=true
			InstallCmake=true
			;;
		*)
			echo "[NERLNET] Skipping Erlang and CMake installation (--all not confirmed)."
			;;
	esac
fi

# Script startds here

function print()
{
    echo "[NERLNET] $1"
}

function write_torch_env_script()
{
	local torch_root="$1"
	local archive_sha="$2"
	local env_dir="$NERLNET_DIR/build"
	local env_file="$env_dir/torch_env.sh"
	mkdir -p "$env_dir"
	cat <<EOF > "$env_file"
#!/bin/bash

export TORCH_VERSION="$TorchVersion"
export TORCH_VARIANT="$TorchVariant"
export TORCH_ARCHIVE_NAME="$TorchArchive"
export TORCH_ARCHIVE_SHA256="$archive_sha"
export TORCH_URL="$TorchUrl"
export TORCH_ROOT="$torch_root"
export TORCH_INCLUDE_MAIN="${torch_root}/include"
export TORCH_INCLUDE_API="${torch_root}/include/torch/csrc/api/include"
export TORCH_INCLUDE="${torch_root}/include:${torch_root}/include/torch/csrc/api/include"
export TORCH_LIB_DIR="${torch_root}/lib"
export TORCH_CMAKE_PREFIX="${torch_root}/share/cmake/Torch"
export TORCH_CXX_FLAGS="$TorchAbiFlag"

case ":\${LD_LIBRARY_PATH:-}:" in
	*":${torch_root}/lib:"*) ;;
	*) export LD_LIBRARY_PATH="${torch_root}/lib:\${LD_LIBRARY_PATH:-}";;
esac

case ":\${CMAKE_PREFIX_PATH:-}:" in
	*":${torch_root}/share/cmake/Torch:"*) ;;
	*) export CMAKE_PREFIX_PATH="${torch_root}/share/cmake/Torch:\${CMAKE_PREFIX_PATH:-}";;
esac
EOF
	chmod +x "$env_file"
	print "Torch environment exported to $env_file"
}

function install_libtorch()
{
	local torch_target_dir="$TORCH_INSTALL_ROOT/${TorchVersion}-${TorchVariant}"
	print "Installing libtorch (${TorchVersion}, ${TorchVariant}) into $torch_target_dir"
	apt update
	apt install -y wget unzip
	local tmp_dir
	tmp_dir=$(mktemp -d)
	local archive_path="$tmp_dir/$TorchArchive"
	if ! wget -O "$archive_path" "$TorchUrl"; then
		rm -rf "$tmp_dir"
		die "Failed to download libtorch from $TorchUrl"
	fi
	local archive_sha
	archive_sha=$(sha256sum "$archive_path" | awk '{print $1}')
	unzip -q "$archive_path" -d "$tmp_dir"
	if [ ! -d "$tmp_dir/libtorch" ]; then
		rm -rf "$tmp_dir"
		die "Downloaded archive missing libtorch directory"
	fi
	rm -rf "$torch_target_dir"
	mkdir -p "$TORCH_INSTALL_ROOT"
	mv "$tmp_dir/libtorch" "$torch_target_dir"
	rm -rf "$tmp_dir"
	write_torch_env_script "$torch_target_dir" "$archive_sha"
	print "libtorch installed successfully. Source $NERLNET_DIR/build/torch_env.sh before building."
}

function install_erlang()
{
  print "Warning - Erlang otp is about to be installed from source"
  print "Warning - please make sure that former erlang/OTP versions are purged"
  sleep 5
  print "Compile and install erlang from source"
  apt update
  apt install -y make gcc libncurses-dev libssl-dev
  git clone https://github.com/erlang/otp.git
  cd otp
  git fetch --all --tags
  git checkout tags/OTP-28.0 -b otp-28.0
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

if [ "$InstallErlang" = true ] ; then
	install_erlang
fi

if [ "$InstallCmake" = true ] ; then
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

if [ "$InstallTorch" = true ] ; then
	install_libtorch
fi

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
	# Always ensure the active workspace (especially build artifacts) belongs to the invoking user
	chown -R $LOGGED_IN_USER $NERLNET_DIR/build 2>/dev/null || true
	chown -R $LOGGED_IN_USER $NERLNET_LOG_DIR
	chown -R $LOGGED_IN_USER $NERLNET_DIR
fi

echo "You can enable and start nerlnet.service using the command: systemctl enable nerlnet.service"

