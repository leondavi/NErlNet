#!/bin/bash

NERLNET_LIB_DIR="/usr/local/lib/nerlnet-lib"
NERLNET_DIR=$NERLNET_LIB_DIR/NErlNet
NERLNET_PREFIX="[NERLNET_SCRIPT]"
NERLNET_BUILD_PREFIX="[Nerlnet Build] "
INPUT_DATA_DIR="inputDataDir"
NERLNET_WORKSPACE="$(pwd)"
TorchEnvFile="$NERLNET_WORKSPACE/build/torch_env.sh"

# arguments parsing 
# Thanks to https://github.com/matejak/argbash
Branch="master"
JobsNum=4
NerlWolf=OFF
NerlTorch=OFF
EnableDebugSymbols=OFF
InfraTarget="openn"
declare -a BuildTargets=()

help()
{
    echo "-------------------------------------" && echo "Nerlnet Build" && echo "-------------------------------------"
    echo "Usage:"
    echo "--p or --pull Warning! this uses checkout -f! and branch name checkout to branch $Branch and pull the latest"
    echo "--w or --wolf wolfram engine workers infra (nerlwolf)"
	echo "--i or --infra <openn|torch|all> select worker infrastructures (default: openn)"
	echo "--j or --jobs number of jobs to cmake build"
	echo "-g  enable compiler debug symbols (-g flag)"
    echo "--c or --clean remove build directory"
    exit 2
}

print()
{
	echo "$NERLNET_BUILD_PREFIX $1"
}

source_torch_environment()
{
	if [ ! -f "$TorchEnvFile" ]; then
		echo "$NERLNET_BUILD_PREFIX Torch environment file not found at $TorchEnvFile"
		echo "$NERLNET_BUILD_PREFIX Please run NerlnetInstall.sh --torch on this machine first"
		exit 1
	fi
	# shellcheck disable=SC1090
	source "$TorchEnvFile"
	if [ -z "$TORCH_ROOT" ]; then
		echo "$NERLNET_BUILD_PREFIX TORCH_ROOT is not defined in $TorchEnvFile"
		exit 1
	fi
	print "Loaded torch environment from $TorchEnvFile"
}

gitOperations()
{
    echo "$NERLNET_PREFIX Warning! git checkout -f is about to be executed"
    sleep 5
    echo "$NERLNET_PREFIX Interrupt is possible in the next 10 seconds"
    sleep 10
    git checkout -f $Branch
    git pull origin $Branch
    git submodule update --init --recursive
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
	local first_option all_short_options='hjpg'
	first_option="${1:0:1}"
	test "$all_short_options" = "${all_short_options/$first_option/}" && return 1 || return 0
}

# THE DEFAULTS INITIALIZATION - OPTIONALS
clean_build_directory()
{
        echo "Are you sure that you want to remove build directory?"
        sleep 1
        echo "Intterupt this process is possible with ctrl+c"
        echo "Remove build directory in 10 seconds"
        sleep 10
        rm -rf build   
}

print_help()
{
	printf 'Usage: %s [-h|--help] [-c|--clean] [-j|--jobs <arg>] [-p|--pull <arg>] [-i|--infra <arg>]\n' "$0"
	printf '\t%s\n' "-j, --jobs: number of jobs (default: '4')"
	printf '\t%s\n' "-p, --pull: pull from branch (default: '4')"
	printf '\t%s\n' "-w, --wolf: wolfram engine extension build (default: 'off')"
	printf '\t%s\n' "-i, --infra: worker infrastructures to build (openn|torch|all, default: 'openn')"
	printf '\t%s\n' "-g, --debug-build, --no-debug-build: add compiler -g debug symbols (default: 'off')"
	printf '\t%s\n' "-c, --clean: clean build directory (default: 'off')"

}


get_opennn_version_sha()
{
	cd $NERLNET_DIR/src_cpp/opennn
	echo "$NERLNET_BUILD_PREFIX OpenNN Commit: $(git rev-parse --verify HEAD)"
	cd -
}

configure_infrastructure_targets()
{
	local selection="${InfraTarget,,}"
	case "$selection" in
		openn)
			InfraTarget="openn"
			NerlTorch=OFF
			BuildTargets=("nerlnet_onn")
		;;
		torch)
			InfraTarget="torch"
			NerlTorch=ON
			BuildTargets=("nerlnet_torch")
		;;
		all)
			InfraTarget="all"
			NerlTorch=ON
			BuildTargets=("nerlnet_onn" "nerlnet_torch")
		;;
		*)
			die "Invalid infrastructure selection '$InfraTarget'. Use openn, torch, or all." 1
		;;
	esac
	print "Worker infrastructures selected: $InfraTarget"
}

parse_commandline()
{
	while test $# -gt 0
	do
		_key="$1"
		case "$_key" in
			-h|--help)
				help
				exit 0
				;;
			-h*)
				help
				exit 0
				;;
		        -c|--clean)
				clean_build_directory
				exit 0
				;;
			-c*)
				clean_build_directory
				exit 0
				;;
			-w|--wolf)
				test $# -lt 2 && die "Missing value for the optional argument '$_key'." 1
				NerlWolf="$2"
				shift
				;;
			--wolf=*)
				NerlWolf="${_key##--wolf=}"
				;;
			-w*)
				NerlWolf="${_key##-w}"
				;;
			-i|--infra)
				test $# -lt 2 && die "Missing value for the optional argument '$_key'." 1
				InfraTarget="$2"
				shift
				;;
			--infra=*)
				InfraTarget="${_key##--infra=}"
				;;
			-i*)
				InfraTarget="${_key##-i}"
				;;
			-g|--debug-build)
				EnableDebugSymbols=ON
				;;
			--no-debug-build)
				EnableDebugSymbols=OFF
				;;
			-g*)
				EnableDebugSymbols=ON
				_next="${_key##-g}"
				if test -n "$_next" -a "$_next" != "$_key"
				then
					{ begins_with_short_option "$_next" && shift && set -- "-g" "-${_next}" "$@"; } || die "The short option '$_key' can't be decomposed to ${_key:0:2} and -${_key:2}, because ${_key:0:2} doesn't accept value and '-${_key:2:1}' doesn't correspond to a short option."
				fi
				;;
			-j|--jobs)
				test $# -lt 2 && die "Missing value for the optional argument '$_key'." 1
				JobsNum="$2"
				shift
				;;
			--jobs=*)
				JobsNum="${_key##--jobs=}"
				;;
			-j*)
				JobsNum="${_key##-j}"
				;;
			-p|--pull)
				test $# -lt 2 && die "Missing value for the optional argument '$_key'." 1
				Branch="$2"
                                gitOperations
				shift
				;;
			--pull=*)
				Branch="${_key##--pull=}"
                                gitOperations
				;;
			-p*)
				Branch="${_key##-p}"
                                gitOperations
				;;
			*)
				_PRINT_HELP=yes die "FATAL ERROR: Got an unexpected argument '$1'" 1
				;;
		esac
		shift
	done
}

parse_commandline "$@"
# end of args parsing
configure_infrastructure_targets
# sourceNIF.erl requires libsource_nif.so even if we only build ONN or Torch workers.
BuildTargets+=("source_nif")
get_opennn_version_sha

OPTION="add_compile_definitions(EIGEN_MAX_ALIGN_BYTES=8)"
is_rasp="$(grep -c raspbian /etc/os-release)"
if [ $is_rasp -gt "0" ]; then 
        echo "$NERLNET_BUILD_PREFIX Detected raspberrypi => setting alignment to 8"
        sed -i "s/^.*#\(${OPTION}\)/\1/" CMakeLists.txt
else 
        echo "$NERLNET_BUILD_PREFIX Using default alignment"
        sed -i "s/^.*\(${OPTION}.*$\)/#\1/" CMakeLists.txt
fi

if [[ ! $NerlTorch =~ OFF ]]; then
	print "NerlTorch is enabled ($NerlTorch)"
	source_torch_environment
	if [ -n "$TORCH_ROOT" ]; then
		print "Torch root detected at $TORCH_ROOT"
	fi
fi

if command -v python3 >/dev/null 2>&1; then
    echo "$NERLNET_BUILD_PREFIX Python 3 is installed"
	# Generate auto-generated files
	set -e
	AUTOGENERATED_WORKER_DEFINITIONS_PATH="`pwd`/src_cpp/common/worker_definitions_ag.h"
	AUTOGENERATED_WORKER_DEFINITIONS_PATH_HRL="`pwd`/src_erl/NerlnetApp/src/worker_definitions_ag.hrl"
	AUTOGENERATED_DC_DEFINITIONS_PATH_HRL="`pwd`/src_erl/NerlnetApp/src/dc_definitions_ag.hrl"
	AUTOGENERATED_SOURCE_DEFINITIONS_PATH_HRL="`pwd`/src_erl/NerlnetApp/src/source_definitions_ag.hrl"
	AUTOGENERATED_ROUTER_DEFINITIONS_PATH_HRL="`pwd`/src_erl/NerlnetApp/src/router_definitions_ag.hrl"
	AUTOGENERATED_LAYERS_TYPE_INDEX_DEFINITIONS_PATH_HRL="`pwd`/src_erl/NerlnetApp/src/Bridge/layers_types_ag.hrl"
	AUTOGENERATED_MODELS_TYPES_INDEX_DEFINITIONS_PATH_HRL="`pwd`/src_erl/NerlnetApp/src/Bridge/models_types_ag.hrl"

	echo "$NERLNET_BUILD_PREFIX Generate auto-generated files"
	python3 src_py/nerlPlanner/CppHeadersExporter.py --output $AUTOGENERATED_WORKER_DEFINITIONS_PATH #--debug
	python3 src_py/nerlPlanner/ErlHeadersExporter.py --gen_worker_fields_hrl --output $AUTOGENERATED_WORKER_DEFINITIONS_PATH_HRL #--debug
	python3 src_py/nerlPlanner/ErlHeadersExporter.py --gen_dc_fields_hrl --output $AUTOGENERATED_DC_DEFINITIONS_PATH_HRL #--debug
	python3 src_py/nerlPlanner/ErlHeadersExporter.py --gen_source_fields_hrl --output $AUTOGENERATED_SOURCE_DEFINITIONS_PATH_HRL #--debug
	python3 src_py/nerlPlanner/ErlHeadersExporter.py --gen_router_fields_hrl --output $AUTOGENERATED_ROUTER_DEFINITIONS_PATH_HRL #--debug
	python3 src_py/nerlPlanner/ErlHeadersExporter.py --gen_layers_type_hrl 	 --output $AUTOGENERATED_LAYERS_TYPE_INDEX_DEFINITIONS_PATH_HRL #--debug
	python3 src_py/nerlPlanner/ErlHeadersExporter.py --gen_models_types_hrl 	 --output $AUTOGENERATED_MODELS_TYPES_INDEX_DEFINITIONS_PATH_HRL #--debug
	set +e
else
    echo "$NERLNET_BUILD_PREFIX Python 3 is not installed"
	echo "Autogenerated files will not be generated"
	echo "These files are based on last generated files brought from the repository"
fi

echo "$NERLNET_BUILD_PREFIX Building Nerlnet Library"
echo "$NERLNET_BUILD_PREFIX Cmake command of Nerlnet NIFPP"
set -e
cmake_cmd=(cmake -S . -B build/release -DNERLWOLF=$NerlWolf -DNERLTORCH=$NerlTorch -DCMAKE_BUILD_TYPE=RELEASE)
if [[ "$EnableDebugSymbols" != "OFF" ]]; then
	print "Compiler debug symbols enabled (-g)"
	debug_c_flags="-g"
	debug_cxx_flags="-g"
	if [ -n "$CMAKE_C_FLAGS" ]; then
		debug_c_flags="$CMAKE_C_FLAGS -g"
	fi
	if [ -n "$CMAKE_CXX_FLAGS" ]; then
		debug_cxx_flags="$CMAKE_CXX_FLAGS -g"
	fi
	cmake_cmd+=(-DCMAKE_C_FLAGS="$debug_c_flags")
	cmake_cmd+=(-DCMAKE_CXX_FLAGS="$debug_cxx_flags")
fi
if [[ ! $NerlTorch =~ OFF ]]; then
	if [ -n "$TORCH_ROOT" ]; then
		cmake_cmd+=(-DTORCH_ROOT="$TORCH_ROOT")
	fi
	if [ -n "$TORCH_CMAKE_PREFIX" ]; then
		cmake_cmd+=(-DTORCH_CMAKE_PREFIX="$TORCH_CMAKE_PREFIX")
		cmake_cmd+=(-DTorch_DIR="$TORCH_CMAKE_PREFIX")
	fi
	if [ -n "$TORCH_CXX_FLAGS" ]; then
		cmake_cmd+=(-DTORCH_CXX_FLAGS="$TORCH_CXX_FLAGS")
	fi
fi
"${cmake_cmd[@]}"
cd build/release
echo "$NERLNET_BUILD_PREFIX Script CWD: $PWD"
echo "$NERLNET_BUILD_PREFIX Build Nerlnet"
echo "Jobs Number: $JobsNum"
if [ ${#BuildTargets[@]} -gt 0 ]; then
	echo "$NERLNET_BUILD_PREFIX Targets: ${BuildTargets[*]}"
	cmake --build . --target "${BuildTargets[@]}" -- -j$JobsNum
else
	cmake --build . -- -j$JobsNum
fi
cd ../../
echo "$NERLNET_BUILD_PREFIX Script CWD: $PWD"
set +e

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
