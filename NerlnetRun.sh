#!/bin/bash
NERLNET_LIB_DIR="/usr/local/lib/nerlnet-lib"
NERLNET_DIR=$NERLNET_LIB_DIR/NErlNet
NERLNET_PREFIX="[NERLNET_RUN]"
INPUT_DATA_DIR="inputDataDir"
BUILD_DIRECTORY="build/release"
buildNerlnetLibrary=0
TMP_DIR_RUN=/tmp/nerlnet/run
REMOTE_JSONS_DIR=/tmp/nerlnet/jsons
TORCH_MODELS_DIR=/tmp/nerlnet/torch/models/pt
NERLNET_APP_BUILD_DIR=build/rebar/default/lib
NERLNET_APP_DIR=src_erl/NerlnetApp
NERLNET_APP_RELEASE_BIN=$NERLNET_DIR/build/rebar/nerlnetApp/bin

function print()
{
    echo "[NERLNET-RUN] $1"
}

function init()
{
    if [ -d "$INPUT_DATA_DIR" ]; then
            print "$NERLNET_PREFIX Input data directory of nerlnet is: $INPUT_DATA_DIR"
    else
            print "$NERLNET_PREFIX $INPUT_DATA_DIR is generated and is empty!"
            mkdir $INPUT_DATA_DIR
            print "$NERLNET_PREFIX Add input data to $INPUT_DATA_DIR"
    fi
    #--------------------------------------------#

    if [ -d "$BUILD_DIRECTORY" ]; then
        print "$NERLNET_PREFIX Build Directory exists"
    else
        print "$NERLNET_PREFIX Build Directory is missing - first time library build is on"
        buildNerlnetLibrary=1
    fi

    if [ -d "$TMP_DIR_RUN" ]; then
        print "$NERLNET_PREFIX Delete temp directory content"
        cd $TMP_DIR_RUN
        rm -rf *
        cd -
    else
        print "$NERLNET_PREFIX Create temp directory"
        mkdir -p $TMP_DIR_RUN
    fi

    if [ -d "$REMOTE_JSONS_DIR" ]; then
        print "$NERLNET_PREFIX Delete $REMOTE_JSONS_DIR directory content"
        cd $REMOTE_JSONS_DIR
        rm -rf *
        cd -
    else
        print "$NERLNET_PREFIX Create $REMOTE_JSONS_DIR directory"
        mkdir -p $REMOTE_JSONS_DIR
    fi

	if [ -d "$TORCH_MODELS_DIR" ]; then
		print "$NERLNET_PREFIX Delete $TORCH_MODELS_DIR directory content"
		rm -rf "$TORCH_MODELS_DIR"
	fi
	mkdir -p "$TORCH_MODELS_DIR"

    # only for raspberry
    is_rasp="$(grep -c raspbian /etc/os-release)"
    if [ $is_rasp -gt "0" ]; then 
        export LD_PRELOAD=/usr/lib/arm-linux-gnueabihf/libatomic.so.1.2.0 
    fi

	pkill beam.smp
}

function status()
{
    cd src_erl/NerlnetApp
    $NERLNET_APP_RELEASE_BIN/nerlnetApp status
    cd -
}

function stop()
{
    cd src_erl/NerlnetApp
    $NERLNET_APP_RELEASE_BIN/nerlnetApp stop
    cd -
}

function run_release_bg()
{
    init
    print "running Nerlnet in background daemon"
    cd src_erl/NerlnetApp
    rebar3 release
    $NERLNET_APP_RELEASE_BIN/nerlnetApp daemon
    cd -
}

function run_release()
{
    init
    print "running Nerlnet release"
    cd src_erl/NerlnetApp
    rebar3 release
	local -a _nerlnet_app_cmd=("$NERLNET_APP_RELEASE_BIN/nerlnetApp" foreground)
	"${_nerlnet_app_cmd[@]}"
	cd -
}

function run_shell()
{
    init
    print "running Nerlnet shell (default)"
    cd src_erl/NerlnetApp
    rebar3 shell 
    cd -
}

#------------------------- S C R I P T   S T A R T -------------------------#

# # When called, the process ends.
# Args:
# 	$1: The exit message (print to stderr)
# 	$2: The exit code (default is 1)
# if env var _PRINT_HELP is set to 'yes', the usage is print to stderr (prior to $1)
# Example:
# 	test -f "$_arg_infile" || _PRINT_HELP=yes die "Can't continue, have to supply file as an argument, got '$_arg_infile'" 4
die()
{
	local _ret="${2:-1}"
	test "${_PRINT_HELP:-no}" = yes && print_help >&2
	echo "$1" >&2
	exit "${_ret}"
}


# Function that evaluates whether a value passed to it begins by a character
# that is a short option of an argument the script knows about.
# This is required in order to support getopts-like short options grouping.
begins_with_short_option()
{
	local first_option all_short_options='rch'
	first_option="${1:0:1}"
	test "$all_short_options" = "${all_short_options/$first_option/}" && return 1 || return 0
}

# THE DEFAULTS INITIALIZATION - OPTIONALS
_arg_run_mode="shell"
_arg_clear="off"


# Function that prints general usage of the script.
# This is useful if users asks for it, or if there is an argument parsing error (unexpected / spurious arguments)
# and it makes sense to remind the user how the script is supposed to be called.
print_help()
{
	printf '%s\n' "NerlnetRun.sh script runs NerlnetApp on device"
    printf '%s\n' "NerlnetInstall.sh and NerlnetBuild.sh must be performed before this script!"
	printf 'Usage: %s [-r|--run-mode <arg>] [-c|--(no-)clear] [-h|--help]\n' "$0"
	printf '\t%s\n' "-r, --run-mode: NerlnetApp running modes: shell, release, release-bg, stop, status (default: 'shell')"
	printf '\t%s\n' "-c, --clear, --no-clear: clear rebar3 directories (off by default)"
	printf '\t%s\n' "-h, --help: Prints help"
}


# The parsing of the command-line
parse_commandline()
{
	while test $# -gt 0
	do
		_key="$1"
		case "$_key" in
			# We support whitespace as a delimiter between option argument and its value.
			# Therefore, we expect the --run-mode or -r value.
			# so we watch for --run-mode and -r.
			# Since we know that we got the long or short option,
			# we just reach out for the next argument to get the value.
			-r|--run-mode)
				test $# -lt 2 && die "Missing value for the optional argument '$_key'." 1
				_arg_run_mode="$2"
				shift
				;;
			# We support the = as a delimiter between option argument and its value.
			# Therefore, we expect --run-mode=value, so we watch for --run-mode=*
			# For whatever we get, we strip '--run-mode=' using the ${var##--run-mode=} notation
			# to get the argument value
			--run-mode=*)
				_arg_run_mode="${_key##--run-mode=}"
				;;
			# We support getopts-style short arguments grouping,
			# so as -r accepts value, we allow it to be appended to it, so we watch for -r*
			# and we strip the leading -r from the argument string using the ${var##-r} notation.
			-r*)
				_arg_run_mode="${_key##-r}"
				;;
			# The clear argurment doesn't accept a value,
			# we expect the --clear or -c, so we watch for them.
			-c|--no-clear|--clear)
				_arg_clear="on"
				test "${1:0:5}" = "--no-" && _arg_clear="off"
				;;
			# We support getopts-style short arguments clustering,
			# so as -c doesn't accept value, other short options may be appended to it, so we watch for -c*.
			# After stripping the leading -c from the argument, we have to make sure
			# that the first character that follows coresponds to a short option.
			-c*)
				_arg_clear="on"
				_next="${_key##-c}"
				if test -n "$_next" -a "$_next" != "$_key"
				then
					{ begins_with_short_option "$_next" && shift && set -- "-c" "-$_next" "$@"; } || die "The short option '$_key' can't be decomposed to ${_key:0:2} and -${_key:2}, because ${_key:0:2} doesn't accept value and '-${_key:2:1}' doesn't correspond to a short option."
				fi
				;;
			# See the comment of option '--clear' to see what's going on here - principle is the same.
			-h|--help)
				print_help
				exit 0
				;;
			# See the comment of option '-c' to see what's going on here - principle is the same.
			-h*)
				print_help
				exit 0
				;;
			*)
				_PRINT_HELP=yes die "FATAL ERROR: Got an unexpected argument '$1'" 1
				;;
		esac
		shift
	done
}

# Now call all the functions defined above that are needed to get the job done
parse_commandline "$@"

case "$_arg_run_mode" in
("shell") run_shell ;;
("release") run_release ;;
("release-bg") run_release_bg ;;
("status") status ;;
("stop") stop ;;
(*) exit 1 ;;
esac

exit 0

#TODO TO BE REMOVED
# only for CI debug
# cd src_erl/NerlnetApp
# echo "$NERLNET_PREFIX Compile NerlnetApp"
# rebar3 compile 
# cd -

# NERLNET_TEST_DIR=/home/parallels/workspace/NErlNet/build/test #tmp
# mkdir -p $NERLNET_TEST_DIR
# LOG_FILE="log.txt" #tmp 

# BUILD_LIB=$NERLNET_LIB_DIR/build/rebar/default/lib
# REBAR3_COMPILED_EBIN_DIRS="jsx/ebin ranch/ebin cowlib/ebin cowboy/ebin nerlnetApp/ebin"
# cd $NERLNET_APP_BUILD_DIR
# echo "erl -noshell -pa $REBAR3_COMPILED_EBIN_DIRS -eval \"nerlnetApp_app:start(a,b).\" -s init stop > "$NERLNET_TEST_DIR/$LOG_FILE""
# erl -pa $REBAR3_COMPILED_EBIN_DIRS -s application start nerlnetApp_app > "$NERLNET_TEST_DIR/$LOG_FILE"
# cat $NERLNET_TEST_DIR/$LOG_FILE