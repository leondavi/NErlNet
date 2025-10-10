#!/bin/bash

NERLNET_LIB_DIR="/usr/local/lib/nerlnet-lib"
NERLNET_DIR=$NERLNET_LIB_DIR/NErlNet
NERLNET_PREFIX="[NERLNET_SCRIPT]"
NERLDESIGNER_PREFIX="[NerlDesigner] "
NERLDESIGNER_ENV_NAME=".nerldesigner_env"
NERLDESIGNER_ENV_PATH="$PWD/$NERLDESIGNER_ENV_NAME"
NERLDESIGNER_REQUIREMENTS_PATH="$PWD/src_py/nerlDesigner/requirements.txt"

# arguments parsing 
# Thanks to https://github.com/matejak/argbash
Port=8082
Host="0.0.0.0"
Install=false
Clean=false

help()
{
    echo "-------------------------------------" && echo "NerlDesigner Launcher" && echo "-------------------------------------"
    echo "Usage:"
    echo "--i or --install: Install/reinstall the Python environment"
    echo "--c or --clean: Clean existing environment and reinstall"
    echo "--p or --port: Set the port for the web interface (default: 8082)"
    echo "--h or --host: Set the host for the web interface (default: 0.0.0.0)"
    echo "--help: Show this help message"
    exit 2
}

print()
{
    echo "$NERLDESIGNER_PREFIX $1"
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
    local first_option all_short_options='hicph'
    first_option="${1:0:1}"
    test "$all_short_options" = "${all_short_options/$first_option/}" && return 1 || return 0
}

print_help()
{
    printf 'Usage: %s [--help] [--install] [--clean] [--port <arg>] [--host <arg>]\n' "$0"
    printf '\t%s\n' "--install, -i: Install/reinstall the Python environment"
    printf '\t%s\n' "--clean, -c: Clean existing environment and reinstall"
    printf '\t%s\n' "--port, -p: Set the port for the web interface (default: 8082)"
    printf '\t%s\n' "--host, -h: Set the host for the web interface (default: 0.0.0.0)"
    printf '\t%s\n' "--help: Show this help message"
}

parse_commandline()
{
    while test $# -gt 0
    do
        _key="$1"
        case "$_key" in
            --help)
                help
                exit 0
                ;;
            -i|--install)
                Install=true
                ;;
            -c|--clean)
                Clean=true
                Install=true
                ;;
            -p|--port)
                test $# -lt 2 && die "Missing value for the optional argument '$_key'." 1
                Port="$2"
                shift
                ;;
            --port=*)
                Port="${_key##--port=}"
                ;;
            -p*)
                Port="${_key##-p}"
                ;;
            -h|--host)
                test $# -lt 2 && die "Missing value for the optional argument '$_key'." 1
                Host="$2"
                shift
                ;;
            --host=*)
                Host="${_key##--host=}"
                ;;
            -h*)
                Host="${_key##-h}"
                ;;
            *)
                _PRINT_HELP=yes die "FATAL ERROR: Got an unexpected argument '$1'" 1
                ;;
        esac
        shift
    done
}

check_python3()
{
    if command -v python3 >/dev/null 2>&1; then
        print "Python 3 is available"
        python3 --version
        return 0
    else
        print "ERROR: Python 3 is not installed"
        print "Please install Python 3.8 or higher"
        return 1
    fi
}

check_venv_module()
{
    if python3 -m venv --help >/dev/null 2>&1; then
        print "Python venv module is available"
        return 0
    else
        print "ERROR: Python venv module is not available"
        print "Please install python3-venv package"
        return 1
    fi
}

clean_environment()
{
    if [ -d "$NERLDESIGNER_ENV_PATH" ]; then
        print "Removing existing environment: $NERLDESIGNER_ENV_PATH"
        rm -rf "$NERLDESIGNER_ENV_PATH"
    fi
}

create_environment()
{
    print "Creating Python virtual environment: $NERLDESIGNER_ENV_NAME"
    python3 -m venv "$NERLDESIGNER_ENV_PATH"
    
    if [ ! -d "$NERLDESIGNER_ENV_PATH" ]; then
        print "ERROR: Failed to create virtual environment"
        return 1
    fi
    
    print "Virtual environment created successfully"
    return 0
}

activate_environment()
{
    if [ ! -f "$NERLDESIGNER_ENV_PATH/bin/activate" ]; then
        print "ERROR: Virtual environment activation script not found"
        return 1
    fi
    
    print "Activating virtual environment"
    source "$NERLDESIGNER_ENV_PATH/bin/activate"
    
    # Verify activation
    if [ "$VIRTUAL_ENV" = "$NERLDESIGNER_ENV_PATH" ]; then
        print "Virtual environment activated: $VIRTUAL_ENV"
        return 0
    else
        print "ERROR: Failed to activate virtual environment"
        return 1
    fi
}

install_dependencies()
{
    print "Installing NerlDesigner dependencies"
    
    # Upgrade pip first
    print "Upgrading pip..."
    python -m pip install --upgrade pip
    
    # Install requirements
    if [ -f "$NERLDESIGNER_REQUIREMENTS_PATH" ]; then
        print "Installing requirements from: $NERLDESIGNER_REQUIREMENTS_PATH"
        pip install -r "$NERLDESIGNER_REQUIREMENTS_PATH"
    else
        print "Requirements file not found, installing basic dependencies"
        pip install nicegui>=1.4.0 pydantic>=2.0.0 networkx>=3.0 plotly>=5.0.0 pandas>=1.5.0 fastapi>=0.100.0
    fi
    
    print "Dependencies installed successfully"
}

check_environment_exists()
{
    if [ -d "$NERLDESIGNER_ENV_PATH" ] && [ -f "$NERLDESIGNER_ENV_PATH/bin/activate" ]; then
        return 0
    else
        return 1
    fi
}

setup_environment()
{
    print "Setting up NerlDesigner Python environment"
    
    # Check if we need to clean
    if [ "$Clean" = true ]; then
        clean_environment
    fi
    
    # Create environment if it doesn't exist
    if ! check_environment_exists; then
        if ! create_environment; then
            return 1
        fi
    fi
    
    # Activate environment
    if ! activate_environment; then
        return 1
    fi
    
    # Install dependencies if requested or if environment is new
    if [ "$Install" = true ] || [ ! -f "$NERLDESIGNER_ENV_PATH/.dependencies_installed" ]; then
        install_dependencies
        touch "$NERLDESIGNER_ENV_PATH/.dependencies_installed"
    fi
    
    return 0
}

launch_nerldesigner()
{
    print "Launching NerlDesigner Web Application"
    print "Host: $Host"
    print "Port: $Port"
    print ""
    print "🌐 To access the server:"
    if [ "$Host" = "0.0.0.0" ]; then
        print "    Local access:"
        print "        http://localhost:$Port"
        print "        or http://127.0.0.1:$Port"
        print ""
        print "    📡 SSH Tunnel (if connecting remotely):"
        print "        On your local machine, run:"
        print "        ssh -L $Port:localhost:$Port <username>@<server-address>"
        print "        Then open: http://localhost:$Port"
        print ""
    else
        print "    Direct access: http://$Host:$Port"
    fi
    print "Press Ctrl+C to stop the application"
    print "=================================================="
    
    # Set environment variables for the application
    export NERLDESIGNER_HOST="$Host"
    export NERLDESIGNER_PORT="$Port"
    
    # Launch the application
    cd src_py/nerlDesigner
    python main.py --host "$Host" --port "$Port"
}

main()
{
    print "NerlDesigner Environment Manager and Launcher"
    print "============================================="
    
    # Parse command line arguments
    parse_commandline "$@"
    
    # Check system requirements
    if ! check_python3; then
        exit 1
    fi
    
    if ! check_venv_module; then
        exit 1
    fi
    
    # Setup environment
    if ! setup_environment; then
        print "ERROR: Failed to setup environment"
        exit 1
    fi
    
    # Launch application if not just installing
    if [ "$Install" = false ] || [ "$Clean" = false ]; then
        launch_nerldesigner
    else
        print "Environment setup completed successfully"
        print "Run this script without --install to launch NerlDesigner"
    fi
}

# Run main function with all arguments
main "$@"