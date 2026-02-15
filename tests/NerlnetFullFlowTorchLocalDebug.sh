#!/bin/bash

set -euo pipefail

DEFAULT_NERLNET_PATH="/usr/local/lib/nerlnet-lib/NErlNet"
if [ -z "${NERLNET_PATH:-}" ]; then
    if [ -d "$PWD/src_erl" ]; then
        export NERLNET_PATH="$PWD"
    else
        export NERLNET_PATH="$DEFAULT_NERLNET_PATH"
    fi
fi

RUN_LOG="/tmp/nerlnet_run_local_debug.log"
WAIT_TIME_FOR_NERLNET_RUN_BOOT=${NERLNET_LOCAL_DEBUG_WAIT:-60}
JSON_DIR=""
DC_PATH=""
CONN_PATH=""
EXP_PATH=""
MANUAL_START=false
KEEP_TEMP=false
NO_IP_REWRITE=false

function print() {
    echo "[NERLNET-TORCH-LOCAL-DEBUG] $1"
}

function usage() {
    cat <<EOF_USAGE
Usage: ${0##*/} [options]

Options:
  --json-dir <dir>     Directory containing dc_*.json/conn_*.json/exp_*.json
  --dc <file>          Explicit dc_*.json (requires --conn and --exp)
  --conn <file>        Explicit conn_*.json (requires --dc and --exp)
  --exp <file>         Explicit exp_*.json (requires --dc and --conn)
  --manual-start       NerlnetApp already running; skip start/stop
  --no-ip-rewrite      Do not replace x.x.x.x placeholder in DC
  --keep-temp          Keep temporary json directory
  -h, --help           Show this help message
EOF_USAGE
}

function detect_ipv4() {
    if command -v ip >/dev/null 2>&1; then
        ip addr | grep -m 2 -E -o "([0-9]{1,3}[\.]){3}[0-9]{1,3}" | head -n 2 | grep -v "127.0.0.1" | head -n 1
        return
    fi
    if command -v ifconfig >/dev/null 2>&1; then
        ifconfig | grep -E "inet " | awk '{print $2}' | grep -v "127.0.0.1" | head -n 1
        return
    fi
    if command -v ipconfig >/dev/null 2>&1; then
        ipconfig getifaddr en0 2>/dev/null || ipconfig getifaddr en1 2>/dev/null || true
        return
    fi
}

while [[ $# -gt 0 ]]; do
    case "$1" in
        --json-dir)
            JSON_DIR="$2"
            shift 2
            ;;
        --dc)
            DC_PATH="$2"
            shift 2
            ;;
        --conn)
            CONN_PATH="$2"
            shift 2
            ;;
        --exp)
            EXP_PATH="$2"
            shift 2
            ;;
        --manual-start)
            MANUAL_START=true
            shift
            ;;
        --no-ip-rewrite)
            NO_IP_REWRITE=true
            shift
            ;;
        --keep-temp)
            KEEP_TEMP=true
            shift
            ;;
        -h|--help)
            usage
            exit 0
            ;;
        *)
            print "Unknown argument: $1"
            usage
            exit 1
            ;;
    esac
done

TEMP_JSON_DIR=""
if [[ -n "$DC_PATH" || -n "$CONN_PATH" || -n "$EXP_PATH" ]]; then
    if [[ -z "$DC_PATH" || -z "$CONN_PATH" || -z "$EXP_PATH" ]]; then
        print "--dc/--conn/--exp must be provided together"
        exit 1
    fi
    TEMP_JSON_DIR="$(mktemp -d /tmp/nerlnet-local-jsons.XXXXXX)"
    cp "$DC_PATH" "$TEMP_JSON_DIR/dc_local.json"
    cp "$CONN_PATH" "$TEMP_JSON_DIR/conn_local.json"
    cp "$EXP_PATH" "$TEMP_JSON_DIR/exp_local.json"
    JSON_DIR="$TEMP_JSON_DIR"
    DC_FILE="$TEMP_JSON_DIR/dc_local.json"
    CONN_FILE="$TEMP_JSON_DIR/conn_local.json"
    EXP_FILE="$TEMP_JSON_DIR/exp_local.json"
else
    if [ -z "$JSON_DIR" ]; then
        JSON_DIR="$NERLNET_PATH/tests/inputTorchJsonsFiles"
    fi
    DC_FILE=$(ls "$JSON_DIR"/dc_*.json 2>/dev/null | sort | head -n 1 || true)
    CONN_FILE=$(ls "$JSON_DIR"/conn_*.json 2>/dev/null | sort | head -n 1 || true)
    EXP_FILE=$(ls "$JSON_DIR"/exp_*.json 2>/dev/null | sort | head -n 1 || true)
fi

if [ -z "${DC_FILE:-}" ] || [ ! -f "$DC_FILE" ]; then
    print "DC json not found in $JSON_DIR"
    exit 1
fi
if [ -z "${CONN_FILE:-}" ] || [ ! -f "$CONN_FILE" ]; then
    print "Connection json not found in $JSON_DIR"
    exit 1
fi
if [ -z "${EXP_FILE:-}" ] || [ ! -f "$EXP_FILE" ]; then
    print "Experiment json not found in $JSON_DIR"
    exit 1
fi

NERLNET_CONFIG_DIR="$NERLNET_PATH/config"
NERLNET_CONFIG_JSONS_DIR="$NERLNET_CONFIG_DIR/jsonsDir.nerlconfig"
NERLNET_CONFIG_JSONS_DIR_BACKUP="$NERLNET_CONFIG_DIR/jsonsDir.nerlconfig.bac"
NERLNET_CONFIG_SUBNETS_DIR="$NERLNET_CONFIG_DIR/subnets.nerlconfig"
NERLNET_CONFIG_SUBNETS_BACKUP="$NERLNET_CONFIG_DIR/subnets.nerlconfig.bac"

print "Using JSON dir: $JSON_DIR"
print "DC:   $DC_FILE"
print "Conn: $CONN_FILE"
print "Exp:  $EXP_FILE"

# Backup config files
cp "$NERLNET_CONFIG_SUBNETS_DIR" "$NERLNET_CONFIG_SUBNETS_BACKUP"
cp "$NERLNET_CONFIG_JSONS_DIR" "$NERLNET_CONFIG_JSONS_DIR_BACKUP"

# Update jsonsDir.nerlconfig
rm -f "$NERLNET_CONFIG_JSONS_DIR"
echo "$JSON_DIR" > "$NERLNET_CONFIG_JSONS_DIR"

# Add host IP to subnet allowlist
HOST_IP="$(detect_ipv4 || true)"
if [ -n "$HOST_IP" ]; then
    print "Detected host IPv4: $HOST_IP"
    echo "" >> "$NERLNET_CONFIG_SUBNETS_DIR"
    echo "$HOST_IP" >> "$NERLNET_CONFIG_SUBNETS_DIR"
else
    print "Could not detect host IPv4; skipping subnet allowlist update"
fi

# Replace placeholder IP in DC if needed
if ! $NO_IP_REWRITE && grep -q "x.x.x.x" "$DC_FILE"; then
    if [ -n "$HOST_IP" ]; then
        print "Replacing x.x.x.x in DC with $HOST_IP"
        sed -i.bak "s/x.x.x.x/$HOST_IP/g" "$DC_FILE"
        rm -f "${DC_FILE}.bak"
    else
        print "x.x.x.x placeholder found but host IP not detected"
    fi
fi

# Start NerlnetApp if not already running
RUN_PID=""
if $MANUAL_START; then
    print "Manual start mode enabled - skipping NerlnetRun start/stop"
else
    print "Starting NerlnetRun (release mode)"
    (cd "$NERLNET_PATH" && ./NerlnetRun.sh --run-mode release > "$RUN_LOG" 2>&1 &)
    RUN_PID=$!
    print "Waiting $WAIT_TIME_FOR_NERLNET_RUN_BOOT seconds for NerlnetApp boot"
    sleep "$WAIT_TIME_FOR_NERLNET_RUN_BOOT"
fi

PYTHON_BIN="${NERLNET_TEST_PYTHON:-python3}"
if ! command -v "$PYTHON_BIN" >/dev/null 2>&1; then
    print "Python executable $PYTHON_BIN not found"
    exit 1
fi

print "Running local debug flow"
PYTHONUNBUFFERED=1 "$PYTHON_BIN" -u "$NERLNET_PATH/src_py/apiServer/experiment_flow_local_debug.py" \
    --dc "$DC_FILE" --conn "$CONN_FILE" --exp "$EXP_FILE"
LOCAL_RC=$?

# Stop NerlnetApp if we started it
if ! $MANUAL_START; then
    print "Stopping NerlnetRun"
    (cd "$NERLNET_PATH" && ./NerlnetRun.sh --run-mode stop) || true
    if [ -n "$RUN_PID" ]; then
        wait "$RUN_PID" || true
    fi
fi

# Restore config backups
print "Restoring config backups"
cp "$NERLNET_CONFIG_JSONS_DIR_BACKUP" "$NERLNET_CONFIG_JSONS_DIR"
cp "$NERLNET_CONFIG_SUBNETS_BACKUP" "$NERLNET_CONFIG_SUBNETS_DIR"
rm -f "$NERLNET_CONFIG_JSONS_DIR_BACKUP" "$NERLNET_CONFIG_SUBNETS_BACKUP"

if [ -n "$TEMP_JSON_DIR" ] && ! $KEEP_TEMP; then
    rm -rf "$TEMP_JSON_DIR"
fi

print "Local debug flow finished with status: $LOCAL_RC"
if [ -f "$RUN_LOG" ]; then
    print "---- NerlnetRun log ($RUN_LOG) ----"
    cat "$RUN_LOG"
fi

exit "$LOCAL_RC"
