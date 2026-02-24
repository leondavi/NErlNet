#!/usr/bin/env bash

set -euo pipefail

print() {
  echo "[NERLPLANNER] $1"
}

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PLANNER_DIR="$ROOT_DIR/web/nerl-planner"
TORCH_EXPORT_VENV_PY="$PLANNER_DIR/.venv-torch-export/bin/python"

if [ ! -d "$PLANNER_DIR" ]; then
  print "Planner folder not found: $PLANNER_DIR"
  exit 1
fi

if ! command -v npm >/dev/null 2>&1; then
  print "npm is required. Install Node.js first."
  exit 1
fi

# Check Node.js version (Vite 5 requires Node 18+)
NODE_VERSION=$(node -v 2>/dev/null | sed 's/v//' | cut -d. -f1)
if [ -z "$NODE_VERSION" ] || [ "$NODE_VERSION" -lt 18 ]; then
  print "Node.js 18 or higher is required. Current: $(node -v 2>/dev/null || echo 'not found')"
  exit 1
fi

cd "$PLANNER_DIR"

if [ ! -d node_modules ]; then
  print "Installing npm dependencies..."
  npm install
else
  print "npm dependencies already installed."
fi

if [ -z "${PYTHON:-}" ] && [ -x "$TORCH_EXPORT_VENV_PY" ]; then
  export PYTHON="$TORCH_EXPORT_VENV_PY"
  print "Using planner Torch export interpreter: $PYTHON"
elif [ -z "${PYTHON:-}" ]; then
  print "No dedicated planner Torch export interpreter found."
  print "To enable Torch export, set PYTHON to an interpreter with torch installed."
  print "Example:"
  print "  python3 -m venv web/nerl-planner/.venv-torch-export"
  print "  web/nerl-planner/.venv-torch-export/bin/pip install torch"
  print "  PYTHON=web/nerl-planner/.venv-torch-export/bin/python ./NerlnetPlanner.sh"
fi

if [ -n "${PYTHON:-}" ] && [ -x "${PYTHON}" ]; then
  if ! "$PYTHON" -c "import importlib.util,sys; sys.exit(0 if importlib.util.find_spec('huggingface_hub') else 1)"; then
    print "Hugging Face dataset listing/download requires huggingface_hub in $PYTHON"
    print "Install with:"
    print "  $PYTHON -m pip install huggingface_hub"
  fi
fi

print "Starting dev server and opening the planner..."
npm run dev -- --open
