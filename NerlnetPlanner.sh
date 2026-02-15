#!/usr/bin/env bash

set -euo pipefail

print() {
  echo "[NERLPLANNER] $1"
}

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PLANNER_DIR="$ROOT_DIR/web/nerl-planner"

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

print "Starting dev server and opening the planner..."
npm run dev -- --open
