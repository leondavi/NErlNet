#!/usr/bin/env bash

set -euo pipefail

REPO_ROOT="${REPO_ROOT:-/workspace/NErlNet}"
ARTIFACTS_DIR="${ARTIFACTS_DIR:-/tmp/nerlnet_docker_validation}"
PTD_REFERENCE_LOG="$REPO_ROOT/PTD_P_POC/PTD_P_Tests.log"
PTD_CURRENT_LOG="$ARTIFACTS_DIR/PTD_P_Tests.current.log"
TORCH_FLOW_LOG="$ARTIFACTS_DIR/NerlnetFullFlowTorchTest.log"
LOSS_COMPARE_REPORT="$ARTIFACTS_DIR/PTD_P_loss_compare.json"

mkdir -p "$ARTIFACTS_DIR"
cd "$REPO_ROOT"

echo "[NERLNET-DOCKER-VALIDATION] Repo root: $REPO_ROOT"
echo "[NERLNET-DOCKER-VALIDATION] Artifacts dir: $ARTIFACTS_DIR"

if [[ ! -f "$REPO_ROOT/build/torch_env.sh" ]]; then
  echo "[NERLNET-DOCKER-VALIDATION] Missing build/torch_env.sh. Did install step run?"
  exit 1
fi

echo "[NERLNET-DOCKER-VALIDATION] Running Torch full-flow test"
if ! tests/NerlnetFullFlowTorchTest.sh | tee "$TORCH_FLOW_LOG"; then
  if [[ -f /tmp/nerlnet_run_log.txt ]]; then
    cp /tmp/nerlnet_run_log.txt "$ARTIFACTS_DIR/NerlnetRun.log"
  fi
  echo "[NERLNET-DOCKER-VALIDATION] Torch full-flow test failed. Log: $TORCH_FLOW_LOG"
  exit 1
fi

echo "[NERLNET-DOCKER-VALIDATION] Running PTD POC simulator"
python3 PTD_P_POC/PTD_P_Tests.py | tee "$PTD_CURRENT_LOG" >/dev/null

echo "[NERLNET-DOCKER-VALIDATION] Comparing PTD losses against baseline log"
python3 tests/parallelism/compare_ptd_poc_losses.py \
  --expected "$PTD_REFERENCE_LOG" \
  --actual "$PTD_CURRENT_LOG" \
  > "$LOSS_COMPARE_REPORT"

cat "$LOSS_COMPARE_REPORT"

echo "[NERLNET-DOCKER-VALIDATION] Validation completed successfully"
echo "[NERLNET-DOCKER-VALIDATION] Torch flow log: $TORCH_FLOW_LOG"
echo "[NERLNET-DOCKER-VALIDATION] PTD log: $PTD_CURRENT_LOG"
echo "[NERLNET-DOCKER-VALIDATION] Loss report: $LOSS_COMPARE_REPORT"
