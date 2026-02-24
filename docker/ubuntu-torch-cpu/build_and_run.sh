#!/usr/bin/env bash

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO_ROOT="$(cd "$SCRIPT_DIR/../.." && pwd)"
IMAGE_TAG="${IMAGE_TAG:-nerlnet:ubuntu-torch-cpu}"
DOCKER_PLATFORM="${DOCKER_PLATFORM:-}"
ARTIFACTS_DIR="${ARTIFACTS_DIR:-$REPO_ROOT/.docker-artifacts/torch-cpu-validation}"

mkdir -p "$ARTIFACTS_DIR"

echo "[NERLNET-DOCKER] Building image: $IMAGE_TAG"
if [[ -n "$DOCKER_PLATFORM" ]]; then
  echo "[NERLNET-DOCKER] Target platform: $DOCKER_PLATFORM"
  docker build \
    --platform "$DOCKER_PLATFORM" \
    -f "$SCRIPT_DIR/Dockerfile" \
    -t "$IMAGE_TAG" \
    "$REPO_ROOT"
else
  echo "[NERLNET-DOCKER] Target platform: host default"
  docker build \
    -f "$SCRIPT_DIR/Dockerfile" \
    -t "$IMAGE_TAG" \
    "$REPO_ROOT"
fi

echo "[NERLNET-DOCKER] Running validation container"
if [[ -n "$DOCKER_PLATFORM" ]]; then
  docker run --rm \
    --platform "$DOCKER_PLATFORM" \
    -e ARTIFACTS_DIR=/tmp/nerlnet_docker_validation \
    -v "$ARTIFACTS_DIR":/tmp/nerlnet_docker_validation \
    "$IMAGE_TAG"
else
  docker run --rm \
    -e ARTIFACTS_DIR=/tmp/nerlnet_docker_validation \
    -v "$ARTIFACTS_DIR":/tmp/nerlnet_docker_validation \
    "$IMAGE_TAG"
fi

echo "[NERLNET-DOCKER] Validation artifacts saved to: $ARTIFACTS_DIR"
