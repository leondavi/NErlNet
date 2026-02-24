# Ubuntu CPU-Torch Validation Container

This image builds NErlNet on Ubuntu and validates:

1. Torch full-flow integration (`tests/NerlnetFullFlowTorchTest.sh`)
2. PTD POC baseline loss parity (`PTD_P_POC/PTD_P_Tests.py` vs `PTD_P_POC/PTD_P_Tests.log`)

## Quick Run

```bash
chmod +x docker/ubuntu-torch-cpu/build_and_run.sh
docker/ubuntu-torch-cpu/build_and_run.sh
```

By default the helper script uses the host platform. Override with
`DOCKER_PLATFORM` when needed.

To force a specific platform explicitly:

```bash
DOCKER_PLATFORM=linux/amd64 docker/ubuntu-torch-cpu/build_and_run.sh
```

Artifacts are written to:

```text
.docker-artifacts/torch-cpu-validation/
```

Key outputs:

- `NerlnetFullFlowTorchTest.log`
- `PTD_P_Tests.current.log`
- `PTD_P_loss_compare.json`
