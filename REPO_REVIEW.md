# NErlNet Repository Review

## Scope
- Review of repository structure, build/test tooling, and CI/CD workflows.
- Emphasis on GitHub Actions pipelines and how local test scripts map to CI steps.

## High-Level Summary
NErlNet is a multi-language distributed ML framework targeting IoT/edge clusters. The repository combines:
- C++ NIFs (OpenNN, Torch, Source, optional Wolfram/Synap) built with CMake.
- Erlang OTP applications for the core runtime and monitoring UI.
- Python API server and tooling (planner, designer, autogen, Jupyter helpers).

The project is built around a system-level install model (`/usr/local/lib/nerlnet-lib/NErlNet`) and uses test scripts that assume this layout. CI builds run inside a pre-baked Docker image that mirrors these expectations.

## Repository Layout
- `src_cpp/`: C++ NIFs and bridges (OpenNN, Torch, Source, optional Wolfram/Synap).
- `src_erl/`: Erlang applications (NerlnetApp core, NerlMonitor tool), rebar3 submodule.
- `src_py/`: Python API server, planners/designers, and autogen tooling.
- `tests/`: Shell-based test harness (NIF tests + full-flow integration tests).
- `config/`: Runtime configuration pointers (jsons dir, subnet allowlist).
- `inputJsonsFiles/`, `examples/`: Sample inputs and notebooks.
- `docs/`: Contributing guidelines and code of conduct.
- `NerlnetInstall.sh`, `NerlnetBuild.sh`, `NerlnetRun.sh`: Core lifecycle scripts.
- `.github/workflows/`: CI workflows (PR validation, Docker image build).

## Architecture and Components
### C++ Layer
- Built via `CMakeLists.txt` at the repo root, which includes `src_cpp/`.
- Outputs shared libraries:
  - `nerlnet_onn`: OpenNN bridge (OpenNN submodule).
  - `source_nif`: Source NIF bridge.
  - `nerlnet_torch`: Torch bridge (optional).
  - `nerlnet_wolf`: Wolfram bridge (optional).
- Auto-generated header files are created by Python scripts under `src_py/autogen/`.

### Erlang Layer
- Core OTP application in `src_erl/NerlnetApp/`.
- Uses `cowboy` and `jsx` dependencies via `rebar.config`.
- Provides core entities: Router, Source, Client, MainServer, Bridge, Stats.
- Builds/releases via `rebar3` (submodule).
- Optional `NerlMonitor` tool in `src_erl/NerlMonitor/` for UI and run control.

### Python Layer
- `src_py/apiServer/`: Flask-based API server and experiment orchestration.
- `src_py/autogen/`: Generates C++/Erlang header definitions from JSON schemas.
- `src_py/nerlPlanner/`: JSON generator tool for distributed config and topology.
- `src_py/nerlDesigner/`: NiceGUI-based web UI for building configurations.

## Build and Installation Workflow
### Installation (`NerlnetInstall.sh`)
- Requires root privileges.
- Optional installs:
  - Erlang OTP 28 from source.
  - CMake 3.26.3 from source.
  - libtorch (downloads to `/usr/local/lib/nerlnet-lib/libtorch` and writes `build/torch_env.sh`).
- Always builds `rebar3` and symlinks it to `/usr/local/bin/rebar3`.
- Creates `/usr/local/lib/nerlnet-lib/NErlNet` symlink to the repo.
- Writes a systemd unit file at `/etc/systemd/system/nerlnet.service`.

### Build (`NerlnetBuild.sh`)
- Supports infra selection: `openn`, `torch`, or `all`.
- Generates C++/Erlang headers via Python autogen scripts.
- Builds C++ NIFs with CMake into `build/release`.
- Ensures `rebar3` is built/installed for Erlang releases.
- Creates an `inputDataDir` if missing.
- Modifies `CMakeLists.txt` alignment settings on Raspberry Pi.

### Runtime (`NerlnetRun.sh`)
- Provides run modes: `shell`, `release`, `release-bg`, `status`, `stop`.
- Cleans and prepares temp directories under `/tmp/nerlnet/`.
- Uses `rebar3` to build and run the Erlang release.

## Configuration and Inputs
- `config/jsonsDir.nerlconfig`: Points to the JSON directory used at runtime.
- `config/subnets.nerlconfig`: Subnet allowlist used to validate network peers.
- JSON configuration files are expected to include:
  - `dc_*.json` (distributed config)
  - `conn_*.json` (connection map)
  - `exp_*.json` (experiment flow)

## Tooling
- `NerlnetJupyterLaunch.sh`: Sets up a Python environment and launches JupyterLab for API usage.
- `NerlnetPlanner.sh`: Installs graphviz dependencies and runs the planner UI.
- `NerlDesigner.sh`: Manages a NiceGUI-based web designer and its venv.
- `NerlnetMonitor.sh`: Launches the Erlang-based monitoring app.

## Testing Overview
Tests are shell scripts that rely on `/usr/local/lib/nerlnet-lib/NErlNet` and expect
the full build/install workflow to have been executed.

### Unit and NIF Tests
- `tests/NerlnetNifTest.sh`:
  - Compiles Erlang NIF test modules and runs `nerlTests:run_tests()`.
  - Focuses on OpenNN NIF integration.
- `tests/NerlnetSourceNifTest.sh`:
  - Compiles Source NIF modules and runs `testSourceNIF:run_tests()`.
- `tests/NerlnetNIFTorchTest.sh`:
  - Generates TorchScript models via `tests/scripts/generate_torch_test_model.py`.
  - Compiles Torch NIF modules and runs `torchTests:run_tests()`.

### Integration (Full Flow) Tests
- `tests/NerlnetFullFlowTest.sh`:
  - Backs up config, rewrites test JSONs with host IP, and runs `experiment_flow_test.py`.
  - Runs the Erlang release via `NerlnetRun.sh` and validates baseline loss/F1 metrics.
- `tests/NerlnetFullFlowTorchTest.sh`:
  - Same as above but using torch-based JSONs and TorchScript test model.
- `tests/NerlnetFullFlowTestPost.sh` / `tests/NerlnetFullFlowTorchTestPost.sh`:
  - Prints `/tmp/nerlnet_run_log.txt` for debugging.

### Test Environment Setup
- `tests/set_env.sh` creates a venv under `/tmp/nerlnet/virtualenv` and installs
  `src_py/requirements.txt`.
- Torch can be installed via `--torch` flag.
- When `RUNNING_IN_DOCKER=true`, venv creation is skipped.

## GitHub Actions CI/CD
### Workflow: PR Validation (`.github/workflows/pr.yml`)
Trigger:
- Pull requests to `master`.

Execution environment:
- `ubuntu-latest` runner, but jobs run inside `leondavi/nerlnet:latest` container.
- `RUNNING_IN_DOCKER=true` is set in the job environment.

Pipeline steps:
1. Checkout (with submodules).
2. `./NerlnetInstall.sh --torch` to install libtorch, set symlinks, and build `rebar3`.
3. Install Python torch: `pip install torch==2.2.1` (CPU wheel).
4. Build: `./NerlnetBuild.sh --infra all` (OpenNN + Torch + Source NIF).
5. Tests (conditional):
   - NIF unit tests (OpenNN): `tests/NerlnetNifTest.sh` (timeout 15m).
   - Source NIF tests: `tests/NerlnetSourceNifTest.sh` (timeout 15m).
   - Torch NIF tests: `tests/NerlnetNIFTorchTest.sh` (timeout 15m).
   - Full-flow test: `tests/NerlnetFullFlowTest.sh` (timeout 20m).
   - Post full-flow log: `tests/NerlnetFullFlowTestPost.sh` (timeout 5m).
   - Torch full-flow test: `tests/NerlnetFullFlowTorchTest.sh` (timeout 20m).
   - Torch post full-flow log: `tests/NerlnetFullFlowTorchTestPost.sh` (timeout 5m).

Key gating conditions:
- Most tests run only if `build` succeeds.
- Torch NIF tests and full-flow tests require the NIF test to pass.
- Torch full-flow depends on the non-torch full-flow step.

### Workflow: Docker Image Build (`.github/workflows/update_image.yml`)
Trigger:
- Pushes to `master` and manual `workflow_dispatch`.

Pipeline steps:
1. Checkout repository.
2. Docker Hub login using `DOCKER_USERNAME` / `DOCKER_PASSWORD` secrets.
3. Build Docker image from `Dockerfile` and tag as:
   - `<user>/nerlnet:latest`
   - `<user>/nerlnet:<git sha>`
4. Push `latest` tag to Docker Hub.

Notes:
- The SHA tag is created locally but not pushed in the workflow.
- The PR workflow consumes `leondavi/nerlnet:latest`, which should match this image.

## Dependencies and Submodules
- Git submodules:
  - OpenNN fork (`src_cpp/opennn`).
  - `rebar3` (`src_erl/rebar3`).
  - `simple-cpp-logger`, `nifpp`.
- Python dependencies are listed in `src_py/requirements.txt` (Flask, numpy, torch, etc.).
- System deps include Erlang, CMake, GCC/G++, and graphviz-related packages for planner.

## Documentation and Contribution
- `README.md` provides installation/build/test/run instructions and overview.
- `docs/CONTRIBUTING.md` details PR guidelines and coding standards.
- `docs/CODE_OF_CONDUCT.md` uses Contributor Covenant v2.0.

## Observations and Potential Improvements
- CI uses a container image (`leondavi/nerlnet:latest`) that is built separately; if the
  image is stale, CI behavior may diverge from `master`.
- `update_image.yml` does not push the SHA tag; only `latest` is published.
- `NerlnetInstall.sh` default libtorch version (2.1.2) differs from the Python torch
  version installed in CI (2.2.1). This may cause compatibility issues for Torch NIF tests.
- Several scripts assume root privileges and `/usr/local/lib/nerlnet-lib` layout; local
  developer workflows that avoid root may need extra documentation or a non-root mode.
- `NerlnetBuild.sh` modifies `CMakeLists.txt` in place for Raspberry Pi alignment, which
  can leave the repo in a dirty state after builds.
- Dockerfile uses `ubuntu:latest` and installs Erlang from a floating apt repo, which
  can introduce non-reproducible builds.

## Suggested Next Steps (Optional)
- Pin container base image and Erlang package versions for reproducibility.
- Align libtorch and Python torch versions across install/test scripts.
- Consider pushing SHA-tagged Docker images for traceability in CI.
- Add a non-root install mode or clarify developer setup in docs.
