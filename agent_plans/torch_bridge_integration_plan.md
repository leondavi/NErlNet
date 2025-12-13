# Torch Bridge Integration Plan

## 1. Goals
- Reimplement a native Nerlnet `torchNIF`/`torchBridge` using libtorch without embedding SynapNIF artifacts.
- Provide install/build hooks that download an official libtorch distribution when `--torch` is requested and expose the paths to CMake/tests.
- Reach feature parity with `opennnBridge` + `onnWorkers` (model lifecycle, training, inference, weight sync, distributed support) entirely inside this repo.
- Deliver targeted Torch test harnesses (`NerlnetTorchNifTest.sh`, `NerlnetFullFlowTorchTest.sh`) so CI validates the new bridge end-to-end.

## 2. Reference Artifacts
- SynapNIF repo (design inspiration only): study `torch_nif`, tensor helpers, and worker ergonomics but do not import code.
- Existing Nerlnet components: `opennnBridge`, Erlang `onnWorkers`, test scripts, and CLI tooling for comparable behaviors.
- libtorch official releases from https://download.pytorch.org/libtorch/ (CPU build by default, optional CUDA later).
- Build surface: `NerlnetInstall.sh`, `NerlnetBuild.sh`, root `CMakeLists.txt`, `src_cpp/CMakeLists.txt`, `tests/` scripts.

## 3. Work Breakdown
### 3.1 Libtorch Provisioning (Install Layer)
1. Add/extend `--torch` flag in `NerlnetInstall.sh` help text.
2. When enabled:
   - Download the requested libtorch archive (default: CPU, configurable via env).
   - Extract into `/usr/local/lib/nerlnet-lib/libtorch/<version>` (owned by Nerlnet, not SynapNIF).
   - Write `build/torch_env.sh` capturing `TORCH_ROOT`, `LD_LIBRARY_PATH`, and helper variables (`TORCH_INCLUDE`, `TORCH_LIBS`).
   - Record metadata (version, checksum) for reproducibility.

### 3.2 Build Script Integration (`NerlnetBuild.sh`)
1. When `--torch` is passed:
   - Source `build/torch_env.sh`, verifying libtorch exists and matches host toolchain.
   - Export `TORCH_ROOT`, `LD_LIBRARY_PATH`, `CMAKE_PREFIX_PATH` and propagate `-DNERLTORCH=ON -DTORCH_ROOT=...` to CMake.
   - Provide guard rails (clear error if env missing, guide user to rerun install script).
2. Allow `--enable-synapnif` to coexist but keep environments isolated (no SynapNIF dependency required for Torch).

### 3.3 CMake Wiring
1. Root `CMakeLists.txt`:
   - Finalize the `NERLTORCH` option and gate `add_subdirectory(src_cpp/torchBridge)`.
   - Append `TORCH_ROOT` to `CMAKE_PREFIX_PATH` before `find_package(Torch REQUIRED)`.
2. `src_cpp/CMakeLists.txt`:
   - Add a proper `torchBridge` target linking common utilities, simple logger, and `${TORCH_LIBRARIES}`.
   - Ensure include directories cover Nerlnet headers plus `${TORCH_ROOT}/include` and `${TORCH_ROOT}/include/torch/csrc/api/include`.
3. `src_cpp/torchBridge/CMakeLists.txt`:
   - Enumerate new sources (workers, tensor helpers, nif entry points).
   - Link with `TORCH_LIBRARIES` and add `-D_GLIBCXX_USE_CXX11_ABI=1` (matching libtorch release) as needed.
   - Optional: parameterize CUDA flags for future GPU builds.

### 3.4 C++ Torch Bridge Implementation
1. Rebuild `torchBridge` contents from scratch:
   - `NerlWorkerTorch` class mirroring `NerlWorkerOpenNN` semantics (train, predict, evaluate, manage datasets).
   - Tensor marshaling helpers converting between Nerlnet binaries and `torch::Tensor` (shape metadata, dtype safety, zero-copy when possible).
   - `torchNIF.cpp` exposing NIF functions: model lifecycle, tensor ops, gradient/training loops, weight import/export, optimizer config.
   - Shared utility modules (thread pools, logging adapters) reused from existing Nerlnet C++ libs where viable.
2. Reference SynapNIF structure only for ideas (naming, error handling) but author clean-room implementations.
3. Implement rigorous error propagation + resource cleanup (enif resource types, destructors) to avoid leaks.

### 3.5 Erlang Torch Workers
1. Under `src_erl/NerlnetApp/.../torchWorkers`, implement modules analogous to `onnWorkers` but calling the new NIF atoms.
2. Define `torchNIF.erl` with wrappers for training/predict/weight ops, binary tensor conversions, and distributed coordination hooks.
3. Update higher-level orchestrators (planner, api server) to accept `bridge=torch` configs without diverging control flow.
4. Ensure remote/distributed modes (federated, planner-driven) use the same message contracts as ONN for parity.

### 3.6 Testing Strategy
1. Extend `tests/NerlnetSynapNifTest.sh --torch` logic into a dedicated `tests/NerlnetTorchNifTest.sh` script that:
   - Sources `build/torch_env.sh`.
   - Builds `torchBridge` targets.
   - Runs Erlang torch unit suites (`torchTests:run_tests/0`, dataset smoke tests, weight round-trips).
2. Add `tests/NerlnetFullFlowTorchTest.sh` replicating existing end-to-end scenarios but pointing configs to torch workers.
3. Provide coverage for failure modes: missing libtorch, serialization mismatches, distributed sync errors.

### 3.7 Documentation & CI
1. Update `README.md`, `docs/`, and installer help text to describe the new Torch path, required dependencies, and env sourcing steps.
2. Document how to update libtorch versions (central place storing download URL + checksum).
3. Extend CI pipeline with conditional Torch jobs (`NERLTORCH=ON`) running the new scripts.
4. Offer troubleshooting guidance (ABI mismatches, `LD_LIBRARY_PATH` issues, CPU vs CUDA builds).

## 4. Execution Order
1. Finish libtorch provisioning and env scripts (Sections 3.1–3.2).
2. Wire up CMake and confirm a stub `torchBridge` target links successfully (Section 3.3).
3. Implement the C++ bridge + Erlang workers (Sections 3.4–3.5) iteratively, validating with unit tests as features land.
4. Build out the Torch-specific test harnesses and add them to CI (Section 3.6).
5. Polish documentation and support materials (Section 3.7).

## 5. Open Questions / Follow-ups
- Preferred libtorch release cadence and whether to pin to a specific SHA.
- CPU-only for MVP, but what CUDA architectures need first-class support?
- Should `torch_env.sh` integrate with existing planner scripts automatically or remain opt-in?
- How to share tensor serialization logic between ONN and Torch bridges to avoid code duplication?
