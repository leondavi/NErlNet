# Libtorch Capability Completion Steps

This checklist expands on the torch bridge plan with file-level tasks required to make libtorch a first-class runtime in Nerlnet. Each step references the current parsing/worker code so the Torch path mirrors the existing OpenNN flow.

## 1. Provision Libtorch Artifacts
1. Extend `NerlnetInstall.sh` so `--torch` downloads an official libtorch archive (CPU by default) into `/usr/local/lib/nerlnet-lib/libtorch/<version>` and emits `build/torch_env.sh` describing `TORCH_ROOT`, `TORCH_INCLUDE`, `TORCH_LIB`, and ABI flags.
2. Allow overrides via env (`NERLNET_TORCH_URL`, `NERLNET_TORCH_VARIANT`) to accommodate CUDA builds later.
3. Add a verification step that hashes the archive and stores the checksum in `build/torch_env.sh` for reproducibility audits.

## 2. Build & CMake Plumbing
1. Teach `NerlnetBuild.sh` to source `build/torch_env.sh` whenever `--torch` is passed, fail fast if it is missing, and forward `-DNERLTORCH=ON -DTORCH_ROOT=... -DCMAKE_PREFIX_PATH=$TORCH_ROOT/share/cmake/Torch` to CMake.
2. In the root `CMakeLists.txt`, guard `add_subdirectory(src_cpp/torchBridge)` behind `NERLTORCH`, append `${TORCH_ROOT}` to `CMAKE_PREFIX_PATH`, and call `find_package(Torch REQUIRED)`.
3. Add a `torchBridge` target inside `src_cpp/CMakeLists.txt` that links against existing utilities (`common`, `simple-cpp-logger`) plus `${TORCH_LIBRARIES}` and exports a `torch_nif` shared library analogous to `opennnBridge`.
4. Capture ABI flags (`-D_GLIBCXX_USE_CXX11_ABI=1` etc.) derived from `torch_env.sh` to avoid runtime mismatches.

## 3. C++ Torch Bridge Implementation
1. Populate `src_cpp/torchBridge/` with:
   - `TensorCodec.{h,cpp}` for binary ↔ `torch::Tensor` conversions mirroring Erlang tensor layout.
   - `NerlWorkerTorch.{h,cpp}` mirroring `opennnBridge/NerlWorkerOpenNN` APIs (train, predict, checkpoint, distributed hooks).
   - `torchNIF.cpp` registering NIFs (`train`, `predict`, `get_weights`, `set_weights`, optimizer utilities) via `nifpp`.
2. Reference SynapNIF only for architectural ideas; reimplement actual logic locally so dependencies remain inside this repo.
3. Ensure cleanup semantics (RAII wrappers for `enif_resource`) match the Erlang expectations illustrated in `opennnBridge`.
4. Add focused unit tests under `src_cpp/torchBridge/tests` (if available) or reuse existing GoogleTest harness to validate tensor marshaling and training loops.

## 4. Erlang Worker Integration (Reusing Current JSON Parsing)
1. Continue to rely on `jsonParser:get_models/1` (`src_erl/NerlnetApp/src/Init/jsonParser.erl`) and the auto-generated macros in `dc_definitions_ag.hrl` to pull worker descriptors from distributed config JSON. This function already extracts `InfraType`, optimizer, and distributed metadata; no new parsing layer should be added for Torch.
2. Introduce `torchWorkers` modules under `src_erl/NerlnetApp/src/Bridge/` that conform to the same callback contract as `onnWorkers`, dispatching to the new `torch_nif` exported functions.
3. Update any dispatch points (e.g., worker supervisors or router handlers) to branch on the existing `InfraType` string that `get_models/1` already injects into the ETS map. Torch-specific logic should key off the same values used by OpenNN (see `?WORKER_FIELD_KEY_INFRA_TYPE_BIN` in `worker_definitions_ag.hrl`).
4. Where the system today routes by worker model SHA (see `get_device_clients/3` and ETS population of `sha_to_models_map`), ensure the Torch workers reuse that map with no schema changes so planners do not distinguish between ONN and Torch models.

## 5. Planner & JSON Alignment
1. Keep using the planner-side JSON builders in `src_py/nerlPlanner/JsonDistributedConfig.py` and the enum tables in `JsonElementsDefinitions.py`. Torch support should therefore appear as:
   - A new entry in `InfraTypeMapping` (consumed by `ErlHeadersExporter.py` and `CppHeadersExporter.py`).
   - Optional Torch-specific defaults in `Worker` objects before they are serialized (learning rate, optimizer, etc.).
2. Because `JsonDistributedConfig.export_dc_json()` already writes `(worker_name, model_sha)` pairs plus a SHA → model dict, extend `Worker.get_as_dict()` (planner side) so Torch-only hyperparameters are embedded, but leave the envelope identical. This ensures `jsonParser:get_models/1` keeps working.
3. Validate that the import path (`import_dc_json`) still succeeds by loading a Torch-enabled JSON and confirming it round-trips through the planner and the Erlang parser without new keys.
4. Document the InfraType → implementation mapping (e.g., `"torch": torchBridge`, `"onn": opennnBridge`) in `agent_plans` and propagate to user documentation.

## 6. Test Harnesses & CI
1. `tests/NerlnetNIFTorchTest.sh` replicates the OpenNN harness for Torch by sourcing `build/torch_env.sh`, staging `torchWorkers`, compiling `nerlTorchNIF`, and invoking `torchTests:run_tests/0`.
2. `tests/NerlnetFullFlowTorchTest.sh` mirrors `NerlnetFullFlowTest.sh` while swapping in the Torch JSON suite (see `tests/inputTorchJsonsFiles/`).
3. Wire both scripts into CI with a toggle so that libtorch steps only execute when the toolchain is available, and expose knobs for CPU vs CUDA libtorch downloads.
4. Expand the smoke-test corpus (`tests/inputTorchJsonsFiles/` plus future `examples/`) so Torch InfraTypes flow through the planner/JSON pipeline using the established `model_sha` contract.

## 7. Documentation & Operational Notes
1. Update `README.md`, `docs/CONTRIBUTING.md`, and any runbooks to explain how to enable Torch (`NerlnetInstall.sh --torch`, `NerlnetBuild.sh --torch`, env sourcing before tests).
2. Add troubleshooting guidance for ABI mismatches, missing `LD_LIBRARY_PATH`, and differences between CPU/CUDA torch builds.
3. Capture the InfraType + worker model parsing expectations (referencing `jsonParser.erl` and planner JSON exporters) in `docs/` so future bridge implementations reuse the same pipeline.

## 8. Python Control Plane Plumbing
1. `networkComponents.py` eagerly resolves Torch `pt_path` entries, verifies checksums, and maps each SHA to both the local artifact and the remote upload target built via `build_torch_remote_model_path/1`.
2. `ApiServer.send_jsons_to_devices()` rewrites the distributed config payload so Torch workers reference `/tmp/nerlnet/jsons/torch_model_<sha>.pt` and attaches the corresponding `.pt` binaries in the multipart POST to `/sendJsons`.
3. The new helpers (`build_torch_remote_model_path`, `_prepare_dc_stream`, `_build_torch_file_payloads`) ensure future CI runs can swap in CUDA/CPU artifacts without touching the JSONs.
