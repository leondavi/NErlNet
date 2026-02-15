# AGENTS.md - NErlNet context

## Repo layout (key dirs)
- `src_py/apiServer/`: Python API server (experiment orchestration, JSON parsing, stats).
- `src_erl/NerlnetApp/`: Erlang runtime (Main Server, entities, routing, NIF bridges).
- `src_cpp/`: C++ bridges (OpenNN + Torch + Source NIFs).
- `web/nerl-planner/`: React/Vite web planner (JSON authoring + Torch export tooling).
- `tests/`: NIF + full-flow integration tests (shell scripts + python runner).
- `config/`: Runtime config pointers (`jsonsDir.nerlconfig`, `subnets.nerlconfig`).

## Pipeline overview
1) API Server (Python) loads DC + Conn + Exp JSONs, builds `NetworkComponents`, and orchestrates phases.
2) Main Server + entities (sources/routers/clients/workers) run on devices; routers forward data, sources emit batches, clients host workers.
3) Training phase runs, then prediction phase runs; stats are collected per phase.

## JSONs (shape + purpose)
- Distributed config (DC, `dc_*.json`):
  - `nerlnetSettings` (frequency, batchSize)
  - `mainServer`/`apiServer` (port, args)
  - `devices` (name, ipv4, entities CSV string)
  - `routers`, `sources`, `clients`
  - `workers` list (name + model_sha)
  - `model_sha` map (sha -> model payload)
- Connection map (`conn_*.json`):
  - `connectionsMap` dict `{entity: [neighbors...]}`; parser adds bidirectional edges.
- Experiment flow (`exp_*.json`):
  - `experimentName`, `experimentType`, `batchSize`, `csvFilePath`, `numOfFeatures`, `numOfLabels`, `headersNames`
  - `Phases`: list of `{phaseName, phaseType, sourcePieces}`
  - `sourcePieces`: `{sourceName, startingSample, numOfBatches, workers, nerltensorType}`
- Torch model payload (inside DC `model_sha` map):
  - `infraType: "torch"`, `pt_path`, `pt_format`, `pt_checksum`, `pt_description`
  - `train_params` (lr, epochs, optimizer, loss, input_tensor_shape, labels_shape, labels_offset, w_init_rand, batch_size)
- OpenNN model payload (inside DC `model_sha` map):
  - `modelType`, `modelArgs`, `layersSizes`, `layerTypesList`, `layers_functions`, `lossMethod`, `lr`, `epochs`, `optimizer`, `infraType`, `distributedSystem*`.

## Runtime constraints (from pipeline code)
- Entity names unique; `mainServer`/`apiServer` reserved.
- Ports must be 1..65535; IPv4 must be valid.
- Each entity assigned to exactly one device; `mainServer` must appear in a device.
- No duplicate ports within the same device.
- Connection map must connect all entities (strongly connected after bidirectional edges).
- Clients should have non-empty worker lists; all client workers must exist in DC `workers`.
- Experiment flow: `experimentType` required; `phaseName` unique; `phaseType` in {training,prediction}.
- Exp `batchSize` must equal DC `batchSize` (asserted in API server).
- CSV must exist and be `.csv`; columns must equal `numOfFeatures + numOfLabels`.
- `startingSample >= 0`, `numOfBatches >= 0`, and `startingSample + numOfBatches*batchSize` must fit CSV samples.
- `nerltensorType` must be one of `float | int16 | int32 | double | uint8`.
- Distributed systems: if `distributedSystemType > 0`, `distributedSystemToken` length must be 5 (not "none").
- Torch: `pt_path` must resolve to an existing file; if `pt_checksum` not `placeholder/none`, checksum must match.
- Torch train params must include keys: `model_path` (auto), `lr`, `epochs`, `optimizer`, `loss`, `input_tensor_shape`, `labels_shape`, `labels_offset`.
- Torch worker effectively supports `adam` or `sgd` optimizers; loss is fixed to MSE in the runtime.

## Web planner (web/nerl-planner)
- `NerlnetPlanner.sh` only runs the planner dev server (`npm install`, `npm run dev -- --open`).
- App views: `SandboxView` (topology + devices), `ModelLabView` (OpenNN + Torch graph), `ExperimentFlowView`, `ExportView`.
  - `SandboxView` is the single topology editor used for connection map export.
- `ModelLabView` uses a unified Model Graph for Torch + OpenNN; layers are added via right/double-click context menus and OpenNN edges are sequential.
- Export utilities: `buildDistributedConfig`, `buildConnectionMap`, `buildExperimentFlow`.
- Planner validation rules live in `web/nerl-planner/src/utils/validation.ts` and surface through `ValidationPanel` in each view.
- Vite dev APIs: `/api/scan` (network scan), `/api/torch/export|import|models`.
  - Torch export uses `web/nerl-planner/scripts/torch_export.py` and saves models under `nerl_designer_models/`.

## Documentation verification
- When referencing external library behavior or usage, always verify current docs online before relying on prior knowledge.

## Tests / CI
- `.github/workflows/pr.yml` runs install, build, NIF tests, and full-flow tests (incl. Torch).
- Full-flow tests use `tests/inputJsonsFiles` and `tests/inputTorchJsonsFiles` with `src_py/apiServer/experiment_flow_test.py`.
- `tests/NerlnetFullFlowTorchTest.sh` generates a TorchScript model and runs the pipeline end-to-end.
- `tests/NerlnetFullFlowTorchLocalDebug.sh` runs a local Torch flow against supplied JSONs and prints verbose logs via `src_py/apiServer/experiment_flow_local_debug.py`.
