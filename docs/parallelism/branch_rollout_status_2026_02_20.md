# NErlNet Parallelism Branch Report (Super Node + PP/TP)

Date: 2026-02-25
Branch: `parallelism-project`
Repository: `NErlNet`

## 1) Executive Summary

This branch introduces a first-class Super Node control-plane and parallelism schema/extensions across runtime, API server, Torch bridge, test suite, CI, Docker validation, and planner UI.

Implemented and working today:
- Optional `superNodes` entity in distributed config, client-to-super assignment, worker parallel metadata, and model `tpPlan` parsing/validation.
- Dual communication path:
  - `legacy` mode keeps existing worker-to-worker behavior.
  - Non-legacy modes route worker traffic through Super Node (`Client -> Super Node -> Client`).
- Super Node runtime process with:
  - client registration
  - heartbeat tracking
  - parallel phase metadata updates
  - Super Node-to-client parallel command channel (`/parallelSuperCommand`)
  - Super Node-issued scheduler grants for pipeline events
  - deterministic abort signaling (`parallelAbort`)
  - scheduler trace validation + grant/event consistency checks (GPipe / 1F1B / Interleaved contract checking).
- Main Server non-legacy routing now treats Super Node as authority:
  - Main Server no longer directly fans out non-legacy `parallelMode`/`parallelExecution`.
  - Clients receive non-legacy config from Super Node command path.
- API-server preflight now validates connection-map connectivity (including Super Node reachability) before experiment initialization.
- Super hierarchy validation is strict:
  - any `clients[].superNode` assignment must match the owning super node `managedClients` list.
- Worker runtime now enforces non-legacy authority contracts:
  - no legacy fallback in microbatch loss handling
  - scheduler-grant consumption required before pipeline forward/backward events
  - explicit worker-side abort signaling on authority/grant violations
- Torch microbatch and optimizer barrier surfaces added in C++/NIF and wired through Erlang worker runtime.
- True stage-sliced pipeline runtime for `parallelExecution.mode=pipeline`:
  - each worker executes only its assigned stage-layer partition
  - stage workers exchange forward activations and backward gradients through Super Node-routed worker messaging
  - optimizer step remains barriered and deterministic per batch
- Pipeline mode layout guard:
  - parser now rejects multi-worker-per-stage layouts in `mode=pipeline` (use `mode=pipeline_tensor` for multi-worker stages).
- Stage0 prediction input normalization:
  - Torch stage0 predict path accepts both feature-only and feature+label-span microbatches without span-mismatch aborts.
- Planner UI and import/export support for `superNodes`, worker parallel fields, phase `parallelExecution`, and model `tpPlan`.
- New tests for schema, scheduler contracts, comm-path contract, abort mapping, and torch NIF contracts.
- Dockerized CPU Torch validation that runs full Torch flow and compares PTD POC loss invariants.
- `pipeline_tensor` runtime now executes end-to-end in Docker smoke and repeated-run soak (2 consecutive runs), with no TP collective timeout.
- Super Node / Client / Worker logs now expose orchestration events (config push, scheduler grants, event acks, routed worker messages) for runtime traceability.

---

## 2) Theoretical Design: New Components and Approach

### 2.1 Design shift: control-plane hierarchy

Target architecture introduced by this branch:
- `Super Node -> Clients -> Workers`
- Parallel modes move away from direct worker-to-worker network routing and toward centralized orchestration and policy enforcement.

Why this design:
- Deterministic behavior for scheduling and abort semantics.
- Better observability and fault isolation at phase level.
- Extensible control point for scheduler plugins and collective coordination.

### 2.2 Dual-path compatibility model

Two explicit execution paths:
- `legacy`: existing behavior and protocols remain unchanged.
- `pipeline`, `tensor`, `pipeline_tensor`: traffic uses Super Node orchestration path.

This allows incremental rollout without breaking old experiments.

### 2.3 Pipeline scheduling contract model

Schedulers supported at contract level:
- `gpipe`
- `1f1b`
- `interleaved`

Runtime side tracks a deterministic event trace and validates received tagged events (`forward/backward`, microbatch, stage). On mismatch, it emits fail-fast abort.

### 2.4 Tensor-parallel model in this branch

TP is explicit-by-config:
- Worker metadata: `tpGroup`, `tpRank`, `tpWorldSize`
- Model metadata: `tpPlan` entries (`layer`, `mode`, `shardAxis`, `group`)

Current state:
- schema validation and compatibility checks implemented.
- runtime TP plan execution implemented in worker microbatch flow.
- TP collectives are orchestrated per `tpPlan` entry using deterministic tokens (`batch`, `microbatch`, `layer`, `mode`).
- non-legacy workers now defer incoming samples while waiting, preventing TP peer desynchronization across batches.

---

## 3) Technical Changes by Priority (Most Important First)

## P0: Core Runtime Path and Failure Control

### P0.1 Super Node runtime process and HTTP handler
- Added:
  - `src_erl/NerlnetApp/src/SuperNode/superNodeGenserver.erl`
  - `src_erl/NerlnetApp/src/SuperNode/superNodeHandler.erl`

Key behavior implemented:
- Maintains managed client set and worker->client mapping.
- Accepts client registration and heartbeat events.
- Receives and forwards `parallel_worker_message` to destination client via `/parallelDeliver` endpoint.
- Receives phase updates and constructs scheduler trace from phase config and worker metadata.
- Validates optional tagged parallel events against scheduler trace.
- Emits `parallelAbort` to Main Server on route failures, heartbeat issues, unknown workers, and schedule mismatches.
- Emits detailed runtime logs for phase updates, scheduler grants, scheduler acks, and worker-message routing.

### P0.2 Main server parallel orchestration hooks and abort handling
- Modified:
  - `src_erl/NerlnetApp/src/MainServer/mainGenserver.erl`
  - `src_erl/NerlnetApp/src/MainServer/actionHandler.erl`

Key changes:
- `clientsPhaseUpdate` now accepts plain phase string (legacy) or JSON payload with `parallelExecution`.
- Extracts mode/superNode and routes non-legacy phase metadata to Super Node via `/parallelPhaseUpdate`.
- Keeps direct client `parallelMode`/`parallelExecution` fanout only for legacy/reset paths.
- Non-legacy configuration authority is delegated to Super Node command fanout.
- Handles `/parallelAbort` with fail-fast transition to idle and ACK back to API server.

### P0.3 Client statem cutover and terminal delivery path
- Modified:
  - `src_erl/NerlnetApp/src/Client/clientStatem.erl`
  - `src_erl/NerlnetApp/src/Client/clientStateHandler.erl`

Key changes:
- Stores per-client `parallel_mode`, `parallel_execution`, `parallel_authority`, `super_node` in ETS.
- New handler endpoints:
  - `/parallelMode`
  - `/parallelExecution`
  - `/parallelSuperCommand`
  - `/parallelDeliver`
- In legacy mode: keep previous worker-to-worker path.
- In non-legacy mode:
  - outbound worker messages route to Super Node (`/parallelWorkerMessage`)
  - inbound terminal delivery path uses local `parallel_deliver` handler to deliver directly to target worker (no reroute loop)
- Super Node commands can:
  - configure non-legacy mode/execution at client and worker scope
  - grant scheduler events to target workers (`parallel_scheduler_grant`)
- Emits `parallel_event` and `parallelAbort` notifications.
- Adds super-node registration + periodic heartbeat loop from client.
- Emits detailed logs for Super Node command reception, mode/execution authority updates, scheduler-grant forwarding, and event forwarding to Super Node.

### P0.4 App wiring for new runtime entity and routes
- Modified:
  - `src_erl/NerlnetApp/src/nerlnetApp_app.erl`

Key changes:
- Adds Super Node listener startup for local device super-node entities.
- Adds new main server route `/parallelAbort`.
- Adds new client routes (`/parallelMode`, `/parallelExecution`, `/parallelDeliver`).

### P0.5 Runtime message safety hardening
- Modified:
  - `src_erl/NerlnetApp/src/Router/routingHandler.erl`
  - `src_erl/NerlnetApp/src/Source/castingHandler.erl`

Key change:
- Replaced unsafe direct `binary_to_term` patterns with guarded `binary_to_term(..., [safe])` wrapper usage.

---

## P1: Schema, Parsing, Validation, and API-Server Control

### P1.1 Distributed config fields and autogenerated definitions
- Modified:
  - `src_py/autogen/JsonDistributedConfigDefs.py`
  - `src_py/nerlPlanner/JsonDistributedConfigDefs.py`
  - `src_py/autogen/ErlHeadersExporter.py`
  - `src_py/nerlPlanner/ErlHeadersExporter.py`
  - `src_erl/NerlnetApp/src/dc_definitions_ag.hrl`

Added keys:
- `superNodes`, `managedClients`, `heartbeatMs`, `maxInflightMicrobatches`
- `clients[].superNode`
- `workers[].parallel` fields (`pipelineStage`, `pipelineWorldSize`, `tpGroup`, `tpRank`, `tpWorldSize`)
- `model_sha[*].tpPlan`

### P1.2 Erlang JSON parser support for superNodes and worker parallel metadata
- Modified:
  - `src_erl/NerlnetApp/src/Init/jsonParser.erl`

Key changes:
- Parses `superNodes` map and per-device super nodes.
- Parses worker parallel metadata into ETS (`workers_parallel`).
- Extends graph vertex creation to include super nodes.
- Updates client payloads to include assigned super node at startup.

### P1.3 Python `NetworkComponents` super/parallel/TP validations
- Modified:
  - `src_py/apiServer/networkComponents.py`

Implemented validations:
- super node uniqueness, assignment to exactly one device, managed clients existence.
- client->super consistency.
- strict client ownership parity: every `clients[].superNode` assignment must appear in that super node's `managedClients`.
- worker parallel integrity:
  - stage/world pair consistency
  - rank/world consistency
  - TP group world/rank uniqueness and completeness
- pipeline stage coverage validation across world size.
- TP group cannot span multiple pipeline stages.
- `tpPlan` shape validation (`layer`, `mode in {column,row}`, group presence).

Additional behavior:
- Extracts torch model assets with path resolution and checksum validation.
- Exposes `validate_connection_map(...)` to enforce one connected runtime graph (after bidirectional completion) and explicit Super Node presence in topology edges.

### P1.4 Experiment flow parallelExecution parsing and sanity checks
- Modified:
  - `src_py/apiServer/experiment_flow_defs.py`
  - `src_py/apiServer/experiment_flow.py`
  - `src_py/apiServer/experiment_phase.py`

New phase-level fields parsed:
- `parallelExecution.mode`
- `superNode`
- `scheduler`
- `microBatchSize`
- `numMicroBatches`
- `virtualStages`

Checks enforced:
- non-legacy requires valid super node.
- pipeline modes require valid scheduler + positive microbatch params.
- `microBatchSize * numMicroBatches == phase batchSize`.
- pipeline mode requires pipeline worker metadata and stage world >= 2.
- pipeline mode currently enforces exactly one worker per pipeline stage (for that phase target set); multi-worker stages must use `mode=pipeline_tensor`.
- tensor modes require TP metadata.

### P1.5 API transmitter + event sync
- Modified:
  - `src_py/apiServer/transmitter.py`
  - `src_py/apiServer/events_sync.py`
  - `src_py/apiServer/apiServer.py`

Key changes:
- `clients_set_phase` now sends JSON payload for non-legacy phases.
- `parallel_abort` mapped into `MAIN_SERVER_ERROR` fail-fast path.
- `ApiServer.initialization(...)` now performs connection-map preflight validation through `NetworkComponents.validate_connection_map(...)`.

---

## P2: Torch Bridge and Worker Runtime for True Stage Pipeline

### P2.1 Torch C++ worker stage partition + stage forward/backward APIs
- Modified:
  - `src_cpp/torchBridge/NerlWorkerTorch.h`
  - `src_cpp/torchBridge/NerlWorkerTorch.cpp`

New API/behavior:
- `train_microbatch(batch, microbatch_id)`
- `optimizer_barrier()`
- `pipeline_stage0_forward(batch, microbatch_id)`
- `pipeline_stage_forward(activation, labels, microbatch_id)`
- `pipeline_stage_last_forward_backward(activation, labels, microbatch_id)`
- `pipeline_stage_backward(grad_output, microbatch_id)`
- `pipeline_predict_stage0_forward(batch)`
- `pipeline_predict_stage_forward(activation)`
- internal deferred gradient accumulation state (`_has_deferred_gradients`, `_deferred_microbatch_count`).
- per-microbatch stage activation cache for delayed backward (`_pipeline_stage_contexts`).
- stage-layer partition computed from worker `pipeline_stage/pipeline_world_size` metadata.

Semantics:
- stage workers hold only local stage-layer execution responsibility for `pipeline` mode.
- forward activations and labels are routed stage-to-stage; backward gradients flow from last stage to first stage.
- optimizer step is deferred until explicit barrier after deterministic batch completion.
- stage0 prediction normalizes both input forms:
  - feature+label-span rows (split and keep feature slice), and
  - feature-only rows (reshape by input span).
- non-microbatch path remains immediate train-step behavior.

### P2.2 Torch NIF API extensions for stage execution
- Modified:
  - `src_cpp/torchBridge/torchNIF.h`
  - `src_cpp/torchBridge/torchNIF.cpp`

Added NIF functions:
- `train_microbatch_nif/4`
- `optimizer_barrier_nif/1`
- `pipeline_stage0_forward_nif/4`
- `pipeline_stage_forward_nif/6`
- `pipeline_stage_last_forward_backward_nif/6`
- `pipeline_stage_backward_nif/4`
- `pipeline_predict_stage0_forward_nif/3`
- `pipeline_predict_stage_forward_nif/3`

Train threaded return now supports microbatch metadata tuple when needed.

### P2.3 Erlang bridge/runtime wiring for stage-sliced path
- Modified:
  - `src_erl/NerlnetApp/src/Bridge/torchWorkers/nerlTorchNIF.erl`
  - `src_erl/NerlnetApp/src/Bridge/onnWorkers/nerlNIF.erl`
  - `src_erl/NerlnetApp/src/Bridge/onnWorkers/workerGeneric.erl`
  - `src_erl/NerlnetApp/src/Bridge/Common/w2wCom.erl`

Key changes:
- Train negotiator supports `start_train_microbatch` and `optimizer_barrier` messages.
- Torch bridge exports stage-call wrappers used by worker runtime (`call_to_pipeline_stage*`).
- Worker runtime adds stage payload buffers and stage dispatch flow:
  - `pipeline_forward_payload`
  - `pipeline_backward_payload`
  - `pipeline_predict_payload`
- `w2wCom` now notifies worker state machines immediately for pipeline payload tags via `{parallel_pipeline_inbox,...}`.
- Worker generic tracks stage batch contexts (`forward_completed`, `backward_completed`, `predict_acc`) and aggregates last-stage outputs.
- Emits scheduler-gated parallel events with stage and microbatch id (forward-only for `pipeline_tensor`, forward+backward for pure pipeline modes).
- Calls optimizer barrier once all microbatches for a batch complete.
- Buffers non-legacy `sample` messages received during `wait` state and dispatches them after batch completion.
- OpenNN bridge provides compatibility stubs for microbatch/barrier APIs.

### P2.4 TP primitive helpers in Erlang bridge
- Added in `nerlTorchNIF.erl`:
- `nerltensor_split_nif/4`
- `nerltensor_concat_nif/3`
- `nerltensor_reduce_sum_list_nif/2`

Status:
- primitives exist and are test-covered at contract level.
- `tpPlan` runtime collectives are wired into non-legacy microbatch execution (`tensor`/`pipeline_tensor`).
- collective orchestration now stays batch-aligned under rapid source casting due deferred-sample queueing.

### P2.5 Build/link hardening for Torch bridge
- Modified:
  - `CMakeLists.txt`
  - `src_cpp/torchBridge/CMakeLists.txt`

Key changes:
- torch bridge link dependencies made private where appropriate.
- explicit link option added for `nerlnet_torch` (`--no-as-needed`) to avoid runtime symbol linkage issues.

---

## P3: Planner UI, Import/Export, and Validation

### P3.1 Planner state/types support
- Modified:
  - `web/nerl-planner/src/data/types.ts`
  - `web/nerl-planner/src/data/defaults.ts`
  - `web/nerl-planner/src/App.tsx`

Added types:
- `SuperNode`
- `WorkerParallelConfig`
- `ParallelExecutionConfig`
- `TpPlanEntry`

### P3.2 Sandbox UI support for super hierarchy + worker parallel config
- Modified:
  - `web/nerl-planner/src/components/SandboxView.tsx`
  - `web/nerl-planner/src/components/EntityNode.tsx`

Implemented:
- Add/remove/edit Super Nodes.
- Assign clients to super nodes and managed-client lists.
- Edit super-node heartbeat and max-inflight fields.
- Edit worker PP/TP fields per worker inside client worker panel.

### P3.3 Experiment flow UI parallel controls
- Modified:
  - `web/nerl-planner/src/components/ExperimentFlowView.tsx`

Implemented:
- phase-level `parallelExecution` editor:
  - mode
  - scheduler
  - super node
  - micro batch size
  - num microbatches
  - virtual stages
- `nerltensorType` surfaced in source-piece editor.

### P3.4 Model Lab TP plan editor
- Modified:
  - `web/nerl-planner/src/components/ModelLabView.tsx`

Implemented:
- explicit TP plan editor (`layer`, `mode`, `shardAxis`, `group`) for both Torch/OpenNN model metadata.

### P3.5 Import/export + validation support
- Modified:
  - `web/nerl-planner/src/utils/exporters.ts`
  - `web/nerl-planner/src/utils/importers.ts`
  - `web/nerl-planner/src/utils/validation.ts`

Implemented:
- DC export/import includes super nodes and worker parallel fields.
- Exp export/import includes `parallelExecution` block.
- Model payload import/export includes `tpPlan`.
- Validation rules for super hierarchy, parallel fields, scheduler params, TP plan entries, and nerltensor type.

---

## P4: Tests, CI, and Docker Validation Tooling

### P4.1 New parallelism test suite
- Added:
  - `tests/parallelism/test_poc_schedule_trace.py`
  - `tests/parallelism/test_scheduler_conformance.py`
  - `tests/parallelism/test_parallel_schema.py`
  - `tests/parallelism/test_supernode_comm_contract.py`
  - `tests/parallelism/test_api_server_parallel_contract.py`
  - `tests/parallelism/test_eventsync_abort.py`
  - `tests/parallelism/test_failure_abort_contract.py`
  - `tests/parallelism/test_parallel_soak.py`
  - `tests/parallelism/test_torch_parallel_nif_contract.py`
  - `tests/parallelism/test_pipeline_stage_execution_contract.py`
  - `tests/parallelism/compare_ptd_poc_losses.py`

### P4.2 POC grounding artifacts
- Added:
  - `PTD_P_POC/PTD_P_Tests.py`
  - `PTD_P_POC/PTD_P_Tests.log`
  - `docs/parallelism/poc_schedule_reference.md`
  - `docs/parallelism/scheduler_execution_contract.md`
  - `docs/parallelism/super_node_schema.md`

### P4.3 CI updates
- Modified:
  - `.github/workflows/pr.yml`

Added explicit parallelism test execution in CI before full build.

### P4.4 Docker CPU Torch validation flow
- Added:
  - `docker/ubuntu-torch-cpu/Dockerfile`
  - `docker/ubuntu-torch-cpu/build_and_run.sh`
  - `docker/ubuntu-torch-cpu/run_validation.sh`
  - `docker/ubuntu-torch-cpu/README.md`
  - `.dockerignore`

Validation performed:
- full Torch flow test run inside Ubuntu container.
- PTD POC loss comparison report generated.

### P4.5 Torch full-flow test determinism fix
- Modified:
  - `src_py/apiServer/experiment_flow_test.py`

Fix:
- replaced brittle `setJsons(0,0,0)` assumption with deterministic filename-based selection to avoid picking the wrong `exp_*.json` when parallel fixtures are present.

### P4.6 Legacy full-flow IPv4 detection hardening
- Modified:
  - `tests/NerlnetFullFlowTest.sh`

Fix:
- replaced fragile IP probing with the same guarded IPv4 detection logic used in the Torch full-flow script:
  - ignore loopback and `0.0.0.0`
  - fallback to `hostname -I`
  - fail fast with explicit error if no usable IPv4 is found.

---

## 4) Verification Status (Latest Run)

Run date: 2026-02-26

Executed and passed (this implementation step, local host):
- `erlc src_erl/NerlnetApp/src/Bridge/Common/w2wCom.erl`
- `erlc src_erl/NerlnetApp/src/SuperNode/superNodeGenserver.erl`
- `erlc src_erl/NerlnetApp/src/Bridge/onnWorkers/workerGeneric.erl`
- `erlc src_erl/NerlnetApp/src/Bridge/torchWorkers/nerlTorchNIF.erl`
- `python3 -m unittest tests.parallelism.test_pipeline_stage_execution_contract tests.parallelism.test_supernode_comm_contract tests.parallelism.test_torch_parallel_nif_contract`
- `python3 -m unittest discover -s tests/parallelism -p 'test_*.py'` (54 tests)

Executed and attempted (local C++ configure/build sanity):
- `cmake -S . -B .build-codex` failed on host due missing Torch CMake package (`TorchConfig.cmake`).
- Interpretation: Erlang/runtime contract tests and Erlang compile checks passed; native Torch C++ build must be validated in an environment with libtorch/Torch CMake config installed (Docker/Linux matrix already covers this in prior runs).

Executed and passed (Docker CPU Torch validation):
- `docker/ubuntu-torch-cpu/build_and_run.sh`
- Artifacts:
  - `.docker-artifacts/torch-cpu-validation/NerlnetFullFlowTorchTest.log`
  - `.docker-artifacts/torch-cpu-validation/PTD_P_Tests.current.log`
  - `.docker-artifacts/torch-cpu-validation/PTD_P_loss_compare.json`
- PTD POC parity:
  - `matched: true`
  - `GPipe: 2404.0`
  - `1F1B: 2404.0`
  - `Interleaved: 2404.0`
  - absolute deltas `0.0` for all modes.
- Includes Linux Torch rebuild after stage-sliced pipeline additions and prediction-input normalization fix.

Executed and passed (Docker, pure `pipeline` smoke with true stage slicing):
- `tests/NerlnetFullFlowTorchLocalDebug.sh` using a 2-worker, 2-stage phase target set (stage0=`w1`, stage1=`w3`) derived from:
  - `tests/inputTorchJsonsFiles/parallel_smoke/dc_torch_parallel_smoke.json.noip`
  - `tests/inputTorchJsonsFiles/parallel_smoke/conn_torch_parallel_smoke.json`
  - `tests/inputTorchJsonsFiles/parallel_smoke/exp_torch_parallel_smoke.json` (reduced to 5 batches)
- Result: `RC=0`, training + prediction completed successfully with Super Node grant/event orchestration logs.
- Verified fix: prediction no longer aborts on stage0 input span mismatch (`batch elements not divisible by sample span`).

Executed and passed (Docker, `pipeline_tensor` TP smoke):
- `tests/NerlnetFullFlowTorchLocalDebug.sh` with:
  - `tests/inputTorchJsonsFiles/parallel_smoke/dc_torch_parallel_smoke.json.noip`
  - `tests/inputTorchJsonsFiles/parallel_smoke/conn_torch_parallel_smoke.json`
  - `tests/inputTorchJsonsFiles/parallel_smoke/exp_torch_pipeline_tensor_smoke.json`
- Result: `RC=0`, training + prediction completed, no `parallelAbort`.
- Result remains stable after prediction-input fix and after enforcing pipeline-mode worker-layout validation in parser.

Executed and passed (schema/runtime contract hardening):
- New parser guard: `mode=pipeline` now rejects multi-worker-per-stage layouts for the phase target set (must use `mode=pipeline_tensor` for that topology).
- Covered by unit test: `tests/parallelism/test_parallel_schema.py::test_pipeline_mode_rejects_multi_worker_stage_layout`.

---

## 5) What Must Be Checked Next (Priority-Ordered)

## P0 (Blockers before production multi-device rollout)

1. Real multi-device distributed run with Super Node control path.
- Validate behavior across at least 3 machines (separate super/client/router placement).
- Validate NAT/firewall and route resilience.

2. Heartbeat robustness under real network jitter.
- Phase-aware heartbeat gating is implemented.
- Remaining work: validate behavior with clock skew/jitter and long GC pauses across real multi-device runs.

3. Security hardening for atom conversion in network handlers.
- Review/replace unsafe atom creation from external payloads in phase parsing paths.
- Use strict whitelisting or `binary_to_existing_atom` with fallback handling.

4. Deterministic abort cleanup guarantees.
- Ensure source casting shutdown, client worker state reset, and no dangling microbatch contexts after abort.

## P1 (Correctness and semantic hardening)

5. Extend non-legacy soak coverage.
- Increase run count and microbatch cardinality for `pipeline_tensor` and `tensor` modes.
- Add queue-growth assertions for deferred sample buffering and W2W inboxes.

6. Add numerical parity assertions for TP paths.
- Compare fixed-seed baseline vs `tensor` and `pipeline_tensor` on toy Torch models.
- Define tolerances once and enforce in CI.

7. Validate cross-device latency tolerance.
- Run with real multi-device topology and induced jitter to verify collective timeout settings.

## P2 (Observability and robustness)

8. Add parallel observability metrics end-to-end.
- Required: scheduler type, per-microbatch latency, stage utilization, collective latency, abort codes.
- Integrate into phase stats output and planner/export visibility.

9. Soak and leak testing.
- Long-duration tests with high microbatch counts.
- Monitor queue sizes, worker process memory, and Super Node mailbox growth.

## P3 (Planner and UX release hardening)

10. Planner round-trip and validation tests.
- Add automated UI/import-export tests for new parallel fields.
- Validate no destructive loss in import/export of PP+TP configs.

11. UX guardrails for invalid configurations.
- Disable incompatible controls contextually (example: scheduler fields for tensor-only mode).
- Add explicit “legacy vs super path” visual cue per phase.

---

## 6) Remaining Gaps vs Original Rollout Plan

Status mapping to original milestones:

- M0 (POC grounding): Mostly complete.
- M1 (schema/parser/validator): Complete for main fields.
- M2 (Super Node skeleton): Complete.
- M3 (dual-path comm cutover): Implemented with contract tests.
- M4/M5/M6 (PP GPipe/1F1B/Interleaved): Complete for current Super Node grant/event orchestration model.
- M7 (TP explicit plan): Complete for current runtime design (`tpPlan`-driven collective execution in worker microbatch path).
- M8 (combined PP+TP): Implemented and passing Docker smoke + short soak on CPU Torch.
- M9 (planner + CI hardening): Mostly complete for schema/UI/export basics; needs more automated UI and end-to-end distributed tests.

---

## 7) Stable Release Plan (Practical Next Steps)

Phase A: Runtime hardening (must-pass)
1. Validate and tune phase-aware heartbeat enforcement under real distributed timing conditions (clock skew, jitter, slow clients).
2. Secure all external payload parsing paths (atom and term safety audit).
3. Complete deterministic abort cleanup path and add failure-injection tests.
4. Add end-to-end multi-device regression test job (nightly).

Exit criteria:
- No false aborts in 100 repeated phase runs.
- Deterministic cleanup verified in worker crash/client disconnect/super timeout scenarios.

Phase B: Distributed correctness hardening
1. Validate Super Node scheduler behavior under real multi-device latency and jitter.
2. Tune collective timeout + heartbeat thresholds from measured traces.
3. Add failure-injection scenarios for worker disconnect during TP collective.

Exit criteria:
- Schedule conformance tests pass using real runtime events, not only synthetic traces.

Phase C: TP/PP+TP parity and robustness
1. Validate numeric parity vs single-worker baseline on toy and medium models.
2. Add CI gates for `tensor` and `pipeline_tensor` deterministic runs.
3. Add long-run soak and memory guard rails.

Exit criteria:
- PP, TP, PP+TP parity thresholds pass repeatedly with fixed seeds.

Phase D: Productization
1. Expand planner automated test coverage.
2. Add parallel metrics to stats reports and UI.
3. Freeze config schema, write migration notes, publish operator runbook.

Exit criteria:
- planner-authored JSONs reliably run from UI -> runtime -> metrics pipeline.

---

## 8) Under-the-Hood Execution Call Flow (Super Node Parallel Path)

This section is the explicit runtime call graph for non-legacy parallel execution (`pipeline`, `tensor`, `pipeline_tensor`) using Super Node routing.

### 8.1 Runtime Boot and Entity Wiring (Erlang side)

1. `NerlnetRun.sh` starts OTP app -> `nerlnetApp_app:start/2`.
2. `nerlnetApp_app:createNerlnetInitiator/1` opens `/sendJsons` listener.
3. `nerlnetApp_app:waitForInit/0` waits for JSON init payload.
4. `nerlnetApp_app:parseJsonAndStartNerlnet/1` calls `jsonParser:parseJsons/3`.
5. `jsonParser:json_to_ets/2` stores parsed DC/Conn entities in ETS:
   - clients/workers map
   - `workers_parallel`
   - `super_nodes`
   - `deviceSuperNodes`
6. `jsonParser:buildCommunicationGraph/2` builds digraph and validates connectivity.
7. `nerlnetApp_app:createRouters/2` -> `routerGenserver:start_link/1` + `routingHandler:init/2`.
8. `nerlnetApp_app:createSuperNodes/1` -> `superNodeGenserver:start_link/1` + `superNodeHandler:init/2` routes:
   - `/registerClient`
   - `/superHeartbeat`
   - `/parallelWorkerMessage`
   - `/parallelPhaseUpdate`
   - `/parallelEvent`
9. `nerlnetApp_app:createClientsAndWorkers/0` -> `clientStatem:start_link/1` + `clientStateHandler:init/2` routes:
   - `/parallelMode`
   - `/parallelExecution`
   - `/parallelDeliver`
   - `/batch`, `/start_stream`, `/end_stream`
10. `nerlnetApp_app:createSources/2` -> `sourceStatem:start_link/1` + `castingHandler:init/2`.
11. `nerlnetApp_app:createMainServer/4` -> `mainGenserver:start_link/1` + `actionHandler/initHandler/ackHandler` routes including:
   - `/clientsPhaseUpdate`
   - `/parallelAbort`
   - `/startCasting`
   - `/lossFunction`
   - `/predictRes`
   - `/statistics`

### 8.2 Experiment Init and Phase Orchestration (Python API side)

1. `ApiServer.initialization(...)`:
   - builds `NetworkComponents(...)`
   - creates `ExperimentFlow(...)`
   - `ExperimentFlow.parse_experiment_flow_json(...)`
   - `ExperimentFlow._parse_parallel_execution(...)` validates and normalizes parallel settings
   - starts Flask receiver (`receiver.initReceiver`)
2. `ApiServer.send_jsons_to_devices()`:
   - `Transmitter.send_jsons_to_devices(...)` -> `/sendJsons`
   - Main server receives per-device ready via `/jsonReceived`
   - `mainGenserver:handle_cast({jsonReceived,...})` -> `ack("received_jsons_done")`
   - Flask `receiver.ack.post` marks `EventSync.SEND_JSONS` as done.
3. `ApiServer.run_current_experiment_phase()` orchestrates each phase:
   - `send_data_to_sources(...)`
   - `transmitter.clients_set_phase(...)`
   - `transmitter.start_casting(...)`
   - waits by `EventSync` gates: `UPDATE_CSV`, `UPDATE_PHASE`, `START_CASTING`.

### 8.3 Phase Setup Handshake (Data + Phase + Parallel Metadata)

1. CSV/source payload path:
   - `ApiServer.send_data_to_sources`
   - `Transmitter.update_csv`
   - Main `/updateCSV` -> `initHandler:init`
   - `gen_server:cast(mainGenserver, {initCSV,...})`
   - `mainGenserver:handle_cast({initCSV,...})` routes to source `/updateCSV`
   - source `/updateCSV` -> `castingHandler:init` -> `sourceStatem:idle({batchList,...})`
   - source notifies `/dataReady`
   - `ackHandler:init(dataReady, ...)` -> `mainGenserver:handle_cast({sourceAckDataReady,...})`
   - once all sources ready: `mainGenserver:ack("update_csv_done")`
   - `receiver.ack.post` sets `EventSync.UPDATE_CSV` done.
2. Parallel phase metadata path:
   - `Transmitter.clients_set_phase(phase, parallelExecution)` posts `/clientsPhaseUpdate` JSON in non-legacy modes.
   - Main `/clientsPhaseUpdate` -> `actionHandler:init` -> `mainGenserver:handle_cast({clientsPhaseUpdate,...})`.
   - `mainGenserver:parse_phase_update_payload` -> `{PhaseAtom, ParallelMode, SuperNode, ParallelExecution}`.
   - `mainGenserver:maybe_update_super_node_phase(...)` posts `/parallelPhaseUpdate`.
   - Super `/parallelPhaseUpdate` -> `superNodeHandler:init` -> `superNodeGenserver:handle_call({parallel_phase_update,...})`:
     - `normalize_parallel_mode`
     - `build_scheduler_trace`
     - reset `scheduler_cursor`
     - broadcast managed-client parallel config via `/parallelSuperCommand`
     - emit first scheduler grant (pipeline modes) via `/parallelSuperCommand`.
3. Client/worker parallel state propagation:
   - Super command path: `superNodeGenserver:send_super_command_to_client(..., parallelSuperCommand, ...)`.
   - Client `/parallelSuperCommand` -> `clientStateHandler:init(parallel_super_command,...)`.
   - `clientStatem:apply_parallel_super_command(...)`:
     - `configure_parallel` -> `apply_parallel_mode(..., super_node)` + `apply_parallel_execution(..., super_node)`.
     - `grant_scheduler_event` -> forwards `{parallel_scheduler_grant,...}` to target worker.
   - Workers receive:
     - `{set_parallel_mode,...}`
     - `{set_parallel_execution,...}`
     - `{set_parallel_authority,true}` in non-legacy Super Node-controlled phases.
4. Phase state propagation:
   - `mainGenserver:update_clients_phase(clientTraining|clientPredict, ...)`
   - clients `/clientTraining` or `/clientPredict`
   - `clientStatem:idle(cast,{training|predict},...)` -> casts to workers, enters `waitforWorkers`
   - workers transition `idle -> train|predict`, call `update_client_avilable_worker`
   - `clientStatem:waitforWorkers(cast,{stateChange,...})`
   - `clientStatem:send_client_is_ready` -> Main `/clientReady`
   - `mainGenserver:handle_cast({clientAck,...})` -> `ack("update_phase_done")`
   - `receiver.ack.post` sets `EventSync.UPDATE_PHASE` done.

### 8.4 Training Execution Path (Parallel + Super Node)

1. Start casting:
   - `Transmitter.start_casting` -> Main `/startCasting`
   - `actionHandler:init(startCasting,...)` -> `mainGenserver:handle_cast({startCasting,...})`
   - `mainGenserver:sources_start_casting(...)` posts `/startCasting` to sources.
2. Source transmit loop:
   - `sourceStatem:idle(cast,{startCasting,...})` -> `spawnTransmitter(...)`
   - `sourceStatem:transmitter/7`
   - `sourceSendingPolicies:send_method_*`
   - `sourceSendingPolicies:prepare_and_send`
   - `sourceSendingPolicies:sendBatch` -> client `/batch`
   - source also sends `/start_stream` and `/end_stream`.
3. Client receives samples:
   - `clientStateHandler:init(batch,...)`
   - `clientStatem:training(cast,{sample,...})`
   - forwards `{sample,...}` to target worker PID.
4. Stage-0 worker true pipeline split/forward:
   - `workerGeneric:train(cast,{sample,...})` in `pipeline` mode and `pipelineStage=0`
   - `prepare_pipeline_stage0_microbatches(...)`
   - `dispatch_pipeline_stage0_forward_microbatch_loop(...)`
   - for each microbatch:
     - consume/validate grant via `maybe_emit_parallel_forward_event(...)`
     - call Torch stage NIF wrapper `call_to_pipeline_stage0_forward(...)`
     - route activation + labels to next stage via `route_pipeline_payload_to_worker(...)`
5. Intermediate/last stage forward:
   - `w2wCom:maybe_notify_pipeline_inbox(...)` casts `{parallel_pipeline_inbox,...}` to worker.
   - `workerGeneric:handle_parallel_pipeline_inbox(...)`
   - `dispatch_pipeline_forward_buffer(...)`
   - non-last stage:
     - `call_to_pipeline_stage_forward(...)`
     - route to next stage as `pipeline_forward_payload`
   - last stage:
     - `call_to_pipeline_stage_last_forward_backward(...)`
     - accumulate loss/time
     - emit first backward payload to previous stage.
6. Backward wave (last -> first):
   - `workerGeneric:dispatch_pipeline_backward_buffer(...)`
   - per microbatch:
     - consume/validate backward grant via `maybe_emit_parallel_backward_event(...)`
     - `call_to_pipeline_stage_backward(...)`
     - route resulting gradient to previous stage.
7. Batch barrier/finalization:
   - each stage waits until deterministic completion counters reach `total_microbatches`.
   - `maybe_call_optimizer_barrier(...)` executes once per stage per batch.
   - last stage sends aggregated `{loss,...}` to client.
8. Client -> MainServer loss upload:
   - `clientStatem:training(cast,{loss,...})`
   - posts `/lossFunction`
   - `mainGenserver:handle_cast({lossFunction,...})`
   - `store_phase_result_data_to_send_ets(...)`.
9. Super Node control-plane during training:
   - Parallel event route:
     - `clientStatem:forward_parallel_event(...)`
     - POST `/parallelEvent`
     - `superNodeGenserver:handle_cast({parallel_event,...})`
     - `maybe_advance_scheduler(...)`
     - `validate_scheduler_event(...)`
     - after successful validation, `maybe_send_next_scheduler_grant(...)`
   - Worker-to-worker data route (non-legacy):
     - `clientStatem:handle_w2w_msg(...)` -> `handle_w2w_msg_super(...)`
     - POST `/parallelWorkerMessage`
     - `superNodeGenserver:handle_cast({parallel_worker_message,...})`
     - forwards `/parallelDeliver` to destination client
     - destination `clientStatem:deliver_parallel_msg(...)` -> target local worker W2W PID.

### 8.5 Prediction Execution Path (Parallel mode active, prediction compute path)

1. Source -> client sample route remains:
   - `sourceSendingPolicies:sendBatch` -> `/batch`
   - `clientStatem:predict(cast,{sample,...})`
   - `workerGeneric:predict(cast,{sample,...})`.
2. Worker predict path in true pipeline mode:
   - Stage-0:
     - `prepare_pipeline_predict_stage0_microbatches(...)`
     - `dispatch_pipeline_stage0_predict_microbatch_loop(...)`
     - `call_to_pipeline_predict_stage0_forward(...)` -> `pipeline_predict_payload`.
   - Intermediate/last stages:
     - `handle_parallel_pipeline_predict_inbox(...)`
     - `dispatch_pipeline_predict_buffer(...)`
     - `call_to_pipeline_predict_stage_forward(...)`.
   - Super Node scheduler for prediction uses forward-only grants (`normalize_phase_name(PhaseName)` in Super Node trace builder).
3. Last stage prediction aggregation:
   - worker stores per-microbatch predictions in `predict_acc`.
   - on final microbatch:
     - concatenate outputs (axis 0) via `nerltensor_concat_nif(...)`
     - send client `{predictRes,...}` once per batch.
4. Client -> MainServer prediction upload:
   - `clientStatem:predict(cast,{predictRes,...})` -> POST `/predictRes`
   - `mainGenserver:handle_cast({predictRes,...})`
   - `store_phase_result_data_to_send_ets(...)`.
5. Any cross-worker message in prediction non-legacy mode still uses Super Node route:
   - `handle_w2w_msg_super -> /parallelWorkerMessage -> /parallelDeliver`.

### 8.6 Phase Finalization, Result Return, and Stats

1. Source done:
   - `sourceStatem:castingData(cast,{finishedCasting,...})`
   - POST `/sourceDone`
   - `mainGenserver:handle_cast({sourceDone,...})`.
2. Main server idles clients when all sources done:
   - `mainGenserver:update_clients_phase(clientIdle, ...)`
   - clients route `/clientIdle`.
3. Client/worker idle handshake:
   - `clientStatem:{training|predict}(cast,{idle},...)` -> cast idle to workers
   - workers transition to `idle` and send `stateChange`
   - `clientStatem:waitforWorkers` waits all done
   - `clientStatem:send_client_is_ready` -> `/clientReady`
   - `mainGenserver:handle_cast({clientAck,...})`.
4. Main server emits phase result payload:
   - on final client ACK:
     - `generate_phase_result_data_map()`
     - action selected by active phase: `trainRes` or `predRes`
     - POST to Flask `/trainRes` or `/predRes`
     - `clean_phase_result_data_to_send_ets()`
     - `ack("start_casting_done")`.
5. Python phase processing:
   - `receiver.trainRes.post` / `receiver.predictRes.post` appends raw payload
   - `ApiServer.run_current_experiment_phase` unblocks on `EventSync.START_CASTING`
   - `ExperimentPhase.process_experiment_phase_data()` decodes and stores per-worker batches/results.
6. Statistics round-trip:
   - `ApiServer.communication_stats()`
   - `Transmitter.statistics(...)` -> Main `/statistics` with `"getStatistics"`
   - `mainGenserver:statistics_requests_to_entities()`
   - entities respond `/statistics`
   - Main aggregates and POSTs API `/statistics`
   - `receiver.statistics.post` updates CommDB/PerfDB and sets `COMMUNICATION_STATS` done
   - Flask sends `transmitter.send_ack_validation()` to `/apiserver_ack_validation`.

### 8.7 Deterministic Fail-Fast Abort Path

1. Abort source can be:
   - Super Node scheduler mismatch (`validate_scheduler_event`)
   - heartbeat timeout/missing heartbeat (`check_heartbeats`)
   - route failures or unknown target worker
   - client-side missing super node / delivery failure (`clientStatem:notify_parallel_abort`).
2. Super Node abort emission:
   - `superNodeGenserver:maybe_notify_parallel_abort(...)`
   - `notify_parallel_abort(...)` -> Main `/parallelAbort`.
3. Main server abort handling:
   - `mainGenserver:handle_cast({parallelAbort,...})`
   - `decode_parallel_abort_body(...)`
   - `maybe_update_super_node_phase(..., legacy, #{})`
   - `update_clients_parallel_execution(#{}), update_clients_parallel_mode(legacy)`
   - `update_clients_phase(clientIdle, ...)`
   - `clean_phase_result_data_to_send_ets()`
   - `ack("parallel_abort")`.
4. API-side fail-fast:
   - `receiver.ack.post` maps `"parallel_abort"` to `EventSync.MAIN_SERVER_ERROR`
   - wait loops (`sync_on_event`) exit with explicit main server error path.

---

## 9) Release Hygiene Items (Must Clean Before Final Merge)

These appear to be local/environmental and should be reviewed before merge:
- `config/jsonsDir.nerlconfig` currently points to a temp directory.
- `config/subnets.nerlconfig` has extra entry `0.0.0.0`.
- Untracked/generated artifacts should not be committed:
  - `.docker-artifacts/`
  - `.build-codex/`
  - `__pycache__` under tests.

---

## 10) Conclusion

This branch has delivered a strong foundational slice for Super Node-based parallelism in NErlNet:
- schema and parser foundations,
- runtime control-plane skeleton,
- dual-path compatibility,
- microbatch/barrier bridge surfaces,
- planner support,
- CI and Docker validation scaffolding.

To make it production-ready for real-world PP/TP experiments, the next release cycle should focus on:
- strict runtime orchestration completeness,
- true TP execution integration,
- multi-device failure-proofing,
- observability and soak stability.
