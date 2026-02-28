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
3) In `parallelExecution.mode=legacy`, client/worker behavior follows legacy flow.
4) In non-legacy parallel modes, Super Node is the control authority (`Super Node -> Clients -> Workers`):
   - Main Server sends phase update to Super Node.
   - Super Node pushes parallel config commands to managed clients.
   - Super Node issues scheduler grants for pipeline events.
   - Clients forward grants to workers; workers must consume grants before emitting pipeline events.
   - In `mode=pipeline`, workers run true stage-sliced execution (not full-model-per-worker): stage0 splits source batches, stages exchange activations/gradients through Super Node-routed worker messages, and per-stage optimizer barriers happen after deterministic microbatch completion.
   - Super Node/Client/Worker logs expose config/grant/event flow for run-time orchestration traceability.
5) Training phase runs, then prediction phase runs; stats are collected per phase.

## JSONs (shape + purpose)
- Distributed config (DC, `dc_*.json`):
  - `nerlnetSettings` (frequency, batchSize)
  - `mainServer`/`apiServer` (port, args)
  - `devices` (name, ipv4, entities CSV string)
  - `routers`, `sources`, `clients`, optional `superNodes`
  - `workers` list (name + model_sha)
  - `model_sha` map (sha -> model payload)
  - Parallel extensions:
    - `clients[].superNode`
    - `workers[].parallel` (`pipelineStage`, `pipelineWorldSize`, `tpGroup`, `tpRank`, `tpWorldSize`)
    - `model_sha[*].tpPlan` (explicit TP shard plan metadata)
- Connection map (`conn_*.json`):
  - `connectionsMap` dict `{entity: [neighbors...]}`; parser adds bidirectional edges.
- Experiment flow (`exp_*.json`):
  - `experimentName`, `experimentType`, `batchSize`, `csvFilePath`, `numOfFeatures`, `numOfLabels`, `headersNames`
  - `Phases`: list of `{phaseName, phaseType, sourcePieces}`
  - `sourcePieces`: `{sourceName, startingSample, numOfBatches, workers, nerltensorType}`
  - Optional per-phase `parallelExecution`:
    - `mode`: `legacy|pipeline|tensor|pipeline_tensor`
    - `superNode`
    - `scheduler`: `gpipe|1f1b|interleaved` (pipeline modes)
    - `microBatchSize`, `numMicroBatches`, `virtualStages` (as relevant)
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
- API preflight validates `connectionsMap` against runtime entities and requires explicit Super Node adjacency when super nodes are configured.
- If non-legacy parallel mode is selected, a valid Super Node must be configured and referenced by phase config.
- Non-legacy client parallel settings are Super Node-driven; Main Server does not directly own non-legacy mode/execution fanout.
- If `clients[].superNode` is set, that client must also be listed under the super node `managedClients`.
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
- Worker non-legacy pipeline events are scheduler-grant-gated when Super Node authority is enabled.
- Super Node scheduler grants are batch-aware and epoch-scoped:
  - client receives `{grant_scheduler_event, Direction, BatchID, MicrobatchID, StageID, Worker, PhaseEpoch}`
  - worker stores grants as `{Direction, BatchID, MicrobatchID, StageID}` (legacy batch-less grants are normalized to `BatchID=any`).
- Non-legacy phases now carry a Super Node epoch (`phase_epoch`): Super Node grants include epoch, clients tag forwarded `parallel_event` meta with epoch, and Super Node ignores stale-epoch events instead of aborting current phase state.
- Client scheduler-grant rejection payloads now include batch + epoch (`schedulerGrantRejected`) so Super Node can deterministically resolve pending grants.
- Client idle transition in non-legacy modes is phase-close-gated: clients request `parallelPhaseClose` from Super Node before idling workers, then wait for `phase_close_granted`.
- Super Node phase-close barrier completes only after all managed clients requested close; then scheduler grants are disabled and `phase_close_granted` is broadcast.
- Client no longer rejects scheduler grants solely because `idle` was requested; close-related grant rejection starts only after explicit `parallelPhaseClose` request state is set (or idle/waitforWorkers terminal states), preventing premature drain deadlocks.
- Client phase-close requests are retried (bounded by `PHASE_CLOSE_RETRY_MS`) until `phase_close_granted`, so transient route loss cannot strand the client in close-pending.
- Super Node treats close-related `schedulerGrantRejected` reasons as a phase-close signal and either finalizes close or emits deterministic abort if global close barrier cannot be completed, preventing infinite grant/reject loops.
- Super Node includes a scheduler grant rejection storm circuit breaker (`scheduler_grant_rejection_storm`) that fails fast and disables scheduling instead of spinning indefinitely.
- In non-legacy modes, workers buffer incoming `sample` messages while in `wait` state (`parallel_deferred_samples`) and dequeue deterministically after batch completion to avoid TP peer batch desynchronization.
- In `pipeline|pipeline_tensor`, worker pipeline inbox handling is batch-aware: out-of-batch forward/backward/predict payloads are retained (`wait_for_batch`) until the active batch context advances.
- If a deferred sample is replayed while the worker is still in `wait`, workers now fast-transition `wait -> train|predict` (when no active parallel batch context/buffers remain) and immediately re-cast that sample, preventing deferred requeue loops and batch-0-only turnover stalls.
- Worker/Client/Super Node now log a shared deterministic parallel event id tuple: `{parallel_event, Worker, Direction, Batch, Microbatch, Stage}`.
- Client logs for `parallelEvent` include router latency (`latency_us`) and router reply payload, allowing transport-level confirmation for each forwarded event.
- Runtime parallel debug logging is opt-in: `NerlnetRun.sh --debug` sets `NERLNET_PARALLEL_DEBUG=1` and enables verbose info logs from parallel control-path modules (Super Node/Client/Worker); default runs keep warnings/errors while suppressing parallel info-level log spam.
- Super Node keeps pending-grant issue timestamps and runs a watchdog that emits deterministic `scheduler_grant_timeout` aborts with expected grant summary + last seen parallel event metadata.
- Super Node parallel worker payload transport is now delivery-tracked: each `/parallelDeliver` message carries a delivery id, clients ACK with `/parallelDeliverAck`, and Super Node retries unacked deliveries (`parallel_delivery_retry_ms`) before deterministic `parallel_delivery_timeout` abort.
- Clients deduplicate retried delivery ids (`parallel_delivery_seen_ids`) so at-least-once transport retries do not duplicate worker payload execution.
- Python worker communication stats now expose TP counters from runtime payloads: `tp_collective_count`, `tp_collective_latency_us`, and derived `tp_collective_avg_latency_us` (zero-safe for mixed-version payloads).
- Python worker communication stats now also expose phase-local model-db completion counters (`batches_completed_train`, `batches_completed_predict`) to avoid stage-0-only ingress counter bias in pipeline summaries.
- `ExperimentSummary` worker batch totals prefer the max of ingress/sent/completed counters and use pipeline-mode fallback propagation so non-stage0 workers are not misreported as zero-total in PP runs.
- `Stats.get_tensor_parallel_stats()` returns per-worker TP observability tables and optional normalization (`tp_collective_per_predict_batch`) when predict batch counters are available.
- `ExperimentSummary` CSV rows now include per-worker TP columns: `TP Collective Count`, `TP Collective Latency (us)`, `TP Avg Collective Latency (us)`.
- Main Server `clientAck` handling is hardened for abort/reset races: if `active_phase` is already cleared (`none|undefined`), phase result upload is skipped safely and result ETS is cleaned.
- Main Server now ignores stale/duplicate `clientAck` messages from clients that are not currently in `clientsWaitingList`, preventing premature phase-ack completion.
- `pipeline` mode stage execution is Torch-only and requires pipeline metadata (`pipelineStage`, `pipelineWorldSize`) on workers.
- `pipeline` mode currently enforces exactly one worker per pipeline stage per phase target set; use `pipeline_tensor` for multi-worker stage layouts.
- For `mode in {pipeline, pipeline_tensor}`, `sourcePieces[].workers` must reference stage-0 workers only (source ingress guard is enforced by API parse + planner validation).
- In `pipeline` training, last-stage workers emit/queue backward scheduler events after last-stage forward/backward compute so Super Node backward grants can be acknowledged deterministically.
- In `pipeline` training, backward payload dispatch is grant-aware by microbatch id (`dispatch_pipeline_backward_buffer_by_grant`) to avoid head-of-line deadlocks when payload arrival order differs from backward grant order.
- In `pipeline` training, backward payload selection is grant-aware by both batch id and microbatch id (`pop_pipeline_backward_payload_for_grant`) to avoid cross-batch collisions.
- In `pipeline` prediction, non-last stages (including stage0) finalize local batch context after all local microbatches are dispatched and then dequeue deferred source samples, preventing batch-0-only stall.
- In `pipeline` prediction, stage0 progress mirrors `forward_dispatched` into `forward_completed` so local batch turnover can complete deterministically.
- In `pipeline` prediction, worker forward `parallel_event` metadata is `predict` (not `training`), which should be reflected in Super Node event logs.
- In `pipeline|pipeline_tensor`, worker `end_stream` is drain-gated: workers queue `end_stream`, defer `stream_ended`, and flush only after active batch context, scheduler grants, pipeline inbox buffers, TP collective inbox, and deferred microbatches/samples are empty.
- During `pipeline|pipeline_tensor` stream-end drain, workers now drop stale scheduler grants if no non-grant runtime work remains; this prevents end-of-data deadlocks where a speculative next-batch grant blocks `stream_ended`/phase-close.
- API phase parsing now injects `parallelExecution.maxBatches` (derived from phase `sourcePieces[].numOfBatches`, max across pieces) for non-legacy modes.
- Super Node scheduler respects `maxBatches` and stops issuing rollover grants once the configured bound is reached, preventing synthetic out-of-range batches (for example, batch `100` when source batches are `0..99`).
- On `maxBatches` completion, Super Node now emits a deterministic phase-done signal to Main Server (`/parallelPhaseDone`) and initiates Super Node phase-close broadcast for managed clients.
- In non-legacy modes, Main Server no longer uses `sourceDone` as the phase-complete trigger; it waits for Super Node `/parallelPhaseDone` and treats that as the completion authority.
- Main Server `parallelPhaseDone` handling is idempotent in non-legacy mode and primes client-ack waiting without issuing duplicate `clientIdle` fanout.
- Client handling of `phase_close_granted` now marks stream bookkeeping done (`all_workers_done=true`, `active_workers_streams=[]`) before `parallel_finalize_idle`, preventing close-handshake stalls.
- Client `phase_close_granted` now also flips `parallel_idle_requested=true`, so local idle-finalize guards can complete even when Main Server does not send a separate duplicate `clientIdle` in non-legacy close.
- Client idle-state fallback now remains in `idle` for unrecognized messages (no accidental idle->training transition).
- Source stream control signals (`start_stream`/`end_stream`) are timeout-bounded per target worker (`STREAM_SIGNAL_TIMEOUT_MS`) and cannot block source transmitter completion indefinitely.
- Stage-sliced worker messaging payload tags are:
  - `pipeline_forward_payload`
  - `pipeline_backward_payload`
  - `pipeline_predict_payload`
- `w2wCom` immediately casts these payloads back into worker state machines as `{parallel_pipeline_inbox, FromWorker, Payload}` while still preserving inbox queue behavior used by TP collectives.
- `pipeline_tensor` uses forward-only scheduler grants, while TP collectives execute per `tpPlan` entry with deterministic `{batch,microbatch,layer,mode}` collective tokens.
- Super Node scheduler trace is forward-only for prediction phase (`phaseType=prediction`) even in `mode=pipeline`, preventing backward-grant deadlocks during prediction.
- Super Node scheduler gate logs now explicitly report non-issuing states (`parallel_active=false`, empty trace, pending grant still open) with cursor/trace context for deadlock triage.
- Torch pipeline stage0 prediction now accepts both feature-only microbatches and feature+label-span microbatches, normalizing input shape before local stage execution.
- Torch stage-training APIs now carry both `batch_id` and `microbatch_id`; delayed backward stage context is keyed by `{batch_id, microbatch_id}` to prevent cross-batch microbatch-id collisions.

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
- Parallel contract and scheduler tests live under `tests/parallelism/`.
- Stage-sliced pipeline contracts are asserted by `tests/parallelism/test_pipeline_stage_execution_contract.py`.
- Docker CPU Torch validation lives under `docker/ubuntu-torch-cpu/` and includes PTD loss parity checks.
- `tests/inputTorchJsonsFiles/parallel_smoke/exp_torch_pipeline_tensor_smoke.json` is the primary smoke for Super Node + `pipeline_tensor` runtime verification.
