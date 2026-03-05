# Super Node Scheduler Execution Contract

This document captures the runtime contract for non-legacy parallel modes (`pipeline`, `tensor`, `pipeline_tensor`).

## Message routing contract

- Non-legacy worker data path:
  - `Worker -> Client -> Super Node (/parallelWorkerMessage) -> Destination Client (/parallelDeliver) -> Destination Worker`
- Legacy mode keeps direct `worker_to_worker_msg`.
- `/parallelDeliver` is terminal delivery on the destination client (no re-route loop).
- `/parallelDeliver` is delivery-tracked:
  - payload includes `DeliveryID`
  - destination client ACKs with `/parallelDeliverAck`
  - Super Node retries unacked deliveries and fails fast with `parallel_delivery_timeout` when retries are exhausted.

## Phase control contract

- Main Server sends parallel phase updates to Super Node (`/parallelPhaseUpdate`) before phase execution.
- Super Node increments and stores a per-phase `phase_epoch`.
- Super Node broadcasts config/grants to managed clients through `/parallelSuperCommand`.
- Non-legacy client idle transitions are phase-close gated:
  - client requests `/parallelPhaseClose`
  - Super Node grants close only after all managed clients request close.

## Scheduler grant contract

- Super Node grant command shape:
  - `{parallel_super_command, grant_scheduler_event, Direction, BatchID, MicrobatchID, StageID, WorkerName, PhaseEpoch}`
- Grant identity is batch-aware and epoch-scoped:
  - `{Direction, BatchID, MicrobatchID, StageID, PhaseEpoch}`
- Clients and workers still accept legacy batch-less grant tuples and normalize them to `BatchID=any`.
- Worker grant matching is tuple-based (not strict head-of-queue):
  - emits can consume any matching `{direction,batch,microbatch,stage}` grant in the local queue.
  - this avoids head-of-line stalls when stale/out-of-order grants exist.
- Batch matching is strict for concrete batches except stage-0 forward ingress in pure `pipeline` mode:
  - for non-stage0 (and for backward), a worker event for batch `B` must match a grant for batch `B`.
  - for stage0 forward in `pipeline`, grant matching is by `{direction,microbatch,stage}` and the emitted event batch is rewritten to the concrete granted batch id.
  - `pipeline_tensor` stage0 remains strict-batch so TP collectives stay aligned per `{batch,microbatch}` token.
  - this keeps scheduler progress deterministic while avoiding TP rank desynchronization.
- Scheduler grant rejections are reported to Super Node as:
  - `{scheduler_grant_rejected, Client, Worker, Direction, BatchID, MicrobatchID, StageID, Reason, PhaseEpoch}`

## Parallel event contract

- Workers emit scheduler-gated events with batch/microbatch/stage metadata.
- Clients forward events to Super Node with epoch-tagged metadata:
  - `{parallel_meta, PhaseEpoch, Meta}`
- Super Node ignores stale-epoch events (instead of applying them to current phase state).

## Pipeline batch isolation contract

- Worker pipeline inbox processing is batch-aware:
  - payloads for non-active batches return `wait_for_batch` and stay buffered.
- Worker scheduler grants are consumed by tuple match (direction/batch/microbatch/stage), not strict queue head only.
- Stage0 forward uses grant-authoritative batch tagging in pure `pipeline` mode:
  - source ingress batch ids may differ from scheduler batch ids.
  - workers bind stage0 runtime context/payload/event ids to the consumed grant batch id so downstream stages remain batch-consistent.
- Worker scheduler grant enqueue is deduplicated by normalized tuple to avoid retry-induced duplicate buildup.
- Backward grant dispatch also uses the grant batch id (not `any`) when selecting queued backward payloads.
- Backward dispatch is grant-aware:
  - worker selects backward payload by `{BatchID, MicrobatchID}` from queued grants, not strict FIFO alone.
- This prevents cross-batch state corruption and head-of-line deadlocks when message arrival order differs from grant order.
- Torch stage context lifecycle is batch-safe for overlap:
  - stage contexts are consumed by backward key (`{batch,microbatch}`) and are not globally flushed on optimizer barriers, so next-batch forward contexts remain valid until their backward arrives.
- Torch optimizer barriers are context-aware:
  - when pipeline stage contexts are still pending backward, optimizer step is deferred to avoid autograd in-place version conflicts.
- Torch optimizer barriers are worker-synchronous:
  - barrier NIF execution is invoked directly from the worker control path (not asynchronous train-negotiator fire-and-forget), preventing cross-process ordering races with stage forward/backward calls.
- Stream-end drain is stale-grant safe:
  - if `end_stream` is queued and no non-grant runtime work remains, workers drop leftover scheduler grants so `stream_ended` can flush and client phase-close can complete.

## Fail-fast contract

- Super Node enforces deterministic aborts for:
  - scheduler trace violations
  - pending grant timeout (`scheduler_grant_timeout`)
  - rejection storm (`scheduler_grant_rejection_storm`)
  - routing/heartbeat/ownership failures
- API synchronization maps `parallel_abort` to Main Server error semantics for immediate phase failure handling.

## API wait contract

- API phase synchronization waits are bounded:
  - `NERLNET_UPDATE_CSV_TIMEOUT_SEC` (default `180`)
  - `NERLNET_UPDATE_PHASE_TIMEOUT_SEC` (default `180`)
  - `NERLNET_START_CASTING_TIMEOUT_SEC` (default `1800`)
- API wait loops now emit heartbeat logs every `NERLNET_EVENT_WAIT_PROGRESS_SEC` seconds (default `15`) while a phase event is still pending.
- `start_casting_done` remains the phase-completion authority from Main Server to API; bounded waits prevent silent infinite waits and preserve deterministic failure semantics.

## Throughput efficiency knobs

- Dataset fetch now prefers local cache: if target dataset `.csv` files already exist under the local repo directory, the API skips HuggingFace re-download by default.
- Force dataset refresh with `NERLNET_DATASET_REFRESH=1`.
- Enforce local/offline dataset usage with `NERLNET_DATASET_OFFLINE=1`.
- Torch pipeline per-layer hot-path logs are disabled by default and can be enabled only when needed with `NERLNET_TORCH_PIPELINE_LAYER_LOGS=1`.
