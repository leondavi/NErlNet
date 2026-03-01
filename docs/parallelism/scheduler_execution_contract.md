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
- Workers consume scheduler grants from the head of the grant queue only; this preserves scheduler order for forward/backward transitions.
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
