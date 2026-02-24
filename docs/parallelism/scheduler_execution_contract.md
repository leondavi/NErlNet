# Super Node Scheduler Execution Contract

This document captures the current runtime contract implemented for non-legacy parallel modes.

## Message routing contract

- Outbound worker message path in non-legacy mode:
  - `Worker -> Client -> SuperNode (/parallelWorkerMessage) -> Destination Client (/parallelDeliver) -> Destination Worker`
- Legacy mode remains unchanged and still supports direct `worker_to_worker_msg` handling.
- `parallelDeliver` is a **terminal delivery** path on the destination client and does not re-route via Super Node.

## Phase control contract

- Main Server sends per-phase parallel metadata to Super Node before phase cast:
  - endpoint: `/parallelPhaseUpdate`
  - body tuple: `{parallel_phase_update, PhaseAtom, ParallelMode, ParallelExecutionMap, WorkerParallelMap}`
- Super Node stores active parallel metadata and builds deterministic scheduler traces for:
  - `gpipe`
  - `1f1b`
  - `interleaved`

## Scheduler trace guard contract

- Super Node accepts optional tagged worker payloads:
  - `{parallel_event, Direction, MicrobatchId, Stage}`
  - `{parallel_event, Direction, MicrobatchId, Stage, Payload}`
- When tagged payloads are present, Super Node validates order against the active scheduler trace.
- On mismatch, Super Node emits deterministic abort to Main Server using `parallelAbort`.

## API server error sync contract

- `parallel_abort` is treated as `MAIN_SERVER_ERROR` in `EventSync`.
- Any waiting sync loop fails fast once this abort is observed.
