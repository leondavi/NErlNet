# Parallelism Branch — Changes & Fixes Summary

## Phase 1 — P0: Deadlock & Correctness (3 fixes)

### P0-A: Gradient Summation vs Averaging
- **Files:** `NerlWorkerTorch.cpp`, `NerlWorkerTorch.h`
- **Problem:** Gradients were summed across microbatches instead of averaged. Effective learning rate was N times too large.
- **Fix:** Added `_num_microbatches_for_loss_scale` field; loss is divided by microbatch count before `loss.backward()`.

### P0-B: TP Collective Busy-Wait
- **File:** `workerGeneric.erl`
- **Problem:** `timer:sleep(2)` in tight recursive loop blocked dirty scheduler for up to 5s.
- **Fix:** Replaced with `receive after 10 -> ok end`, added explicit retry counter (max 600 iterations), added `LOG_ERROR` on timeout.

### P0-C: Optimizer Barrier Silent Gradient Loss
- **Files:** `NerlWorkerTorch.cpp`, `NerlWorkerTorch.h`
- **Problem:** After 20 defers, silently wiped cached stage contexts and discarded accumulated gradients.
- **Fix:** Increased max defers from 20 to 100, promoted log from `LogWarning` to `LogError` with "GRADIENT LOSS" message.

---

## Phase 2 — P1: Reliability (4 fixes)

### P1-A: Phase-Close Barrier No Timeout
- **File:** `superNodeGenserver.erl`
- **Problem:** If one client hangs/crashes, all clients wait forever for phase close.
- **Fix:** Added `phase_close_deadline_ref` timer (default 30s, configurable via `phaseCloseDeadlineMs`). On expiry, logs missing clients and proceeds.

### P1-B: HTTP Routing No Retry
- **Files:** `nerl_tools.erl`, `superNodeGenserver.erl`
- **Problem:** Single HTTP failure aborted entire phase.
- **Fix:** Added `http_router_request_with_retry/6` with exponential backoff (3 retries). Applied to Super Node command routing.

### P1-C: Delivery Retry Ceiling
- **File:** `superNodeGenserver.erl`
- **Problem:** After 20 retries (5s), delivery abandoned too aggressively.
- **Fix:** Increased to 120 retries (30s). Upgraded abandoned delivery log to `LOG_ERROR`.

### P1-D: NIF Execution Timing
- **File:** `NerlWorkerTorch.cpp`
- **Problem:** NIF calls block with no visibility into how long they take.
- **Fix:** Added wall-clock timing to pipeline NIF entry points; logs `LogError` if any exceeds 30s.

---

## Phase 3 — P2: Performance (2 fixes)

### P2-A: Multi-Grant Infrastructure
- **File:** `superNodeGenserver.erl`
- **Problem:** Only 1 grant in-flight, one slow worker stalls entire pipeline.
- **Fix:** Refactored `pending_grant` to `pending_grants` map with `max_inflight_grants` config (default: 1 — infrastructure only, same behavior).

### P2-B: Hub-and-Spoke Documentation
- **File:** `superNodeGenserver.erl`
- **Finding:** Pipeline tensor data already routes worker-to-worker (not through Super Node). Added documentation comment clarifying this.

---

## Phase 4 — P3: Polish (4 fixes)

### P3-A: Dedup Ring Buffer Overflow
- **File:** `clientStatem.erl`
- **Fix:** Increased `PARALLEL_DELIVERY_SEEN_MAX` from 2048 to 8192, epoch-scoped clearing.

### P3-B: End-Stream Drain Bound
- **File:** `workerGeneric.erl`
- **Fix:** Bounded drain polling to 100 retries (1s), force-flush with `LOG_WARNING`.

### P3-C: Configurable Loss Function
- **Files:** `NerlWorkerTorch.cpp`, `NerlWorkerTorch.h`
- **Fix:** Added `LossFunctionType` enum (MSE/CrossEntropy/L1/Huber), `compute_loss()` helper. Default MSE for backward compatibility.

### P3-D: Deferred Samples Cap
- **File:** `workerGeneric.erl`
- **Fix:** Capped `parallel_deferred_samples` at 256, drops oldest when full.

---

## Congestion-Aware Dropping Feature (Phases 1-7)

**Design invariant:** A microbatch is either processed at ALL pipeline stages or skipped at ALL stages.

### Layer 0 — Source Ingress Drop
- **File:** `workerGeneric.erl`
- Stage-0 workers drop incoming samples when queue >= `deferredSampleSoftLimit`. Sets congestion signal flag, piggybacks queue depth on `parallel_event` Meta.

### Layer 1 — Scheduler Proactive Skip
- **File:** `superNodeGenserver.erl`
- Super Node checks worker congestion signals before issuing forward grants. If queue depth >= `congestionSkipThreshold`, skips microbatch at ALL stages via `relay_skip_command_to_targets`.

### Layer 2 — Worker Buffer Cap
- **File:** `workerGeneric.erl`
- Workers reject grants when combined forward+backward buffer >= `pipelineBufferCap`. Flows through existing rejection path with `MAX_GRANT_REJECTION_STREAK = 256` circuit breaker.

### Loss Scaling
- **File:** `workerGeneric.erl`
- Both non-pipeline and pipeline loss finalization paths now use `max(1, TotalMicrobatches - SkippedMicrobatches)` as divisor.

### Stats
- **File:** `stats.erl`
- 4 new counters: `congestion_drop_deferred_sample`, `congestion_signal_emitted`, `congestion_grant_rejected`, `skip_congestion_drop`.

### Config
All opt-in, default off:

| Key | Type | Default | Layer |
|-----|------|---------|-------|
| `congestionDropEnabled` | bool | `false` | Master switch |
| `deferredSampleSoftLimit` | int | `256` | L0: queue watermark |
| `congestionSkipThreshold` | int | `0` (off) | L1: deferred depth triggering skip |
| `pipelineBufferCap` | int | `0` (off) | L2: max fwd+bwd buffer entries |

---

## Files Modified (total)

| File | Changes |
|------|---------|
| `NerlWorkerTorch.cpp` | P0-A, P0-C, P1-D, P3-C |
| `NerlWorkerTorch.h` | P0-A, P0-C, P3-C |
| `workerGeneric.erl` | P0-B, P3-B, P3-D, L0/L2 congestion, loss scaling, config |
| `superNodeGenserver.erl` | P1-A, P1-B, P1-C, P2-A, P2-B, L1 congestion, config |
| `nerl_tools.erl` | P1-B |
| `clientStatem.erl` | P3-A |
| `stats.erl` | Congestion counters |
| `AGENTS.md` | Runtime constraints documentation |

---

## Verification Status

Not yet verified end-to-end. Pending:
- `NerlnetFullFlowTorchPipelineTest.sh` (all 5 variants)
- `python3 -m pytest tests/parallelism/ -v`
- PTD Batch Experiments notebook (all experiments)
