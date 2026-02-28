# PP/TP + Interleaved Planner Runbook (Detailed)

## Objective
Produce valid `dc_*.json`, `conn_*.json`, and `exp_*.json` from NerlnetPlanner for:
1. Two-device PP.
2. Two-device PP+TP.
3. Four-device PP.
4. Four-device PP+TP.

Also run scheduler variants for each PP-capable setup:
- `gpipe`
- `1f1b`
- `interleaved` (with `virtualStages`)

## Hard Constraints (Current Runtime)
1. Non-legacy modes require a Super Node and valid `phase.parallelExecution.superNode`.
2. In `pipeline` and `pipeline_tensor`, each source piece can target **stage 0 workers only**.
3. All pipeline workers must share one `pipelineWorldSize` and cover every stage `0..pipelineWorldSize-1`.
4. TP groups cannot span pipeline stages.
5. For meaningful TP, a TP group should have `tpWorldSize >= 2` (world=1 is functionally valid but degenerate).

## Planner Views You Will Use
1. `Sandbox`: topology, entities, device assignment, worker PP/TP metadata.
2. `Model Lab`: Torch architecture export and optional TP plan (`tpPlan`).
3. `Experiment Flow`: dataset + phases + `parallelExecution` config.
4. `Export`: download `dc`, `conn`, `exp` JSONs.

## One-Time Model Preparation (Model Lab)
1. Create Torch model.
2. Build architecture (recommended): `5 -> 64 -> 32 -> 16 -> 3` with Linear/ReLU blocks.
3. Verify `Input Tensor Shape` and `Labels Shape` match your dataset (`[N,5]` and `[N,3]` for synthetic norm).
4. Click `Export TorchScript` and confirm success.
5. If running TP (`pipeline_tensor`), add `Tensor Parallel Plan` entries:
- `Layer`: layer name exactly as shown in layer stack/list.
- `Mode`: `column` or `row`.
- `Shard Axis`: usually `0` for column, `1` for row.
- `TP Group`: must match worker `TP Group` names.

## Common Topology Pattern (Sandbox)
Use this star routing for all experiments:
- `mainServer <-> router_1`
- `apiServer <-> router_1`
- `super_0 <-> router_1`
- each `source <-> router_1`
- each `client <-> router_1`

### Entity creation sequence
1. Right-click canvas -> add `Router`, `Source`, `Client`(s), `Super Node`.
2. Configure each entity (right-click node -> `Configure`).
3. Assign each entity to a device in the node config panel.
4. For each client:
- set `Super Node` field to `super_0`
- `Manage Workers` -> add workers and select model
- fill worker PP/TP fields (`PP Stage`, `PP World`, `TP Group`, `TP Rank`, `TP World`)
5. Configure `super_0`:
- `Managed Clients`: select all clients used in the run
- set heartbeat and max inflight microbatches

## Common Experiment Flow Pattern (Experiment Flow)
1. Set metadata:
- `experimentType=classification`
- `batchSize` must match cluster batch size
- dataset CSV path (or HF dataset download)
- `numOfFeatures`, `numOfLabels`, headers
2. Add two phases:
- `Training1` (`training`)
- `Prediction1` (`prediction`)
3. For each phase add one source piece:
- same source
- `startingSample=0` (or your split)
- `numOfBatches` as desired
- `nerltensorType=float`
- workers list: **stage 0 workers only**
4. Set `Parallel Execution` per phase:
- `mode`: `pipeline` or `pipeline_tensor`
- `superNode`: `super_0`
- `scheduler`: `gpipe|1f1b|interleaved`
- `microBatchSize`: positive integer
- `numMicroBatches`: positive integer
- `virtualStages`: required only for `interleaved`

## Scheduler Sweep (Include Interleaved)
For each experiment topology, run three variants by changing only phase scheduler fields:
1. GPipe run: `scheduler=gpipe`, `virtualStages` ignored.
2. 1F1B run: `scheduler=1f1b`, `virtualStages` ignored.
3. Interleaved run: `scheduler=interleaved`, set `virtualStages=2` (start here).

Keep all other fields unchanged between scheduler variants.

---

## Experiment 1: Two Devices, PP Only

### Device layout
- Device A: `mainServer`, `apiServer`, `super_0`, `router_1`, `source_1`
- Device B: `client_1`

### Client + workers
- `client_1` has `w1`, `w2`
- `client_1.superNode=super_0`

### Worker metadata
| Worker | PP Stage | PP World | TP Group | TP Rank | TP World |
|---|---:|---:|---|---|---|
| w1 | 0 | 2 | (empty) | (empty) | (empty) |
| w2 | 1 | 2 | (empty) | (empty) | (empty) |

### Phase source piece workers
- `Training1`: `w1`
- `Prediction1`: `w1`

### Parallel execution
- `mode=pipeline`
- scheduler sweep: `gpipe`, `1f1b`, `interleaved`

---

## Experiment 2: Two Devices, PP+TP

### Important note
With only two workers and two PP stages, true TP sharding is degenerate unless each stage has >=2 workers. This setup is valid mainly as a `pipeline_tensor` path check.

### Device layout
- Same as Experiment 1

### Worker metadata (degenerate but valid combined mode)
| Worker | PP Stage | PP World | TP Group | TP Rank | TP World |
|---|---:|---:|---|---:|---:|
| w1 | 0 | 2 | tp_s0 | 0 | 1 |
| w2 | 1 | 2 | tp_s1 | 0 | 1 |

### TP Plan guidance
- Add entries with groups `tp_s0` for early-stage layers and `tp_s1` for later-stage layers.
- Because `TP World=1`, this is functional validation, not performance TP.

### Phase source piece workers
- `Training1`: `w1`
- `Prediction1`: `w1`

### Parallel execution
- `mode=pipeline_tensor`
- scheduler sweep: `gpipe`, `1f1b`, `interleaved`

### If you want real TP effect on two devices
- Use at least 4 workers total: two workers on stage 0 and two on stage 1 (TP world 2 per stage).

---

## Experiment 3: Four Devices, PP Only

### Device layout
- Device A: `mainServer`, `apiServer`, `super_0`, `router_1`, `source_1`
- Device B: `client_1` (4 workers)
- Device C: `client_2` (4 workers)
- Device D: `client_3` (4 workers)

### Clients
- `client_1.superNode=super_0`
- `client_2.superNode=super_0`
- `client_3.superNode=super_0`
- `super_0.managedClients=[client_1,client_2,client_3]`

### Worker metadata (12-stage pipeline)
| Worker | PP Stage | PP World |
|---|---:|---:|
| w1 | 0 | 12 |
| w2 | 1 | 12 |
| w3 | 2 | 12 |
| w4 | 3 | 12 |
| w5 | 4 | 12 |
| w6 | 5 | 12 |
| w7 | 6 | 12 |
| w8 | 7 | 12 |
| w9 | 8 | 12 |
| w10 | 9 | 12 |
| w11 | 10 | 12 |
| w12 | 11 | 12 |

TP fields remain empty for all workers.

### Phase source piece workers
- `Training1`: `w1`
- `Prediction1`: `w1`

### Parallel execution
- `mode=pipeline`
- scheduler sweep: `gpipe`, `1f1b`, `interleaved`

### Practical note
A 12-stage CPU pipeline can be slow. Start with smaller `numOfBatches` for sanity checks.

---

## Experiment 4: Four Devices, PP+TP

### Device layout
- Same as Experiment 3

### Worker metadata (6 pipeline stages, 2-way TP per stage)
| Worker | PP Stage | PP World | TP Group | TP Rank | TP World |
|---|---:|---:|---|---:|---:|
| w1 | 0 | 6 | tp_g0 | 0 | 2 |
| w2 | 0 | 6 | tp_g0 | 1 | 2 |
| w3 | 1 | 6 | tp_g1 | 0 | 2 |
| w4 | 1 | 6 | tp_g1 | 1 | 2 |
| w5 | 2 | 6 | tp_g2 | 0 | 2 |
| w6 | 2 | 6 | tp_g2 | 1 | 2 |
| w7 | 3 | 6 | tp_g3 | 0 | 2 |
| w8 | 3 | 6 | tp_g3 | 1 | 2 |
| w9 | 4 | 6 | tp_g4 | 0 | 2 |
| w10 | 4 | 6 | tp_g4 | 1 | 2 |
| w11 | 5 | 6 | tp_g5 | 0 | 2 |
| w12 | 5 | 6 | tp_g5 | 1 | 2 |

### TP Plan guidance
- Add tpPlan entries whose `group` values are only from `tp_g0..tp_g5`.
- Keep each group tied to layers that logically belong to one PP stage chunk.
- Do not reuse one TP group across two different stages.

### Phase source piece workers
- `Training1`: `w1,w2` (both stage 0 workers)
- `Prediction1`: `w1,w2`

### Parallel execution
- `mode=pipeline_tensor`
- scheduler sweep: `gpipe`, `1f1b`, `interleaved`

---

## Exact Export Steps (Export View)
1. Open `Export` view.
2. Resolve all errors in `Export validation` panel.
3. Download three files:
- `Download DC` -> `dc_<experimentName>.json`
- `Download Conn` -> `conn_<experimentName>.json`
- `Download Exp` -> `exp_<experimentName>.json`
4. Repeat export after each scheduler variant change.

## Quick JSON Sanity Checklist Before Running
1. `dc`: every worker has model SHA and intended parallel fields.
2. `dc`: every client has `superNode`; `super_0` manages those clients.
3. `conn`: contains all entities and router links (including super node).
4. `exp`: in PP modes, source piece workers are stage 0 only.
5. `exp`: `parallelExecution.mode`, `superNode`, `scheduler`, `microBatchSize`, `numMicroBatches` are set.
6. `exp`: `virtualStages` is set for interleaved runs.

## Suggested Run Labels for Benchmark CSV
Use one label per run in notebook cell:
1. `exp1_pp_gpipe`
2. `exp1_pp_1f1b`
3. `exp1_pp_interleaved`
4. `exp2_pptp_gpipe`
5. `exp2_pptp_1f1b`
6. `exp2_pptp_interleaved`
7. `exp3_pp_gpipe`
8. `exp3_pp_1f1b`
9. `exp3_pp_interleaved`
10. `exp4_pptp_gpipe`
11. `exp4_pptp_1f1b`
12. `exp4_pptp_interleaved`

## Minimal Failure Triage
1. Validation panel errors first.
2. Check source piece workers are stage 0 only.
3. Check TP groups do not cross stages.
4. Check Super Node manages all clients used in phases.
5. Check logs for grant timeout / phase-close barrier issues.
