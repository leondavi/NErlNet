# Super Node Schema and Runtime Notes

This document describes the first integration slice for Super Node orchestration.

## Distributed Config Additions (`dc_*.json`)

- `superNodes`: optional list of super-node entities.
- `clients[].superNode`: optional super-node owner for the client.
- `workers[].parallel`: optional worker-level parallel metadata.
- `model_sha[*].tpPlan`: optional tensor-parallel layer plan.

Example:

```json
{
  "superNodes": [
    {
      "name": "super_0",
      "port": 6601,
      "managedClients": ["client_a", "client_b"],
      "heartbeatMs": 1000,
      "maxInflightMicrobatches": 16
    }
  ]
}
```

## Experiment Flow Additions (`exp_*.json`)

- `Phases[].parallelExecution` (optional):
  - `mode`: `legacy | pipeline | tensor | pipeline_tensor`
  - `superNode`
  - `scheduler`: `gpipe | 1f1b | interleaved`
  - `microBatchSize`
  - `numMicroBatches`
  - `virtualStages`

If `parallelExecution` is missing, runtime behavior is `legacy`.

## Runtime Behavior (Current Slice)

1. Main Server parses and propagates parallel mode per phase.
2. Clients update internal `parallel_mode` (`legacy` by default).
3. For non-legacy mode, worker-to-worker messages are routed to Super Node.
4. Super Node forwards worker message traffic to destination clients.
5. Clients emit heartbeat messages to Super Node.
6. Super Node emits fail-fast parallel abort notifications on routing/heartbeat faults.

## Compatibility

- Legacy JSONs remain valid.
- Existing experiments without `superNodes` and `parallelExecution` keep old behavior.
