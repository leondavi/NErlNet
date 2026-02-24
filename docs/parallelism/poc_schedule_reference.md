# PTD POC Scheduler Reference

This document captures the invariants extracted from:

- `PTD_P_POC/PTD_P_Tests.py`
- `PTD_P_POC/PTD_P_Tests.log`

The POC is treated as the baseline behavior contract for scheduler sequencing and
tensor-parallel collective semantics, and is compared against the integrated runtime flows.

## Key Invariants

1. Global setup:
- `Microbatch count = 5`

2. GPipe schedule:
- Forward order is `0,1,2,3,4`
- Backward order is `4,3,2,1,0`
- No backward step appears before the final forward step

3. 1F1B schedule:
- Warmup starts with a forward-only prefix
- Forward microbatch order is `0,1,2,3,4`
- Backward microbatch order is `0,1,2,3,4`
- Interleaved phase follows the expected forward/backward coupling shape

4. Interleaved schedule:
- Warmup followed by interleaved forward/backward, then drain
- Forward microbatch order is `0,1,2,3,4`
- Backward microbatch order is `0,1,2,3,4`

5. Tensor parallel collectives:
- Column-parallel path emits all-gather behavior markers
- Row-parallel path emits all-reduce(sum) behavior markers

6. Loss consistency:
- `GPipe: total loss before update = 2404`
- `1F1B: total loss before update = 2404`
- `Interleaved: total loss before update = 2404`
- Final summary repeats these values.

## Executable Verification

The invariants above are enforced by:

- `tests/parallelism/test_poc_schedule_trace.py`

Run locally:

```bash
python3 tests/parallelism/test_poc_schedule_trace.py
```
