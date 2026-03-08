# NErlNet `parallelism-project` branch - AI In Data Centers Course Project

## Scope

This branch was developed as a final project for the **AI In Data Centers** course. Its purpose is to extend NErlNet from a general distributed machine-learning platform into a system that can build, run, measure, and compare:

- pipeline parallelism,
- tensor parallelism,
- and hybrid pipeline-plus-tensor execution.

The branch is not just a proof of concept. It is an end-to-end workflow for:

1. defining distributed experiments,
2. running them across multiple devices,
3. validating correctness and liveness,
4. collecting runtime and communication statistics,
5. and generating report-ready results and plots.

## What This Branch Adds

The central change in this branch is the move from loosely coordinated execution to an explicit **parallel control plane**.

In practice, that means:

- non-legacy parallel runs are coordinated through a **Super Node**,
- workers can participate in **pipeline stages** and **tensor-parallel groups**,
- schedulers such as **GPipe**, **1F1B**, and **Interleaved** are treated as first-class execution modes,
- phase progression, close, retry, and timeout behavior are much more explicit,
- and the system now produces richer experimental outputs that can support an academic report rather than only a pass/fail run.

At a high level, the branch evolved through four priorities:

1. **Correctness**
   - getting the PP and TP computation model right.
2. **Reliability**
   - preventing hangs, deadlocks, and non-deterministic close behavior.
3. **Observability**
   - exposing communication, timing, and progress information in a usable form.
4. **Experimentation**
   - supporting repeatable runs, matrix-based evaluation, and plot generation.

## Parallel Modes Supported

### Legacy

This is the original NErlNet execution path. It remains useful as a baseline and as a low-complexity reference for throughput and quality.

### Pipeline Parallelism

The model is split into sequential stages, and different workers are responsible for different stage segments. This mode is useful for evaluating schedule behavior and pipeline bubbles.

Supported schedules:

- `gpipe`
- `1f1b`
- `interleaved`

### Tensor Parallelism

Workers cooperate inside the same layer or stage by partitioning the computation across ranks. This mode is useful for evaluating collective communication cost and intra-stage parallel execution.

### Pipeline + Tensor Parallelism

This is the hybrid mode. The model is partitioned across stages, and some stages are also partitioned across tensor-parallel ranks. This is the most demanding and most representative mode for PTD-style experiments.

## Overall Model of the Runtime

For development purposes, it helps to think of the system in layers:

1. **Python API / experiment orchestration**
   - loads JSON definitions,
   - prepares the experiment,
   - runs phases,
   - collects results and statistics.

2. **Main Server**
   - manages high-level phase transitions and entity coordination.

3. **Super Node**
   - controls non-legacy parallel execution,
   - issues scheduler grants,
   - tracks progress,
   - coordinates phase close in parallel modes.

4. **Clients and Workers**
   - host the actual model execution,
   - run stage-local or tensor-parallel work,
   - exchange activations, gradients, and control events.

5. **Sources and Routers**
   - deliver data and move messages through the distributed network.

The important design distinction is:

- `legacy` mode follows the older NErlNet behavior,
- all non-legacy parallel modes rely on explicit parallel coordination.

## How Experiments Are Defined

Every experiment is built from three JSON files:

### 1. Distributed Configuration

This file defines the distributed system itself:

- devices,
- entity placement,
- main server and API server settings,
- sources, routers, clients, workers,
- optional Super Node,
- and the model artifact metadata.

For parallel experiments, this is also where worker-level placement metadata is defined, such as:

- pipeline stage index,
- pipeline world size,
- tensor-parallel group,
- tensor-parallel rank,
- tensor-parallel world size.

### 2. Connection Map

This file defines how entities are connected. In practice, it is the topology of the run.

For non-legacy parallel modes, this topology must include the Super Node in the expected communication path.

### 3. Experiment Flow

This file defines what actually runs:

- experiment name and dataset information,
- training and prediction phases,
- source pieces,
- batch counts,
- and optional `parallelExecution` information for each phase.

For parallel runs, phase configuration selects:

- `mode`
- `scheduler`
- microbatch settings
- Super Node ownership
- and any mode-specific parameters.

## Rules That Matter When Authoring Parallel Experiments

If you are creating your own experiment, these rules matter early:

1. **Use a Super Node for non-legacy modes**
   - pipeline, tensor, and hybrid runs require explicit parallel control.

2. **Keep batch size consistent**
   - the batch size in the distributed configuration and the experiment flow must match.

3. **Use valid worker metadata**
   - pipeline runs need stage placement,
   - tensor runs need TP group and rank information,
   - hybrid runs need both.

4. **Target stage-0 workers from the source in pipeline-style runs**
   - the source should inject work into the first stage, not into arbitrary stages.

5. **Use true multi-device setups for meaningful PP or TP evaluation**
   - the runtime identifies devices by IP, so repeating the same device identity does not reproduce a real distributed layout.

6. **Treat hybrid runs as the strictest configuration**
   - if pipeline-only and tensor-only work but hybrid does not, the issue is usually coordination or phase-close logic rather than the basic model itself.

## Get Started

The recommended way to approach this branch as a developer is to move in layers: build, validate, run a known-good experiment, then create your own.

### 1. Build the Torch-capable runtime

On every device that will host NErlNet entities:

```bash
sudo ./NerlnetInstall.sh --torch
./NerlnetBuild.sh --infra torch
```

If C++ or header files changed, remove the local `build` directory before rebuilding.

### 2. Validate the branch before changing experiments

Start with the existing test suite. The practical order is:

```bash
./tests/NerlnetNIFTorchPipelineTest.sh
./tests/NerlnetFullFlowTorchPipelineTest.sh
./tests/NerlnetFullFlowTorchTest.sh
./tests/NerlnetFullFlowTest.sh
```

This gives you three levels of confidence:

- NIF-level correctness,
- local end-to-end PP/TP behavior,
- and general full-flow regression coverage.

### 3. Start the runtime on the participating devices

On each participating machine:

```bash
./NerlnetRun.sh
```

For deeper runtime debugging:

```bash
./NerlnetRun.sh --debug
```

Use the debug mode when investigating scheduler behavior, phase-close problems, transport retries, or worker-level PP/TP progression.

### 4. Run a known experiment first

You have two recommended entry paths.

#### Option A: Existing full-flow tests

This is the fastest way to check whether the branch is healthy:

```bash
./tests/NerlnetFullFlowTorchPipelineTest.sh
```

This exercises the main parallel combinations in a controlled way.

#### Option B: The report pipeline

This is the preferred workflow for branch-scale experimentation and report generation.

Generate explicit JSONs:

```bash
python3 JupyterLabDir/report_pipeline/generate_jsons.py \
  --profile report_core \
  --manifest JupyterLabDir/report_pipeline/device_manifest.json \
  --matrix JupyterLabDir/report_pipeline/experiment_matrix.report_core.json \
  --out JupyterLabDir/report_pipeline/generated_jsons
```

Run a quick smoke subset:

```bash
python3 JupyterLabDir/report_pipeline/run_matrix.py \
  --profile report_core \
  --manifest JupyterLabDir/report_pipeline/device_manifest.json \
  --matrix JupyterLabDir/report_pipeline/experiment_matrix.report_core.json \
  --json-dir JupyterLabDir/report_pipeline/generated_jsons \
  --out JupyterLabDir/report_pipeline/output_quick \
  --quick \
  --fail-fast
```

Run the full matrix:

```bash
python3 JupyterLabDir/report_pipeline/run_matrix.py \
  --profile report_core \
  --manifest JupyterLabDir/report_pipeline/device_manifest.json \
  --matrix JupyterLabDir/report_pipeline/experiment_matrix.report_core.json \
  --json-dir JupyterLabDir/report_pipeline/generated_jsons \
  --out JupyterLabDir/report_pipeline/output_report_core \
  --fail-fast
```

If you prefer Jupyter, the notebook wrapper under the report pipeline workspace calls the same scripts rather than maintaining a separate execution path.

### 5. Create your own experiment

Once the branch is validated on a known-good run, create your own experiment in this order:

1. define the devices and entity placement,
2. define the connection topology,
3. define the training and prediction phases,
4. choose the parallel mode,
5. choose the scheduler and microbatch settings,
6. verify the experiment with a small run before scaling it up.

For developer productivity, keep the first version of a new experiment simple:

- start with one training phase and one prediction phase,
- keep the dataset and batch settings modest,
- validate pipeline-only or tensor-only before attempting hybrid PP+TP,
- and only then increase device count, microbatches, or workload size.

## Recommended Development Workflow

The branch is easiest to work with if you use a staged workflow:

### Stage 1: Local and deterministic

Use the NIF and full-flow tests to verify that the current tree is healthy.

### Stage 2: Two-device experiments

Before moving to larger topologies, validate:

- pipeline-only runs,
- tensor-only runs,
- then small hybrid runs.

This is usually the fastest way to isolate whether a problem is in schedule progression, tensor collectives, or close behavior.

### Stage 3: Matrix-based evaluation

Once the runtime is stable, use the report pipeline to generate:

- explicit experiment JSONs,
- repeatable result directories,
- raw trace and summary data,
- and plot-ready outputs.

### Stage 4: Report-facing analysis

Use the generated summaries to study:

- throughput,
- communication cost,
- microbatch effects,
- bubble or idle behavior,
- and completion, drop, or skip patterns under heavier configurations.

## What the Report Pipeline Produces

The report pipeline turns the branch into a proper experiment system rather than only a runtime.

The outputs include:

- per-run summaries,
- per-phase summaries,
- step-level timing summaries,
- microbatch trace data,
- communication summaries,
- quality metrics,
- and plots for report use.

This is what makes the branch suitable for a final project: it can support not only implementation claims, but measured comparisons and discussion.

## Common Failure Modes to Watch Early

When developing new experiments, the same categories tend to matter most:

- **Topology mistakes**
  - entity placement or connections do not match the intended parallel design.

- **Invalid parallel metadata**
  - workers are missing stage or TP placement information.

- **Phase-close issues**
  - a run appears hung because one component never reaches the close condition.

- **Overload and congestion**
  - the system continues running, but timing, drops, or skips show that the configuration has entered an unstable regime.

- **Device readiness**
  - one participant was not rebuilt, not started, or not reachable over SSH.

The safest response is usually not to scale the experiment down immediately, but to first decide whether the problem is:

- correctness,
- liveness,
- or just performance pressure.

## Where This Branch Stands Conceptually

The `parallelism-project` branch should be viewed as a bridge between three things:

1. **A simplified conceptual understanding of PP and TP**
2. **A real distributed runtime with coordination and fault handling**
3. **A reproducible experiment pipeline for a course report**

That combination is the real achievement of the branch. It is not only an implementation of model-parallel ideas, and not only a simulator. It is a developer-usable workflow for building, testing, and studying parallel experiments in NErlNet.

## Suggested First Tasks for a New Developer

If you are joining this branch and want the shortest practical path:

1. build NErlNet with Torch support on all participating nodes,
2. run the NIF and full-flow parallel tests,
3. run the report pipeline quick smoke,
4. inspect the generated experiment matrix,
5. then modify one small two-device experiment before touching larger hybrid runs.

That sequence gives you a much better chance of understanding failures for the right reason instead of treating every issue as a generic distributed bug.
