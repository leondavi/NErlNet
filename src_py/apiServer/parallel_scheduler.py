"""Deterministic schedule builders for pipeline-oriented parallel execution.

This module is intentionally framework-agnostic so it can be used by:
- parser/validation logic (phase sanity checks)
- runtime orchestration traces
- unit tests that lock ordering semantics
"""

from __future__ import annotations

from dataclasses import dataclass
from typing import Dict, Iterable, List, Sequence, Tuple


Direction = str


@dataclass(frozen=True)
class ScheduleEvent:
    """A single scheduler event for one stage and microbatch."""

    direction: Direction  # "forward" | "backward"
    microbatch_id: int
    stage_id: int


def build_gpipe_schedule(stage_count: int, num_microbatches: int) -> List[ScheduleEvent]:
    _validate_scheduler_dims(stage_count, num_microbatches)
    events: List[ScheduleEvent] = []
    for microbatch_id in range(num_microbatches):
        for stage_id in range(stage_count):
            events.append(ScheduleEvent("forward", microbatch_id, stage_id))
    for microbatch_id in range(num_microbatches - 1, -1, -1):
        for stage_id in range(stage_count - 1, -1, -1):
            events.append(ScheduleEvent("backward", microbatch_id, stage_id))
    return events


def build_1f1b_schedule(stage_count: int, num_microbatches: int) -> List[ScheduleEvent]:
    _validate_scheduler_dims(stage_count, num_microbatches)
    warmup = max(0, stage_count - 1)
    events: List[ScheduleEvent] = []

    for microbatch_id in range(num_microbatches):
        for stage_id in range(stage_count):
            events.append(ScheduleEvent("forward", microbatch_id, stage_id))

        if microbatch_id >= warmup:
            backward_mb = microbatch_id - warmup
            for stage_id in range(stage_count - 1, -1, -1):
                events.append(ScheduleEvent("backward", backward_mb, stage_id))

    for backward_mb in range(max(0, num_microbatches - warmup), num_microbatches):
        for stage_id in range(stage_count - 1, -1, -1):
            events.append(ScheduleEvent("backward", backward_mb, stage_id))

    return events


def build_interleaved_schedule(
    stage_count: int,
    num_microbatches: int,
    virtual_stages: int,
) -> List[ScheduleEvent]:
    _validate_scheduler_dims(stage_count, num_microbatches)
    if virtual_stages < 1:
        raise ValueError("virtual_stages must be >= 1")

    # Interleaving is represented as a flattened virtual pipeline where each
    # physical stage owns `virtual_stages` chunks.
    return build_1f1b_schedule(stage_count * virtual_stages, num_microbatches)


def build_scheduler_schedule(
    scheduler: str,
    stage_count: int,
    num_microbatches: int,
    virtual_stages: int = 1,
) -> List[ScheduleEvent]:
    normalized = scheduler.strip().lower()
    if normalized == "gpipe":
        return build_gpipe_schedule(stage_count, num_microbatches)
    if normalized == "1f1b":
        return build_1f1b_schedule(stage_count, num_microbatches)
    if normalized == "interleaved":
        return build_interleaved_schedule(stage_count, num_microbatches, virtual_stages)
    raise ValueError(f"Unsupported scheduler '{scheduler}'")


def summarize_microbatch_lifecycle(events: Sequence[ScheduleEvent]) -> Dict[int, Tuple[int, int]]:
    """Return per-microbatch counts as {id: (num_forwards, num_backwards)}."""

    summary: Dict[int, List[int]] = {}
    for event in events:
        counts = summary.setdefault(event.microbatch_id, [0, 0])
        if event.direction == "forward":
            counts[0] += 1
        elif event.direction == "backward":
            counts[1] += 1
        else:
            raise ValueError(f"Unknown direction '{event.direction}'")
    return {microbatch_id: (counts[0], counts[1]) for microbatch_id, counts in summary.items()}


def assert_scheduler_trace_integrity(
    events: Sequence[ScheduleEvent],
    stage_count: int,
    num_microbatches: int,
) -> None:
    """Validate no duplicates/orphans and complete forward/backward coverage."""

    _validate_scheduler_dims(stage_count, num_microbatches)
    expected_cardinality = stage_count * num_microbatches

    seen_forward = set()
    seen_backward = set()

    for event in events:
        key = (event.microbatch_id, event.stage_id)
        if event.microbatch_id < 0 or event.microbatch_id >= num_microbatches:
            raise AssertionError(f"Invalid microbatch id in event: {event}")
        if event.stage_id < 0 or event.stage_id >= stage_count:
            raise AssertionError(f"Invalid stage id in event: {event}")
        if event.direction == "forward":
            if key in seen_forward:
                raise AssertionError(f"Duplicate forward transition for {key}")
            seen_forward.add(key)
        elif event.direction == "backward":
            if key in seen_backward:
                raise AssertionError(f"Duplicate backward transition for {key}")
            seen_backward.add(key)
        else:
            raise AssertionError(f"Invalid direction in event: {event}")

    if len(seen_forward) != expected_cardinality:
        raise AssertionError(
            f"Forward coverage mismatch. expected={expected_cardinality} got={len(seen_forward)}"
        )
    if len(seen_backward) != expected_cardinality:
        raise AssertionError(
            f"Backward coverage mismatch. expected={expected_cardinality} got={len(seen_backward)}"
        )


def infer_stage_world_size(worker_parallel_map: Dict[str, Dict[str, int]]) -> int:
    """Infer stage world size from worker parallel metadata."""

    max_stage = -1
    max_world = 0
    for _, cfg in worker_parallel_map.items():
        pipeline_stage = cfg.get("pipelineStage")
        pipeline_world_size = cfg.get("pipelineWorldSize")
        if isinstance(pipeline_stage, int):
            max_stage = max(max_stage, pipeline_stage)
        if isinstance(pipeline_world_size, int):
            max_world = max(max_world, pipeline_world_size)
    return max(1, max_world, max_stage + 1)


def _validate_scheduler_dims(stage_count: int, num_microbatches: int) -> None:
    if stage_count < 1:
        raise ValueError("stage_count must be >= 1")
    if num_microbatches < 1:
        raise ValueError("num_microbatches must be >= 1")
