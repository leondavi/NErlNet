"""
Pipeline + tensor parallelism simulator with very detailed logging.

This script intentionally uses tiny integer tensors and explicit matrix math so each
operation is easy to trace and verify by hand.

It demonstrates:
1) GPipe schedule (all forward, then all backward)
2) 1F1B schedule (warmup, interleaved forward/backward, drain)
3) Interleaved schedule using virtual pipeline chunks

Tensor parallel mechanisms demonstrated in layers:
- Column-parallel linear (split output columns across TP ranks + all-gather)
- Row-parallel linear (split input columns across TP ranks + all-reduce)
"""

from __future__ import annotations

from dataclasses import dataclass
from typing import Dict, List, Sequence, Tuple

Tensor = List[List[int]]
Vector = List[int]


class TraceLogger:
    def __init__(self) -> None:
        self.step = 0

    def log(self, message: str) -> None:
        self.step += 1
        print(f"[{self.step:05d}] {message}")

    def matrix(self, name: str, matrix: Tensor) -> None:
        rows, cols = shape_of(matrix)
        self.log(f"{name} (shape {rows}x{cols})")
        for r, row in enumerate(matrix):
            self.log(f"  row {r}: {row}")

    def vector(self, name: str, vector: Vector) -> None:
        self.log(f"{name} (len {len(vector)}): {vector}")


def shape_of(matrix: Tensor) -> Tuple[int, int]:
    if not matrix:
        return 0, 0
    return len(matrix), len(matrix[0])


def copy_matrix(matrix: Tensor) -> Tensor:
    return [row[:] for row in matrix]


def zeros(rows: int, cols: int) -> Tensor:
    return [[0 for _ in range(cols)] for _ in range(rows)]


def zeros_like(matrix: Tensor) -> Tensor:
    r, c = shape_of(matrix)
    return zeros(r, c)


def zeros_vector(length: int) -> Vector:
    return [0 for _ in range(length)]


def transpose(matrix: Tensor, logger: TraceLogger, label: str) -> Tensor:
    rows, cols = shape_of(matrix)
    logger.log(f"{label}: transpose from {rows}x{cols} to {cols}x{rows}")
    out = zeros(cols, rows)
    for i in range(rows):
        for j in range(cols):
            out[j][i] = matrix[i][j]
            logger.log(
                f"{label}: out[{j}][{i}] = in[{i}][{j}] = {matrix[i][j]}"
            )
    logger.matrix(f"{label}: transpose result", out)
    return out


def matmul(a: Tensor, b: Tensor, logger: TraceLogger, label: str) -> Tensor:
    a_rows, a_cols = shape_of(a)
    b_rows, b_cols = shape_of(b)
    if a_cols != b_rows:
        raise ValueError(
            f"{label}: shape mismatch for matmul: {a_rows}x{a_cols} @ {b_rows}x{b_cols}"
        )
    logger.log(f"{label}: matmul {a_rows}x{a_cols} @ {b_rows}x{b_cols}")
    out = zeros(a_rows, b_cols)
    for i in range(a_rows):
        for j in range(b_cols):
            terms: List[str] = []
            total = 0
            for k in range(a_cols):
                mult = a[i][k] * b[k][j]
                terms.append(f"{a[i][k]}*{b[k][j]}={mult}")
                total += mult
            out[i][j] = total
            logger.log(
                f"{label}: out[{i}][{j}] = "
                + " + ".join(terms)
                + f" = {total}"
            )
    logger.matrix(f"{label}: matmul result", out)
    return out


def add_bias(matrix: Tensor, bias: Vector, logger: TraceLogger, label: str) -> Tensor:
    rows, cols = shape_of(matrix)
    if cols != len(bias):
        raise ValueError(
            f"{label}: cannot add bias len={len(bias)} to matrix with cols={cols}"
        )
    logger.log(f"{label}: add bias {bias} to matrix shape {rows}x{cols}")
    out = zeros(rows, cols)
    for i in range(rows):
        for j in range(cols):
            out[i][j] = matrix[i][j] + bias[j]
            logger.log(
                f"{label}: out[{i}][{j}] = {matrix[i][j]} + {bias[j]} = {out[i][j]}"
            )
    logger.matrix(f"{label}: add_bias result", out)
    return out


def split_columns(
    matrix: Tensor, sizes: Sequence[int], logger: TraceLogger, label: str
) -> List[Tensor]:
    rows, cols = shape_of(matrix)
    if sum(sizes) != cols:
        raise ValueError(
            f"{label}: split sizes {list(sizes)} do not sum to {cols} columns"
        )
    logger.log(f"{label}: split matrix columns by sizes {list(sizes)}")
    shards: List[Tensor] = []
    start = 0
    for shard_id, width in enumerate(sizes):
        shard = [row[start : start + width] for row in matrix]
        shards.append(shard)
        logger.matrix(f"{label}: shard {shard_id}", shard)
        start += width
    return shards


def concat_columns(parts: Sequence[Tensor], logger: TraceLogger, label: str) -> Tensor:
    if not parts:
        return []
    row_count = len(parts[0])
    for idx, part in enumerate(parts):
        if len(part) != row_count:
            raise ValueError(f"{label}: part {idx} row count mismatch")
    logger.log(f"{label}: concat {len(parts)} parts along columns")
    out: Tensor = []
    for r in range(row_count):
        row: List[int] = []
        for p, part in enumerate(parts):
            logger.log(f"{label}: taking row {r} from part {p}: {part[r]}")
            row.extend(part[r])
        out.append(row)
    logger.matrix(f"{label}: concat result", out)
    return out


def sum_matrices(parts: Sequence[Tensor], logger: TraceLogger, label: str) -> Tensor:
    if not parts:
        return []
    rows, cols = shape_of(parts[0])
    for idx, part in enumerate(parts):
        if shape_of(part) != (rows, cols):
            raise ValueError(f"{label}: part {idx} shape mismatch")
    logger.log(f"{label}: summing {len(parts)} matrices of shape {rows}x{cols}")
    out = zeros(rows, cols)
    for i in range(rows):
        for j in range(cols):
            terms: List[str] = []
            total = 0
            for part in parts:
                terms.append(str(part[i][j]))
                total += part[i][j]
            out[i][j] = total
            logger.log(f"{label}: out[{i}][{j}] = {' + '.join(terms)} = {total}")
    logger.matrix(f"{label}: sum result", out)
    return out


def add_in_place_matrix(
    accumulator: Tensor, increment: Tensor, logger: TraceLogger, label: str
) -> None:
    rows, cols = shape_of(accumulator)
    if shape_of(increment) != (rows, cols):
        raise ValueError(f"{label}: matrix shape mismatch for accumulation")
    logger.log(f"{label}: accumulate matrix gradient")
    for i in range(rows):
        for j in range(cols):
            before = accumulator[i][j]
            accumulator[i][j] += increment[i][j]
            logger.log(
                f"{label}: acc[{i}][{j}] = {before} + {increment[i][j]} = {accumulator[i][j]}"
            )


def add_in_place_vector(
    accumulator: Vector, increment: Vector, logger: TraceLogger, label: str
) -> None:
    if len(accumulator) != len(increment):
        raise ValueError(f"{label}: vector length mismatch for accumulation")
    logger.log(f"{label}: accumulate vector gradient")
    for i in range(len(accumulator)):
        before = accumulator[i]
        accumulator[i] += increment[i]
        logger.log(f"{label}: acc[{i}] = {before} + {increment[i]} = {accumulator[i]}")


def sum_rows(matrix: Tensor, logger: TraceLogger, label: str) -> Vector:
    rows, cols = shape_of(matrix)
    logger.log(f"{label}: sum rows over matrix shape {rows}x{cols}")
    out = zeros_vector(cols)
    for i in range(rows):
        for j in range(cols):
            before = out[j]
            out[j] += matrix[i][j]
            logger.log(f"{label}: out[{j}] = {before} + {matrix[i][j]} = {out[j]}")
    logger.vector(f"{label}: row-sum result", out)
    return out


def sse_loss_and_grad(
    prediction: Tensor, target: Tensor, logger: TraceLogger, label: str
) -> Tuple[int, Tensor]:
    rows, cols = shape_of(prediction)
    if shape_of(target) != (rows, cols):
        raise ValueError(f"{label}: target shape mismatch")
    logger.log(f"{label}: SSE loss over shape {rows}x{cols}")
    grad = zeros(rows, cols)
    total_loss = 0
    for i in range(rows):
        for j in range(cols):
            diff = prediction[i][j] - target[i][j]
            sq = diff * diff
            total_loss += sq
            grad[i][j] = 2 * diff
            logger.log(
                f"{label}: pred[{i}][{j}]={prediction[i][j]}, target={target[i][j]}, "
                f"diff={diff}, sq={sq}, grad={grad[i][j]}"
            )
    logger.log(f"{label}: total loss = {total_loss}")
    logger.matrix(f"{label}: dLoss/dPrediction", grad)
    return total_loss, grad


class DenseLinear:
    def __init__(self, name: str, weight: Tensor, bias: Vector) -> None:
        self.name = name
        self.weight = copy_matrix(weight)
        self.bias = bias[:]
        self.grad_weight = zeros_like(self.weight)
        self.grad_bias = zeros_vector(len(self.bias))
        self.input_cache: Dict[int, Tensor] = {}

    def zero_grad(self, logger: TraceLogger) -> None:
        self.grad_weight = zeros_like(self.weight)
        self.grad_bias = zeros_vector(len(self.bias))
        logger.log(f"{self.name}: gradients reset to zero")

    def describe(self, logger: TraceLogger) -> None:
        logger.matrix(f"{self.name}: weight", self.weight)
        logger.vector(f"{self.name}: bias", self.bias)

    def forward(self, microbatch_id: int, x: Tensor, logger: TraceLogger) -> Tensor:
        logger.log(f"{self.name}: FORWARD for microbatch {microbatch_id}")
        logger.matrix(f"{self.name}: input", x)
        logger.matrix(f"{self.name}: weight", self.weight)
        logger.vector(f"{self.name}: bias", self.bias)
        z = matmul(x, self.weight, logger, f"{self.name} forward matmul")
        y = add_bias(z, self.bias, logger, f"{self.name} forward add_bias")
        self.input_cache[microbatch_id] = copy_matrix(x)
        logger.log(f"{self.name}: cached input for microbatch {microbatch_id}")
        logger.matrix(f"{self.name}: output", y)
        return y

    def backward(
        self, microbatch_id: int, grad_output: Tensor, logger: TraceLogger
    ) -> Tensor:
        logger.log(f"{self.name}: BACKWARD for microbatch {microbatch_id}")
        logger.matrix(f"{self.name}: incoming grad_output", grad_output)
        x = self.input_cache.pop(microbatch_id)
        logger.matrix(f"{self.name}: cached input", x)

        weight_t = transpose(self.weight, logger, f"{self.name} backward weight^T")
        grad_input = matmul(
            grad_output, weight_t, logger, f"{self.name} backward grad_input"
        )

        x_t = transpose(x, logger, f"{self.name} backward input^T")
        grad_weight_inc = matmul(
            x_t, grad_output, logger, f"{self.name} backward grad_weight"
        )
        grad_bias_inc = sum_rows(grad_output, logger, f"{self.name} backward grad_bias")

        add_in_place_matrix(
            self.grad_weight, grad_weight_inc, logger, f"{self.name} grad_weight accum"
        )
        add_in_place_vector(
            self.grad_bias, grad_bias_inc, logger, f"{self.name} grad_bias accum"
        )

        logger.matrix(f"{self.name}: outgoing grad_input", grad_input)
        return grad_input

    def step(self, lr: int, logger: TraceLogger) -> None:
        logger.log(f"{self.name}: PARAMETER UPDATE with lr={lr}")
        for i in range(len(self.weight)):
            for j in range(len(self.weight[0])):
                before = self.weight[i][j]
                delta = lr * self.grad_weight[i][j]
                self.weight[i][j] -= delta
                logger.log(
                    f"{self.name}: weight[{i}][{j}] = {before} - {delta} = {self.weight[i][j]}"
                )
        for j in range(len(self.bias)):
            before = self.bias[j]
            delta = lr * self.grad_bias[j]
            self.bias[j] -= delta
            logger.log(
                f"{self.name}: bias[{j}] = {before} - {delta} = {self.bias[j]}"
            )


class ColumnParallelLinear:
    def __init__(
        self, name: str, weight_shards: Sequence[Tensor], bias_shards: Sequence[Vector]
    ) -> None:
        if len(weight_shards) != len(bias_shards):
            raise ValueError(f"{name}: weight/bias shard count mismatch")
        self.name = name
        self.weight_shards = [copy_matrix(w) for w in weight_shards]
        self.bias_shards = [b[:] for b in bias_shards]
        self.grad_weight_shards = [zeros_like(w) for w in self.weight_shards]
        self.grad_bias_shards = [zeros_vector(len(b)) for b in self.bias_shards]
        self.input_cache: Dict[int, Tensor] = {}

    def output_shard_sizes(self) -> List[int]:
        return [shape_of(w)[1] for w in self.weight_shards]

    def zero_grad(self, logger: TraceLogger) -> None:
        self.grad_weight_shards = [zeros_like(w) for w in self.weight_shards]
        self.grad_bias_shards = [zeros_vector(len(b)) for b in self.bias_shards]
        logger.log(f"{self.name}: gradients reset to zero (column-parallel shards)")

    def describe(self, logger: TraceLogger) -> None:
        for r, w in enumerate(self.weight_shards):
            logger.matrix(f"{self.name}: weight shard rank {r}", w)
            logger.vector(f"{self.name}: bias shard rank {r}", self.bias_shards[r])

    def forward(self, microbatch_id: int, x: Tensor, logger: TraceLogger) -> Tensor:
        logger.log(
            f"{self.name}: FORWARD for microbatch {microbatch_id} (Column Parallel)"
        )
        self.input_cache[microbatch_id] = copy_matrix(x)
        logger.matrix(f"{self.name}: replicated input to TP ranks", x)

        local_outputs: List[Tensor] = []
        for rank, (w_shard, b_shard) in enumerate(
            zip(self.weight_shards, self.bias_shards)
        ):
            logger.log(
                f"{self.name}: TP rank {rank} local matmul on output-column shard"
            )
            logger.matrix(f"{self.name}: rank {rank} weight shard", w_shard)
            z_local = matmul(
                x, w_shard, logger, f"{self.name} rank {rank} forward matmul"
            )
            y_local = add_bias(
                z_local, b_shard, logger, f"{self.name} rank {rank} forward add_bias"
            )
            logger.matrix(f"{self.name}: rank {rank} local output", y_local)
            local_outputs.append(y_local)

        logger.log(f"{self.name}: all-gather local outputs from TP ranks")
        y = concat_columns(local_outputs, logger, f"{self.name} forward all_gather")
        logger.matrix(f"{self.name}: output after all-gather", y)
        return y

    def backward(
        self, microbatch_id: int, grad_output: Tensor, logger: TraceLogger
    ) -> Tensor:
        logger.log(
            f"{self.name}: BACKWARD for microbatch {microbatch_id} (Column Parallel)"
        )
        logger.matrix(f"{self.name}: incoming grad_output", grad_output)
        x = self.input_cache.pop(microbatch_id)
        logger.matrix(f"{self.name}: cached input", x)

        shard_sizes = self.output_shard_sizes()
        grad_output_shards = split_columns(
            grad_output, shard_sizes, logger, f"{self.name} backward split grad_output"
        )

        grad_input_partials: List[Tensor] = []
        for rank, (w_shard, go_shard) in enumerate(
            zip(self.weight_shards, grad_output_shards)
        ):
            logger.log(
                f"{self.name}: TP rank {rank} local backward on its output-column shard"
            )
            w_t = transpose(
                w_shard, logger, f"{self.name} rank {rank} backward shard_weight^T"
            )
            grad_input_local = matmul(
                go_shard,
                w_t,
                logger,
                f"{self.name} rank {rank} backward grad_input_local",
            )
            grad_input_partials.append(grad_input_local)

            x_t = transpose(x, logger, f"{self.name} rank {rank} backward input^T")
            grad_weight_inc = matmul(
                x_t,
                go_shard,
                logger,
                f"{self.name} rank {rank} backward grad_weight_local",
            )
            grad_bias_inc = sum_rows(
                go_shard, logger, f"{self.name} rank {rank} backward grad_bias_local"
            )

            add_in_place_matrix(
                self.grad_weight_shards[rank],
                grad_weight_inc,
                logger,
                f"{self.name} rank {rank} grad_weight accum",
            )
            add_in_place_vector(
                self.grad_bias_shards[rank],
                grad_bias_inc,
                logger,
                f"{self.name} rank {rank} grad_bias accum",
            )

        logger.log(
            f"{self.name}: all-reduce(sum) grad_input partials across TP ranks"
        )
        grad_input = sum_matrices(
            grad_input_partials, logger, f"{self.name} backward all_reduce_grad_input"
        )
        logger.matrix(f"{self.name}: outgoing grad_input", grad_input)
        return grad_input

    def step(self, lr: int, logger: TraceLogger) -> None:
        logger.log(f"{self.name}: PARAMETER UPDATE with lr={lr} (per TP shard)")
        for rank in range(len(self.weight_shards)):
            w = self.weight_shards[rank]
            gw = self.grad_weight_shards[rank]
            b = self.bias_shards[rank]
            gb = self.grad_bias_shards[rank]
            logger.log(f"{self.name}: updating shard rank {rank}")
            for i in range(len(w)):
                for j in range(len(w[0])):
                    before = w[i][j]
                    delta = lr * gw[i][j]
                    w[i][j] -= delta
                    logger.log(
                        f"{self.name}: shard {rank} weight[{i}][{j}] = "
                        f"{before} - {delta} = {w[i][j]}"
                    )
            for j in range(len(b)):
                before = b[j]
                delta = lr * gb[j]
                b[j] -= delta
                logger.log(
                    f"{self.name}: shard {rank} bias[{j}] = {before} - {delta} = {b[j]}"
                )


class RowParallelLinear:
    def __init__(
        self, name: str, weight_shards: Sequence[Tensor], bias: Vector
    ) -> None:
        self.name = name
        self.weight_shards = [copy_matrix(w) for w in weight_shards]
        self.bias = bias[:]
        self.grad_weight_shards = [zeros_like(w) for w in self.weight_shards]
        self.grad_bias = zeros_vector(len(self.bias))
        self.input_cache: Dict[int, Tensor] = {}

    def input_shard_sizes(self) -> List[int]:
        return [shape_of(w)[0] for w in self.weight_shards]

    def zero_grad(self, logger: TraceLogger) -> None:
        self.grad_weight_shards = [zeros_like(w) for w in self.weight_shards]
        self.grad_bias = zeros_vector(len(self.bias))
        logger.log(f"{self.name}: gradients reset to zero (row-parallel shards)")

    def describe(self, logger: TraceLogger) -> None:
        for r, w in enumerate(self.weight_shards):
            logger.matrix(f"{self.name}: weight shard rank {r}", w)
        logger.vector(f"{self.name}: shared bias", self.bias)

    def forward(self, microbatch_id: int, x: Tensor, logger: TraceLogger) -> Tensor:
        logger.log(f"{self.name}: FORWARD for microbatch {microbatch_id} (Row Parallel)")
        self.input_cache[microbatch_id] = copy_matrix(x)
        logger.matrix(f"{self.name}: full input", x)

        shard_sizes = self.input_shard_sizes()
        x_shards = split_columns(x, shard_sizes, logger, f"{self.name} forward input split")

        local_partials: List[Tensor] = []
        for rank, (x_shard, w_shard) in enumerate(zip(x_shards, self.weight_shards)):
            logger.log(
                f"{self.name}: TP rank {rank} local matmul on input-row shard"
            )
            logger.matrix(f"{self.name}: rank {rank} x shard", x_shard)
            logger.matrix(f"{self.name}: rank {rank} weight shard", w_shard)
            partial = matmul(
                x_shard, w_shard, logger, f"{self.name} rank {rank} forward matmul"
            )
            logger.matrix(f"{self.name}: rank {rank} local partial output", partial)
            local_partials.append(partial)

        logger.log(
            f"{self.name}: all-reduce(sum) local partial outputs across TP ranks"
        )
        reduced = sum_matrices(local_partials, logger, f"{self.name} forward all_reduce")
        y = add_bias(reduced, self.bias, logger, f"{self.name} forward add_bias")
        logger.matrix(f"{self.name}: output", y)
        return y

    def backward(
        self, microbatch_id: int, grad_output: Tensor, logger: TraceLogger
    ) -> Tensor:
        logger.log(f"{self.name}: BACKWARD for microbatch {microbatch_id} (Row Parallel)")
        logger.matrix(f"{self.name}: incoming grad_output", grad_output)
        x = self.input_cache.pop(microbatch_id)
        logger.matrix(f"{self.name}: cached full input", x)

        shard_sizes = self.input_shard_sizes()
        x_shards = split_columns(x, shard_sizes, logger, f"{self.name} backward input split")

        grad_input_shards: List[Tensor] = []
        for rank, (x_shard, w_shard) in enumerate(zip(x_shards, self.weight_shards)):
            logger.log(
                f"{self.name}: TP rank {rank} local backward on its input-row shard"
            )
            w_t = transpose(
                w_shard, logger, f"{self.name} rank {rank} backward shard_weight^T"
            )
            grad_x_shard = matmul(
                grad_output,
                w_t,
                logger,
                f"{self.name} rank {rank} backward grad_input_shard",
            )
            grad_input_shards.append(grad_x_shard)

            x_t = transpose(
                x_shard, logger, f"{self.name} rank {rank} backward input_shard^T"
            )
            grad_weight_inc = matmul(
                x_t,
                grad_output,
                logger,
                f"{self.name} rank {rank} backward grad_weight_local",
            )
            add_in_place_matrix(
                self.grad_weight_shards[rank],
                grad_weight_inc,
                logger,
                f"{self.name} rank {rank} grad_weight accum",
            )

        grad_bias_inc = sum_rows(grad_output, logger, f"{self.name} backward grad_bias")
        add_in_place_vector(
            self.grad_bias, grad_bias_inc, logger, f"{self.name} grad_bias accum"
        )

        logger.log(
            f"{self.name}: gather grad_input shards (each rank owns different input columns)"
        )
        grad_input = concat_columns(
            grad_input_shards, logger, f"{self.name} backward gather_grad_input"
        )
        logger.matrix(f"{self.name}: outgoing grad_input", grad_input)
        return grad_input

    def step(self, lr: int, logger: TraceLogger) -> None:
        logger.log(f"{self.name}: PARAMETER UPDATE with lr={lr} (row-parallel shards)")
        for rank in range(len(self.weight_shards)):
            w = self.weight_shards[rank]
            gw = self.grad_weight_shards[rank]
            logger.log(f"{self.name}: updating shard rank {rank}")
            for i in range(len(w)):
                for j in range(len(w[0])):
                    before = w[i][j]
                    delta = lr * gw[i][j]
                    w[i][j] -= delta
                    logger.log(
                        f"{self.name}: shard {rank} weight[{i}][{j}] = "
                        f"{before} - {delta} = {w[i][j]}"
                    )
        for j in range(len(self.bias)):
            before = self.bias[j]
            delta = lr * self.grad_bias[j]
            self.bias[j] -= delta
            logger.log(f"{self.name}: bias[{j}] = {before} - {delta} = {self.bias[j]}")


@dataclass
class PipelineChunk:
    name: str
    physical_stage_id: int
    layer: object


class PipelineModel:
    def __init__(self, chunks: Sequence[PipelineChunk]) -> None:
        self.chunks = list(chunks)

    def zero_grad(self, logger: TraceLogger) -> None:
        logger.log("Model: zero_grad on all chunks")
        for chunk in self.chunks:
            chunk.layer.zero_grad(logger)

    def describe(self, logger: TraceLogger) -> None:
        logger.log("Model: parameter snapshot")
        for i, chunk in enumerate(self.chunks):
            logger.log(
                f"Chunk {i}: {chunk.name}, physical_stage={chunk.physical_stage_id}"
            )
            chunk.layer.describe(logger)

    def forward_microbatch(
        self, microbatch_id: int, x: Tensor, logger: TraceLogger
    ) -> Tensor:
        logger.log(f"Model: FORWARD pass start for microbatch {microbatch_id}")
        activation = copy_matrix(x)
        for i, chunk in enumerate(self.chunks):
            logger.log(
                f"Model: forward microbatch {microbatch_id} entering chunk {i} "
                f"({chunk.name}) on physical stage {chunk.physical_stage_id}"
            )
            activation = chunk.layer.forward(microbatch_id, activation, logger)
            logger.matrix(
                f"Model: microbatch {microbatch_id} activation after chunk {i}",
                activation,
            )
        logger.log(f"Model: FORWARD pass end for microbatch {microbatch_id}")
        return activation

    def backward_microbatch(
        self, microbatch_id: int, grad_output: Tensor, logger: TraceLogger
    ) -> Tensor:
        logger.log(f"Model: BACKWARD pass start for microbatch {microbatch_id}")
        grad = copy_matrix(grad_output)
        for rev_idx in range(len(self.chunks) - 1, -1, -1):
            chunk = self.chunks[rev_idx]
            logger.log(
                f"Model: backward microbatch {microbatch_id} entering chunk {rev_idx} "
                f"({chunk.name}) on physical stage {chunk.physical_stage_id}"
            )
            grad = chunk.layer.backward(microbatch_id, grad, logger)
            logger.matrix(
                f"Model: microbatch {microbatch_id} grad after chunk {rev_idx}",
                grad,
            )
        logger.log(f"Model: BACKWARD pass end for microbatch {microbatch_id}")
        return grad

    def step(self, lr: int, logger: TraceLogger) -> None:
        logger.log("Model: optimizer step on all chunks")
        for chunk in self.chunks:
            chunk.layer.step(lr, logger)


def build_demo_model(interleaved_mapping: bool) -> PipelineModel:
    l0 = DenseLinear(
        name="L0_Dense",
        weight=[[1, 0], [2, 1]],
        bias=[0, 1],
    )

    l1 = ColumnParallelLinear(
        name="L1_ColumnTP",
        weight_shards=[
            [[1], [0]],  # rank 0 shard
            [[2], [1]],  # rank 1 shard
        ],
        bias_shards=[
            [1],  # rank 0 bias shard
            [0],  # rank 1 bias shard
        ],
    )

    l2 = RowParallelLinear(
        name="L2_RowTP",
        weight_shards=[
            [[1, 1]],  # rank 0: input feature 0
            [[2, 0]],  # rank 1: input feature 1
        ],
        bias=[0, 1],
    )

    l3 = DenseLinear(
        name="L3_Dense",
        weight=[[1, 0], [0, 1]],
        bias=[0, 0],
    )

    if interleaved_mapping:
        # Virtual chunks alternate physical stages: 0,1,0,1
        physical_map = [0, 1, 0, 1]
    else:
        # Standard contiguous mapping: first half on stage 0, second half on stage 1
        physical_map = [0, 0, 1, 1]

    chunks = [
        PipelineChunk("Chunk0_L0_Dense", physical_map[0], l0),
        PipelineChunk("Chunk1_L1_ColumnTP", physical_map[1], l1),
        PipelineChunk("Chunk2_L2_RowTP", physical_map[2], l2),
        PipelineChunk("Chunk3_L3_Dense", physical_map[3], l3),
    ]
    return PipelineModel(chunks)


def demo_microbatches() -> Tuple[List[Tensor], List[Tensor]]:
    inputs = [
        [[1, 2]],
        [[0, 1]],
        [[2, 0]],
        [[1, 1]],
        [[3, 1]],
    ]
    targets = [
        [[2, 1]],
        [[1, 0]],
        [[2, 2]],
        [[1, 1]],
        [[3, 2]],
    ]
    return inputs, targets


def forward_with_loss_capture(
    model: PipelineModel,
    microbatch_id: int,
    x: Tensor,
    target: Tensor,
    loss_grads: Dict[int, Tensor],
    logger: TraceLogger,
    scheduler_name: str,
) -> int:
    logger.log(
        f"{scheduler_name}: FORWARD microbatch {microbatch_id} (compute prediction + loss)"
    )
    prediction = model.forward_microbatch(microbatch_id, x, logger)
    loss, grad_loss_pred = sse_loss_and_grad(
        prediction, target, logger, f"{scheduler_name} mb{microbatch_id} loss"
    )
    loss_grads[microbatch_id] = grad_loss_pred
    return loss


def backward_from_stored_loss(
    model: PipelineModel,
    microbatch_id: int,
    loss_grads: Dict[int, Tensor],
    logger: TraceLogger,
    scheduler_name: str,
) -> None:
    logger.log(f"{scheduler_name}: BACKWARD microbatch {microbatch_id}")
    grad_loss_pred = loss_grads[microbatch_id]
    model.backward_microbatch(microbatch_id, grad_loss_pred, logger)


def run_gpipe(
    model: PipelineModel,
    inputs: List[Tensor],
    targets: List[Tensor],
    lr: int,
    logger: TraceLogger,
) -> int:
    scheduler_name = "GPipe"
    logger.log("=" * 80)
    logger.log(
        "Running GPipe schedule: all forward microbatches first, then all backward microbatches."
    )
    model.zero_grad(logger)
    model.describe(logger)

    total_loss = 0
    loss_grads: Dict[int, Tensor] = {}

    for mb in range(len(inputs)):
        logger.log(f"{scheduler_name}: forward phase step for microbatch {mb}")
        total_loss += forward_with_loss_capture(
            model, mb, inputs[mb], targets[mb], loss_grads, logger, scheduler_name
        )

    for mb in range(len(inputs) - 1, -1, -1):
        logger.log(f"{scheduler_name}: backward phase step for microbatch {mb}")
        backward_from_stored_loss(model, mb, loss_grads, logger, scheduler_name)

    logger.log(f"{scheduler_name}: total loss before update = {total_loss}")
    model.step(lr, logger)
    logger.log(f"{scheduler_name}: parameter snapshot after update")
    model.describe(logger)
    return total_loss


def run_1f1b(
    model: PipelineModel,
    inputs: List[Tensor],
    targets: List[Tensor],
    lr: int,
    physical_pipeline_stages: int,
    logger: TraceLogger,
) -> int:
    scheduler_name = "1F1B"
    logger.log("=" * 80)
    logger.log(
        "Running 1F1B schedule: warmup forwards, then interleaved (1 forward + 1 backward), then drain backwards."
    )
    model.zero_grad(logger)
    model.describe(logger)

    total_loss = 0
    loss_grads: Dict[int, Tensor] = {}
    m = len(inputs)
    warmup = max(0, physical_pipeline_stages - 1)
    warmup = min(warmup, m)
    logger.log(f"{scheduler_name}: physical stages = {physical_pipeline_stages}")
    logger.log(f"{scheduler_name}: warmup microbatches = {warmup}")

    for mb in range(warmup):
        logger.log(f"{scheduler_name}: warmup forward for microbatch {mb}")
        total_loss += forward_with_loss_capture(
            model, mb, inputs[mb], targets[mb], loss_grads, logger, scheduler_name
        )

    for mb in range(warmup, m):
        logger.log(
            f"{scheduler_name}: interleaved step -> forward microbatch {mb}, "
            f"then backward microbatch {mb - warmup}"
        )
        total_loss += forward_with_loss_capture(
            model, mb, inputs[mb], targets[mb], loss_grads, logger, scheduler_name
        )
        backward_from_stored_loss(
            model, mb - warmup, loss_grads, logger, scheduler_name
        )

    for mb in range(m - warmup, m):
        logger.log(f"{scheduler_name}: drain backward for microbatch {mb}")
        backward_from_stored_loss(model, mb, loss_grads, logger, scheduler_name)

    logger.log(f"{scheduler_name}: total loss before update = {total_loss}")
    model.step(lr, logger)
    logger.log(f"{scheduler_name}: parameter snapshot after update")
    model.describe(logger)
    return total_loss


def run_interleaved(
    model: PipelineModel,
    inputs: List[Tensor],
    targets: List[Tensor],
    lr: int,
    virtual_pipeline_chunks: int,
    logger: TraceLogger,
) -> int:
    scheduler_name = "Interleaved"
    logger.log("=" * 80)
    logger.log(
        "Running Interleaved schedule: warmup based on virtual chunks, then "
        "interleaved forward/backward, then drain."
    )
    logger.log(
        "Note: this is a conceptual scheduler simulation (easy-to-follow ordering), "
        "not a cycle-accurate hardware runtime."
    )
    model.zero_grad(logger)
    model.describe(logger)

    total_loss = 0
    loss_grads: Dict[int, Tensor] = {}
    m = len(inputs)
    warmup = max(0, virtual_pipeline_chunks - 1)
    warmup = min(warmup, m)
    logger.log(f"{scheduler_name}: virtual chunks = {virtual_pipeline_chunks}")
    logger.log(f"{scheduler_name}: warmup microbatches = {warmup}")

    for mb in range(warmup):
        logger.log(f"{scheduler_name}: warmup forward for microbatch {mb}")
        total_loss += forward_with_loss_capture(
            model, mb, inputs[mb], targets[mb], loss_grads, logger, scheduler_name
        )

    for mb in range(warmup, m):
        logger.log(
            f"{scheduler_name}: interleaved step -> forward microbatch {mb}, "
            f"then backward microbatch {mb - warmup}"
        )
        total_loss += forward_with_loss_capture(
            model, mb, inputs[mb], targets[mb], loss_grads, logger, scheduler_name
        )
        backward_from_stored_loss(
            model, mb - warmup, loss_grads, logger, scheduler_name
        )

    for mb in range(m - warmup, m):
        logger.log(f"{scheduler_name}: drain backward for microbatch {mb}")
        backward_from_stored_loss(model, mb, loss_grads, logger, scheduler_name)

    logger.log(f"{scheduler_name}: total loss before update = {total_loss}")
    model.step(lr, logger)
    logger.log(f"{scheduler_name}: parameter snapshot after update")
    model.describe(logger)
    return total_loss


def main() -> None:
    logger = TraceLogger()
    lr = 1

    logger.log("Preparing integer microbatches and integer targets.")
    inputs, targets = demo_microbatches()
    logger.log(f"Microbatch count = {len(inputs)}")
    for mb in range(len(inputs)):
        logger.matrix(f"Input microbatch {mb}", inputs[mb])
        logger.matrix(f"Target microbatch {mb}", targets[mb])

    logger.log("=" * 80)
    logger.log("Build model for GPipe (standard 2-stage mapping).")
    gpipe_model = build_demo_model(interleaved_mapping=False)
    gpipe_loss = run_gpipe(gpipe_model, inputs, targets, lr, logger)

    logger.log("=" * 80)
    logger.log("Build model for 1F1B (standard 2-stage mapping).")
    f1b1_model = build_demo_model(interleaved_mapping=False)
    f1b1_loss = run_1f1b(
        f1b1_model,
        inputs,
        targets,
        lr,
        physical_pipeline_stages=2,
        logger=logger,
    )

    logger.log("=" * 80)
    logger.log("Build model for Interleaved schedule (virtual chunks alternate stages).")
    interleaved_model = build_demo_model(interleaved_mapping=True)
    interleaved_loss = run_interleaved(
        interleaved_model,
        inputs,
        targets,
        lr,
        virtual_pipeline_chunks=4,
        logger=logger,
    )

    logger.log("=" * 80)
    logger.log("FINAL LOSS SUMMARY (before each model update):")
    logger.log(f"GPipe loss = {gpipe_loss}")
    logger.log(f"1F1B loss = {f1b1_loss}")
    logger.log(f"Interleaved loss = {interleaved_loss}")
    logger.log(
        "If the schedules are consistent with this synchronous simulator, "
        "these losses should match."
    )


if __name__ == "__main__":
    main()
