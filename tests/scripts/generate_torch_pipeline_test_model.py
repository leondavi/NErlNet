#!/usr/bin/env python3
"""Generate a pipeline-compatible TorchScript model for NIF pipeline/tensor parallelism tests.

Creates a simple feedforward model with individual layers exposed via a ``layers``
ModuleList attribute so that the Torch NIF bridge can discover and partition them
across pipeline stages at runtime.

Architecture (default):
  layers.0  Linear(5, 30)
  layers.1  ReLU
  layers.2  Linear(30, 5)
  layers.3  ReLU
  layers.4  Linear(5, 3)

With pipeline_world_size=2 the bridge partitions as:
  Stage 0 → layers 0,1,2   (input [B,5] → output [B,5])
  Stage 1 → layers 3,4     (input [B,5] → output [B,3])

With pipeline_world_size=3:
  Stage 0 → layers 0,1     (input [B,5] → output [B,30])
  Stage 1 → layers 2,3     (input [B,30] → output [B,5])
  Stage 2 → layer  4       (input [B,5]  → output [B,3])
"""
from __future__ import annotations

import argparse
import pathlib

import torch

DEFAULT_RELATIVE_OUTPUT = pathlib.Path(
    "tests/inputTorchJsonsFiles/models/placeholder_pipeline.pt"
)


class PipelineTestModel(torch.nn.Module):
    """Feedforward model with explicit ``layers`` attribute for pipeline partitioning."""

    def __init__(
        self, input_size: int, hidden_sizes: list[int], output_size: int
    ) -> None:
        super().__init__()
        layer_modules: list[torch.nn.Module] = []
        prev_size = input_size
        for hidden in hidden_sizes:
            layer_modules.append(torch.nn.Linear(prev_size, hidden))
            layer_modules.append(torch.nn.ReLU())
            prev_size = hidden
        layer_modules.append(torch.nn.Linear(prev_size, output_size))
        self.layers = torch.nn.ModuleList(layer_modules)

    def forward(self, x: torch.Tensor) -> torch.Tensor:
        for layer in self.layers:
            x = layer(x)
        return x


def parse_args() -> argparse.Namespace:
    repo_root = pathlib.Path(__file__).resolve().parents[2]
    default_output = (repo_root / DEFAULT_RELATIVE_OUTPUT).resolve()
    parser = argparse.ArgumentParser(
        description="Generate pipeline-compatible TorchScript test model"
    )
    parser.add_argument(
        "--output",
        type=pathlib.Path,
        default=default_output,
        help="Destination .pt file (default: %(default)s)",
    )
    parser.add_argument("--input-size", type=int, default=5)
    parser.add_argument("--hidden-sizes", type=int, nargs="*", default=[30, 5])
    parser.add_argument("--labels", type=int, default=3)
    parser.add_argument("--samples", type=int, default=200)
    parser.add_argument("--epochs", type=int, default=50)
    parser.add_argument("--lr", type=float, default=1e-2)
    parser.add_argument("--seed", type=int, default=2024)
    return parser.parse_args()


def main() -> None:
    args = parse_args()
    torch.manual_seed(args.seed)

    model = PipelineTestModel(args.input_size, args.hidden_sizes, args.labels)

    # Brief training to initialise weights to reasonable values
    features = torch.rand(args.samples, args.input_size) * 5.0
    labels = torch.rand(args.samples, args.labels)
    criterion = torch.nn.MSELoss()
    for _ in range(args.epochs):
        preds = model(features)
        loss = criterion(preds, labels)
        loss.backward()
        with torch.no_grad():
            for param in model.parameters():
                if param.grad is not None:
                    param -= args.lr * param.grad
                    param.grad.zero_()

    model.eval()
    scripted = torch.jit.script(model)
    output_path = args.output.expanduser().resolve()
    output_path.parent.mkdir(parents=True, exist_ok=True)
    scripted.save(str(output_path))
    print(f"[PipelineTestModel] Wrote TorchScript model to {output_path}")
    print(f"[PipelineTestModel] Layer count: {len(list(model.layers))}")
    for i, layer in enumerate(model.layers):
        print(f"  [{i}] {layer}")


if __name__ == "__main__":
    main()
