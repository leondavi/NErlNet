#!/usr/bin/env python3
"""Generate a TorchScript model that mirrors the synthetic perceptron used by NerlnetNifTest.

The OpenNN NIF test (`nerlTests.erl`) exercises a 4-layer perceptron with synthetic data
(5 input features, 30 and 5 hidden neurons, and 3 output neurons). This script builds an
identical architecture in PyTorch, fits it on similarly generated synthetic samples, and
exports the trained network as a TorchScript `.pt` artefact that Nerlnet's Torch worker can
load during `NerlnetNIFTorchTest.sh`.
"""
from __future__ import annotations

import argparse
import pathlib
from typing import Sequence

import torch

DEFAULT_RELATIVE_OUTPUT = pathlib.Path("tests/inputTorchJsonsFiles/models/placeholder_perceptron.pt")


def parse_args() -> argparse.Namespace:
    repo_root = pathlib.Path(__file__).resolve().parents[2]
    default_output = (repo_root / DEFAULT_RELATIVE_OUTPUT).resolve()
    parser = argparse.ArgumentParser(description="Generate TorchScript test model for Nerlnet")
    parser.add_argument("--output", type=pathlib.Path, default=default_output,
                        help="Destination .pt file (default: %(default)s)")
    parser.add_argument("--samples", type=int, default=500, help="Synthetic sample count")
    parser.add_argument("--input-size", type=int, default=5, help="Input feature dimension")
    parser.add_argument("--hidden-sizes", type=int, nargs="*", default=[32,8],
                        help="Hidden layer sizes (default: 32 8)")
    parser.add_argument("--labels", type=int, default=3, help="Output dimension (number of labels)")
    parser.add_argument("--epochs", type=int, default=200, help="Training epochs")
    parser.add_argument("--lr", type=float, default=1e-2, help="Training learning rate")
    parser.add_argument("--seed", type=int, default=2024, help="Random seed for reproducibility")
    return parser.parse_args()


def generate_dataset(num_samples: int, num_features: int, num_labels: int, seed: int) -> tuple[torch.Tensor, torch.Tensor]:
    generator = torch.Generator().manual_seed(seed)
    features = torch.rand((num_samples, num_features), generator=generator) * 10.0
    weight = torch.randn((num_features, num_labels), generator=generator)
    bias = torch.randn(num_labels, generator=generator)
    logits = features @ weight + bias
    labels = torch.softmax(logits, dim=1)
    return features, labels


def build_model(input_size: int, hidden_sizes: Sequence[int], output_size: int) -> torch.nn.Module:
    layers = []
    in_dim = input_size
    for hidden in hidden_sizes:
        layers.append(torch.nn.Linear(in_dim, hidden))
        layers.append(torch.nn.ReLU())
        in_dim = hidden
    layers.append(torch.nn.Linear(in_dim, output_size))
    return torch.nn.Sequential(*layers)


class FeatureSliceModule(torch.nn.Module):
    """Wrap a backbone to ignore label columns appended to each sample."""

    def __init__(self, backbone: torch.nn.Module, feature_width: int) -> None:
        super().__init__()
        self.backbone = backbone
        self.feature_width = int(feature_width)

    def forward(self, inputs: torch.Tensor) -> torch.Tensor:  # pragma: no cover - scripted
        if inputs.size(-1) < self.feature_width:
            raise RuntimeError(
                f"Expected at least {self.feature_width} columns, received {inputs.size(-1)}"
            )
        features_only = inputs[..., : self.feature_width]
        return self.backbone(features_only)


def train_model(model: torch.nn.Module, features: torch.Tensor, labels: torch.Tensor, epochs: int, lr: float) -> None:
    """Train with a simple manual SGD step to avoid importing torch.optim."""
    criterion = torch.nn.MSELoss()
    for _ in range(max(1, epochs)):
        preds = model(features)
        loss = criterion(preds, labels)
        loss.backward()
        with torch.no_grad():
            for param in model.parameters():
                if param.grad is None:
                    continue
                param -= lr * param.grad
                param.grad.zero_()


def main() -> None:
    args = parse_args()
    torch.manual_seed(args.seed)
    features, labels = generate_dataset(args.samples, args.input_size, args.labels, args.seed)
    model = build_model(args.input_size, args.hidden_sizes, args.labels)
    train_model(model, features, labels, args.epochs, args.lr)
    model.eval()
    wrapped_model = FeatureSliceModule(model, args.input_size)
    scripted = torch.jit.script(wrapped_model)
    output_path = args.output.expanduser().resolve()
    output_path.parent.mkdir(parents=True, exist_ok=True)
    scripted.save(str(output_path))
    print(f"[TorchTestModel] Wrote TorchScript model to {output_path}")


if __name__ == "__main__":
    main()
