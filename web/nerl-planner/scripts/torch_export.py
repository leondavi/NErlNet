#!/usr/bin/env python3
import argparse
import hashlib
import json
import os
import re
import sys
from typing import Dict, List, Tuple

try:
    import torch
    import torch.nn as nn
except ImportError as exc:
    raise SystemExit(f"Torch not available: {exc}")


def slugify(name: str) -> str:
    safe = re.sub(r"[^a-zA-Z0-9_-]+", "_", name.strip())
    return safe.strip("_") or "torch_model"


def ensure_unique_dir(base_dir: str, slug: str) -> str:
    candidate = os.path.join(base_dir, slug)
    if not os.path.exists(candidate):
        os.makedirs(candidate, exist_ok=True)
        return candidate
    idx = 2
    while True:
        next_name = f"{slug}_{idx}"
        candidate = os.path.join(base_dir, next_name)
        if not os.path.exists(candidate):
            os.makedirs(candidate, exist_ok=True)
            return candidate
        idx += 1


def parse_shape(shape_text: str, batch_size: int) -> List[int]:
    cleaned = shape_text.strip().strip("[]")
    parts = [part for part in re.split(r"[,\s]+", cleaned) if part]
    if not parts:
        raise ValueError("Empty input shape")
    dims: List[int] = []
    for part in parts:
        if part.upper() == "N":
            dims.append(batch_size)
        else:
            dims.append(int(part))
    return dims


def conv_out(size: int, kernel: int, stride: int, padding: int) -> int:
    return (size + 2 * padding - kernel) // stride + 1


def to_pair(value, default: Tuple[int, int]) -> Tuple[int, int]:
    if value is None:
        return default
    if isinstance(value, list) and len(value) == 2:
        return int(value[0]), int(value[1])
    return int(value), int(value)


def to_int(value, default: int) -> int:
    if value is None:
        return default
    return int(value)


def infer_shape(node_type: str, params: Dict, input_shape: List[int]) -> List[int]:
    if node_type == "conv2d":
        batch, channels, height, width = input_shape
        out_channels = to_int(params.get("outChannels"), 1)
        kernel = to_pair(params.get("kernel"), (3, 3))
        stride = to_pair(params.get("stride"), (1, 1))
        padding = to_pair(params.get("padding"), (0, 0))
        out_h = conv_out(height, kernel[0], stride[0], padding[0])
        out_w = conv_out(width, kernel[1], stride[1], padding[1])
        return [batch, out_channels, out_h, out_w]
    if node_type == "maxpool2d":
        batch, channels, height, width = input_shape
        kernel = to_pair(params.get("kernel"), (2, 2))
        stride = to_pair(params.get("stride"), kernel)
        padding = to_pair(params.get("padding"), (0, 0))
        out_h = conv_out(height, kernel[0], stride[0], padding[0])
        out_w = conv_out(width, kernel[1], stride[1], padding[1])
        return [batch, channels, out_h, out_w]
    if node_type == "conv1d":
        batch, channels, length = input_shape
        out_channels = to_int(params.get("outChannels"), 1)
        kernel = to_int(params.get("kernel"), 3)
        stride = to_int(params.get("stride"), 1)
        padding = to_int(params.get("padding"), 0)
        out_l = conv_out(length, kernel, stride, padding)
        return [batch, out_channels, out_l]
    if node_type == "maxpool1d":
        batch, channels, length = input_shape
        kernel = to_int(params.get("kernel"), 2)
        stride = to_int(params.get("stride"), kernel)
        padding = to_int(params.get("padding"), 0)
        out_l = conv_out(length, kernel, stride, padding)
        return [batch, channels, out_l]
    if node_type == "linear":
        batch = input_shape[0]
        out_features = to_int(params.get("outFeatures"), 1)
        return [batch, out_features]
    if node_type == "flatten":
        if len(input_shape) < 2:
            raise ValueError("Flatten expects input with at least 2 dimensions")
        batch = input_shape[0]
        features = 1
        for dim in input_shape[1:]:
            features *= dim
        return [batch, features]
    if node_type == "transformer":
        batch, seq, _features = input_shape
        d_model = to_int(params.get("dModel"), 1)
        return [batch, seq, d_model]
    return input_shape


def build_module(node_type: str, params: Dict, input_shape: List[int]) -> nn.Module:
    if node_type == "conv2d":
        channels = input_shape[1]
        out_channels = to_int(params.get("outChannels"), 1)
        kernel = to_pair(params.get("kernel"), (3, 3))
        stride = to_pair(params.get("stride"), (1, 1))
        padding = to_pair(params.get("padding"), (0, 0))
        return nn.Conv2d(channels, out_channels, kernel_size=kernel, stride=stride, padding=padding)
    if node_type == "maxpool2d":
        kernel = to_pair(params.get("kernel"), (2, 2))
        stride = to_pair(params.get("stride"), kernel)
        padding = to_pair(params.get("padding"), (0, 0))
        return nn.MaxPool2d(kernel_size=kernel, stride=stride, padding=padding)
    if node_type == "conv1d":
        channels = input_shape[1]
        out_channels = to_int(params.get("outChannels"), 1)
        kernel = to_int(params.get("kernel"), 3)
        stride = to_int(params.get("stride"), 1)
        padding = to_int(params.get("padding"), 0)
        return nn.Conv1d(channels, out_channels, kernel_size=kernel, stride=stride, padding=padding)
    if node_type == "maxpool1d":
        kernel = to_int(params.get("kernel"), 2)
        stride = to_int(params.get("stride"), kernel)
        padding = to_int(params.get("padding"), 0)
        return nn.MaxPool1d(kernel_size=kernel, stride=stride, padding=padding)
    if node_type == "relu":
        return nn.ReLU()
    if node_type == "sigmoid":
        return nn.Sigmoid()
    if node_type == "softmax":
        dim = to_int(params.get("dim"), -1)
        return nn.Softmax(dim=dim)
    if node_type == "batchnorm2d":
        channels = input_shape[1]
        return nn.BatchNorm2d(channels)
    if node_type == "dropout":
        dropout = float(params.get("dropout", 0.25))
        return nn.Dropout(p=dropout)
    if node_type == "linear":
        features = 1
        for dim in input_shape[1:]:
            features *= dim
        out_features = to_int(params.get("outFeatures"), 1)
        return nn.Linear(features, out_features)
    if node_type == "flatten":
        return nn.Flatten(start_dim=1)
    if node_type == "transformer":
        d_model = to_int(params.get("dModel"), 128)
        n_head = to_int(params.get("nHead"), 4)
        feedforward = to_int(params.get("dimFeedforward"), 256)
        dropout = float(params.get("dropout", 0.1))
        return nn.TransformerEncoderLayer(
            d_model=d_model,
            nhead=n_head,
            dim_feedforward=feedforward,
            dropout=dropout,
            batch_first=True
        )
    if node_type == "layernorm":
        normalized_shape = to_int(params.get("normalizedShape"), input_shape[-1])
        return nn.LayerNorm(normalized_shape)
    return nn.Identity()


class GraphModule(nn.Module):
    def __init__(self, nodes: List[Dict]):
        super().__init__()
        self.nodes = nodes
        self.layers = nn.ModuleList([node["module"] for node in nodes])

    def forward(self, x):
        outputs: List[torch.Tensor] = []
        for idx, node in enumerate(self.nodes):
            inputs = node["inputs"]
            if node["type"] == "residual":
                if len(inputs) < 2:
                    raise RuntimeError("Residual node missing inputs")
                out = outputs[inputs[0]] + outputs[inputs[1]]
            else:
                if inputs:
                    out = outputs[inputs[0]]
                else:
                    out = x
                if node["type"] == "linear" and out.dim() > 2:
                    out = torch.flatten(out, 1)
                out = self.layers[idx](out)
            outputs.append(out)
        return outputs[-1] if outputs else x


def build_graph_model(config: Dict) -> Tuple[GraphModule, List[int]]:
    graph = config.get("graph", {})
    nodes = graph.get("nodes", [])
    edges = graph.get("edges", [])
    batch_size = int(config.get("batchSize", 1) or 1)
    input_shape = parse_shape(graph.get("inputShape", ""), batch_size)

    if not nodes:
        raise ValueError("No layers provided for export")

    # Validate required node fields
    for i, node in enumerate(nodes):
        if "id" not in node:
            raise ValueError(f"Node at index {i} is missing 'id' field")
        if "type" not in node:
            raise ValueError(f"Node '{node.get('id', i)}' is missing 'type' field")

    node_ids = [node["id"] for node in nodes]
    incoming: Dict[str, List[str]] = {node_id: [] for node_id in node_ids}
    for edge in edges:
        incoming.setdefault(edge["to"], []).append(edge["from"])

    incoming_counts = {node_id: len(incoming.get(node_id, [])) for node_id in node_ids}
    ready = [node_id for node_id in node_ids if incoming_counts.get(node_id, 0) == 0]
    order: List[str] = []
    while ready:
        current = ready.pop(0)
        order.append(current)
        for edge in edges:
            if edge["from"] == current:
                incoming_counts[edge["to"]] = incoming_counts.get(edge["to"], 0) - 1
                if incoming_counts[edge["to"]] == 0:
                    ready.append(edge["to"])

    if len(order) != len(node_ids):
        raise ValueError("Torch graph contains cycles or unresolved dependencies")

    id_to_index = {node_id: idx for idx, node_id in enumerate(order)}
    shape_map: Dict[str, List[int]] = {}
    node_specs: List[Dict] = []

    for node_id in order:
        node = next(item for item in nodes if item["id"] == node_id)
        sources = [src for src in incoming.get(node_id, []) if src in shape_map]
        input_shape_for_node = shape_map[sources[0]] if sources else input_shape
        node_type = node.get("type", "linear")
        params = node.get("params", {}) or {}
        if node_type == "residual":
            if len(sources) < 2:
                raise ValueError("Residual node missing inputs")
            if shape_map[sources[0]] != shape_map[sources[1]]:
                raise ValueError("Residual node shapes do not match")
            output_shape = shape_map[sources[0]]
            module = nn.Identity()
        else:
            module = build_module(node_type, params, input_shape_for_node)
            output_shape = infer_shape(node_type, params, input_shape_for_node)
        shape_map[node_id] = output_shape
        inputs_idx = [id_to_index[src] for src in sources]
        node_specs.append({"type": node_type, "inputs": inputs_idx, "module": module})

    return GraphModule(node_specs), input_shape


def write_json(path: str, payload: Dict) -> None:
    with open(path, "w", encoding="utf-8") as handle:
        json.dump(payload, handle, indent=2)


def compute_sha256(path: str) -> str:
    digest = hashlib.sha256()
    with open(path, "rb") as handle:
        for chunk in iter(lambda: handle.read(1024 * 1024), b""):
            digest.update(chunk)
    return digest.hexdigest()


def main() -> None:
    parser = argparse.ArgumentParser(description="Export TorchScript model from graph config")
    parser.add_argument("--config", required=True, help="Path to model config JSON")
    parser.add_argument("--output-dir", required=True, help="Directory to store model artifacts")
    parser.add_argument("--base-dir", required=True, help="Base directory for relative paths")
    args = parser.parse_args()

    with open(args.config, "r", encoding="utf-8") as handle:
        config = json.load(handle)

    name = str(config.get("name", "torch_model"))
    slug = slugify(name)
    target_dir = ensure_unique_dir(args.output_dir, slug)
    model_path = os.path.join(target_dir, "model.pt")

    model, input_shape = build_graph_model(config)
    model.eval()

    dummy = torch.zeros(input_shape)
    try:
        scripted = torch.jit.script(model)
    except Exception as script_error:
        sys.stderr.write(f"Warning: torch.jit.script failed ({script_error}), falling back to torch.jit.trace. "
                        "Traced models may not correctly capture conditional logic or dynamic behavior.\n")
        scripted = torch.jit.trace(model, dummy)

    scripted.save(model_path)
    checksum = compute_sha256(model_path)

    rel_pt_path = os.path.relpath(model_path, args.base_dir)
    rel_config_path = os.path.relpath(os.path.join(target_dir, "model.json"), args.base_dir)

    output = {
        "name": name,
        "graph": config.get("graph"),
        "ptPath": rel_pt_path,
        "ptChecksum": checksum,
        "ptFormat": "torchscript",
        "ptDescription": str(config.get("ptDescription", "")),
        "trainParams": config.get("trainParams", {}),
        "configPath": rel_config_path
    }

    write_json(os.path.join(target_dir, "model.json"), output)

    print(json.dumps(output))


if __name__ == "__main__":
    try:
        main()
    except Exception as exc:
        sys.stderr.write(f"{exc}\n")
        sys.exit(1)
