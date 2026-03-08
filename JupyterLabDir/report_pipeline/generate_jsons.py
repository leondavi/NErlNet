#!/usr/bin/env python3
from __future__ import annotations

import argparse
import ast
import hashlib
import json
from copy import deepcopy
from pathlib import Path
from typing import Any


REPO_ROOT = Path(__file__).resolve().parents[2]
DEFAULT_MANIFEST = REPO_ROOT / "JupyterLabDir" / "report_pipeline" / "device_manifest.json"
DEFAULT_MATRIX = REPO_ROOT / "JupyterLabDir" / "report_pipeline" / "experiment_matrix.report_core.json"
DEFAULT_OUT_ROOT = REPO_ROOT / "JupyterLabDir" / "report_pipeline" / "generated_jsons"


def load_json(path: Path) -> dict[str, Any]:
    with path.open("r", encoding="utf-8") as handle:
        return json.load(handle)


def repo_resolve(path_str: str) -> Path:
    path = Path(path_str)
    if path.is_absolute():
        return path
    return (REPO_ROOT / path).resolve()


def normalize_shape(shape_value: Any, batch_size: int) -> str:
    if not shape_value:
        return ""
    try:
        parsed = ast.literal_eval(str(shape_value))
    except (ValueError, SyntaxError):
        return str(shape_value)
    if isinstance(parsed, (list, tuple)) and parsed:
        parsed = list(parsed)
        parsed[0] = int(batch_size)
        return json.dumps(parsed)
    return str(shape_value)


def normalize_train_params(model_cfg: dict[str, Any], batch_size: int) -> dict[str, str]:
    train_params = deepcopy(model_cfg.get("trainParams", {}))
    return {
        "lr": str(train_params.get("lr", "0.001")),
        "epochs": str(train_params.get("epochs", "1")),
        "optimizer": str(train_params.get("optimizer", "adam")),
        "loss": str(train_params.get("loss", "mse")),
        "batch_size": str(batch_size),
        "input_tensor_shape": normalize_shape(train_params.get("inputTensorShape", f"[{batch_size}, 5]"), batch_size),
        "labels_offset": str(train_params.get("labelsOffset", "default")),
        "labels_shape": normalize_shape(train_params.get("labelsShape", f"[{batch_size}, 3]"), batch_size),
        "w_init_rand": str(train_params.get("wInitRand", "True")),
    }


def load_model_payload(matrix_cfg: dict[str, Any], batch_size: int, tp_plan: list[dict[str, Any]] | None) -> tuple[str, dict[str, Any]]:
    model_section = matrix_cfg["model"]
    model_cfg = load_json(repo_resolve(model_section["configPath"]))
    model_sha = str(model_cfg.get("ptChecksum") or "")
    if not model_sha or model_sha == "placeholder":
        model_sha = hashlib.sha256(json.dumps(model_cfg, sort_keys=True).encode("utf-8")).hexdigest()
    model_payload = {
        "infraType": "torch",
        "distributedSystemType": "0",
        "distributedSystemArgs": "",
        "distributedSystemToken": "none",
        "pt_path": str(repo_resolve(model_section["ptPath"])),
        "pt_format": str(model_cfg.get("ptFormat", "torchscript")),
        "pt_checksum": str(model_cfg.get("ptChecksum", model_sha)),
        "pt_description": str(model_cfg.get("ptDescription", model_cfg.get("name", "PTD report model"))),
        "train_params": normalize_train_params(model_cfg, batch_size),
    }
    if tp_plan:
        model_payload["tpPlan"] = tp_plan
    return model_sha, model_payload


def build_tp_plan(tp_group_to_layers: dict[str, list[str]]) -> list[dict[str, Any]]:
    plan: list[dict[str, Any]] = []
    for group_name, layers in tp_group_to_layers.items():
        for layer_name in layers:
            is_relu = "relu" in layer_name.lower()
            is_output = layer_name.lower().endswith("linear_out")
            plan.append(
                {
                    "layer": layer_name,
                    "mode": "row" if is_relu or is_output else "column",
                    "shardAxis": 1 if is_relu or is_output else 0,
                    "group": group_name,
                }
            )
    return plan


def make_parallel_execution(mode: str, scheduler: str, micro_batch_size: int, num_microbatches: int, super_node: str, instrumentation: dict[str, Any], virtual_stages: int | None = None) -> dict[str, Any]:
    payload: dict[str, Any] = {
        "mode": mode,
        "superNode": super_node,
        "scheduler": scheduler,
        "microBatchSize": int(micro_batch_size),
        "numMicroBatches": int(num_microbatches),
        "instrumentation": deepcopy(instrumentation),
    }
    if virtual_stages is not None:
        payload["virtualStages"] = int(virtual_stages)
    return payload


def make_phases(label: str, dataset_cfg: dict[str, Any], batch_size: int, train_batches: int, predict_batches: int, stage0_workers: list[str], parallel_execution: dict[str, Any] | None) -> list[dict[str, Any]]:
    phases = []
    workers_csv = ",".join(stage0_workers)
    for phase_name, phase_type, starting_sample, num_batches in [
        ("Training1", "training", 0, train_batches),
        ("Prediction1", "prediction", int(dataset_cfg.get("predictionStartSample", 50000)), predict_batches),
    ]:
        phase = {
            "phaseName": phase_name,
            "phaseType": phase_type,
            "sourcePieces": [
                {
                    "sourceName": "s1",
                    "startingSample": int(starting_sample),
                    "numOfBatches": int(num_batches),
                    "workers": workers_csv,
                    "nerltensorType": str(dataset_cfg.get("nerltensorType", "float")),
                }
            ],
        }
        if parallel_execution is not None:
            phase["parallelExecution"] = deepcopy(parallel_execution)
        phases.append(phase)
    return phases


def build_connection_map(client_names: list[str], include_super: bool) -> dict[str, list[str]]:
    router_neighbors = ["mainServer", "s1", *client_names]
    if include_super:
        router_neighbors.append("super_0")
    conn = {
        "mainServer": ["apiServer", "r1"],
        "apiServer": ["mainServer"],
        "r1": router_neighbors,
        "s1": ["r1"],
    }
    if include_super:
        conn["super_0"] = ["r1"]
    for client_name in client_names:
        conn[client_name] = ["r1"]
    return conn


def make_client(name: str, port: int, workers: list[str], include_super: bool) -> dict[str, Any]:
    payload: dict[str, Any] = {
        "name": name,
        "port": str(port),
        "workers": ",".join(workers),
    }
    if include_super:
        payload["superNode"] = "super_0"
    return payload


def make_worker(name: str, model_sha: str, parallel_cfg: dict[str, Any] | None) -> dict[str, Any]:
    payload: dict[str, Any] = {"name": name, "model_sha": model_sha}
    if parallel_cfg:
        payload["parallel"] = parallel_cfg
    return payload


def select_nodes(manifest: dict[str, Any], total_devices: int) -> list[dict[str, Any]]:
    nodes = manifest.get("nodes", [])
    controller_id = manifest.get("controller")
    node_map = {node["id"]: node for node in nodes}
    if controller_id not in node_map:
        raise ValueError(f"controller '{controller_id}' is missing from manifest")
    selected = [node_map[controller_id]]
    for node in nodes:
        if node["id"] == controller_id:
            continue
        if len(selected) >= total_devices:
            break
        selected.append(node)
    if len(selected) < total_devices:
        raise ValueError(f"manifest provides {len(selected)} usable nodes but topology needs {total_devices}")
    return selected


def build_device_entries(selected_nodes: list[dict[str, Any]], client_assignments: list[tuple[str, str]], include_super: bool) -> list[dict[str, Any]]:
    client_by_node: dict[str, list[str]] = {}
    for client_name, node_id in client_assignments:
        client_by_node.setdefault(node_id, []).append(client_name)

    devices = []
    for index, node in enumerate(selected_nodes):
        entities = []
        if index == 0:
            entities.extend(["mainServer", "apiServer", "r1", "s1"])
            if include_super:
                entities.append("super_0")
        entities.extend(client_by_node.get(node["id"], []))
        devices.append(
            {
                "name": node["name"],
                "ipv4": node["ip"],
                "entities": ",".join(entities),
            }
        )
    return devices


def build_dc_json(label: str, manifest: dict[str, Any], matrix_cfg: dict[str, Any], topology: dict[str, Any]) -> dict[str, Any]:
    defaults = matrix_cfg["defaults"]
    dataset_cfg = matrix_cfg["dataset"]
    batch_size = int(defaults["batchSize"])
    include_super = topology["mode"] != "legacy"
    selected_nodes = select_nodes(manifest, topology["device_count"])
    tp_plan = topology.get("tp_plan")
    model_sha, model_payload = load_model_payload(matrix_cfg, batch_size, tp_plan)
    source_port = 29001
    router_port = 29002
    super_node_port = 29003

    clients = []
    workers = []
    client_assignments: list[tuple[str, str]] = []
    for index, worker_spec in enumerate(topology["workers"], start=1):
        client_name = f"client_{index}"
        clients.append(make_client(client_name, 29100 + index, [worker_spec["name"]], include_super))
        client_assignments.append((client_name, worker_spec["node_id"]))
        workers.append(make_worker(worker_spec["name"], model_sha, worker_spec.get("parallel")))

    dc_json = {
        "nerlnetSettings": {
            "frequency": str(defaults["frequency"]),
            "batchSize": str(batch_size),
        },
        "mainServer": {"port": "8081", "args": ""},
        "apiServer": {"port": "8082", "args": ""},
        "devices": build_device_entries(selected_nodes, client_assignments, include_super),
        "routers": [{"name": "r1", "port": str(router_port), "policy": "0"}],
        "sources": [
            {
                "name": "s1",
                "port": str(source_port),
                "frequency": str(defaults["frequency"]),
                "policy": "0",
                "epochs": "1",
                "type": "0",
            }
        ],
        "clients": clients,
        "workers": workers,
        "model_sha": {model_sha: model_payload},
    }
    if include_super:
        parallel_defaults = defaults["parallel"]
        dc_json["superNodes"] = [
            {
                "name": parallel_defaults["superNode"],
                "port": str(super_node_port),
                "managedClients": [client["name"] for client in clients],
                "heartbeatMs": int(parallel_defaults["heartbeatMs"]),
                "maxInflightMicrobatches": int(parallel_defaults["maxInflightMicrobatches"]),
            }
        ]
    return dc_json


def build_exp_json(label: str, matrix_cfg: dict[str, Any], topology: dict[str, Any]) -> dict[str, Any]:
    defaults = matrix_cfg["defaults"]
    dataset_cfg = matrix_cfg["dataset"]
    batch_size = int(defaults["batchSize"])
    parallel_execution = None
    if topology["mode"] != "legacy":
        parallel_execution = make_parallel_execution(
            mode=topology["mode"],
            scheduler=topology["scheduler"],
            micro_batch_size=topology["micro_batch_size"],
            num_microbatches=topology["num_microbatches"],
            super_node=defaults["parallel"]["superNode"],
            instrumentation=defaults["instrumentation"],
            virtual_stages=topology.get("virtual_stages"),
        )

    return {
        "experimentName": f"synthetic_experiment_{label}",
        "experimentType": "classification",
        "batchSize": batch_size,
        "csvFilePath": str(repo_resolve(dataset_cfg["path"])),
        "numOfFeatures": int(dataset_cfg["numOfFeatures"]),
        "numOfLabels": int(dataset_cfg["numOfLabels"]),
        "headersNames": str(dataset_cfg["headersNames"]),
        "Phases": make_phases(
            label=label,
            dataset_cfg=dataset_cfg,
            batch_size=batch_size,
            train_batches=int(defaults["trainBatches"]),
            predict_batches=int(defaults["predictBatches"]),
            stage0_workers=topology["stage0_workers"],
            parallel_execution=parallel_execution,
        ),
    }


def topo_baseline(manifest: dict[str, Any]) -> dict[str, Any]:
    nodes = select_nodes(manifest, 1)
    return {
        "label": "baseline_p1_t1_legacy",
        "kind": "baseline_p1_t1_legacy",
        "device_count": 1,
        "mode": "legacy",
        "scheduler": "",
        "micro_batch_size": 64,
        "num_microbatches": 1,
        "stage0_workers": ["w1"],
        "workers": [
            {
                "name": "w1",
                "node_id": nodes[0]["id"],
                "parallel": None,
            }
        ],
    }


def topo_pp2(manifest: dict[str, Any], scheduler: str, microbatches: int) -> dict[str, Any]:
    nodes = select_nodes(manifest, 2)
    return {
        "label": f"sweep_pp2_{scheduler}_m{microbatches:02d}",
        "kind": "sweep_pp2",
        "device_count": 2,
        "mode": "pipeline",
        "scheduler": scheduler,
        "micro_batch_size": 64 // microbatches,
        "num_microbatches": microbatches,
        # Current NErlNet interleaved PP runtime is validated in-tree with one
        # logical stage per worker. Emitting virtualStages=2 here creates a
        # four-stage scheduler trace for a two-worker topology and aborts with
        # {missing_stage_worker,2}. Keep the report matrix aligned with the
        # runtime contract that the full-flow tests actually exercise.
        "virtual_stages": 1,
        "stage0_workers": ["w1"],
        "workers": [
            {
                "name": "w1",
                "node_id": nodes[0]["id"],
                "parallel": {"pipelineStage": 0, "pipelineWorldSize": 2},
            },
            {
                "name": "w2",
                "node_id": nodes[1]["id"],
                "parallel": {"pipelineStage": 1, "pipelineWorldSize": 2},
            },
        ],
    }


def topo_pp4(manifest: dict[str, Any]) -> dict[str, Any]:
    nodes = select_nodes(manifest, 4)
    workers = []
    for index, node in enumerate(nodes, start=1):
        workers.append(
            {
                "name": f"w{index}",
                "node_id": node["id"],
                "parallel": {"pipelineStage": index - 1, "pipelineWorldSize": 4},
            }
        )
    return {
        "label": "topo_pp4_1f1b_m8",
        "kind": "topo_pp4_1f1b_m8",
        "device_count": 4,
        "mode": "pipeline",
        "scheduler": "1f1b",
        "micro_batch_size": 8,
        "num_microbatches": 8,
        "stage0_workers": ["w1"],
        "workers": workers,
    }


def topo_tp2(manifest: dict[str, Any]) -> dict[str, Any]:
    nodes = select_nodes(manifest, 2)
    tp_plan = build_tp_plan(
        {
            "tp_g0": [
                "L0_linear",
                "L1_relu",
                "L2_linear",
                "L3_relu",
                "L4_linear",
                "L5_relu",
                "L6_linear",
                "L7_relu",
                "L8_linear",
                "L9_relu",
                "L10_linear",
                "L11_relu",
                "L12_linear_out",
            ]
        }
    )
    return {
        "label": "topo_tp2_1f1b_m8",
        "kind": "topo_tp2_1f1b_m8",
        "device_count": 2,
        "mode": "tensor",
        "scheduler": "1f1b",
        "micro_batch_size": 8,
        "num_microbatches": 8,
        "stage0_workers": ["w1", "w2"],
        "tp_plan": tp_plan,
        "workers": [
            {
                "name": "w1",
                "node_id": nodes[0]["id"],
                "parallel": {"pipelineStage": 0, "pipelineWorldSize": 1, "tpGroup": "tp_g0", "tpRank": 0, "tpWorldSize": 2},
            },
            {
                "name": "w2",
                "node_id": nodes[1]["id"],
                "parallel": {"pipelineStage": 0, "pipelineWorldSize": 1, "tpGroup": "tp_g0", "tpRank": 1, "tpWorldSize": 2},
            },
        ],
    }


def topo_p2_t2(manifest: dict[str, Any]) -> dict[str, Any]:
    nodes = select_nodes(manifest, 4)
    tp_plan = build_tp_plan(
        {
            "tp_s0": ["L0_linear", "L1_relu", "L2_linear", "L3_relu", "L4_linear", "L5_relu"],
            "tp_s1": ["L6_linear", "L7_relu", "L8_linear", "L9_relu", "L10_linear", "L11_relu", "L12_linear_out"],
        }
    )
    return {
        "label": "topo_p2_t2_1f1b_m8",
        "kind": "topo_p2_t2_1f1b_m8",
        "device_count": 4,
        "mode": "pipeline_tensor",
        "scheduler": "1f1b",
        "micro_batch_size": 8,
        "num_microbatches": 8,
        "stage0_workers": ["w1", "w2"],
        "tp_plan": tp_plan,
        "workers": [
            {
                "name": "w1",
                "node_id": nodes[0]["id"],
                "parallel": {"pipelineStage": 0, "pipelineWorldSize": 2, "tpGroup": "tp_s0", "tpRank": 0, "tpWorldSize": 2},
            },
            {
                "name": "w2",
                "node_id": nodes[1]["id"],
                "parallel": {"pipelineStage": 0, "pipelineWorldSize": 2, "tpGroup": "tp_s0", "tpRank": 1, "tpWorldSize": 2},
            },
            {
                "name": "w3",
                "node_id": nodes[2]["id"],
                "parallel": {"pipelineStage": 1, "pipelineWorldSize": 2, "tpGroup": "tp_s1", "tpRank": 0, "tpWorldSize": 2},
            },
            {
                "name": "w4",
                "node_id": nodes[3]["id"],
                "parallel": {"pipelineStage": 1, "pipelineWorldSize": 2, "tpGroup": "tp_s1", "tpRank": 1, "tpWorldSize": 2},
            },
        ],
    }


def expand_topologies(manifest: dict[str, Any], matrix_cfg: dict[str, Any]) -> list[dict[str, Any]]:
    topologies: list[dict[str, Any]] = []
    for experiment in matrix_cfg["experiments"]:
        kind = experiment["kind"]
        if kind == "baseline_p1_t1_legacy":
            topologies.append(topo_baseline(manifest))
        elif kind == "sweep_pp2":
            for scheduler in experiment["schedulers"]:
                for microbatches in experiment["microbatches"]:
                    topologies.append(topo_pp2(manifest, scheduler, int(microbatches)))
        elif kind == "topo_pp4_1f1b_m8":
            topologies.append(topo_pp4(manifest))
        elif kind == "topo_tp2_1f1b_m8":
            topologies.append(topo_tp2(manifest))
        elif kind == "topo_p2_t2_1f1b_m8":
            topologies.append(topo_p2_t2(manifest))
        else:
            raise ValueError(f"unsupported report topology kind: {kind}")
    return topologies


def build_report_profile(profile: str, manifest_path: Path, matrix_path: Path, out_root: Path) -> Path:
    manifest = load_json(manifest_path)
    matrix_cfg = load_json(matrix_path)
    if profile != matrix_cfg.get("profile"):
        raise ValueError(f"matrix profile '{matrix_cfg.get('profile')}' does not match requested '{profile}'")

    profile_dir = out_root / profile
    profile_dir.mkdir(parents=True, exist_ok=True)

    entries = []
    for topology in expand_topologies(manifest, matrix_cfg):
        label = topology["label"]
        dc_path = profile_dir / f"dc_{label}.json"
        conn_path = profile_dir / f"conn_{label}.json"
        exp_path = profile_dir / f"exp_{label}.json"

        dc_payload = build_dc_json(label, manifest, matrix_cfg, topology)
        conn_payload = {"connectionsMap": build_connection_map([f"client_{i}" for i in range(1, len(topology['workers']) + 1)], topology["mode"] != "legacy")}
        exp_payload = build_exp_json(label, matrix_cfg, topology)

        dc_path.write_text(json.dumps(dc_payload, indent=2), encoding="utf-8")
        conn_path.write_text(json.dumps(conn_payload, indent=2), encoding="utf-8")
        exp_path.write_text(json.dumps(exp_payload, indent=2), encoding="utf-8")

        entries.append(
            {
                "label": label,
                "kind": topology["kind"],
                "mode": topology["mode"],
                "scheduler": topology["scheduler"],
                "numMicroBatches": topology["num_microbatches"],
                "microBatchSize": topology["micro_batch_size"],
                "deviceCount": topology["device_count"],
                "dc": str(dc_path),
                "conn": str(conn_path),
                "exp": str(exp_path),
            }
        )

    manifest_path_out = profile_dir / "manifest.json"
    manifest_path_out.write_text(
        json.dumps(
            {
                "profile": profile,
                "manifest": str(manifest_path.resolve()),
                "matrix": str(matrix_path.resolve()),
                "generatedDir": str(profile_dir.resolve()),
                "quickLabels": matrix_cfg.get("quickLabels", []),
                "defaults": matrix_cfg.get("defaults", {}),
                "experiments": entries,
            },
            indent=2,
        ),
        encoding="utf-8",
    )
    return manifest_path_out


def parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser(description="Generate explicit PTD report JSONs from manifest + matrix")
    parser.add_argument("--profile", default="report_core")
    parser.add_argument("--manifest", type=Path, default=DEFAULT_MANIFEST)
    parser.add_argument("--matrix", type=Path, default=DEFAULT_MATRIX)
    parser.add_argument("--out", type=Path, default=DEFAULT_OUT_ROOT)
    return parser.parse_args()


def main() -> int:
    args = parse_args()
    manifest_out = build_report_profile(args.profile, args.manifest.resolve(), args.matrix.resolve(), args.out.resolve())
    print(manifest_out)
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
