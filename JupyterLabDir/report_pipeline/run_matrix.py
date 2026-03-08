#!/usr/bin/env python3
from __future__ import annotations

import argparse
import json
import os
import shlex
import socket
import subprocess
import sys
import time
from pathlib import Path
from typing import Any

import pandas as pd

SCRIPT_DIR = Path(__file__).resolve().parent
REPO_ROOT = SCRIPT_DIR.parents[1]
sys.path.insert(0, str(SCRIPT_DIR))
sys.path.insert(0, str(REPO_ROOT / "src_py" / "apiServer"))

from apiServer import ApiServer  # noqa: E402
from generate_jsons import DEFAULT_MANIFEST, DEFAULT_MATRIX, DEFAULT_OUT_ROOT, build_report_profile, load_json  # noqa: E402
from plot_results import render_plots  # noqa: E402

SSH_OPTIONS = [
    "-o", "BatchMode=yes",
    "-o", "ConnectTimeout=10",
    "-o", "ServerAliveInterval=10",
    "-o", "ServerAliveCountMax=3",
    "-o", "StrictHostKeyChecking=no",
    "-o", "UserKnownHostsFile=/dev/null",
]
LOG_FILES = [
    "/tmp/nerlnet_main.log",
    "/tmp/nerlnet_release_boot.log",
    "/tmp/nerlnet_run_log.txt",
]


def log(msg: str) -> None:
    print(msg, flush=True)


def _run_local(cmd: str, timeout: int = 300) -> subprocess.CompletedProcess[str]:
    return subprocess.run(cmd, shell=True, text=True, capture_output=True, timeout=timeout)


def _run_ssh(node: dict[str, Any], remote_cmd: str, timeout: int = 300) -> subprocess.CompletedProcess[str]:
    cmd = ["ssh", *SSH_OPTIONS]
    ssh_key = str(node.get("ssh_key", "")).strip()
    if ssh_key:
        cmd += ["-i", os.path.expanduser(ssh_key)]
    cmd += [f"{node['user']}@{node['ip']}", "bash -lc " + shlex.quote(remote_cmd)]
    return subprocess.run(cmd, text=True, capture_output=True, timeout=timeout)


def run_node_cmd(node: dict[str, Any], cmd: str, timeout: int = 300) -> subprocess.CompletedProcess[str]:
    if bool(node.get("local", False)):
        return _run_local(cmd, timeout=timeout)
    return _run_ssh(node, cmd, timeout=timeout)


def shell_dir_expr(path: str, is_local: bool) -> str:
    path = str(path).strip()
    if is_local:
        return shlex.quote(os.path.expanduser(path))
    if path == "~":
        return "$HOME"
    if path.startswith("~/"):
        parts = [part for part in path[2:].split("/") if part]
        if not parts:
            return "$HOME"
        return "$HOME/" + "/".join(shlex.quote(part) for part in parts)
    return shlex.quote(path)


def check_port_open(host: str, port: int, timeout_sec: float = 2.0) -> bool:
    try:
        with socket.create_connection((host, port), timeout=timeout_sec):
            return True
    except OSError:
        return False


def wait_for_init_targets(ips: list[str], port: int = 8484, timeout_sec: int = 120) -> None:
    deadline = time.time() + timeout_sec
    pending = set(ips)
    while pending and time.time() < deadline:
        ready = {ip for ip in pending if check_port_open(ip, port)}
        pending -= ready
        if pending:
            time.sleep(1.0)
    if pending:
        raise RuntimeError(f"Init listener not ready on all devices after {timeout_sec}s: {sorted(pending)}")


def load_node_map(manifest_path: Path) -> dict[str, dict[str, Any]]:
    manifest = load_json(manifest_path)
    return {node["ip"]: node for node in manifest.get("nodes", [])}


def preflight_manifest(manifest_path: Path, used_ips: list[str]) -> None:
    node_map = load_node_map(manifest_path)
    missing = [ip for ip in used_ips if ip not in node_map]
    if missing:
        raise RuntimeError(f"Manifest is missing node definitions for IPs: {missing}")
    for ip in used_ips:
        node = node_map[ip]
        if bool(node.get("local", False)):
            continue
        result = _run_ssh(node, "echo ssh_ok", timeout=20)
        if result.returncode != 0 or "ssh_ok" not in result.stdout:
            raise RuntimeError(f"SSH preflight failed for {ip}: {result.stderr.strip() or result.stdout.strip()}")


def restart_cluster_for_dc(dc_path: Path, manifest_path: Path, run_debug: bool, timeout_sec: int = 240) -> None:
    node_map = load_node_map(manifest_path)
    dc = load_json(dc_path)
    used_ips = [str(device["ipv4"]).strip() for device in dc.get("devices", []) if str(device.get("ipv4", "")).strip()]
    used_ips = list(dict.fromkeys(used_ips))
    debug_flag = " --debug" if run_debug else ""
    for ip in used_ips:
        node = node_map[ip]
        nerl_dir = shell_dir_expr(node["nerl_dir"], bool(node.get("local", False)))
        remote_cmd = (
            f"cd {nerl_dir} && "
            f"./NerlnetRun.sh --run-mode stop || true; "
            f"nohup bash -lc './NerlnetRun.sh --run-mode release --beam-kill{debug_flag} > /tmp/nerlnet_main.log 2>&1' "
            f">/dev/null 2>&1 &"
        )
        result = run_node_cmd(node, remote_cmd, timeout=60)
        if result.returncode != 0:
            raise RuntimeError(f"Failed restarting runtime on {ip}: {result.stderr.strip() or result.stdout.strip()}")
    wait_for_init_targets(used_ips, port=8484, timeout_sec=timeout_sec)


def collect_logs(run_dir: Path, dc_path: Path, manifest_path: Path) -> None:
    node_map = load_node_map(manifest_path)
    dc = load_json(dc_path)
    logs_root = run_dir / "logs"
    logs_root.mkdir(parents=True, exist_ok=True)
    for device in dc.get("devices", []):
        ip = str(device.get("ipv4", "")).strip()
        if not ip or ip not in node_map:
            continue
        node = node_map[ip]
        node_dir = logs_root / ip.replace(".", "_")
        node_dir.mkdir(parents=True, exist_ok=True)
        for log_path in LOG_FILES:
            output_path = node_dir / Path(log_path).name
            if bool(node.get("local", False)):
                source_path = Path(log_path)
                if source_path.exists():
                    output_path.write_text(source_path.read_text(encoding="utf-8", errors="ignore"), encoding="utf-8")
                continue
            cmd = f"test -f {shlex.quote(log_path)} && cat {shlex.quote(log_path)}"
            result = _run_ssh(node, cmd, timeout=60)
            if result.returncode == 0 and result.stdout:
                output_path.write_text(result.stdout, encoding="utf-8")


def flatten_entity_comm(stats_obj, repeat_idx: int, entry: dict[str, Any]) -> list[dict[str, Any]]:
    rows = []
    phase_name = stats_obj.experiment_phase.get_name()
    phase_type = stats_obj.experiment_phase.get_phase_type()

    def add_rows(entity_type: str, payload: dict[str, dict[str, Any]] | dict[str, Any]) -> None:
        if entity_type == "main_server":
            rows.append(
                {
                    "run_label": entry["label"],
                    "repeat_idx": repeat_idx,
                    "phase_name": phase_name,
                    "phase_type": phase_type,
                    "entity_type": entity_type,
                    "entity_name": "mainServer",
                    **payload,
                }
            )
            return
        for entity_name, entity_stats in payload.items():
            rows.append(
                {
                    "run_label": entry["label"],
                    "repeat_idx": repeat_idx,
                    "phase_name": phase_name,
                    "phase_type": phase_type,
                    "entity_type": entity_type,
                    "entity_name": entity_name,
                    **entity_stats,
                }
            )

    add_rows("main_server", stats_obj.get_communication_stats_main_server())
    add_rows("super_node", stats_obj.get_communication_stats_super_nodes())
    add_rows("worker", stats_obj.get_communication_stats_workers())
    add_rows("source", stats_obj.get_communication_stats_sources())
    add_rows("client", stats_obj.get_communication_stats_clients())
    add_rows("router", stats_obj.get_communication_stats_routers())
    return rows


def quality_metrics_df(stats_obj, repeat_idx: int, entry: dict[str, Any]) -> pd.DataFrame:
    if stats_obj.get_phase() != "prediction":
        return pd.DataFrame()
    try:
        _, confusion_worker = stats_obj.get_confusion_matrices()
        perf_df = stats_obj.get_model_performence_stats(confusion_worker)
    except Exception:
        return pd.DataFrame()
    if perf_df is None or perf_df.empty:
        return pd.DataFrame()
    perf_df = perf_df.copy()
    perf_df.insert(0, "run_label", entry["label"])
    perf_df.insert(1, "repeat_idx", repeat_idx)
    perf_df.insert(2, "phase_name", stats_obj.experiment_phase.get_name())
    perf_df.insert(3, "phase_type", stats_obj.experiment_phase.get_phase_type())
    return perf_df


def build_run_summary(phase_summary_df: pd.DataFrame, entry: dict[str, Any], repeat_idx: int) -> pd.DataFrame:
    row: dict[str, Any] = {
        "run_label": entry["label"],
        "repeat_idx": repeat_idx,
        "kind": entry["kind"],
        "mode": entry["mode"],
        "scheduler": entry["scheduler"],
        "device_count": entry["deviceCount"],
        "num_microbatches": entry["numMicroBatches"],
        "micro_batch_size": entry["microBatchSize"],
    }
    for _, phase_row in phase_summary_df.iterrows():
        prefix = str(phase_row["phase_type"])
        for key in [
            "parallel_worker_slots",
            "expected_microbatches",
            "completed_microbatches",
            "skipped_microbatches",
            "source_dropped_batches",
            "wall_clock_sec",
            "throughput_sps",
            "avg_step_time_us",
            "bubble_fraction",
            "comm_fraction",
            "scaling_efficiency_vs_p1_t1",
        ]:
            row[f"{prefix}_{key}"] = phase_row.get(key)
    return pd.DataFrame([row])


def validate_phase_completion(stats_list: list[Any], exp_path: Path, run_label: str) -> None:
    exp_payload = load_json(exp_path)
    expected = [
        (str(phase.get("phaseName", "")), str(phase.get("phaseType", "")))
        for phase in exp_payload.get("Phases", [])
    ]
    actual = [
        (str(stats_obj.experiment_phase.get_name()), str(stats_obj.experiment_phase.get_phase_type()))
        for stats_obj in stats_list
    ]
    missing = [phase for phase in expected if phase not in actual]
    unexpected = [phase for phase in actual if phase not in expected]
    if missing or unexpected or len(actual) != len(expected):
        raise RuntimeError(
            "Incomplete phase execution for "
            f"{run_label}: expected={expected}, actual={actual}, missing={missing}, unexpected={unexpected}"
        )


def write_run_artifacts(run_dir: Path, stats_list: list[Any], entry: dict[str, Any], repeat_idx: int) -> dict[str, pd.DataFrame]:
    raw_dir = run_dir / "raw"
    raw_dir.mkdir(parents=True, exist_ok=True)

    trace_frames = []
    step_frames = []
    phase_frames = []
    comm_rows: list[dict[str, Any]] = []
    quality_frames = []

    for stats_obj in stats_list:
        trace_df = stats_obj.get_parallel_trace_df().copy()
        if not trace_df.empty:
            trace_df["run_label"] = entry["label"]
            trace_df.insert(0, "repeat_idx", repeat_idx)
            trace_frames.append(trace_df)

        step_df = stats_obj.get_step_summary_df().copy()
        if not step_df.empty:
            step_df["run_label"] = entry["label"]
            step_df.insert(0, "repeat_idx", repeat_idx)
            step_frames.append(step_df)

        phase_df = stats_obj.get_report_metrics_df().copy()
        if not phase_df.empty:
            phase_df["run_label"] = entry["label"]
            phase_df.insert(0, "repeat_idx", repeat_idx)
            phase_df.insert(1, "kind", entry["kind"])
            phase_df.insert(2, "device_count", entry["deviceCount"])
            phase_frames.append(phase_df)

        comm_rows.extend(flatten_entity_comm(stats_obj, repeat_idx, entry))
        quality_df = quality_metrics_df(stats_obj, repeat_idx, entry)
        if not quality_df.empty:
            quality_df["run_label"] = entry["label"]
            quality_frames.append(quality_df)

    trace_df = pd.concat(trace_frames, ignore_index=True) if trace_frames else pd.DataFrame()
    step_df = pd.concat(step_frames, ignore_index=True) if step_frames else pd.DataFrame()
    phase_df = pd.concat(phase_frames, ignore_index=True) if phase_frames else pd.DataFrame()
    comm_df = pd.DataFrame(comm_rows)
    quality_df = pd.concat(quality_frames, ignore_index=True) if quality_frames else pd.DataFrame()
    run_summary_df = build_run_summary(phase_df, entry, repeat_idx) if not phase_df.empty else pd.DataFrame()

    trace_df.to_csv(raw_dir / "microbatch_trace.csv", index=False)
    step_df.to_csv(raw_dir / "step_summary.csv", index=False)
    phase_df.to_csv(raw_dir / "phase_summary.csv", index=False)
    run_summary_df.to_csv(raw_dir / "run_summary.csv", index=False)
    comm_df.to_csv(raw_dir / "entity_comm_phase.csv", index=False)
    quality_df.to_csv(raw_dir / "quality_metrics.csv", index=False)
    (raw_dir / "generated_json_manifest.json").write_text(json.dumps(entry, indent=2), encoding="utf-8")

    return {
        "trace": trace_df,
        "step": step_df,
        "phase": phase_df,
        "run": run_summary_df,
        "comm": comm_df,
        "quality": quality_df,
    }


def run_one(entry: dict[str, Any], repeat_idx: int, restart_policy: str, manifest_path: Path, run_debug: bool, timeout_restart_sec: int) -> dict[str, pd.DataFrame]:
    run_dir = out_run_dir(args.out.resolve(), entry["label"], repeat_idx)
    run_dir.mkdir(parents=True, exist_ok=True)
    dc_path = Path(entry["dc"])
    conn_path = Path(entry["conn"])
    exp_path = Path(entry["exp"])

    if restart_policy == "between_runs":
        restart_cluster_for_dc(dc_path, manifest_path, run_debug, timeout_sec=timeout_restart_sec)

    api = ApiServer()
    api.reset()
    experiment_name = f"report_{entry['label']}_repeat_{repeat_idx:02d}"
    api.initialization(experiment_name, str(dc_path), str(conn_path), str(exp_path))
    api.send_jsons_to_devices()
    stats_list = api.run_all_experiment_phases()
    if not stats_list:
        raise RuntimeError(f"No phase stats were returned for {entry['label']} repeat {repeat_idx}")
    validate_phase_completion(stats_list, exp_path, entry["label"])
    artifacts = write_run_artifacts(run_dir, stats_list, entry, repeat_idx)
    render_plots(run_dir, run_dir / "plots")
    return artifacts


def out_run_dir(out_root: Path, label: str, repeat_idx: int) -> Path:
    return out_root / "runs" / label / f"repeat_{repeat_idx:02d}"


def apply_scaling_efficiency(phase_df: pd.DataFrame, run_df: pd.DataFrame) -> tuple[pd.DataFrame, pd.DataFrame]:
    if phase_df.empty:
        return phase_df, run_df
    baseline_rows = phase_df[
        (phase_df["run_label"] == "baseline_p1_t1_legacy") & (phase_df["phase_type"] == "training")
    ]
    baseline = float(baseline_rows["throughput_sps"].mean()) if not baseline_rows.empty else 0.0
    if baseline <= 0:
        return phase_df, run_df
    phase_df = phase_df.copy()
    phase_df["scaling_efficiency_vs_p1_t1"] = phase_df.apply(
        lambda row: float(row["throughput_sps"]) / (baseline * max(1, int(row["parallel_worker_slots"])))
        if float(row.get("throughput_sps", 0) or 0) > 0
        else pd.NA,
        axis=1,
    )
    if not run_df.empty:
        run_df = run_df.copy()
        if "training_throughput_sps" in run_df.columns and "training_parallel_worker_slots" in run_df.columns:
            run_df["training_scaling_efficiency_vs_p1_t1"] = run_df.apply(
                lambda row: float(row["training_throughput_sps"]) / (baseline * max(1, int(row["training_parallel_worker_slots"])))
                if pd.notna(row.get("training_throughput_sps")) and float(row.get("training_throughput_sps") or 0) > 0
                else pd.NA,
                axis=1,
            )
    return phase_df, run_df


def parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser(description="Run the PTD report matrix from explicit generated JSON files")
    parser.add_argument("--profile", default="report_core")
    parser.add_argument("--manifest", type=Path, default=DEFAULT_MANIFEST)
    parser.add_argument("--matrix", type=Path, default=DEFAULT_MATRIX)
    parser.add_argument("--json-dir", type=Path, default=DEFAULT_OUT_ROOT)
    parser.add_argument("--out", type=Path, required=True)
    parser.add_argument("--quick", action="store_true")
    parser.add_argument("--repeats", type=int, default=0)
    parser.add_argument("--restart-policy", choices=["between_runs", "none"], default="between_runs")
    parser.add_argument("--run-debug", action="store_true")
    parser.add_argument("--timeout-restart-sec", type=int, default=240)
    parser.add_argument("--fail-fast", action="store_true")
    return parser.parse_args()


def load_generated_manifest(profile: str, manifest_path: Path, matrix_path: Path, json_dir: Path) -> dict[str, Any]:
    manifest_file = json_dir / profile / "manifest.json"
    if not manifest_file.exists():
        manifest_file = build_report_profile(profile, manifest_path.resolve(), matrix_path.resolve(), json_dir.resolve())
    return load_json(manifest_file)


def main() -> int:
    global args
    args = parse_args()
    generated_manifest = load_generated_manifest(args.profile, args.manifest, args.matrix, args.json_dir)
    experiments = generated_manifest["experiments"]
    if args.quick:
        quick_labels = set(generated_manifest.get("quickLabels", []))
        experiments = [entry for entry in experiments if entry["label"] in quick_labels]
    if not experiments:
        raise RuntimeError("No experiments selected for execution")

    used_ips = sorted(
        {
            str(device["ipv4"]).strip()
            for entry in experiments
            for device in load_json(Path(entry["dc"]))["devices"]
            if str(device.get("ipv4", "")).strip()
        }
    )
    preflight_manifest(args.manifest.resolve(), used_ips)

    repeats = int(args.repeats) if int(args.repeats) > 0 else int(generated_manifest.get("defaults", {}).get("quickRepeats" if args.quick else "repeats", 1))
    args.out.resolve().mkdir(parents=True, exist_ok=True)

    all_trace = []
    all_step = []
    all_phase = []
    all_run = []
    all_comm = []
    all_quality = []
    failures = []

    if args.restart_policy == "between_runs":
        log(f"Restart policy: {args.restart_policy}")

    for entry in experiments:
        for repeat_idx in range(1, repeats + 1):
            log(f"Running {entry['label']} repeat {repeat_idx}/{repeats}")
            run_dir = out_run_dir(args.out.resolve(), entry["label"], repeat_idx)
            try:
                artifacts = run_one(entry, repeat_idx, args.restart_policy, args.manifest.resolve(), args.run_debug, args.timeout_restart_sec)
                if not artifacts["trace"].empty:
                    all_trace.append(artifacts["trace"])
                if not artifacts["step"].empty:
                    all_step.append(artifacts["step"])
                if not artifacts["phase"].empty:
                    all_phase.append(artifacts["phase"])
                if not artifacts["run"].empty:
                    all_run.append(artifacts["run"])
                if not artifacts["comm"].empty:
                    all_comm.append(artifacts["comm"])
                if not artifacts["quality"].empty:
                    all_quality.append(artifacts["quality"])
            except Exception as exc:
                failures.append({"run_label": entry["label"], "repeat_idx": repeat_idx, "error": str(exc)})
                collect_logs(run_dir, Path(entry["dc"]), args.manifest.resolve())
                log(f"Run failed: {entry['label']} repeat {repeat_idx}: {exc}")
                if args.fail_fast:
                    raise

    raw_root = args.out.resolve() / "raw"
    raw_root.mkdir(parents=True, exist_ok=True)

    trace_df = pd.concat(all_trace, ignore_index=True) if all_trace else pd.DataFrame()
    step_df = pd.concat(all_step, ignore_index=True) if all_step else pd.DataFrame()
    phase_df = pd.concat(all_phase, ignore_index=True) if all_phase else pd.DataFrame()
    run_df = pd.concat(all_run, ignore_index=True) if all_run else pd.DataFrame()
    comm_df = pd.concat(all_comm, ignore_index=True) if all_comm else pd.DataFrame()
    quality_df = pd.concat(all_quality, ignore_index=True) if all_quality else pd.DataFrame()
    phase_df, run_df = apply_scaling_efficiency(phase_df, run_df)

    trace_df.to_csv(raw_root / "microbatch_trace.csv", index=False)
    step_df.to_csv(raw_root / "step_summary.csv", index=False)
    phase_df.to_csv(raw_root / "phase_summary.csv", index=False)
    run_df.to_csv(raw_root / "run_summary.csv", index=False)
    comm_df.to_csv(raw_root / "entity_comm_phase.csv", index=False)
    quality_df.to_csv(raw_root / "quality_metrics.csv", index=False)
    (raw_root / "generated_json_manifest.json").write_text(json.dumps(generated_manifest, indent=2), encoding="utf-8")
    (args.out.resolve() / "failures.json").write_text(json.dumps(failures, indent=2), encoding="utf-8")

    render_plots(args.out.resolve(), args.out.resolve() / "plots")
    if failures:
        raise SystemExit(1)
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
