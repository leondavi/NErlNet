#!/usr/bin/env python3
from __future__ import annotations

import argparse
import json
from pathlib import Path
from typing import Callable

import matplotlib.pyplot as plt
import numpy as np
import pandas as pd
import seaborn as sns
from pandas.errors import EmptyDataError


sns.set_theme(style="whitegrid")


def _label_series(df: pd.DataFrame) -> pd.Series:
    phase = df.get("phase_type", pd.Series([""] * len(df)))
    return df["run_label"].astype(str) + " [" + phase.astype(str) + "]"


def _write_plot(fig: plt.Figure, out_dir: Path, stem: str, title: str, saved_paths: list[dict[str, str]]) -> None:
    png_path = out_dir / f"{stem}.png"
    svg_path = out_dir / f"{stem}.svg"
    fig.tight_layout()
    fig.savefig(png_path, dpi=180, bbox_inches="tight")
    fig.savefig(svg_path, bbox_inches="tight")
    plt.close(fig)
    saved_paths.append({"name": title, "png": str(png_path), "svg": str(svg_path)})


def _pin_legend(ax: plt.Axes, outside: bool = True) -> None:
    legend = ax.get_legend()
    if legend is None:
        return
    title = legend.get_title().get_text() if legend.get_title() is not None else None
    if outside:
        ax.legend(loc="upper left", bbox_to_anchor=(1.02, 1.0), borderaxespad=0.0, title=title)
    else:
        ax.legend(loc="upper right", title=title)


def _read_csv_if_present(path: Path) -> pd.DataFrame:
    if not path.exists():
        return pd.DataFrame()
    try:
        return pd.read_csv(path, low_memory=False)
    except EmptyDataError:
        return pd.DataFrame()


def render_plots(input_root: Path, out_dir: Path) -> list[dict[str, str]]:
    raw_dir = input_root / "raw"
    phase_summary = _read_csv_if_present(raw_dir / "phase_summary.csv")
    step_summary = _read_csv_if_present(raw_dir / "step_summary.csv")
    out_dir.mkdir(parents=True, exist_ok=True)
    saved: list[dict[str, str]] = []

    if not step_summary.empty:
        breakdown = (
            step_summary.groupby(["run_label", "phase_type"], as_index=False)[["compute_us", "pp_comm_us", "tp_collective_us", "optimizer_barrier_us", "idle_us"]]
            .mean()
        )
        breakdown["label"] = _label_series(breakdown)
        fig, ax = plt.subplots(figsize=(14, 6))
        bottom = pd.Series([0.0] * len(breakdown))
        colors = {
            "compute_us": "#264653",
            "pp_comm_us": "#2a9d8f",
            "tp_collective_us": "#e9c46a",
            "optimizer_barrier_us": "#f4a261",
            "idle_us": "#e76f51",
        }
        for column in ["compute_us", "pp_comm_us", "tp_collective_us", "optimizer_barrier_us", "idle_us"]:
            ax.bar(breakdown["label"], breakdown[column], bottom=bottom, label=column.replace("_us", ""), color=colors[column])
            bottom += breakdown[column]
        ax.set_title("Average Step Time Breakdown")
        ax.set_ylabel("Microseconds")
        ax.tick_params(axis="x", rotation=45)
        ax.legend(loc="upper right")
        _write_plot(fig, out_dir, "plot_01_step_time_breakdown", "Average Step Time Breakdown", saved)

        cdf_df = step_summary[["run_label", "phase_type", "step_time_us"]].copy()
        cdf_df = cdf_df[cdf_df["step_time_us"] > 0].sort_values("step_time_us")
        if not cdf_df.empty:
            cdf_df["cdf"] = cdf_df.groupby(["run_label", "phase_type"]).cumcount() + 1
            cdf_df["cdf"] = cdf_df["cdf"] / cdf_df.groupby(["run_label", "phase_type"])["cdf"].transform("max")
            max_points_per_series = 2000
            sampled_groups = []
            for _, group in cdf_df.groupby(["run_label", "phase_type"], sort=False):
                if len(group) > max_points_per_series:
                    group = group.iloc[
                        np.linspace(0, len(group) - 1, max_points_per_series, dtype=int)
                    ].copy()
                sampled_groups.append(group)
            cdf_df = pd.concat(sampled_groups, ignore_index=True)
            cdf_df["label"] = _label_series(cdf_df)
            fig, ax = plt.subplots(figsize=(14, 6))
            sns.lineplot(data=cdf_df, x="step_time_us", y="cdf", hue="label", ax=ax)
            ax.set_title("Step Time CDF")
            ax.set_xlabel("Step Time (us)")
            ax.set_ylabel("CDF")
            _pin_legend(ax)
            _write_plot(fig, out_dir, "plot_06_step_time_cdf", "Step Time CDF", saved)

            comm_df = (
                step_summary.groupby(["run_label", "phase_type"], as_index=False)[["bytes_total_comm"]]
                .mean()
            )
            comm_df["label"] = _label_series(comm_df)
            fig, ax = plt.subplots(figsize=(14, 6))
            sns.barplot(data=comm_df, x="label", y="bytes_total_comm", ax=ax, color="#457b9d")
            ax.set_title("Average Communication Volume Per Step")
            ax.set_ylabel("Bytes")
            ax.tick_params(axis="x", rotation=45)
            _write_plot(fig, out_dir, "plot_05_comm_volume_per_step", "Average Communication Volume Per Step", saved)

    if not phase_summary.empty:
        phase_summary = phase_summary.copy()
        phase_summary["label"] = _label_series(phase_summary)

        bubble_df = phase_summary[phase_summary["phase_type"] == "training"].copy()
        if not bubble_df.empty:
            fig, ax = plt.subplots(figsize=(12, 6))
            sns.lineplot(data=bubble_df, x="num_microbatches", y="bubble_fraction", hue="scheduler", marker="o", ax=ax)
            ax.set_title("Bubble Fraction vs Microbatches")
            ax.set_xlabel("Microbatches")
            ax.set_ylabel("Bubble Fraction")
            _pin_legend(ax)
            _write_plot(fig, out_dir, "plot_02_bubble_vs_microbatches", "Bubble Fraction vs Microbatches", saved)

            fig, ax = plt.subplots(figsize=(12, 6))
            sns.lineplot(data=bubble_df, x="num_microbatches", y="throughput_sps", hue="scheduler", marker="o", ax=ax)
            ax.set_title("Throughput vs Microbatches")
            ax.set_xlabel("Microbatches")
            ax.set_ylabel("Samples / sec")
            _pin_legend(ax)
            _write_plot(fig, out_dir, "plot_03_throughput_vs_microbatches", "Throughput vs Microbatches", saved)

        completion_df = phase_summary[["label", "expected_microbatches", "completed_microbatches", "skipped_microbatches", "source_dropped_batches"]].copy()
        completion_df = completion_df.melt(id_vars=["label"], var_name="metric", value_name="value")
        fig, ax = plt.subplots(figsize=(14, 6))
        sns.barplot(data=completion_df, x="label", y="value", hue="metric", ax=ax)
        ax.set_title("Completion / Drop / Skip Summary")
        ax.set_ylabel("Count")
        ax.tick_params(axis="x", rotation=45)
        _pin_legend(ax)
        _write_plot(fig, out_dir, "plot_04_completion_drop_skip", "Completion / Drop / Skip Summary", saved)

    (out_dir / "figure_manifest.json").write_text(json.dumps(saved, indent=2), encoding="utf-8")
    return saved


def parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser(description="Render PTD report plots from raw CSV artifacts")
    parser.add_argument("--input", type=Path, required=True)
    parser.add_argument("--out", type=Path, required=True)
    return parser.parse_args()


def main() -> int:
    args = parse_args()
    render_plots(args.input.resolve(), args.out.resolve())
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
