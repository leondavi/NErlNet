#!/usr/bin/env python3
import argparse
import csv
import json
import os
import sys
from typing import Dict, List

DEFAULT_DOWNLOAD_DIR = "/tmp/nerlnet/data/NerlnetData-master/nerlnet"
os.environ.setdefault("HF_HUB_DISABLE_PROGRESS_BARS", "1")

try:
    from huggingface_hub import HfApi, snapshot_download
except ImportError as exc:
    raise SystemExit(f"huggingface_hub not available: {exc}")


def load_repo_ids(repo_file: str) -> List[Dict]:
    with open(repo_file, "r", encoding="utf-8") as handle:
        payload = json.load(handle)
    datasets = payload.get("datasets", [])
    if not isinstance(datasets, list):
        raise ValueError("Invalid hf_repo_ids.json format: datasets must be a list")
    return datasets


def list_datasets(repo_file: str) -> Dict:
    api = HfApi()
    datasets = []
    for repo in load_repo_ids(repo_file):
        repo_id = str(repo.get("id", "")).strip()
        if not repo_id:
            continue
        repo_name = str(repo.get("name", "")).strip() or repo_id.split("/")[-1]
        csv_files: List[str] = []
        error = ""
        try:
            files = api.list_repo_files(repo_id=repo_id, repo_type="dataset")
            csv_files = [file_name for file_name in files if file_name.lower().endswith(".csv")]
            csv_files.sort()
        except Exception as exc:
            error = str(exc)
        local_meta = resolve_local_dataset_metadata(DEFAULT_DOWNLOAD_DIR, repo_name)
        datasets.append(
            {
                "id": repo_id,
                "idx": int(repo.get("idx", len(datasets))),
                "name": repo_name,
                "description": str(repo.get("description", "")),
                "csvFiles": csv_files,
                "error": error,
                "datasetPath": local_meta["datasetPath"],
                "firstCsvPath": local_meta["firstCsvPath"],
                "sampleRows": local_meta["sampleRows"],
                "sampleColumns": local_meta["sampleColumns"],
            }
        )
    datasets.sort(key=lambda item: item["idx"])
    return {"datasets": datasets}


def find_csv_files(dataset_path: str) -> List[str]:
    csv_files: List[str] = []
    for root, _, files in os.walk(dataset_path):
        for file_name in files:
            if file_name.lower().endswith(".csv"):
                csv_files.append(os.path.join(root, file_name))
    csv_files.sort()
    return csv_files


def inspect_csv_dimensions(csv_path: str) -> Dict[str, int]:
    columns = 0
    rows = 0
    with open(csv_path, "r", encoding="utf-8", newline="") as handle:
        reader = csv.reader(handle)
        first_row = next(reader, None)
        if first_row is None:
            return {"rows": 0, "columns": 0}
        columns = len(first_row)
        for _ in reader:
            rows += 1
    return {"rows": rows, "columns": columns}


def resolve_local_dataset_metadata(download_dir: str, repo_name: str) -> Dict[str, object]:
    dataset_path = os.path.join(download_dir, repo_name)
    if not os.path.isdir(dataset_path):
        return {
            "datasetPath": dataset_path,
            "firstCsvPath": "",
            "sampleRows": None,
            "sampleColumns": None,
        }

    csv_files = find_csv_files(dataset_path)
    first_csv = csv_files[0] if csv_files else ""
    if not first_csv:
        return {
            "datasetPath": dataset_path,
            "firstCsvPath": "",
            "sampleRows": None,
            "sampleColumns": None,
        }

    try:
        dims = inspect_csv_dimensions(first_csv)
        return {
            "datasetPath": dataset_path,
            "firstCsvPath": first_csv,
            "sampleRows": int(dims["rows"]),
            "sampleColumns": int(dims["columns"]),
        }
    except Exception:
        return {
            "datasetPath": dataset_path,
            "firstCsvPath": first_csv,
            "sampleRows": None,
            "sampleColumns": None,
        }


def download_dataset(repo_file: str, repo_idx: int, download_dir: str) -> Dict:
    repo = next(
        (entry for entry in load_repo_ids(repo_file) if int(entry.get("idx", -1)) == repo_idx),
        None,
    )
    if repo is None:
        raise ValueError(f"Dataset idx '{repo_idx}' not found in repository list")

    repo_id = str(repo.get("id", "")).strip()
    if not repo_id:
        raise ValueError(f"Dataset idx '{repo_idx}' has an empty repository id")

    repo_name = str(repo.get("name", "")).strip() or repo_id.split("/")[-1]
    dataset_path = os.path.join(download_dir, repo_name)
    os.makedirs(dataset_path, exist_ok=True)

    snapshot_download(repo_id=repo_id, local_dir=dataset_path, repo_type="dataset")
    csv_files = find_csv_files(dataset_path)
    first_csv = csv_files[0] if csv_files else ""
    sample_rows = None
    sample_columns = None
    if first_csv:
        try:
            dims = inspect_csv_dimensions(first_csv)
            sample_rows = int(dims["rows"])
            sample_columns = int(dims["columns"])
        except Exception:
            sample_rows = None
            sample_columns = None

    return {
        "id": repo_id,
        "idx": repo_idx,
        "name": repo_name,
        "datasetPath": dataset_path,
        "csvFiles": csv_files,
        "firstCsvPath": first_csv,
        "sampleRows": sample_rows,
        "sampleColumns": sample_columns,
    }


def parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser(description="List and download Nerlnet Hugging Face datasets")
    parser.add_argument("--action", choices=["list", "download"], required=True)
    parser.add_argument("--repo-file", required=True)
    parser.add_argument("--repo-idx", type=int)
    parser.add_argument("--download-dir", default=DEFAULT_DOWNLOAD_DIR)
    return parser.parse_args()


def main() -> int:
    args = parse_args()

    try:
        if args.action == "list":
            payload = list_datasets(args.repo_file)
        else:
            if args.repo_idx is None:
                raise ValueError("--repo-idx is required for download action")
            payload = download_dataset(args.repo_file, args.repo_idx, args.download_dir)

        print(json.dumps(payload))
        return 0
    except Exception as exc:
        print(str(exc), file=sys.stderr)
        return 1


if __name__ == "__main__":
    raise SystemExit(main())
