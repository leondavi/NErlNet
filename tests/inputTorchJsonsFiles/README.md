# Torch Test JSON Inputs

This directory mirrors `tests/inputJsonsFiles` but describes a single-device deployment where all workers use the Torch bridge. Key differences:

- Distributed config (`dc_torch_synt_1d_2c_1s_4r_4w*.json`) sets `infraType` to `"torch"` for all worker models and adds a `pt_path` attribute that points to a TorchScript `.pt` artifact under `models/`.
- `train_params` must describe how the flattened Nerltensor splits into features vs. labels using `input_tensor_shape`, `labels_offset` (per-sample column offset; `"default"` means labels appear immediately after the feature columns), and `labels_shape`; keep these values aligned with the experiment batch/feature counts so Torch workers can reconstruct tensors correctly.
- Optional `w_init_rand` flag randomizes the TorchScript parameters one time after the model loads; set it to the string `"True"` when you need each worker to start from fresh weights.
- Experiment/connection maps are copies of the synthetic baseline so the Python acceptance test can reuse the same dataset and routing topology.
- `baseline/` reuses the OpenNN reference metrics so the torch full-flow harness can compare results until torch-specific baselines are produced.

`models/placeholder_perceptron.pt` is populated at test time by `tests/scripts/generate_torch_test_model.py`; regenerate it before running the torch full-flow test so the JSON `pt_path` entries resolve to an actual TorchScript model.
