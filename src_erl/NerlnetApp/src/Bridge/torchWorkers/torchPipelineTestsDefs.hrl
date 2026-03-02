%% Pipeline/tensor parallelism test definitions
%% Model path for pipeline-compatible TorchScript model

-define(TORCH_PIPELINE_TEST_MODEL_RELATIVE_PATH,
        "tests/inputTorchJsonsFiles/models/placeholder_pipeline.pt").
-define(TORCH_PIPELINE_TEST_MODEL_DEFAULT_PATH,
        "/usr/local/lib/nerlnet-lib/NErlNet/tests/inputTorchJsonsFiles/models/placeholder_pipeline.pt").

%% Model architecture:
%%   layers.0  Linear(5, 30)
%%   layers.1  ReLU
%%   layers.2  Linear(30, 5)
%%   layers.3  ReLU
%%   layers.4  Linear(5, 3)
%% Total: 5 child modules for pipeline partitioning.

-define(PIPELINE_NUM_FEATURES, 5).
-define(PIPELINE_NUM_LABELS, 3).
-define(PIPELINE_BATCH_SIZE, 10).
-define(PIPELINE_MICROBATCH_SIZE, 5).
