-module(torchPipelineTests).
-author("Nerlnet Pipeline/Tensor Parallelism NIF Tests").

-include_lib("kernel/include/logger.hrl").
-include("../nerlTensor.hrl").
-include("torchPipelineTestsDefs.hrl").
-include("../layers_types_ag.hrl").
-include("../models_types_ag.hrl").

-define(NERLTEST_PRINT_STR, "[NERLTEST-PIPELINE] ").
-define(NIF_REPLY_TIMEOUT_MS, 15000).

-export([run_tests/0]).

-import(nerlTorchNIF, [
    encode_nif/2, decode_nif/2, nerltensor_conversion/2,
    test_nerlworker_nif/4, remove_nerlworker_nif/1,
    nif_preload/0,
    pipeline_stage0_forward_nif/5,
    pipeline_stage_forward_nif/7,
    pipeline_stage_last_forward_backward_nif/7,
    pipeline_stage_backward_nif/5,
    pipeline_predict_stage0_forward_nif/3,
    pipeline_predict_stage_forward_nif/3,
    optimizer_barrier_nif/1,
    train_microbatch_nif/4,
    nerltensor_split_nif/4,
    nerltensor_concat_nif/3,
    nerltensor_reduce_sum_list_nif/2
]).
-import(nerlTensor, [nerltensor_sum_erl/2]).
-import(nerl, [compare_floats_L/3, string_format/2]).

%%====================================================================
%% Test Entry Point
%%====================================================================

run_tests() ->
    nerl:logger_settings(torchPipelineTests),
    ensure_torch_nif_loaded(),
    nerltest_print("Starting pipeline/tensor parallelism NIF tests"),

    %% Tensor parallelism primitive tests
    test_nerltensor_split_concat_axis0(),
    test_nerltensor_split_concat_axis1(),
    test_nerltensor_reduce_sum(),

    %% Pipeline worker creation
    test_pipeline_worker_creation(),

    %% Pipeline training - 2 stages
    test_pipeline_2stage_train(),

    %% Pipeline training - 3 stages (tests intermediate stage_forward)
    test_pipeline_3stage_train(),

    %% Pipeline training - multi microbatch
    test_pipeline_2stage_multi_microbatch_train(),

    %% Pipeline prediction
    test_pipeline_2stage_predict(),

    %% Microbatch training (non-pipeline) + optimizer barrier
    test_train_microbatch_with_barrier(),

    nerltest_print("All pipeline/tensor parallelism NIF tests PASSED"),
    ok.

%%====================================================================
%% Tensor Split/Concat Axis 0 Roundtrip
%%====================================================================

test_nerltensor_split_concat_axis0() ->
    nerltest_print("test_nerltensor_split_concat_axis0 start"),
    DimX = 10, DimY = 8, DimZ = 1,
    ErlTensor = generate_float_tensor(DimX, DimY, DimZ),
    {Bin, BinType} = encode_nif(ErlTensor, float),
    NumShards = 2,
    Shards = nerltensor_split_nif(Bin, BinType, NumShards, 0),
    %% Verify shard count
    NumShards = length(Shards),
    %% Verify each shard has half the rows
    lists:foreach(
        fun({ShardBin, ShardType}) ->
            {Decoded, _} = decode_nif(ShardBin, ShardType),
            [SDimX, _SDimY, _SDimZ | _Data] = Decoded,
            ExpectedRows = DimX div NumShards,
            case round(SDimX) of
                ExpectedRows -> ok;
                Other -> erlang:error({shard_axis0_dimx_mismatch, ExpectedRows, Other})
            end
        end,
        Shards
    ),
    %% Concat back and verify roundtrip
    {ReconstructedBin, ReconstructedType} = nerltensor_concat_nif(Shards, BinType, 0),
    {ReconstructedErl, _} = decode_nif(ReconstructedBin, ReconstructedType),
    ensure_tensor_match(ErlTensor, ReconstructedErl),
    nerltest_print("test_nerltensor_split_concat_axis0 PASSED").

%%====================================================================
%% Tensor Split/Concat Axis 1 Roundtrip
%%====================================================================

test_nerltensor_split_concat_axis1() ->
    nerltest_print("test_nerltensor_split_concat_axis1 start"),
    DimX = 10, DimY = 8, DimZ = 1,
    ErlTensor = generate_float_tensor(DimX, DimY, DimZ),
    {Bin, BinType} = encode_nif(ErlTensor, float),
    NumShards = 2,
    Shards = nerltensor_split_nif(Bin, BinType, NumShards, 1),
    NumShards = length(Shards),
    %% Each shard should have half the columns
    lists:foreach(
        fun({ShardBin, ShardType}) ->
            {Decoded, _} = decode_nif(ShardBin, ShardType),
            [_SDimX, SDimY, _SDimZ | _Data] = Decoded,
            ExpectedCols = DimY div NumShards,
            case round(SDimY) of
                ExpectedCols -> ok;
                Other -> erlang:error({shard_axis1_dimy_mismatch, ExpectedCols, Other})
            end
        end,
        Shards
    ),
    {ReconstructedBin, ReconstructedType} = nerltensor_concat_nif(Shards, BinType, 1),
    {ReconstructedErl, _} = decode_nif(ReconstructedBin, ReconstructedType),
    ensure_tensor_match(ErlTensor, ReconstructedErl),
    nerltest_print("test_nerltensor_split_concat_axis1 PASSED").

%%====================================================================
%% Tensor Reduce Sum
%%====================================================================

test_nerltensor_reduce_sum() ->
    nerltest_print("test_nerltensor_reduce_sum start"),
    DimX = 4, DimY = 3, DimZ = 1,
    TensorA = generate_float_tensor(DimX, DimY, DimZ),
    TensorB = generate_float_tensor(DimX, DimY, DimZ),
    TensorC = generate_float_tensor(DimX, DimY, DimZ),
    %% Compute expected sum in Erlang
    SumAB = nerltensor_sum_erl({TensorA, erl_float}, {TensorB, erl_float}),
    Expected = nerltensor_sum_erl({SumAB, erl_float}, {TensorC, erl_float}),
    %% Encode all tensors
    {BinA, TypeA} = encode_nif(TensorA, float),
    {BinB, _} = encode_nif(TensorB, float),
    {BinC, _} = encode_nif(TensorC, float),
    Tensors = [{BinA, TypeA}, {BinB, TypeA}, {BinC, TypeA}],
    {ResultBin, ResultType} = nerltensor_reduce_sum_list_nif(Tensors, float),
    {ResultErl, _} = decode_nif(ResultBin, ResultType),
    ensure_tensor_match(Expected, ResultErl),
    nerltest_print("test_nerltensor_reduce_sum PASSED").

%%====================================================================
%% Pipeline Worker Creation (2-stage)
%%====================================================================

test_pipeline_worker_creation() ->
    nerltest_print("test_pipeline_worker_creation start"),
    ModelPath = resolve_pipeline_model_path(),
    %% Create stage 0 worker
    ModelId0 = erlang:unique_integer([positive]),
    Params0 = build_pipeline_train_params(ModelPath, 0, 2),
    ok = test_nerlworker_nif(ModelId0, "0", "", Params0),
    nerltest_print("  Stage 0 worker created"),
    %% Create stage 1 worker
    ModelId1 = erlang:unique_integer([positive]),
    Params1 = build_pipeline_train_params(ModelPath, 1, 2),
    ok = test_nerlworker_nif(ModelId1, "0", "", Params1),
    nerltest_print("  Stage 1 worker created"),
    %% Cleanup
    _ = remove_nerlworker_nif(ModelId0),
    _ = remove_nerlworker_nif(ModelId1),
    nerltest_print("test_pipeline_worker_creation PASSED").

%%====================================================================
%% Pipeline 2-Stage Training (single microbatch)
%% Stage 0: Linear(5,30) + ReLU + Linear(30,5)  -- input [B,5] → output [B,5]
%% Stage 1: ReLU + Linear(5,3)                  -- input [B,5] → output [B,3]
%%====================================================================

test_pipeline_2stage_train() ->
    nerltest_print("test_pipeline_2stage_train start"),
    ModelPath = resolve_pipeline_model_path(),
    ModelId0 = erlang:unique_integer([positive]),
    ModelId1 = erlang:unique_integer([positive]),
    Params0 = build_pipeline_train_params(ModelPath, 0, 2),
    Params1 = build_pipeline_train_params(ModelPath, 1, 2),
    ok = test_nerlworker_nif(ModelId0, "0", "", Params0),
    ok = test_nerlworker_nif(ModelId1, "0", "", Params1),

    %% Generate batch: [BatchSize, NumFeatures+NumLabels, 1]
    {BatchBin, BatchType} = generate_pipeline_batch(?PIPELINE_BATCH_SIZE),
    BatchID = 0,
    MicrobatchID = 0,

    %% --- Forward pass ---
    %% Stage 0 forward
    {ok, pipeline_stage0_forward, ActBin, ActType, LabBin, LabType, Time0} =
        pipeline_stage0_forward_nif(ModelId0, BatchBin, BatchType, BatchID, MicrobatchID),
    nerltest_print(string_format("  Stage 0 forward: ~.3f ms", [Time0 / 1000])),
    assert_valid_tensor(ActBin, ActType, "stage0_activation"),
    assert_valid_tensor(LabBin, LabType, "stage0_labels"),

    %% Stage 1 (last) forward + backward
    {ok, pipeline_stage_last, LossBin, LossType, GradBin, GradType, Time1} =
        pipeline_stage_last_forward_backward_nif(
            ModelId1, ActBin, ActType, LabBin, LabType, BatchID, MicrobatchID),
    nerltest_print(string_format("  Stage 1 fwd+bwd: ~.3f ms", [Time1 / 1000])),
    assert_valid_tensor(LossBin, LossType, "loss"),
    assert_valid_tensor(GradBin, GradType, "stage1_grad"),

    %% --- Backward pass ---
    %% Stage 0 backward
    {ok, pipeline_stage_backward, GradInputBin, GradInputType, Time2} =
        pipeline_stage_backward_nif(ModelId0, GradBin, GradType, BatchID, MicrobatchID),
    nerltest_print(string_format("  Stage 0 backward: ~.3f ms", [Time2 / 1000])),
    assert_valid_tensor(GradInputBin, GradInputType, "stage0_grad_input"),

    %% --- Optimizer step ---
    ok = optimizer_barrier_nif(ModelId0),
    ok = optimizer_barrier_nif(ModelId1),
    nerltest_print("  Optimizer barriers done"),

    %% Validate loss
    validate_loss_tensor(LossBin, LossType),

    _ = remove_nerlworker_nif(ModelId0),
    _ = remove_nerlworker_nif(ModelId1),
    nerltest_print("test_pipeline_2stage_train PASSED").

%%====================================================================
%% Pipeline 3-Stage Training (tests intermediate pipeline_stage_forward)
%% Stage 0: Linear(5,30) + ReLU     -- input [B,5]  → output [B,30]
%% Stage 1: Linear(30,5) + ReLU     -- input [B,30] → output [B,5]
%% Stage 2: Linear(5,3)             -- input [B,5]  → output [B,3]
%%====================================================================

test_pipeline_3stage_train() ->
    nerltest_print("test_pipeline_3stage_train start"),
    ModelPath = resolve_pipeline_model_path(),
    ModelId0 = erlang:unique_integer([positive]),
    ModelId1 = erlang:unique_integer([positive]),
    ModelId2 = erlang:unique_integer([positive]),
    Params0 = build_pipeline_train_params(ModelPath, 0, 3),
    Params1 = build_pipeline_train_params(ModelPath, 1, 3),
    Params2 = build_pipeline_train_params(ModelPath, 2, 3),
    ok = test_nerlworker_nif(ModelId0, "0", "", Params0),
    ok = test_nerlworker_nif(ModelId1, "0", "", Params1),
    ok = test_nerlworker_nif(ModelId2, "0", "", Params2),

    {BatchBin, BatchType} = generate_pipeline_batch(?PIPELINE_BATCH_SIZE),
    BatchID = 0,
    MicrobatchID = 0,

    %% --- Forward pass ---
    %% Stage 0 forward
    {ok, pipeline_stage0_forward, Act0Bin, Act0Type, Lab0Bin, Lab0Type, _T0} =
        pipeline_stage0_forward_nif(ModelId0, BatchBin, BatchType, BatchID, MicrobatchID),
    assert_valid_tensor(Act0Bin, Act0Type, "3s_stage0_act"),
    nerltest_print("  3-stage: Stage 0 forward done"),

    %% Stage 1 (intermediate) forward
    {ok, pipeline_stage_forward, Act1Bin, Act1Type, Lab1Bin, Lab1Type, _T1} =
        pipeline_stage_forward_nif(
            ModelId1, Act0Bin, Act0Type, Lab0Bin, Lab0Type, BatchID, MicrobatchID),
    assert_valid_tensor(Act1Bin, Act1Type, "3s_stage1_act"),
    nerltest_print("  3-stage: Stage 1 (intermediate) forward done"),

    %% Stage 2 (last) forward + backward
    {ok, pipeline_stage_last, LossBin, LossType, Grad2Bin, Grad2Type, _T2} =
        pipeline_stage_last_forward_backward_nif(
            ModelId2, Act1Bin, Act1Type, Lab1Bin, Lab1Type, BatchID, MicrobatchID),
    assert_valid_tensor(LossBin, LossType, "3s_loss"),
    assert_valid_tensor(Grad2Bin, Grad2Type, "3s_stage2_grad"),
    nerltest_print("  3-stage: Stage 2 (last) fwd+bwd done"),

    %% --- Backward pass ---
    %% Stage 1 backward
    {ok, pipeline_stage_backward, Grad1Bin, Grad1Type, _T3} =
        pipeline_stage_backward_nif(ModelId1, Grad2Bin, Grad2Type, BatchID, MicrobatchID),
    assert_valid_tensor(Grad1Bin, Grad1Type, "3s_stage1_grad"),
    nerltest_print("  3-stage: Stage 1 backward done"),

    %% Stage 0 backward
    {ok, pipeline_stage_backward, Grad0Bin, Grad0Type, _T4} =
        pipeline_stage_backward_nif(ModelId0, Grad1Bin, Grad1Type, BatchID, MicrobatchID),
    assert_valid_tensor(Grad0Bin, Grad0Type, "3s_stage0_grad"),
    nerltest_print("  3-stage: Stage 0 backward done"),

    %% Optimizer step for all stages
    ok = optimizer_barrier_nif(ModelId0),
    ok = optimizer_barrier_nif(ModelId1),
    ok = optimizer_barrier_nif(ModelId2),

    validate_loss_tensor(LossBin, LossType),

    _ = remove_nerlworker_nif(ModelId0),
    _ = remove_nerlworker_nif(ModelId1),
    _ = remove_nerlworker_nif(ModelId2),
    nerltest_print("test_pipeline_3stage_train PASSED").

%%====================================================================
%% Pipeline 2-Stage Multi-Microbatch Training (2 microbatches)
%%
%% Exercises the pipeline context cache keyed by {batch_id, microbatch_id}
%% and verifies gradient accumulation across microbatches.
%%====================================================================

test_pipeline_2stage_multi_microbatch_train() ->
    nerltest_print("test_pipeline_2stage_multi_microbatch_train start"),
    ModelPath = resolve_pipeline_model_path(),
    ModelId0 = erlang:unique_integer([positive]),
    ModelId1 = erlang:unique_integer([positive]),
    Params0 = build_pipeline_train_params(ModelPath, 0, 2),
    Params1 = build_pipeline_train_params(ModelPath, 1, 2),
    ok = test_nerlworker_nif(ModelId0, "0", "", Params0),
    ok = test_nerlworker_nif(ModelId1, "0", "", Params1),

    BatchID = 0,
    {MB0Bin, MB0Type} = generate_pipeline_batch(?PIPELINE_MICROBATCH_SIZE),
    {MB1Bin, MB1Type} = generate_pipeline_batch(?PIPELINE_MICROBATCH_SIZE),

    %% --- Forward microbatch 0 ---
    {ok, pipeline_stage0_forward, Act0_MB0, ActType0, Lab0_MB0, LabType0, _} =
        pipeline_stage0_forward_nif(ModelId0, MB0Bin, MB0Type, BatchID, 0),
    {ok, pipeline_stage_last, Loss_MB0, LossType0, Grad1_MB0, GradType0, _} =
        pipeline_stage_last_forward_backward_nif(
            ModelId1, Act0_MB0, ActType0, Lab0_MB0, LabType0, BatchID, 0),
    nerltest_print("  Multi-MB: microbatch 0 forward done"),

    %% --- Forward microbatch 1 ---
    {ok, pipeline_stage0_forward, Act0_MB1, ActType1, Lab0_MB1, LabType1, _} =
        pipeline_stage0_forward_nif(ModelId0, MB1Bin, MB1Type, BatchID, 1),
    {ok, pipeline_stage_last, Loss_MB1, LossType1, Grad1_MB1, GradType1, _} =
        pipeline_stage_last_forward_backward_nif(
            ModelId1, Act0_MB1, ActType1, Lab0_MB1, LabType1, BatchID, 1),
    nerltest_print("  Multi-MB: microbatch 1 forward done"),

    %% --- Backward microbatch 0 ---
    {ok, pipeline_stage_backward, _, _, _} =
        pipeline_stage_backward_nif(ModelId0, Grad1_MB0, GradType0, BatchID, 0),
    nerltest_print("  Multi-MB: microbatch 0 backward done"),

    %% --- Backward microbatch 1 ---
    {ok, pipeline_stage_backward, _, _, _} =
        pipeline_stage_backward_nif(ModelId0, Grad1_MB1, GradType1, BatchID, 1),
    nerltest_print("  Multi-MB: microbatch 1 backward done"),

    %% Optimizer barrier (flushes accumulated gradients)
    ok = optimizer_barrier_nif(ModelId0),
    ok = optimizer_barrier_nif(ModelId1),

    validate_loss_tensor(Loss_MB0, LossType0),
    validate_loss_tensor(Loss_MB1, LossType1),

    _ = remove_nerlworker_nif(ModelId0),
    _ = remove_nerlworker_nif(ModelId1),
    nerltest_print("test_pipeline_2stage_multi_microbatch_train PASSED").

%%====================================================================
%% Pipeline 2-Stage Prediction
%%====================================================================

test_pipeline_2stage_predict() ->
    nerltest_print("test_pipeline_2stage_predict start"),
    ModelPath = resolve_pipeline_model_path(),
    ModelId0 = erlang:unique_integer([positive]),
    ModelId1 = erlang:unique_integer([positive]),
    Params0 = build_pipeline_train_params(ModelPath, 0, 2),
    Params1 = build_pipeline_train_params(ModelPath, 1, 2),
    ok = test_nerlworker_nif(ModelId0, "0", "", Params0),
    ok = test_nerlworker_nif(ModelId1, "0", "", Params1),

    %% Generate features-only batch for prediction
    {PredBin, PredType} = generate_predict_batch(?PIPELINE_BATCH_SIZE),

    %% Stage 0 predict forward
    {ok, pipeline_predict_stage0, Act0Bin, Act0Type, _T0} =
        pipeline_predict_stage0_forward_nif(ModelId0, PredBin, PredType),
    assert_valid_tensor(Act0Bin, Act0Type, "predict_stage0_act"),
    nerltest_print("  Predict: Stage 0 forward done"),

    %% Stage 1 predict forward
    {ok, pipeline_predict_stage, OutputBin, OutputType, _T1} =
        pipeline_predict_stage_forward_nif(ModelId1, Act0Bin, Act0Type),
    assert_valid_tensor(OutputBin, OutputType, "predict_stage1_output"),
    nerltest_print("  Predict: Stage 1 forward done"),

    %% Validate output shape: [BatchSize, NumLabels]
    {OutputErl, _} = decode_nif(OutputBin, OutputType),
    [OutDimX, OutDimY, _OutDimZ | _Data] = OutputErl,
    ExpectedBatch = ?PIPELINE_BATCH_SIZE,
    ExpectedLabels = ?PIPELINE_NUM_LABELS,
    case {round(OutDimX), round(OutDimY)} of
        {ExpectedBatch, ExpectedLabels} ->
            nerltest_print(string_format("  Predict output shape: [~p, ~p]",
                                         [ExpectedBatch, ExpectedLabels]));
        {ActualX, ActualY} ->
            erlang:error({pipeline_predict_output_shape_mismatch,
                          {expected, {ExpectedBatch, ExpectedLabels}},
                          {got, {ActualX, ActualY}}})
    end,

    _ = remove_nerlworker_nif(ModelId0),
    _ = remove_nerlworker_nif(ModelId1),
    nerltest_print("test_pipeline_2stage_predict PASSED").

%%====================================================================
%% Train Microbatch + Optimizer Barrier (non-pipeline mode)
%%
%% Tests train_microbatch_nif and optimizer_barrier_nif without pipeline
%% partitioning (pipeline_world_size=1). Exercises gradient accumulation
%% and deferred optimizer step.
%%====================================================================

test_train_microbatch_with_barrier() ->
    nerltest_print("test_train_microbatch_with_barrier start"),
    ModelPath = resolve_pipeline_model_path(),
    %% Non-pipeline worker (world_size=1 → pipeline disabled)
    ModelId = erlang:unique_integer([positive]),
    Params = build_pipeline_train_params(ModelPath, 0, 1),
    ok = test_nerlworker_nif(ModelId, "0", "", Params),

    %% Generate 2 microbatches
    {MB0Bin, MB0Type} = generate_pipeline_batch(?PIPELINE_MICROBATCH_SIZE),
    {MB1Bin, MB1Type} = generate_pipeline_batch(?PIPELINE_MICROBATCH_SIZE),

    %% Train microbatch 0 (async - sends result via message)
    ok = train_microbatch_nif(ModelId, MB0Bin, MB0Type, 0),
    await_nerlnif_reply(train_microbatch_0),
    nerltest_print("  Microbatch 0 trained"),

    %% Train microbatch 1
    ok = train_microbatch_nif(ModelId, MB1Bin, MB1Type, 1),
    await_nerlnif_reply(train_microbatch_1),
    nerltest_print("  Microbatch 1 trained"),

    %% Optimizer barrier to flush accumulated gradients
    ok = optimizer_barrier_nif(ModelId),
    nerltest_print("  Optimizer barrier done"),

    _ = remove_nerlworker_nif(ModelId),
    nerltest_print("test_train_microbatch_with_barrier PASSED").

%%====================================================================
%% Helper Functions
%%====================================================================

nerltest_print(String) ->
    logger:notice(?NERLTEST_PRINT_STR ++ String).

ensure_torch_nif_loaded() ->
    case catch nif_preload() of
        done -> ok;
        {'EXIT', Reason} -> throw({torch_nif_not_loaded, Reason})
    end.

%% Generate an Erlang-format float tensor with random data.
%% Returns [DimX, DimY, DimZ | Data] with float dims and float data.
generate_float_tensor(DimX, DimY, DimZ) ->
    DataLength = DimX * DimY * DimZ,
    Data = [rand:uniform() * 10 || _ <- lists:seq(1, DataLength)],
    [float(DimX), float(DimY), float(DimZ)] ++ Data.

%% Generate encoded batch tensor for pipeline training.
%% Layout per sample row: [feat1..featN, label1..labelM]
generate_pipeline_batch(BatchSize) ->
    DimY = ?PIPELINE_NUM_FEATURES + ?PIPELINE_NUM_LABELS,
    ErlTensor = generate_float_tensor(BatchSize, DimY, 1),
    nerltensor_conversion({ErlTensor, erl_float}, float).

%% Generate encoded features-only batch for prediction.
generate_predict_batch(BatchSize) ->
    ErlTensor = generate_float_tensor(BatchSize, ?PIPELINE_NUM_FEATURES, 1),
    nerltensor_conversion({ErlTensor, erl_float}, float).

%% Build TrainParams map for pipeline workers.
%% input_tensor_shape covers ONLY features (not labels) so
%% split_training_batch correctly separates features from labels.
build_pipeline_train_params(ModelPath, PipelineStage, PipelineWorldSize) ->
    BatchSizeStr = integer_to_list(?PIPELINE_BATCH_SIZE),
    NumFeatStr = integer_to_list(?PIPELINE_NUM_FEATURES),
    NumLabStr = integer_to_list(?PIPELINE_NUM_LABELS),
    InputShapeStr = "[" ++ BatchSizeStr ++ "," ++ NumFeatStr ++ "]",
    LabelsShapeStr = "[" ++ BatchSizeStr ++ "," ++ NumLabStr ++ "]",
    #{
        "model_path" => ModelPath,
        "model_format" => "torchscript",
        "model_checksum" => "",
        "model_description" => "pipeline_nif_test",
        "lr" => "0.01",
        "learning_rate" => "0.01",
        "epochs" => "5",
        "optimizer" => "sgd",
        "optim" => "sgd",
        "loss" => "mse",
        "input_tensor_shape" => InputShapeStr,
        "labels_shape" => LabelsShapeStr,
        "labels_offset" => NumFeatStr,
        "pipeline_stage" => integer_to_list(PipelineStage),
        "pipeline_world_size" => integer_to_list(PipelineWorldSize)
    }.

%% Resolve the pipeline model .pt path, trying NERLNET_PATH then default.
resolve_pipeline_model_path() ->
    BasePath = case os:getenv("NERLNET_PATH") of
        false -> ?NERLNET_PATH;
        Value -> Value
    end,
    Candidates = [
        filename:join(BasePath, ?TORCH_PIPELINE_TEST_MODEL_RELATIVE_PATH),
        ?TORCH_PIPELINE_TEST_MODEL_DEFAULT_PATH
    ],
    case lists:dropwhile(fun(P) -> not filelib:is_regular(P) end, Candidates) of
        [Found | _] -> Found;
        [] ->
            nerltest_print("WARNING: pipeline model not found, using first candidate"),
            hd(Candidates)
    end.

%% Assert that a binary tensor can be decoded and has more than just dims.
assert_valid_tensor(Bin, Type, Label) when is_binary(Bin), is_atom(Type) ->
    {Decoded, _ListType} = decode_nif(Bin, Type),
    case is_list(Decoded) andalso length(Decoded) > 3 of
        true -> ok;
        false ->
            erlang:error({invalid_tensor, Label, {decoded_length, length(Decoded)}})
    end;
assert_valid_tensor(_Bin, _Type, Label) ->
    erlang:error({invalid_tensor_args, Label}).

%% Validate loss tensor is a non-negative finite scalar.
validate_loss_tensor(LossBin, LossType) ->
    {LossErl, _} = decode_nif(LossBin, LossType),
    [_DimX, _DimY, _DimZ | LossData] = LossErl,
    case LossData of
        [LossVal | _] when is_float(LossVal), LossVal >= 0.0 ->
            nerltest_print(string_format("  Loss value: ~.6f", [LossVal]));
        [LossVal | _] ->
            erlang:error({invalid_loss_value, LossVal});
        [] ->
            erlang:error(empty_loss_tensor)
    end.

%% Compare two Erlang float lists with tolerance.
ensure_tensor_match(Expected, Actual) ->
    case compare_floats_L(Expected, Actual, 4) of
        true -> ok;
        false -> erlang:error({tensor_mismatch, Expected, Actual})
    end.

%% Wait for async NIF reply (used by train_microbatch_nif).
await_nerlnif_reply(Action) ->
    receive
        {nerlnif, error, Reason} -> erlang:error({Action, Reason});
        {nerlnif, _Payload, _PayloadType, _Time} -> ok;
        {nerlnif, _Payload, _PayloadType, _Time, _MicrobatchId} -> ok;
        {nerlnif, _Payload, _Time} -> ok
    after ?NIF_REPLY_TIMEOUT_MS -> erlang:error({Action, timeout})
    end.
