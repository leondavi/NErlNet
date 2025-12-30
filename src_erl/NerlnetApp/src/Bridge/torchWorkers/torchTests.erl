-module(torchTests).
-author("Nerlnet Torch Team").

-include_lib("kernel/include/logger.hrl").
-include("../nerlTensor.hrl").
-include("torchTestsDefs.hrl").
-include("../layers_types_ag.hrl").
-include("../models_types_ag.hrl").

-define(NERLTEST_PRINT_STR, "[NERLTEST-TORCH] ").
-define(DIMX_RAND_MAX, 8).
-define(DIMY_RAND_MAX, 8).
-define(ENCODE_DECODE_ROUNDS, 20).
-define(SUM_ROUNDS, 20).
-define(SCALAR_MUL_ROUNDS, 20).
-define(TRAIN_PHASE_ROUNDS, 2).
-define(PREDICT_PHASE_ROUNDS, 2).
-define(NIF_REPLY_TIMEOUT_MS, 10000).
-define(NERLWORKER_TEST_NUM_SAMPLES, 500).
-define(TORCH_TRAIN_PARAM_MANDATORY_GROUPS,
      [#{label => model_path, keys => ["model_path"], allow_blank => false},
       #{label => learning_rate, keys => ["lr", "learning_rate"], allow_blank => false},
       #{label => epochs, keys => ["epochs"], allow_blank => false},
       #{label => optimizer, keys => ["optimizer", "optim"], allow_blank => false},
       #{label => loss, keys => ["loss"], allow_blank => false}]).
-define(TORCH_TRAIN_PARAM_EXPECTED_KEYS,
      ["model_format",
       "model_checksum",
       "model_description"]).

-export([run_tests/0]).

-import(nerlTorchNIF,[encode_nif/2, decode_nif/2, nerltensor_conversion/2, get_all_binary_types/0,
                      nerltensor_sum_nif/3, nerltensor_scalar_multiplication_nif/3, nerltensor_scalar_multiplication_erl/2,
                      test_nerlworker_nif/4, remove_nerlworker_nif/1, nif_preload/0,
                      train_nif/3, predict_nif/3]).
-import(nerlTensor,[nerltensor_sum_erl/2, split_cols_erl_tensor/3]).
-import(nerl,[compare_floats_L/3, string_format/2, logger_settings/1, tic/0, toc/1]).

nerltest_print(String) ->
      logger:notice(?NERLTEST_PRINT_STR++String).

test_envelope(Func, TestName, Rounds) ->
      nerltest_print(nerl:string_format("~p test starts for ~p rounds",[TestName, Rounds])),
      {TimeTookMicro, _RetVal} = timer:tc(Func, [Rounds]),
      nerltest_print(nerl:string_format("Elapsed: ~p~p",[TimeTookMicro / 1000, ms])), ok.

test_envelope_nif_performance(Func, TestName, Rounds) ->
      nerltest_print(nerl:string_format("~p test starts for ~p rounds",[TestName, Rounds])),
      {TimeTookMicro, AccPerfromance} = timer:tc(Func, [Rounds]),
      AveragedPerformance = AccPerfromance/Rounds,
      nerltest_print(nerl:string_format("Elapsed: ~p~p Average nif performance: ~.3f~p",[TimeTookMicro/1000,ms, AveragedPerformance, ms])), ok.

run_tests()->
      nerl:logger_settings(torchTests),
      ensure_torch_nif_loaded(),
      EncodeDecodeTest = fun(Rounds) -> encode_decode_nifs_test(Rounds, 0) end,
      test_envelope_nif_performance(EncodeDecodeTest, "encode_decode_nifs", ?ENCODE_DECODE_ROUNDS),

      SumTestFloat = fun(Rounds) -> nerltensor_sum_nif_test(float, Rounds, 0) end,
      test_envelope_nif_performance(SumTestFloat, "nerltensor_sum_nif float", ?SUM_ROUNDS),

      SumTestDouble = fun(Rounds) -> nerltensor_sum_nif_test(double, Rounds, 0) end,
      test_envelope_nif_performance(SumTestDouble, "nerltensor_sum_nif double", ?SUM_ROUNDS),

      ScalarMulFloat = fun(Rounds) -> nerltensor_scalar_multiplication_test(float, Rounds, 0) end,
      test_envelope_nif_performance(ScalarMulFloat, "nerltensor_scalar_multiplication float", ?SCALAR_MUL_ROUNDS),

      ScalarMulDouble = fun(Rounds) -> nerltensor_scalar_multiplication_test(double, Rounds, 0) end,
      test_envelope_nif_performance(ScalarMulDouble, "nerltensor_scalar_multiplication double", ?SCALAR_MUL_ROUNDS),

      nerlTorch_training_params_integrity_test(),
      nerlTorch_worker_creation_matrix_test(),
      nerlTorch_dual_worker_lifecycle_test(),
      nerlTorch_concurrent_train_predict_test(),

            WorkerTest = fun(_Rounds) ->
                  nerlTorch_worker_smoke()
            end,
            test_envelope(WorkerTest, "torch_worker_smoke", 1),

      nerltest_print("Torch tests completed"),
      ok.

ensure_torch_nif_loaded() ->
      case catch nif_preload() of
            done -> ok;
            {'EXIT', Reason} -> throw({torch_nif_not_loaded, Reason})
      end.

encode_decode_nifs_test(0, Performance) -> Performance;
encode_decode_nifs_test(Rounds, Performance) ->
      Type = random_pick_binary_type(),
      Tensor = generate_nerltensor_rand_dims(Type),
      Tic = nerl:tic(),
      {Encoded, BinaryType} = encode_nif(Tensor, Type),
      {Decoded, _} = decode_nif(Encoded, BinaryType),
      {Elapsed, _} = nerl:toc(Tic),
      PerformanceNew = Performance + Elapsed,
      ensure_tensor_match(Tensor, Decoded, Type),
      encode_decode_nifs_test(Rounds - 1, PerformanceNew).

nerltensor_sum_nif_test(_Type, 0, Performance) -> Performance;
nerltensor_sum_nif_test(Type, Rounds, Performance) ->
      DimX = rand:uniform(?DIMX_RAND_MAX),
      DimY = rand:uniform(?DIMY_RAND_MAX),
      TensorA = generate_nerltensor(Type, DimX, DimY, 1),
      TensorB = generate_nerltensor(Type, DimX, DimY, 1),
      Expected = nerlTensor:nerltensor_sum_erl({TensorA, erl_float}, {TensorB, erl_float}),
      Tic = nerl:tic(),
      {EncA, _} = encode_nif(TensorA, Type),
      {EncB, _} = encode_nif(TensorB, Type),
      {EncRes, SumType} = nerltensor_sum_nif(EncA, EncB, Type),
      {Elapsed, _} = nerl:toc(Tic),
      PerformanceNew = Performance + Elapsed,
      {DecodedRes, erl_float} = nerltensor_conversion({EncRes, SumType}, erl_float),
      ensure_tensor_match(Expected, DecodedRes, float),
      nerltensor_sum_nif_test(Type, Rounds - 1, PerformanceNew).

nerltensor_scalar_multiplication_test(_Type, 0, Performance) -> Performance;
nerltensor_scalar_multiplication_test(Type, Rounds, Performance) ->
      Scalar = rand:uniform() * 10,
      Tensor = generate_nerltensor_rand_dims(Type),
      Expected = nerltensor_scalar_multiplication_erl({Tensor, erl_float}, Scalar),
      Tic = nerl:tic(),
      {Encoded, _} = encode_nif(Tensor, Type),
      {ResultBin, ResultType} = nerltensor_scalar_multiplication_nif(Encoded, Type, Scalar),
      {Elapsed, _} = nerl:toc(Tic),
      PerformanceNew = Performance + Elapsed,
      {Decoded, erl_float} = nerltensor_conversion({ResultBin, ResultType}, erl_float),
      ensure_tensor_match(Expected, Decoded, float),
      nerltensor_scalar_multiplication_test(Type, Rounds - 1, PerformanceNew).

nerlTorch_worker_smoke() ->
      NeuralNetworkTestingModelList = ?NEURAL_NETWORK_TESTING_MODELS_LIST,
      lists:foreach(fun setup_and_teardown_worker/1, NeuralNetworkTestingModelList),
      ok.

setup_and_teardown_worker({ModelId,ModelType,ModelArgs,LayersSizes, LayersTypes, _LayersFunctionalityCodes,
                           LearningRate, Epochs, OptimizerType, OptimizerArgs,
                           LossMethod, DistributedSystemType, DistributedSystemArg}) ->
      DistributedArgs = DistributedSystemArg,
      ResolvedModelArgs = resolve_model_args(ModelArgs),
      TrainParams = build_train_params(ResolvedModelArgs, LearningRate, Epochs, OptimizerType, LossMethod, OptimizerArgs, LayersSizes, LayersTypes),
      ensure_train_params_cover_required_keys(ModelId, TrainParams),
      ok = ensure_worker_created(ModelId, DistributedSystemType, DistributedArgs, TrainParams),
      exercise_worker_train_predict(ModelId, ModelType, LayersSizes, LayersTypes),
      _ = remove_nerlworker_nif(ModelId),
      ok.

ensure_worker_created(ModelId, DistributedSystemType, DistributedSystemArgs, TrainParams) ->
      try
            test_nerlworker_nif(ModelId, DistributedSystemType, DistributedSystemArgs, TrainParams),
            ok
      catch
            Class:Reason:Stack -> erlang:error({torch_worker_setup_failed, Class, Reason, Stack})
      end.

exercise_worker_train_predict(ModelId, ModelType, LayersSizes, LayersTypes) ->
      {TrainBatch, PredictBatch} = prepare_worker_batches(ModelType, LayersSizes, LayersTypes),
      TrainBatches = lists:duplicate(?TRAIN_PHASE_ROUNDS, TrainBatch),
      PredictBatches = lists:duplicate(?PREDICT_PHASE_ROUNDS, PredictBatch),
      run_train_cycles(ModelId, TrainBatches, 1),
      run_predict_cycles(ModelId, PredictBatches, 1).

run_train_cycles(_ModelId, [], _) -> ok;
run_train_cycles(ModelId, [{BatchBinary, BatchType} | Rest], CycleIdx) ->
      ok = train_nif(ModelId, BatchBinary, BatchType),
      await_nerlnif_reply(train),
      nerltest_print(nerl:string_format("Train cycle #~p completed", [CycleIdx])),
      run_train_cycles(ModelId, Rest, CycleIdx + 1).

run_predict_cycles(_ModelId, [], _) -> ok;
run_predict_cycles(ModelId, [{BatchBinary, BatchType} | Rest], CycleIdx) ->
      ok = predict_nif(ModelId, BatchBinary, BatchType),
      await_nerlnif_reply(predict),
      nerltest_print(nerl:string_format("Predict cycle #~p completed", [CycleIdx])),
      run_predict_cycles(ModelId, Rest, CycleIdx + 1).

build_train_params(ModelPath, LearningRate, Epochs, OptimizerType, LossMethod, OptimizerArgs, LayersSizes, LayersTypes) ->
      OptimizerName = optimizer_code_to_name(OptimizerType, OptimizerArgs),
      {InputShape, LabelShape} = derive_shape_metadata(LayersSizes, LayersTypes),
      InputShapeStr = shape_list_to_string(InputShape),
      LabelShapeStr = shape_list_to_string(LabelShape),
      #{
            "model_path" => ModelPath,
            "model_format" => "torchscript",
            "model_checksum" => "",
            "model_description" => "torch_test_model",
            "lr" => LearningRate,
            "learning_rate" => LearningRate,
            "epochs" => Epochs,
            "optimizer" => OptimizerName,
            "optim" => OptimizerName,
            "loss" => LossMethod,
            "input_tensor_shape" => InputShapeStr,
            "labels_shape" => LabelShapeStr,
            "labels_offset" => "default"
       }.

nerlTorch_training_params_integrity_test() ->
      SampleParams = build_train_params("/tmp/model.pt", "0.1", "10", "2", "mse", undefined, "5,3", "1,3"),
      ensure_train_params_cover_required_keys(sample_model_id, SampleParams),
      nerltest_print("Verified Torch training parameter coverage").

nerlTorch_worker_creation_matrix_test() ->
      BaseParams = default_train_params(),
      Cases = [
            #{label => valid_model_path, params => BaseParams, expect => ok},
            #{label => missing_model_file, params => BaseParams#{"model_path" := "/tmp/nerlnet/does_not_exist.pt"}, expect => ok},
            #{label => alt_optimizer_alias, params => BaseParams#{"optimizer" := "AdamW", "optim" := "AdamW"}, expect => ok},
            #{label => blank_metadata, params => BaseParams#{"model_description" := ""}, expect => ok},
            #{label => invalid_train_params_term, params => ['not', a, map], expect => badarg}
      ],
      lists:foreach(fun run_worker_creation_case/1, Cases),
      nerltest_print("Torch worker creation matrix completed").

nerlTorch_dual_worker_lifecycle_test() ->
      [ModelA, ModelB | _] = ?NEURAL_NETWORK_TESTING_MODELS_LIST,
      Parent = self(),
      Pids = [spawn(fun() ->
                        run_worker_lifecycle(Model, Parent)
                  end) || Model <- [ModelA, ModelB]],
      await_worker_lifecycle(Pids),
      nerltest_print("Dual worker lifecycle completed").

nerlTorch_concurrent_train_predict_test() ->
      [ModelSpec | _] = ?NEURAL_NETWORK_TESTING_MODELS_LIST,
      {ModelId,ModelType,ModelArgs,LayersSizes, LayersTypes, _
            , LearningRate, Epochs, OptimizerType, OptimizerArgs,
            LossMethod, DistributedSystemType, DistributedSystemArg} = ModelSpec,
      ResolvedModelArgs = resolve_model_args(ModelArgs),
      TrainParams = build_train_params(ResolvedModelArgs, LearningRate, Epochs, OptimizerType, LossMethod, OptimizerArgs, LayersSizes, LayersTypes),
      ensure_train_params_cover_required_keys(ModelId, TrainParams),
      ok = ensure_worker_created(ModelId, DistributedSystemType, DistributedSystemArg, TrainParams),
      {TrainBatch, PredictBatch} = prepare_worker_batches(ModelType, LayersSizes, LayersTypes),
      launch_async_train_predict(ModelId, TrainBatch, PredictBatch),
      _ = remove_nerlworker_nif(ModelId),
      nerltest_print("Concurrent train/predict test completed").

default_train_params() ->
      ResolvedModelPath = resolve_model_args(?TORCH_TEST_MODEL_PERCEPTRON_RELATIVE_PATH),
      build_train_params(ResolvedModelPath, "0.01", "5", "2", "2", "", "5,30,5,3", "1,3,3,3").

run_worker_creation_case(#{label := Label, params := Params, expect := Expect}) ->
      ModelId = erlang:unique_integer([positive]),
      DistType = "0",
      DistArgs = "",
      Result = catch test_nerlworker_nif(ModelId, DistType, DistArgs, Params),
      case {Expect, Result} of
            {ok, ok} ->
                  _ = remove_nerlworker_nif(ModelId),
                  ok;
            {badarg, {'EXIT', {badarg, _}}} -> ok;
            {_, {'EXIT', Reason}} -> erlang:error({worker_creation_failed, Label, Reason});
            {Expected, Actual} -> erlang:error({unexpected_worker_creation_result, Label, Expected, Actual})
      end.

run_worker_lifecycle(Model, Parent) ->
      try
            setup_and_teardown_worker(Model),
            Parent ! {worker_lifecycle_done, self(), ok}
      catch
            Class:Reason:Stack -> Parent ! {worker_lifecycle_done, self(), {error, {Class, Reason, Stack}}}
      end.

derive_shape_metadata(LayersSizes, LayersTypes) ->
      [FirstLayerSize | LayerSizesList] = re:split(LayersSizes, ",", [{return, list}]),
      [LastLayerSize | _] = lists:reverse(LayerSizesList),
      [FirstLayerType | _] = re:split(LayersTypes, ",", [{return, list}]),
      BatchSize = ?NERLWORKER_TEST_NUM_SAMPLES,
      case FirstLayerType of
            ?LAYERS_TYPE_CONV_IDX ->
                  [DimXComplex, DimYComplex, DimZComplex | _] = re:split(FirstLayerSize, "x", [{return, list}]),
                  {DimXComplexInt, _} = string:to_integer(DimXComplex),
                  {DimYComplexInt, _} = string:to_integer(DimYComplex),
                  {DimZComplexInt, _} = string:to_integer(DimZComplex),
                  {LastLayerSizeInt, _} = string:to_integer(LastLayerSize),
                  InputFeatures = DimXComplexInt * DimYComplexInt * DimZComplexInt,
                  TotalColumns = InputFeatures + LastLayerSizeInt,
                  {[BatchSize, TotalColumns], [BatchSize, LastLayerSizeInt]};
            _ ->
                  {FirstLayerSizeInt, _} = string:to_integer(FirstLayerSize),
                  {LastLayerSizeInt, _} = string:to_integer(LastLayerSize),
                  TotalColumns = FirstLayerSizeInt + LastLayerSizeInt,
                  {[BatchSize, TotalColumns], [BatchSize, LastLayerSizeInt]}
      end.

shape_list_to_string(ShapeList) ->
      Strings = [integer_to_list(Dim) || Dim <- ShapeList],
      "[" ++ string:join(Strings, ",") ++ "]".

await_worker_lifecycle([]) -> ok;
await_worker_lifecycle(Pending) ->
      receive
            {worker_lifecycle_done, Pid, ok} ->
                  await_worker_lifecycle(lists:delete(Pid, Pending));
            {worker_lifecycle_done, Pid, Error} -> erlang:error({worker_lifecycle_failed, Pid, Error})
      after ?NIF_REPLY_TIMEOUT_MS -> erlang:error({worker_lifecycle_timeout, Pending})
      end.

launch_async_train_predict(ModelId, {TrainBinary, TrainType}, {PredictBinary, PredictType}) ->
      Parent = self(),
      Launch = fun(Label, Fun) ->
            spawn(fun() ->
                  Result = catch Fun(),
                  case Result of
                        ok -> Parent ! {async_done, Label, ok};
                        {'EXIT', Reason} -> Parent ! {async_done, Label, {error, Reason}};
                        Other -> Parent ! {async_done, Label, {unexpected, Other}}
                  end
            end)
      end,
      _TrainPid = Launch(train, fun() ->
            ok = train_nif(ModelId, TrainBinary, TrainType),
            await_nerlnif_reply(train)
      end),
      _PredictPid = Launch(predict, fun() ->
            ok = predict_nif(ModelId, PredictBinary, PredictType),
            await_nerlnif_reply(predict)
      end),
      await_async_results(2).

await_async_results(0) -> ok;
await_async_results(Remaining) ->
      receive
            {async_done, _Label, ok} -> await_async_results(Remaining - 1);
            {async_done, Label, Error} -> erlang:error({async_task_failed, Label, Error})
      after ?NIF_REPLY_TIMEOUT_MS -> erlang:error({async_tasks_timeout, Remaining})
      end.

ensure_train_params_cover_required_keys(ModelId, TrainParams) ->
      ensure_mandatory_groups(ModelId, TrainParams, ?TORCH_TRAIN_PARAM_MANDATORY_GROUPS),
      ensure_expected_keys_present(ModelId, TrainParams, ?TORCH_TRAIN_PARAM_EXPECTED_KEYS).

ensure_mandatory_groups(_ModelId, _TrainParams, []) -> ok;
ensure_mandatory_groups(ModelId, TrainParams, [Group | Rest]) ->
      #{label := Label, keys := Keys, allow_blank := AllowBlank} = Group,
      case find_first_present_key(Keys, TrainParams) of
            {ok, Key} ->
                  Value = maps:get(Key, TrainParams),
                  case AllowBlank of
                        true -> ok;
                        false ->
                              case is_invalid_train_param_value(Value) of
                                    true -> erlang:error({torch_train_params_invalid_values, ModelId, [Key]});
                                    false -> ok
                              end
                  end;
            error -> erlang:error({torch_train_params_missing_keys, ModelId, [Label]})
      end,
      ensure_mandatory_groups(ModelId, TrainParams, Rest).

ensure_expected_keys_present(_ModelId, _TrainParams, []) -> ok;
ensure_expected_keys_present(ModelId, TrainParams, [Key | Rest]) ->
      case maps:is_key(Key, TrainParams) of
            true -> ensure_expected_keys_present(ModelId, TrainParams, Rest);
            false -> erlang:error({torch_train_params_missing_keys, ModelId, [Key]})
      end.

find_first_present_key([], _TrainParams) -> error;
find_first_present_key([Key | Rest], TrainParams) ->
      case maps:is_key(Key, TrainParams) of
            true -> {ok, Key};
            false -> find_first_present_key(Rest, TrainParams)
      end.

is_invalid_train_param_value(Value) when Value =:= undefined; Value =:= <<>> -> true;
is_invalid_train_param_value(Value) when is_list(Value) -> length(string:trim(Value)) =:= 0;
is_invalid_train_param_value(Value) when is_binary(Value) ->
      Trimmed = string:trim(binary_to_list(Value)),
      length(Trimmed) =:= 0;
is_invalid_train_param_value(_) -> false.

optimizer_code_to_name("5", _) -> "adam";
optimizer_code_to_name("2", _) -> "sgd";
optimizer_code_to_name(_, Args) ->
      case string:lowercase(normalize_optimizer_arg(Args)) of
            "adam" -> "adam";
            _ -> "sgd"
      end.

normalize_optimizer_arg(undefined) -> "";
normalize_optimizer_arg(Args) when is_list(Args) -> Args;
normalize_optimizer_arg(Args) when is_binary(Args) -> binary_to_list(Args);
normalize_optimizer_arg(Args) when is_atom(Args) -> atom_to_list(Args);
normalize_optimizer_arg(Other) -> lists:flatten(io_lib:format("~p", [Other])).

await_nerlnif_reply(Action) ->
      receive
            {nerlnif, error, Reason} -> erlang:error({Action, Reason});
            {nerlnif, _Payload, _PayloadType, _Time} -> ok;
            {nerlnif, _Payload, _Time} -> ok
      after ?NIF_REPLY_TIMEOUT_MS -> erlang:error({Action, timeout})
      end.

random_pick_binary_type()->
      Types = get_all_binary_types(),
      RandomIndex = rand:uniform(length(Types)),
      lists:nth(RandomIndex, Types).

generate_nerltensor_rand_dims(Type)->
      DimX = rand:uniform(?DIMX_RAND_MAX),
      DimY = rand:uniform(?DIMY_RAND_MAX),
      DimZ = 1,
      generate_nerltensor(Type,DimX,DimY,DimZ).

generate_nerltensor(BinType,DimX,DimY,DimZ) ->
      DataLength = DimX * DimY * DimZ,
      IsInt = lists:member(BinType , ?LIST_BINARY_INT_NERLTENSOR_TYPE),
      IsFloat = lists:member(BinType , ?LIST_BINARY_FLOAT_NERLTENSOR_TYPE),
      if
            IsInt -> Data = [rand:uniform(255) || _ <- lists:seq(1, DataLength)],
                      [DimX,DimY,DimZ] ++ Data;
            IsFloat ->
                      DimXf = float(DimX),
                      DimYf = float(DimY),
                      DimZf = float(DimZ),
                      Data = [rand:uniform() * 10 || _ <- lists:seq(1, DataLength)],
                      [DimXf,DimYf,DimZf] ++ Data;
            true -> wrong_type
      end.

ensure_tensor_match(Expected, Actual, Type) ->
      FloatType = (Type == float) orelse (Type == double),
      case FloatType of
            true ->
                  case compare_floats_L(Expected, Actual, 4) of
                        true -> ok;
                        false -> erlang:error({tensor_mismatch, Expected, Actual})
                  end;
            false ->
                  case Expected == Actual of
                        true -> ok;
                        false -> erlang:error({tensor_mismatch, Expected, Actual})
                  end
      end.

prepare_worker_batches(ModelType, LayersSizes, LayersTypes) ->
      {DataBinary, DataType, ErlTensor, ErlType, NumOfFeatures, _NumOfLabels} =
            generate_worker_dataset(LayersSizes, LayersTypes, ?NERLWORKER_TEST_NUM_SAMPLES),
      case is_autoencoder_type(ModelType) of
            true ->
                  {FeatureTensor, _} = split_cols_erl_tensor(ErlTensor, ErlType, NumOfFeatures),
                  {FeatureBinary, FeatureType} = nerltensor_conversion({FeatureTensor, erl_float}, float),
                  {{FeatureBinary, FeatureType}, {FeatureBinary, FeatureType}};
            false ->
                  {PredictTensor, _} = split_cols_erl_tensor(ErlTensor, ErlType, NumOfFeatures),
                  {PredictBinary, PredictType} = nerltensor_conversion({PredictTensor, erl_float}, float),
                  {{DataBinary, DataType}, {PredictBinary, PredictType}}
      end.

generate_worker_dataset(LayersSizes, LayersTypes, NumOfSamples) ->
      [FirstLayerSize | LayerSizesList] = re:split(LayersSizes, ",", [{return, list}]),
      [LastLayerSize | _] = lists:reverse(LayerSizesList),
      [FirstLayerType | _] = re:split(LayersTypes, ",", [{return, list}]),
      {DimX, DimY, DimZ} = dims_from_layer(FirstLayerType, FirstLayerSize, LastLayerSize, NumOfSamples),
      ErlDataTensor = generate_nerltensor(float, DimX, DimY, DimZ),
      {NumOfLabels, _} = string:to_integer(LastLayerSize),
      NumOfFeatures = DimY - NumOfLabels,
      {BinaryTensor, BinaryType} = nerltensor_conversion({ErlDataTensor, erl_float}, float),
      {BinaryTensor, BinaryType, ErlDataTensor, erl_float, NumOfFeatures, NumOfLabels}.

dims_from_layer(?LAYERS_TYPE_DEFAULT_IDX, FirstLayerSize, LastLayerSize, NumOfSamples) ->
      {FirstLayerSizeInt, _} = string:to_integer(FirstLayerSize),
      {LastLayerSizeInt, _} = string:to_integer(LastLayerSize),
      {NumOfSamples, LastLayerSizeInt + FirstLayerSizeInt, 1};
dims_from_layer(?LAYERS_TYPE_SCALING_IDX, FirstLayerSize, LastLayerSize, NumOfSamples) ->
      {FirstLayerSizeInt, _} = string:to_integer(FirstLayerSize),
      {LastLayerSizeInt, _} = string:to_integer(LastLayerSize),
      {NumOfSamples, LastLayerSizeInt + FirstLayerSizeInt, 1};
dims_from_layer(?LAYERS_TYPE_CONV_IDX, FirstLayerSize, LastLayerSize, NumOfSamples) ->
      [DimXComplex, DimYComplex, DimZComplex | _] = re:split(FirstLayerSize, "x", [{return, list}]),
      {DimXComplexInt, _} = string:to_integer(DimXComplex),
      {DimYComplexInt, _} = string:to_integer(DimYComplex),
      {DimZComplexInt, _} = string:to_integer(DimZComplex),
      {LastLayerSizeInt, _} = string:to_integer(LastLayerSize),
      {NumOfSamples, DimXComplexInt * DimYComplexInt * DimZComplexInt + LastLayerSizeInt, 1};
dims_from_layer(?LAYERS_TYPE_PERCEPTRON_IDX, FirstLayerSize, LastLayerSize, NumOfSamples) ->
      {FirstLayerSizeInt, _} = string:to_integer(FirstLayerSize),
      {LastLayerSizeInt, _} = string:to_integer(LastLayerSize),
      {NumOfSamples, LastLayerSizeInt + FirstLayerSizeInt, 1};
dims_from_layer(?LAYERS_TYPE_POOLING_IDX, FirstLayerSize, LastLayerSize, NumOfSamples) ->
      {FirstLayerSizeInt, _} = string:to_integer(FirstLayerSize),
      {LastLayerSizeInt, _} = string:to_integer(LastLayerSize),
      {NumOfSamples, LastLayerSizeInt + FirstLayerSizeInt, 1};
dims_from_layer(?LAYERS_TYPE_PROBABILISTIC_IDX, FirstLayerSize, LastLayerSize, NumOfSamples) ->
      {FirstLayerSizeInt, _} = string:to_integer(FirstLayerSize),
      {LastLayerSizeInt, _} = string:to_integer(LastLayerSize),
      {NumOfSamples, LastLayerSizeInt + FirstLayerSizeInt, 1};
dims_from_layer(?LAYERS_TYPE_UNSCALING_IDX, FirstLayerSize, LastLayerSize, NumOfSamples) ->
      {FirstLayerSizeInt, _} = string:to_integer(FirstLayerSize),
      {LastLayerSizeInt, _} = string:to_integer(LastLayerSize),
      {NumOfSamples, LastLayerSizeInt + FirstLayerSizeInt, 1};
dims_from_layer(_OtherType, _FirstLayerSize, _LastLayerSize, _NumOfSamples) ->
      {1, 1, 1}.

is_autoencoder_type(ModelType) ->
      lists:member(ModelType, [?MODEL_TYPE_AUTOENCODER_IDX, ?MODEL_TYPE_AE_CLASSIFIER_IDX]).

resolve_model_args(ModelArgs) when is_list(ModelArgs) ->
      Candidates = candidate_model_paths(ModelArgs),
      case lists:dropwhile(fun(Path) -> not filelib:is_regular(Path) end, Candidates) of
            [Existing | _] -> Existing;
            [] -> ModelArgs
      end.

candidate_model_paths(ModelArgs) ->
      Pathtype = filename:pathtype(ModelArgs),
      RootCandidates = candidate_roots(),
      RelativeCandidates =
            case Pathtype of
                  absolute -> [ModelArgs];
                  _ -> [filename:join(Root, ModelArgs) || Root <- RootCandidates]
            end,
      DefaultCandidates =
            lists:flatmap(
              fun(Path) ->
                      case filename:pathtype(Path) of
                            absolute -> [Path];
                            _ -> []
                      end
              end,
              [
                    ?TORCH_TEST_MODEL_PERCEPTRON_DEFAULT_PATH,
                    ?TORCH_TEST_MODEL_AUTOENCODER_DEFAULT_PATH,
                    ?TORCH_TEST_MODEL_CNN_DEFAULT_PATH
              ]
            ),
      lists:usort(RelativeCandidates ++ DefaultCandidates).

candidate_roots() ->
      EnvPath = case os:getenv("NERLNET_PATH") of
                      false -> [];
                      Value -> [Value]
                end,
      lists:usort(EnvPath ++ [?NERLNET_PATH]).