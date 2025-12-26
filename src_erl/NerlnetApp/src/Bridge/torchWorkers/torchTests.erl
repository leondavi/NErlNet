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

-export([run_tests/0]).

-import(nerlTorchNIF,[encode_nif/2, decode_nif/2, nerltensor_conversion/2, get_all_binary_types/0,
                      nerltensor_sum_nif/3, nerltensor_scalar_multiplication_nif/3, nerltensor_scalar_multiplication_erl/2,
                      test_nerlworker_nif/14, remove_nerlworker_nif/1, nif_preload/0,
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

      WorkerTest = fun(_Rounds) -> nerlTorch_worker_smoke() end,
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

setup_and_teardown_worker({ModelId,ModelType,ModelArgs,LayersSizes, LayersTypes, LayersFunctionalityCodes,
                           LearningRate, Epochs, OptimizerType, OptimizerArgs,
                           LossMethod, DistributedSystemType, DistributedSystemArg}) ->
      LossArgs = "",
      DistributedArgs = DistributedSystemArg,
      ResolvedModelArgs = resolve_model_args(ModelArgs),
      ok = ensure_worker_created(ModelId, ModelType, ResolvedModelArgs, LayersSizes, LayersTypes, LayersFunctionalityCodes,
                                 LearningRate, Epochs, OptimizerType, OptimizerArgs, LossMethod,
                                 LossArgs, DistributedSystemType, DistributedArgs),
      exercise_worker_train_predict(ModelId, ModelType, LayersSizes, LayersTypes),
      _ = remove_nerlworker_nif(ModelId),
      ok.

ensure_worker_created(ModelId, ModelType, ModelArgs, LayersSizes, LayersTypes, LayersFunctionalityCodes,
                      LearningRate, Epochs, OptimizerType, OptimizerArgs, LossMethod,
                      LossArgs, DistributedSystemType, DistributedSystemArgs) ->
      try
            test_nerlworker_nif(ModelId,ModelType,ModelArgs,LayersSizes, LayersTypes,
                                LayersFunctionalityCodes, LearningRate, Epochs, OptimizerType,
                                OptimizerArgs, LossMethod, LossArgs, DistributedSystemType, DistributedSystemArgs),
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