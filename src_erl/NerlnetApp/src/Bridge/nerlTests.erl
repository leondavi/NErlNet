-module(nerlTests).
-author("David Leon").
-include("nerlTensor.hrl").
-include("neural_networks_testing_models.hrl").
-include("layers_types_ag.hrl").
-include("models_types_ag.hrl").

-compile(nerlNIF).
-export([run_tests/0]).

-import(nerlNIF,[decode_nif/2, nerltensor_binary_decode/2]).
-import(nerlNIF,[encode_nif/2, nerltensor_encode/5, nerltensor_conversion/2, get_all_binary_types/0, get_all_nerltensor_list_types/0]).
-import(nerlNIF,[nerltensor_sum_nif/3]).
-import(nerlNIF,[test_nerlworker_nif/12, remove_nerlworker_nif/1]).
-import(nerlNIF,[nerltensor_scalar_multiplication_nif/3, nerltensor_scalar_multiplication_erl/2]).
-import(nerl,[compare_floats_L/3, string_format/2, logger_settings/1]).
-import(nerlTensor,[nerltensor_sum_erl/2, sum_nerltensors_lists/2]).

-define(NERLTEST_PRINT_STR, "[NERLTEST] ").

nerltest_print(String) ->
      logger:notice(?NERLTEST_PRINT_STR++String).

% encode_decode test macros
-define(DIMX_RAND_MAX, 13).
-define(DIMY_RAND_MAX, 13).
-define(SUM_NIF_ROUNDS, 50).
-define(ENCODE_DECODE_ROUNDS, 50).
-define(NERLTENSOR_CONVERSION_ROUNDS, 50).
-define(NERLTENSOR_SCALAR_MULTIPLICATION_ROUNDS, 50).
-define(NERLTENSORS_SUM_LIST_MAX_SIZE, 50).
-define(NERLTESNORS_SUM_LIST_ROUNDS, 30).
-define(NERLWORKER_TEST_ROUNDS, 1).

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
      nerl:logger_settings(nerlTests),

      EncodeDecodeTestFunc = fun(Rounds) -> Performance = 0, encode_decode_nifs_test(Rounds,[], Performance) end,
      EncodeDecodeTestName = "encode_decode_nifs",
      test_envelope_nif_performance(EncodeDecodeTestFunc, EncodeDecodeTestName, ?ENCODE_DECODE_ROUNDS ),

      SumNifTestFloatFunc = fun(Rounds) -> Performance = 0, nerltensor_sum_nif_test(float, Rounds, Performance) end,
      SumNifTestFloatTestName = "nerltensor_sum_nif float",
      test_envelope_nif_performance(SumNifTestFloatFunc, SumNifTestFloatTestName, ?SUM_NIF_ROUNDS ),

      SumNifTestDoubleFunc = fun(Rounds) -> Performance = 0, nerltensor_sum_nif_test(double, Rounds, Performance) end,
      SumNifTestDoubleTestName = "nerltensor_sum_nif double",
      test_envelope_nif_performance(SumNifTestDoubleFunc, SumNifTestDoubleTestName, ?SUM_NIF_ROUNDS ),

      ScalarMultiplicationNifFloatFunc = fun(Rounds) -> Performance = 0, nerltensor_scalar_multiplication_nif_test(float, Rounds, Performance) end,
      ScalarMultiplicationNifFloatTestName = "nerltensor_scalar_multiplication_nif float",
      test_envelope_nif_performance(ScalarMultiplicationNifFloatFunc, ScalarMultiplicationNifFloatTestName, ?NERLTENSOR_SCALAR_MULTIPLICATION_ROUNDS ),

      ScalarMultiplicationNifDoubleFunc = fun(Rounds) -> Performance = 0, nerltensor_scalar_multiplication_nif_test(double, Rounds, Performance) end,
      ScalarMultiplicationNifDoubleTestName = "nerltensor_scalar_multiplication_nif double",
      test_envelope_nif_performance(ScalarMultiplicationNifDoubleFunc, ScalarMultiplicationNifDoubleTestName, ?NERLTENSOR_SCALAR_MULTIPLICATION_ROUNDS ),

      ConversionTestFunc = fun(Rounds) -> nerltensor_conversion_test(Rounds) end,
      ConversionTestName = "nerltensor_conversion",
      test_envelope(ConversionTestFunc, ConversionTestName, ?NERLTENSOR_CONVERSION_ROUNDS ),

      SumNerlTensorsListFloatFunc = fun(Rounds) ->  Performance = 0, sum_nerltensors_lists_test(float, Rounds, Performance) end,
      SumNerlTensorsListFloatName = "sum_nerltensors_lists float",
      test_envelope_nif_performance(SumNerlTensorsListFloatFunc, SumNerlTensorsListFloatName, ?NERLTESNORS_SUM_LIST_ROUNDS ),

      SumNerlTensorsListDoubleFunc = fun(Rounds) ->  Performance = 0, sum_nerltensors_lists_test(double, Rounds, Performance) end,
      SumNerlTensorsListDoubleName = "sum_nerltensors_lists double",
      test_envelope_nif_performance(SumNerlTensorsListDoubleFunc, SumNerlTensorsListDoubleName, ?NERLTESNORS_SUM_LIST_ROUNDS ),

      NeuralNetworkTestingModelList = ?NEURAL_NETWORK_TESTING_MODELS_LIST,
      NerlworkerTestFunc = fun(_Rounds) ->  Performance = 0, nerlworker_test(NeuralNetworkTestingModelList, Performance) end, 
      NerlworkerTestName = "nerlworker_test",
      test_envelope_nif_performance(NerlworkerTestFunc, NerlworkerTestName, length(NeuralNetworkTestingModelList) ),

      nerltest_print("Tests Completed"),
      ok.

random_pick_nerltensor_type()->
      RandomIndex = rand:uniform(length(nerlNIF:get_all_binary_types())),
      lists:nth(RandomIndex, nerlNIF:get_all_binary_types()).


generate_nerltensor_rand_dims(Type)->
      DimX = rand:uniform(?DIMX_RAND_MAX),
      DimY = rand:uniform(?DIMY_RAND_MAX),
      DimZ = 1,
      generate_nerltensor(Type,DimX,DimY,DimZ).

generate_nerltensor(BinType,DimX,DimY,DimZ) -> 
      DataLength = DimX * DimY * DimZ,
      if  
            (BinType == int32) or (BinType == int16) -> Data = [rand:uniform(255) || _ <- lists:seq(1, DataLength)],
                        [DimX,DimY,DimZ] ++ Data;
            (BinType == double) or (BinType == float) -> 
                        DimXf = float(DimX),
                        DimYf = float(DimY),
                        DimZf = float(DimZ),
                        Data = [rand:uniform() * 10 || _ <- lists:seq(1, DataLength)], %% Where are the labels generated?
                        [DimXf,DimYf,DimZf] ++ Data;
            true -> wrong_type
      end.

nerltensor_scalar_multiplication_nif_test(_Type,0, Performance) -> Performance;
nerltensor_scalar_multiplication_nif_test(Type, N, Performance) ->
      ScalarRand = rand:uniform() * rand:uniform(100),
      NerlTensor = generate_nerltensor_rand_dims(Type),
      ExpectedResult = nerlNIF:nerltensor_scalar_multiplication_erl({NerlTensor, erl_float}, ScalarRand),
      Tic = nerl:tic(),
      {NerlTensorEnc, Type} = nerlNIF:nerltensor_conversion({NerlTensor, erl_float}, Type), % encode
      NerlTensorEncRes = nerlNIF:nerltensor_scalar_multiplication_nif(NerlTensorEnc,  Type, ScalarRand),
      {NerlTensorEncResDec, erl_float} = nerlNIF:nerltensor_conversion(NerlTensorEncRes, erl_float), % encode
      {TocRes, _} = nerl:toc(Tic),
      PerformanceNew = TocRes + Performance,
     % io:format("ExpectedResult: ~p~n",[ExpectedResult]),
     % io:format("NerlTensorEncResDec: ~p~n~n",[NerlTensorEncResDec]),
      CompareFloats = nerl:compare_floats_L(NerlTensorEncResDec, ExpectedResult, 3), % Erlang accuracy is double
      if 
            CompareFloats -> nerltensor_scalar_multiplication_nif_test(Type, N-1, PerformanceNew);
            true -> throw(nerl:string_format("test failed - not equal ~n ExpectedResult: ~p ~n NerlTensorEncResDec: ~p",[ExpectedResult, NerlTensorEncResDec]))
      end.


nerltensor_sum_nif_test(_Type,0, Performance) -> Performance;
nerltensor_sum_nif_test(Type, N, Performance) ->
      DimX = rand:uniform(?DIMX_RAND_MAX),
      DIMY = rand:uniform(?DIMX_RAND_MAX),
      NewTensorA =  generate_nerltensor(Type,DimX,DIMY,1),
      NewTensorB =  generate_nerltensor(Type,DimX,DIMY,1),
    %  io:format("NewTensorA ~p~n NewTensorB ~p~n",[NewTensorA, NewTensorB]),
      ExpectedResult = nerlTensor:nerltensor_sum_erl({NewTensorA, erl_float}, {NewTensorB, erl_float}), %TODO add tensor addition element wise
    %  io:format("ExpectedResult: ~p~n",[ExpectedResult]),
      Tic = nerl:tic(),
      {NewTensorAEnc, Type} = nerlNIF:encode_nif(NewTensorA, Type),
      {NewTensorBEnc, Type} = nerlNIF:encode_nif(NewTensorB, Type),
      %io:format("NewTensorAEnc ~p~n",[NewTensorAEnc]),
      {ResultTensorCEnc, Type} = nerlNIF:nerltensor_sum_nif(NewTensorAEnc, NewTensorBEnc, Type),
      {TocRes, _} = nerl:toc(Tic),
      PerformanceNew = TocRes + Performance,
    %  io:format("ResultTensorCEnc ~p Type ~p~n",[ResultTensorCEnc, Type]),

      {ResultTensorCEncDec, erl_float} = nerlNIF:nerltensor_conversion({ResultTensorCEnc, Type}, erl_float),
      CompareFloats = nerl:compare_floats_L(ResultTensorCEncDec, ExpectedResult, 4), % Erlang accuracy is double
      %  io:format("ResultTensorCEncDec ~p~n",[ResultTensorCEncDec]),
      if 
            CompareFloats -> nerltensor_sum_nif_test(Type, N-1, PerformanceNew);
            true -> throw(nerl:string_format("test failed - not equal ~n ExpectedResult: ~p ~n ResultTensorCEncDec: ~p",[ExpectedResult, ResultTensorCEncDec]))
      end.

% only erl_float types are supported
sum_nerltensors_lists_test(_Type, 0, Performance) -> Performance;
sum_nerltensors_lists_test(Type, N, Performance) -> 
      DimX = rand:uniform(?DIMX_RAND_MAX),
      DIMY = rand:uniform(?DIMX_RAND_MAX),
      Elements = rand:uniform(?NERLTENSORS_SUM_LIST_MAX_SIZE) + 1, % Min 2 elements
      NerlTensors =  [generate_nerltensor(Type,DimX,DIMY,1) || _X <- lists:seq(1, Elements)],
     % io:format("NerlTensors ~p~n", [NerlTensors]),
      NerlTensorsEencoded = [element(1, nerlNIF:encode_nif(NerlTensor, Type)) || NerlTensor <- NerlTensors],
      [ExpectedSumResult] = nerlTensor:sum_nerltensors_lists_erl(NerlTensors, erl_float),
      %io:format("NerlTensorsEencoded ~p~n",[NerlTensorsEencoded]),
      %io:format("ExpectedSumResult ~p~n",[ExpectedSumResult]),
      Tic = nerl:tic(),
      [ResultSumEncoded] = nerlTensor:sum_nerltensors_lists(NerlTensorsEencoded, Type),
      {TocRes, _} = nerl:toc(Tic),
      PerformanceNew = (TocRes / (Elements - 1)) + Performance, % average sum nif performance (dividing by # of sum ops)
      %io:format("ResultSumEncoded ~p~n",[ResultSumEncoded]),
      {ResultSumEncodedDecoded, erl_float} = nerlNIF:nerltensor_conversion({ResultSumEncoded, Type}, erl_float),
     % io:format("ResultSumEncodedDecoded ~p~n",[ResultSumEncodedDecoded]),
      CompareFloats = nerl:compare_floats_L(ResultSumEncodedDecoded, ExpectedSumResult, 4), % Erlang accuracy is double
      if 
            CompareFloats -> sum_nerltensors_lists_test(Type, N-1, PerformanceNew);
            true -> throw(nerl:string_format("test failed - not equal ~n ExpectedResult: ~p ~n ResultTensorCEncDec: ~p",[ExpectedSumResult, ResultSumEncodedDecoded]))
      end.



encode_decode_nifs_test(0, _Res, Performance) -> Performance ;
encode_decode_nifs_test(N, Res, Performance) ->
      EncodeType = random_pick_nerltensor_type(),
      NerlTensor = generate_nerltensor_rand_dims(EncodeType),
      Tic = nerl:tic(),
      {EncodedNerlTensor, NerlTensorType} = nerlNIF:encode_nif(NerlTensor, EncodeType),
      {DecodedTensor, DecodedType} = nerlNIF:decode_nif(EncodedNerlTensor, NerlTensorType),
      {TocRes, _} = nerl:toc(Tic),
      PerformanceNew = TocRes + Performance,
      %io:format("Encoded: ~p t ~p~n",[EncodedNerlTensor, NerlTensorType]),
      %io:format("NerlTensorOriginal: ~p ~n",[{NerlTensor, EncodeType}]),
      %io:format("NerlTensorEncDec: ~p ~n",[{DecodedTensor, DecodedType}]),
      FloatCase = EncodeType == float,
      CompareFloats = nerl:compare_floats_L(NerlTensor, DecodedTensor, 6),
      if
            FloatCase and CompareFloats-> encode_decode_nifs_test(N-1, Res ++ [], PerformanceNew);
            NerlTensor == DecodedTensor -> encode_decode_nifs_test(N-1, Res ++ [], PerformanceNew);
            true -> throw(nerl:string_format("test failed - not equal ~n Origin: ~p ~n EncDec: ~p",[{NerlTensor, EncodeType},{DecodedTensor, DecodedType}]))
      end.

nerltensor_conversion_test(0) -> ok;
nerltensor_conversion_test(Rounds) -> 
      BinType = random_pick_nerltensor_type(),
      BinFloatType = lists:member(BinType, ?LIST_BINARY_FLOAT_NERLTENSOR_TYPE),
      RandomIndex = rand:uniform(length(nerlNIF:get_all_nerltensor_list_types())),
      ErlType = lists:nth(RandomIndex, nerlNIF:get_all_nerltensor_list_types()),
      NerlTensorErl = generate_nerltensor_rand_dims(BinType),
      try  
            {NerlTensorEnc, _} =  nerlNIF:nerltensor_conversion({NerlTensorErl,ErlType},BinType),
            {NerlTensorEncDecErl, _ } = nerlNIF:nerltensor_conversion({NerlTensorEnc,BinType},ErlType),
            CompareFloats = nerl:compare_floats_L(NerlTensorErl, NerlTensorEncDecErl, 6),
            %  io:format("test failed - not equal ~n Origin: ~p ~n EncDec: ~p",[NerlTensorErl,NerlTensorEncDecErl]),
              if
                    BinFloatType and CompareFloats-> nerltensor_conversion_test(Rounds - 1);
                    NerlTensorErl == NerlTensorEncDecErl -> nerltensor_conversion_test(Rounds - 1);
                    true -> throw(nerl:string_format("test failed - not equal ~n Origin: ~p ~n EncDec: ~p",[NerlTensorErl,NerlTensorEncDecErl]))
              end
      catch
            throw:Reason -> BinTypeInt = lists:member(BinType, ?LIST_BINARY_INT_NERLTENSOR_TYPE),
                            case {ErlType,BinTypeInt,BinFloatType} of 
                                 {erl_int, false, true} -> nerltensor_conversion_test(Rounds - 1); % continues normal
                                 {erl_float, true, false} -> nerltensor_conversion_test(Rounds - 1); % continues normal
                                 _ -> throw(nerl:string_format("unknown nerltensor conversion exception Reason: ~p",[Reason]))
                            end 
      end.


nerlworker_test_generate_data(LayersSizes, LayerTypes, NumOfSamples) -> %% Ask David about where to split and if a 'if' statement is needed
      % extract first and last sizes
      % use module re to extract complex layer sizes
      [FirstLayerSize | LayerSizesList] = re:split(LayersSizes,",",[{return,list}]),
      [LastLayerSize|_] =  lists:reverse(LayerSizesList), 
      [FirstLayerType| _] = re:split(LayerTypes,",",[{return,list}]),
     %TODO simple layer types in inline function
      {DimX, DimY, DimZ} =
            case FirstLayerType of
                  ?LAYERS_TYPE_DEFAULT_IDX  -> {FirstLayerSizeInt,_} = string:to_integer(FirstLayerSize),
                                               {LastLayerSizeInt,_} = string:to_integer(LastLayerSize), 
                                               {NumOfSamples,LastLayerSizeInt+FirstLayerSizeInt, 1};
                  ?LAYERS_TYPE_SCALING_IDX -> {FirstLayerSizeInt,_} = string:to_integer(FirstLayerSize),
                                              {LastLayerSizeInt,_} = string:to_integer(LastLayerSize), 
                                              {NumOfSamples,LastLayerSizeInt+FirstLayerSizeInt, 1};
                  ?LAYERS_TYPE_CNN_IDX    ->  [DimXComplex, DimYComplex, DimZComplex | _] = re:split(FirstLayerSize,"x",[{return,list}]),
                                              {DimXComplexInt,_} = string:to_integer(DimXComplex),
                                              {DimYComplexInt,_} = string:to_integer(DimYComplex),
                                              {DimZComplexInt,_} = string:to_integer(DimZComplex),
                                              {LastLayerSizeInt,_} = string:to_integer(LastLayerSize),
                                              {NumOfSamples, DimXComplexInt*DimYComplexInt*DimZComplexInt+LastLayerSizeInt, 1};
                  ?LAYERS_TYPE_PERCEPTRON_IDX ->{FirstLayerSizeInt,_} = string:to_integer(FirstLayerSize),
                                                {LastLayerSizeInt,_} = string:to_integer(LastLayerSize), 
                                                {NumOfSamples,LastLayerSizeInt+FirstLayerSizeInt, 1};
                  ?LAYERS_TYPE_POOLING_IDX ->   {FirstLayerSizeInt,_} = string:to_integer(FirstLayerSize),
                                                {LastLayerSizeInt,_} = string:to_integer(LastLayerSize), 
                                                {NumOfSamples,LastLayerSizeInt+FirstLayerSizeInt, 1};
                  ?LAYERS_TYPE_PROBABILISTIC_IDX -> {FirstLayerSizeInt,_} = string:to_integer(FirstLayerSize),
                                                    {LastLayerSizeInt,_} = string:to_integer(LastLayerSize), 
                                                    {NumOfSamples,LastLayerSizeInt+FirstLayerSizeInt, 1};
                  ?LAYERS_TYPE_LSTM_IDX       -> {1, 1, 1};
                  ?LAYERS_TYPE_RECCURRENT_IDX -> {1, 1, 1};
                  ?LAYERS_TYPE_UNSCALING_IDX  -> {FirstLayerSizeInt,_} = string:to_integer(FirstLayerSize),
                                                 {LastLayerSizeInt,_} = string:to_integer(LastLayerSize), 
                                                 {NumOfSamples,LastLayerSizeInt+FirstLayerSizeInt, 1}
            end,
      ErlDataTensor = generate_nerltensor(float, DimX, DimY, DimZ),
      {NumOfFeatures ,_} = string:to_integer(FirstLayerSize),
      {NumOfLabels ,_} = string:to_integer(LastLayerSize),
      % io:format("ErlDataTensor of length ~p : ~p~n",[length(ErlDataTensor),ErlDataTensor]),
      %%{ErlDataTensor , float , NumOfFeatures , NumOfLabels},
      %% {SamplesFeatures , SamplesLabels} = nerlTensor:split_erl_tensor(ErlDataTensor , NumOfFeatures , float),
      %% io:format("Splitted SamplesFeatures (Of Length ~p) is ~p~n",[length(SamplesFeatures) , SamplesFeatures]),
      %% io:format("Splitted SamplesLabels (Of Length ~p) is ~p~n",[length(SamplesLabels) , SamplesLabels]).
      {NerlTensor , Type} = nerlNIF:nerltensor_conversion({ErlDataTensor,erl_float} , float),
      {NerlTensor , Type , ErlDataTensor , erl_float , NumOfFeatures , NumOfLabels}.



nerlworker_test([], _Performance) -> _Performance;
nerlworker_test([CurrentModel | Tail], Performance) -> 
     {ModelId,ModelType,LayersSizes, LayersTypes, LayersFunctionalityCodes,
      LearningRate, Epochs, OptimizerType, OptimizerArgs,
      LossMethod, DistributedSystemType, DistributedSystemArg} = CurrentModel,
      nerlNIF:test_nerlworker_nif(ModelId,ModelType,LayersSizes, LayersTypes, 
      LayersFunctionalityCodes, LearningRate, Epochs, OptimizerType, 
      OptimizerArgs, LossMethod, DistributedSystemType, DistributedSystemArg),
      NumOfSamples = 5,
      {NerlTensorDataBin , NerlTensorDataBinType , NerlTensorDataErl , NerlTensorDataErlType , NumOfFeatures , _NumOfLabels} = nerlworker_test_generate_data(LayersSizes, LayersTypes, NumOfSamples),
      io:format("After genrate GOT HERE~n"),
      if 
            (ModelType == ?MODEL_TYPE_AUTOENCODER_IDX) or (ModelType == ?MODEL_TYPE_AE_CLASSIFIER_IDX) -> %% AE or AEC
                  io:format("@AE: GOT HERE~n"),
                  {DataTensorErlFeatures , _DataTensorErlLabels} = nerlTensor:split_cols_erl_tensor(NerlTensorDataErl , NerlTensorDataErlType , NumOfFeatures), 
                  {NerlTensorDataBinTrain , _Type} = nerlNIF:nerltensor_conversion({DataTensorErlFeatures, erl_float}, float),
                  NerlTensorDataBinPredict = NerlTensorDataBinTrain;
      true -> 
            NerlTensorDataBinTrain = NerlTensorDataBin,
            {DataTensorErlPredictFeatures , DataTensorErlPredictLabels} = nerlTensor:split_cols_erl_tensor(NerlTensorDataErl , NerlTensorDataErlType , NumOfFeatures), 
            NerlTensorDataBinPredict = nerlNIF:nerltensor_conversion({DataTensorErlPredictFeatures, erl_float}, float),
            io:format("@true: GOT HERE~n")
      end,

      %% nerltest_print(nerl:string_format("ModelId : ~p  Type: ~p DataTensorEncodedDecoded:~p ",[ModelId,Type,DataTensorEncodedDecoded])),
      io:format("Before trainNIF GOT HERE~n"),
      nerlNIF:train_nif(ModelId , NerlTensorDataBinTrain , NerlTensorDataBinType), % ask Guy about receiver block
      io:format("After trainNIF GOT HERE 2~n"),
      receive 
            {nerlnif , _LossValue , _TrainTime} ->
                  io:format("Got LossValue~n")
      after 100000 -> throw("timeout")
      end,
      %block receive to get loss values from worker
      nerltest_print("after train_nif"),
      % TODO remove labels from generated data - ask David if we need to change "generate_tensor"
      % TODO Ori - implement predict
      nerlNIF:remove_nerlworker_nif(ModelId),
      nerlworker_test(Tail, Performance).



% % neural_network_sample_1() ->
% % {      ModelId  = erlang:unique_integer([positive]),
%       ModelType = "0",
%       LayersSizes = "128x128k3x3p1x1s2,64x64k3p1s2,1024,256,128,8,2",%"5,10,5,3",
%       LayersTypes = "2,2,3,3,3,3,3",
%       LayersFunctionalityCodes = "5,6,11,11,11,11,11", % change scaler functionality to 6 to check exception handling
%       LearningRate = "0.01",
%       Epochs = "1",
%       OptimizerType = "2",
%       OptimizerArgs = "",
%       LossMethod = "2",
%       DistributedSystemType = "0",
%       DistributedSystemArg = "",


