-module(nerlTests).
-author("David Leon").
-include("nerlTensor.hrl").

-compile(nerlNIF).
-export([run_tests/0]).

-import(nerlNIF,[init/0,create_nif/6,train_nif/5,call_to_train/6,predict_nif/2,call_to_predict/5,get_weights_nif/1,printTensor/2]).
-import(nerlNIF,[call_to_get_weights/1,call_to_set_weights/2]).
-import(nerlNIF,[decode_nif/2, nerltensor_binary_decode/2]).
-import(nerlNIF,[encode_nif/2, nerltensor_encode/5, nerltensor_conversion/2, get_all_binary_types/0, get_all_nerltensor_list_types/0]).
-import(nerlNIF,[nerltensor_sum_nif/3, nerltensor_sum_erl/2]).
-import(nerlNIF,[nerltensor_scalar_multiplication_nif/3, nerltensor_scalar_multiplication_erl/2, sum_nerltensors_lists/2]).
-import(nerl,[compare_floats_L/3, string_format/2, logger_settings/1]).

-define(NERLTEST_PRINT_STR, "[NERLTEST] ").

nerltest_print(String) ->
      logger:notice(?NERLTEST_PRINT_STR++String).

% encode_decode test macros
-define(DIMX_RAND_MAX, 200).
-define(DIMY_RAND_MAX, 200).
-define(SUM_NIF_ROUNDS, 50).
-define(ENCODE_DECODE_ROUNDS, 50).
-define(NERLTENSOR_CONVERSION_ROUNDS, 50).
-define(NERLTENSOR_SCALAR_MULTIPLICATION_ROUNDS, 50).
-define(NERLTENSORS_SUM_LIST_MAX_SIZE, 50).
-define(NERLTESNORS_SUM_LIST_ROUNDS, 30).

test_envelope(Func, TestName, Rounds) ->
      nerltest_print(nerl:string_format("~p test starts for ~p rounds",[TestName, Rounds])),
      {TimeTookMicro, _RetVal} = timer:tc(Func, [Rounds]),
      nerltest_print(nerl:string_format("Elapsed: ~p~p",[TimeTookMicro / 1000, ms])), ok.

test_envelope_nif_performance(Func, TestName, Rounds) ->
      nerltest_print(nerl:string_format("~p test starts for ~p rounds",[TestName, Rounds])),
      {TimeTookMicro, AvgPerformance} = timer:tc(Func, [Rounds]),
      nerltest_print(nerl:string_format("Elapsed: ~p~p Average nif performance: ~.3f~p",[TimeTookMicro/1000,ms, AvgPerformance/Rounds, ms])), ok.

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

generate_nerltensor(Type,DimX,DimY,DimZ) -> 
      DataLength = DimX * DimY * DimZ,
      if  
            (Type == int32) or (Type == int16) -> Data = [rand:uniform(255) || _ <- lists:seq(1, DataLength)],
                        [DimX,DimY,DimZ] ++ Data;
            (Type == double) or (Type == float) -> DimXf = float(DimX),
                        DimYf = float(DimY),
                        DimZf = float(DimZ),
                        Data = [rand:uniform() * 10 || _ <- lists:seq(1, DataLength)],
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
            true -> throw(ner:string_format("test failed - not equal ~n ExpectedResult: ~p ~n NerlTensorEncResDec: ~p",[ExpectedResult, NerlTensorEncResDec]))
      end.


nerltensor_sum_nif_test(_Type,0, Performance) -> Performance;
nerltensor_sum_nif_test(Type, N, Performance) ->
      DimX = rand:uniform(?DIMX_RAND_MAX),
      DIMY = rand:uniform(?DIMX_RAND_MAX),
      NewTensorA =  generate_nerltensor(Type,DimX,DIMY,1),
      NewTensorB =  generate_nerltensor(Type,DimX,DIMY,1),
    %  io:format("NewTensorA ~p~n NewTensorB ~p~n",[NewTensorA, NewTensorB]),
      ExpectedResult = nerlNIF:nerltensor_sum_erl({NewTensorA, erl_float}, {NewTensorB, erl_float}), %TODO add tensor addition element wise
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
            true -> throw(ner:string_format("test failed - not equal ~n ExpectedResult: ~p ~n ResultTensorCEncDec: ~p",[ExpectedResult, ResultTensorCEncDec]))
      end.

% only erl_float types are supported
sum_nerltensors_lists_test(_Type, 0, Performance) -> Performance;
sum_nerltensors_lists_test(Type, N, Performance) -> 
      DimX = rand:uniform(?DIMX_RAND_MAX),
      DIMY = rand:uniform(?DIMX_RAND_MAX),
      Elements = rand:uniform(?NERLTENSORS_SUM_LIST_MAX_SIZE),
      NerlTensors =  [generate_nerltensor(Type,DimX,DIMY,1) || _X <- lists:seq(1, Elements)],
     % io:format("NerlTensors ~p~n", [NerlTensors]),
      NerlTensorsEencoded = [element(1, nerlNIF:encode_nif(NerlTensor, Type)) || NerlTensor <- NerlTensors],
      [ExpectedSumResult] = nerlNIF:sum_nerltensors_lists_erl(NerlTensors, erl_float),
      %io:format("NerlTensorsEencoded ~p~n",[NerlTensorsEencoded]),
      %io:format("ExpectedSumResult ~p~n",[ExpectedSumResult]),
      Tic = nerl:tic(),
      [ResultSumEncoded] = nerlNIF:sum_nerltensors_lists(NerlTensorsEencoded, Type),
      {TocRes, _} = nerl:toc(Tic),
      PerformanceNew = TocRes + Performance,
      %io:format("ResultSumEncoded ~p~n",[ResultSumEncoded]),
      {ResultSumEncodedDecoded, erl_float} = nerlNIF:nerltensor_conversion({ResultSumEncoded, Type}, erl_float),
     % io:format("ResultSumEncodedDecoded ~p~n",[ResultSumEncodedDecoded]),
      CompareFloats = nerl:compare_floats_L(ResultSumEncodedDecoded, ExpectedSumResult, 4), % Erlang accuracy is double
      if 
            CompareFloats -> sum_nerltensors_lists_test(Type, N-1, PerformanceNew);
            true -> throw(ner:string_format("test failed - not equal ~n ExpectedResult: ~p ~n ResultTensorCEncDec: ~p",[ExpectedSumResult, ResultSumEncodedDecoded]))
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