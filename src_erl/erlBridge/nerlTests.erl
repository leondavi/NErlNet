-module(nerlTests).
-author("David Leon").
-include("nerlTensor.hrl").

-compile(nerlNIF).
-export([run_tests/0]).

-import(nerlNIF,[init/0,create_nif/6,train_nif/5,call_to_train/6,predict_nif/2,call_to_predict/5,get_weights_nif/1,printTensor/2]).
-import(nerlNIF,[call_to_get_weights/1,call_to_set_weights/2]).
-import(nerlNIF,[decode_nif/2, nerltensor_binary_decode/2]).
-import(nerlNIF,[encode_nif/2, nerltensor_encode/5, nerltensor_conversion/2, get_all_binary_types/0, get_all_nerltensor_list_types/0]).
-import(nerlNIF,[nerltensor_sum_nif/3]).
-import(nerl,[compare_floats_L/3, string_format/2, logger_settings/1]).

-define(NERLTEST_PRINT_STR, "[NERLTEST] ").

nerltest_print(String) ->
      logger:notice(?NERLTEST_PRINT_STR++String).

% encode_decode test macros
-define(DIMX_RAND_MAX, 2).
-define(DIMY_RAND_MAX, 2).
-define(SUM_NIF_ROUNDS, 100).
-define(ENCODE_DECODE_ROUNDS, 100).
-define(NERLTENSOR_CONVERSION_ROUNDS, 50).

run_tests()->
      nerl:logger_settings(nerlTests),
      DimXStr = integer_to_list(?DIMX_RAND_MAX),
      DimYStr = integer_to_list(?DIMY_RAND_MAX),
      nerltest_print("encode decode test starts "++integer_to_list(?ENCODE_DECODE_ROUNDS)++" tests up to ("++DimXStr++","++DimYStr++")"),
      Tic_niftest_encode_decode = nerl:tic(),
      encode_decode_nifs_test(?ENCODE_DECODE_ROUNDS,[]), % throws if a test fails
      {TDiff_niftest_encode_decode, TimeUnit} = nerl:toc(Tic_niftest_encode_decode),
      nerltest_print(nerl:string_format("Elapsed: ~p~p",[TDiff_niftest_encode_decode,TimeUnit])),

      nerltest_print("nerltensor sum_nif float test starts "++integer_to_list(?SUM_NIF_ROUNDS)++" tests"),
      nerltest_print("Performance is measured for the following operations:"),
      nerltest_print("encode_nif x2 and nerltensor sum_nif"),

      Tic_nerltensor_sum_nif_test_float = nerl:tic(),
      Perfromance = 0,
      PerformanceSumNifFloat = nerltensor_sum_nif_test(float, ?SUM_NIF_ROUNDS, Perfromance) / ?SUM_NIF_ROUNDS, 
      {TDiff_nerltensor_sum_nif_test_float, _} = nerl:toc(Tic_nerltensor_sum_nif_test_float),
      nerltest_print(nerl:string_format("Elapsed: ~p~p, Avg nif operations: ~.4f~p",[TDiff_nerltensor_sum_nif_test_float,TimeUnit,PerformanceSumNifFloat,TimeUnit])),
      
      nerltest_print("nerltensor sum_nif double test starts "++integer_to_list(?SUM_NIF_ROUNDS)++" tests"),
      nerltest_print("Performance is measured for the following operations:"),
      nerltest_print("encode_nif x2 and nerltensor sum_nif"),
      Tic_nerltensor_sum_nif_test_double = nerl:tic(),
      Perfromance = 0,
      PerformanceSumNifDouble = nerltensor_sum_nif_test(double, ?SUM_NIF_ROUNDS, Perfromance) / ?SUM_NIF_ROUNDS, 
      {TDiff_nerltensor_sum_nif_test_double, _} = nerl:toc(Tic_nerltensor_sum_nif_test_double),
      nerltest_print(nerl:string_format("Elapsed: ~p~p, Avg nif operations: ~.4f~p",[TDiff_nerltensor_sum_nif_test_double,TimeUnit,PerformanceSumNifDouble,TimeUnit])),

      nerltest_print("nerltensor_conversion_test starts "++integer_to_list(?NERLTENSOR_CONVERSION_ROUNDS)++" tests"),
      nerltensor_conversion_test(?NERLTENSOR_CONVERSION_ROUNDS),

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


encode_decode_nifs_test(0, _Res) -> ok ;
encode_decode_nifs_test(N, Res) ->
      EncodeType = random_pick_nerltensor_type(),
      NerlTensor = generate_nerltensor_rand_dims(EncodeType),
      {EncodedNerlTensor, NerlTensorType} = nerlNIF:encode_nif(NerlTensor, EncodeType),
      {DecodedTensor, DecodedType} = nerlNIF:decode_nif(EncodedNerlTensor, NerlTensorType),
      %io:format("Encoded: ~p t ~p~n",[EncodedNerlTensor, NerlTensorType]),
      %io:format("NerlTensorOriginal: ~p ~n",[{NerlTensor, EncodeType}]),
      %io:format("NerlTensorEncDec: ~p ~n",[{DecodedTensor, DecodedType}]),
      FloatCase = EncodeType == float,
      CompareFloats = nerl:compare_floats_L(NerlTensor, DecodedTensor, 6),
      if
            FloatCase and CompareFloats-> encode_decode_nifs_test(N-1, Res ++ []);
            NerlTensor == DecodedTensor -> encode_decode_nifs_test(N-1, Res ++ []);
            true -> throw(ner:string_format("test failed - not equal ~n Origin: ~p ~n EncDec: ~p",[{NerlTensor, EncodeType},{DecodedTensor, DecodedType}]))
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