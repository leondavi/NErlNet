-module(nerlTests).
-author("David Leon").
-include("nerlTensor.hrl").

-compile(nerlNIF).
-export([run_tests/0]).

-import(nerlNIF,[init/0,create_nif/6,train_nif/5,call_to_train/6,predict_nif/2,call_to_predict/5,get_weights_nif/1,printTensor/2]).
-import(nerlNIF,[call_to_get_weights/1,call_to_set_weights/2]).
-import(nerlNIF,[decode_nif/2, nerltensor_binary_decode/2]).
-import(nerlNIF,[encode_nif/2, nerltensor_encode/5, nerltensor_conversion/2]).
-import(nerl,[compare_floats_L/3, string_format/2, logger_settings/1]).

-define(NERLTEST_PRINT_STR, "[NERLTEST] ").

nerltest_print(String) ->
      logger:notice(?NERLTEST_PRINT_STR++String).

% encode_decode test macros
-define(ENCODE_DECODE_ROUNDS, 200).
-define(DIMX_RAND_MAX, 30).
-define(DIMY_RAND_MAX, 30).

run_tests()->
      nerl:logger_settings(nerlTests),
      nerltest_print("encode decode running "++integer_to_list(?ENCODE_DECODE_ROUNDS)++" tests"),
      Tic = nerl:tic(),
      niftest_encode_decode(?ENCODE_DECODE_ROUNDS,[]), % throws if a test fails
      {NiftestEncDecTimeDiff, NiftestEncDecTimeUnit} = nerl:toc(Tic),
      nerltest_print(nerl:string_format("Elapsed: ~p~p",[NiftestEncDecTimeDiff,NiftestEncDecTimeUnit])),
      ok.


random_pick_nerltensor_type()->
      RandomIndex = rand:uniform(length(?LIST_BINARY_GROUP_NERLTENSOR_TYPE)),
      lists:nth(RandomIndex, ?LIST_BINARY_GROUP_NERLTENSOR_TYPE).

generate_nerltensor(Type)->
      DimX = rand:uniform(?DIMX_RAND_MAX),
      DimY = rand:uniform(?DIMY_RAND_MAX),
      DimZ = 1,
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

niftest_encode_decode(0, _Res) -> ok ;
niftest_encode_decode(N, Res) ->
      EncodeType = random_pick_nerltensor_type(),
      NerlTensor = generate_nerltensor(EncodeType),
      {EncodedNerlTensor, NerlTensorType} = nerlNIF:encode_nif(NerlTensor, EncodeType),
      {DecodedTensor, DecodedType} = nerlNIF:decode_nif(EncodedNerlTensor, NerlTensorType),
      %io:format("Encoded: ~p t ~p~n",[EncodedNerlTensor, NerlTensorType]),
      %io:format("NerlTensorOriginal: ~p ~n",[{NerlTensor, EncodeType}]),
      %io:format("NerlTensorEncDec: ~p ~n",[{DecodedTensor, DecodedType}]),
      FloatCase = EncodeType == float,
      CompareFloats = nerl:compare_floats_L(NerlTensor, DecodedTensor, 6),
      if
            FloatCase and CompareFloats-> niftest_encode_decode(N-1, Res ++ []);
            NerlTensor == DecodedTensor -> niftest_encode_decode(N-1, Res ++ []);
            true -> throw(ner:string_format("test failed - not equal ~n Origin: ~p ~n EncDec: ~p",[{NerlTensor, EncodeType},{DecodedTensor, DecodedType}]))
      end.

