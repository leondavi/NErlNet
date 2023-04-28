-module(nerlTests).
-author("David Leon").
-include("nerlTensor.hrl").

-compile(nerlNIF).
-export([run_tests/0]).
-define(DIMX_RAND_MAX, 5).
-define(DIMY_RAND_MAX, 5).

-import(nerlNIF,[init/0,create_nif/6,train_nif/5,call_to_train/6,predict_nif/2,call_to_predict/5,get_weights_nif/1,printTensor/2]).
-import(nerlNIF,[call_to_get_weights/1,call_to_set_weights/2]).
-import(nerlNIF,[decode_nif/2, nerltensor_binary_decode/2]).
-import(nerlNIF,[encode_nif/2, nerltensor_encode/5, nerltensor_conversion/2]).

run_tests()->
      niftest_encode().


random_pick_nerltensor_type()->
      RandomIndex = rand:uniform(length(?LIST_BINARY_GROUP_NERLTENSOR_TYPE)),
      lists:nth(RandomIndex, ?LIST_BINARY_GROUP_NERLTENSOR_TYPE).

generate_nerltensor(Type)->
      case Type of 
            integer -> ok;
            float ->    DimX = float(rand:uniform(?DIMX_RAND_MAX)),
                        DimY = float(rand:uniform(?DIMY_RAND_MAX)),
                        DimZ = 1.0,
                        DataLength = round(DimX * DimY * DimZ),
                        Data = [rand:uniform() * 10 || _ <- lists:seq(1, DataLength)],
                        [DimX,DimY,DimZ] ++ Data
      end.

niftest_encode() ->
      EncodeType = random_pick_nerltensor_type(),
      NerlTensor = generate_nerltensor(float),
      io:format("~p ~p",[EncodeType,NerlTensor]),
      {EncodedNerlTensor, NerlTensorType} = nerlNIF:encode_nif(NerlTensor, EncodeType),
      io:format("Encoded: ~p t ~p~n",[EncodedNerlTensor, NerlTensorType]),
      {DecodedTensor, DecodedType} = nerlNIF:decode_nif(EncodedNerlTensor, NerlTensorType),
      io:format("NerlTensorOriginal: ~p ~n",[{NerlTensor, EncodeType}]),
      io:format("NerlTensorEncDec: ~p ~n",[{DecodedTensor, DecodedType}]).

