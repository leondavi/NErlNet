
-module(niftest).
-author("David Leon").
-include("nerlTensor.hrl").

-export([run_tests/0]).
-define(DIMX_RAND_MAX, 5).
-define(DIMY_RAND_MAX, 5).
                  io:format("///// worker miss train batch ~n "),
                 % io:format("///// worker miss predict batch ~n "), 

-import(nerlNIF,[init/0,create_nif/6,train_nif/5,call_to_train/6,predict_nif/2,call_to_predict/5,get_weights_nif/1,printTensor/2]).
-import(nerlNIF,[call_to_get_weights/1,call_to_set_weights/2]).
-import(nerlNIF,[decode_nif/2, nerltensor_binary_decode/2]).
-import(nerlNIF,[encode_nif/2, nerltensor_encode/5, nerltensor_conversion/2]).

run_tests()->
      io:fwrite("hey"), ok.%niftest_encode().


random_pick_nerltensor_type()->
      RandomIndex = rand:uniform(length(?LIST_BINARY_GROUP_NERLTENSOR_TYPE)),
      lists:nth(RandomIndex, ?LIST_BINARY_GROUP_NERLTENSOR_TYPE).

generate_nerltensor()->
      DimX = rand:uniform(?DIMX_RAND_MAX),
      DimY = rand:uniform(?DIMY_RAND_MAX),
      DimZ = 1,
      DataLength = DimX * DimY * DimZ,
      Data = [rand:uniform() * 10 || _ <- lists:seq(1, DataLength)],
      [DimX,DimY,DimZ] ++ Data.

niftest_encode() ->
      EncodeType = random_pick_nerltensor_type(),
      NerlTensor = generate_nerltensor(),
      {EncodedNerlTensor, NerlTensorType} = nerlNIF:encode_nif(NerlTensor, EncodeType),
      DecodedTensor = nerlNIF:decode_nif(EncodedNerlTensor, NerlTensorType),
      io:format("NerlTensorOriginal: ~p ~n",[{NerlTensor, EncodeType}]),
      io:format("NerlTensorEncDec: ~p ~n",[DecodedTensor]).

