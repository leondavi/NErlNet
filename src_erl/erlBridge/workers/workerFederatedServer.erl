-module(workerFederatedServer).

-export([controller/2]).

-import(nerlNIF,[nerltensor_sum_nif/2, nerltensor_scalar_multiplication_nif/3]).

-define(ETS_WID_IDX, 1).
-define(ETS_TYPE_IDX, 2).
-define(ETS_WEIGHTS_AND_BIAS_NERLTENSOR_IDX, 3).
-define(ETS_NERLTENSOR_TYPE_IDX, 2).


controller(FuncName, Data) -> 
  case FuncName of
    init -> init(Data);
    pre_train -> pre_train(Data);
    post_train -> post_train(Data);
    pre_predict -> pre_predict(Data);
    post_predict -> post_predict(Data)
  end.

%% handshake with workers / server (DONE IN GENERIC)
init(Data) -> Data,
  Type = float, % update from data
  ets:new(federated_server,[named_table, set]), ets:insert(federated_server,{nerltensor_type, Type}).%TODO insert workers by their name {WID, worker, WeightsAndBiasNerlTensor}

%% every countLimit batches, get updated model (NOTHING FOR SERVER)
pre_train(Data) -> Data.

%% every countLimit batches, send updated weights
post_train(Data) -> Data,
    generate_avg_weights(). %TODO complete

%% nothing?
pre_predict(Data) -> Data.

%% nothing?
post_predict(Data) -> Data.

even(X) when X >= 0 -> (X band 1) == 0.
odd(X) when X > 0 -> not even(X).

generate_avg_weights() ->
  BinaryType = ets:lookup_element(federated_server, nerltensor_type, ?ETS_NERLTENSOR_TYPE_IDX),
  ListOfWorkersNerlTensors = [ element(?ETS_WEIGHTS_AND_BIAS_NERLTENSOR_IDX, Attr) || Attr <- ets:tab2list(federated_server), element(?ETS_TYPE_IDX, Attr) == worker],
  NerlTensors = length(ListOfWorkersNerlTensors),
  FinalSumNerlTensor = sum_nerltensors_lists(ListOfWorkersNerlTensors, BinaryType),
  nerltensor_scalar_multiplication_nif(FinalSumNerlTensor, BinaryType, 1.0/NerlTensors).


sum_nerltensors_lists([], _BinaryType) ->  throw("Zero length given to sum_nerltensors_even_lists");
sum_nerltensors_lists(NerltensorList, _BinaryType) when length(NerltensorList) == 1 ->  NerltensorList;
sum_nerltensors_lists(NerltensorList, BinaryType)  -> 
  OddLength = odd(NerltensorList),
  {OddFirstElement, EvenNerltensorList} =  
  if OddLength -> {hd(NerltensorList), tl(NerltensorList)};
     true -> {[], NerltensorList}
  end,

  % Split to high and low lists
  NerlTensorsHalfListA = lists:sublist(EvenNerltensorList, length(EvenNerltensorList)/2),
  NerlTensorsHalfListB = EvenNerltensorList -- NerlTensorsHalfListA,
  % sum high and low lists
  SumResultOfTwoHalfs = lists:zipwith(fun(NerlTensorA,NerlTensorB) -> nerlNIF:nerltensor_sum_nif(NerlTensorA, NerlTensorB, BinaryType) end, NerlTensorsHalfListA, NerlTensorsHalfListB),
  % take care to the first element in case of odd length
  SumResultOfAll = 
  if OddLength -> nerlNIF:nerltensor_sum_nif(OddFirstElement, SumResultOfTwoHalfs, BinaryType);
     true -> SumResultOfTwoHalfs % nothing to do with first element in case of even list
  end,
  sum_nerltensors_lists(SumResultOfAll, BinaryType).

