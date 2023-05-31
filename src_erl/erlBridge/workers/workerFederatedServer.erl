-module(workerFederatedServer).

-export([controller/2]).

-import(nerlNIF,[nerltensor_sum_erl/2, nerltensor_scalar_multiplication_erl/2]).

-define(ETS_WID_IDX, 1).
-define(ETS_TYPE_IDX, 2).
-define(ETS_WEIGHTS_AND_BIAS_NERLTENSOR_IDX, 3).


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
  ets:new(federated_server,[named_table, set]).%TODO insert workers by their name {WID, worker, {WeightsAndBiasNerlTensor, type}}

%% every countLimit batches, get updated model (NOTHING FOR SERVER)
pre_train(Data) -> Data.

%% every countLimit batches, send updated weights
post_train(Data) -> Data.

%% nothing?
pre_predict(Data) -> Data.

%% nothing?
post_predict(Data) -> Data.

even(X) when X >= 0 -> (X band 1) == 0.
odd(X) when X > 0 -> not even(X).

generate_avg_weights() -> 
  ListOfWorkersNerlTensors = [ element(?ETS_WEIGHTS_AND_BIAS_NERLTENSOR_IDX, Attr) || Attr <- ets:tab2list(federated_server), element(?ETS_TYPE_IDX, Attr) == worker],
  OddLength = odd(length(ListOfWorkersNerlTensors)),
  {OddFirstElement, ListOfWorkersNerlTensorsEven} =  
  if OddLength -> {hd(ListOfWorkersNerlTensors), tl(ListOfWorkersNerlTensors)};
     true -> {[], ListOfWorkersNerlTensors}
  end,
  WorkersNerlTensorsHalfListA = lists:sublist(ListOfWorkersNerlTensorsEven, length(ListOfWorkersNerlTensorsEven)/2),
  WorkersNerlTensorsHalfListB = ListOfWorkersNerlTensorsEven -- WorkersNerlTensorsHalfListA,
  lists:zipwith(fun(X,Y) -> nerltensor_sum_erl(X, Y) end, WorkersNerlTensorsHalfListA, WorkersNerlTensorsHalfListB).
  

