-module(workerFederatedServer).

-export([controller/2]).

-import(nerlNIF,[sum_nerltensors_lists/2, nerltensor_scalar_multiplication_nif/3]).

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

%% every countLimit batches, get updated model (NOTHING FOR SERVER) ??? maybe he needs to go to state receive from other workers?
pre_train(Data) -> Data.

%% every countLimit batches, send updated weights
post_train(Data) -> Data,
    generate_avg_weights(). %TODO complete

%% nothing?
pre_predict(Data) -> Data.

%% nothing?
post_predict(Data) -> Data.

generate_avg_weights() ->
  BinaryType = ets:lookup_element(federated_server, nerltensor_type, ?ETS_NERLTENSOR_TYPE_IDX),
  ListOfWorkersNerlTensors = [ element(?ETS_WEIGHTS_AND_BIAS_NERLTENSOR_IDX, Attr) || Attr <- ets:tab2list(federated_server), element(?ETS_TYPE_IDX, Attr) == worker],
  NerlTensors = length(ListOfWorkersNerlTensors),
  FinalSumNerlTensor = nerlNIF:sum_nerltensors_lists(ListOfWorkersNerlTensors, BinaryType),
  nerltensor_scalar_multiplication_nif(FinalSumNerlTensor, BinaryType, 1.0/NerlTensors).

