-module(workerFederatedServer).

-export([controller/2]).

-include("workerDefinitions.hrl").

-import(nerlNIF,[sum_nerltensors_lists/2, nerltensor_scalar_multiplication_nif/3]).

-define(ETS_WID_IDX, 1).
-define(ETS_TYPE_IDX, 2).
-define(ETS_WEIGHTS_AND_BIAS_NERLTENSOR_IDX, 3).
-define(ETS_NERLTENSOR_TYPE_IDX, 2).


controller(FuncName, {GenWorkerEts, WorkerData}) -> 
  case FuncName of
    init -> init({GenWorkerEts, WorkerData});
    pre_idle -> pre_idle({GenWorkerEts, WorkerData});
    post_idle -> post_idle({GenWorkerEts, WorkerData});
    pre_train -> pre_train({GenWorkerEts, WorkerData});
    post_train -> post_train({GenWorkerEts, WorkerData});
    pre_predict -> pre_predict({GenWorkerEts, WorkerData});
    post_predict -> post_predict({GenWorkerEts, WorkerData});
    update -> update({GenWorkerEts, WorkerData})
  end.

get_this_server_ets(GenWorkerEts) -> 
  ets:lookup_element(GenWorkerEts, federated_server_ets, ?ETS_KEYVAL_VAL_IDX).
  
%% handshake with workers / server
init({GenWorkerEts, WorkerData}) -> ok,
  Type = float, % update from data
  % {SyncMaxCount, SyncCounter = 0, MyName, WorkersNamesList = []} = WorkerData,
  % #workerFederatedServer{clientPID = ClientPid, workersNamesList = WorkersNamesList} = WorkerData,
  FederatedServerEts = ets:new(federated_server,[set]),
  ets:insert(GenWorkerEts, federated_server_ets, FederatedServerEts),

  ets:insert(FederatedServerEts,{nerltensor_type, Type}).

pre_idle({GenWorkerEts, WorkerName}) when is_atom(WorkerName) -> 
  ThisEts = get_this_server_ets(GenWorkerEts),
  ?LOG_NOTICE("adding worker ~p to fed workers",[WorkerName]),
  add_worker(ThisEts, WorkerName).

post_idle({_GenWorkerEts, _WorkerData}) -> ok.

%% every countLimit batches, get updated model (NOTHING FOR SERVER) ??? maybe he needs to go to state receive from other workers?
pre_train({GenWorkerEts, WorkerData}) -> ok.

%% every countLimit batches, send updated weights
post_train({GenWorkerEts, WorkerData}) -> ok,
    Weights = generate_avg_weights(),

    gen_statem:cast({update, Weights}). %TODO complete send to all workers in lists:foreach

%% nothing?
pre_predict({GenWorkerEts, WorkerData}) -> ok.

%% nothing?
post_predict({GenWorkerEts, WorkerData}) -> ok.

update({GenWorkerEts, WorkerData}) ->
  {WorkerName, Weights} = WorkerData,
  ThisEts = get_this_server_ets(GenWorkerEts),
  ets:update_element(ThisEts, server_update, {?ETS_KEYVAL_VAL_IDX, true}),
    %% this is an ets list of tuples: {WorkerName, worker, WeightsAndBiasNerlTensor}
  WorkerExists = ets:member(ThisEts, WorkerName),
  if WorkerExists ->
    ets:update_element(ThisEts, WorkerName, [ {?ETS_TYPE_IDX, worker}, {?ETS_KEYVAL_VAL_IDX, NerlTensorWeights}]);    %% TODO: validate WorkerName is atom
  true ->
    ets:insert(ThisEts, {WorkerName, worker, empty_nerltensor})
  end.


generate_avg_weights() ->
  BinaryType = ets:lookup_element(federated_server, nerltensor_type, ?ETS_NERLTENSOR_TYPE_IDX),
  ListOfWorkersNerlTensors = [ element(?ETS_WEIGHTS_AND_BIAS_NERLTENSOR_IDX, Attr) || Attr <- ets:tab2list(federated_server), element(?ETS_TYPE_IDX, Attr) == worker],
  NerlTensors = length(ListOfWorkersNerlTensors),
  FinalSumNerlTensor = nerlNIF:sum_nerltensors_lists(ListOfWorkersNerlTensors, BinaryType),
  nerlNIF:nerltensor_scalar_multiplication_nif(FinalSumNerlTensor, BinaryType, 1.0/NerlTensors).

add_worker(ThisEts, WorkerData) ->
  Workers = ets:lookup_element(ThisEts, workers, ?ETS_KEYVAL_VAL_IDX),
  ets:insert(ThisEts, workers, Workers++WorkerData).