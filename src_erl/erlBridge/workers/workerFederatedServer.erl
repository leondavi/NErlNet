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
  {SyncMaxCount, MyName, WorkersNamesList} = WorkerData,
  % #workerFederatedServer{clientPID = ClientPid, workersNamesList = WorkersNamesList} = WorkerData,
  FederatedServerEts = ets:new(federated_server,[set]),
  ets:insert(GenWorkerEts, {federated_server_ets, FederatedServerEts}),
  ets:insert(FederatedServerEts, {workers, [MyName]}),    %% start with only self in list, get others in network thru handshake
  ets:insert(FederatedServerEts, {sync_max_count, SyncMaxCount}),
  ets:insert(FederatedServerEts, {sync_count, SyncMaxCount}),
  ets:insert(FederatedServerEts, {my_name, MyName}),
  ets:insert(FederatedServerEts, {nerltensor_type, Type}).

pre_idle({GenWorkerEts, WorkerName}) -> ok.

post_idle({GenWorkerEts, WorkerName}) -> 
  ThisEts = get_this_server_ets(GenWorkerEts),
  io:format("adding worker ~p to fed workers~n",[WorkerName]),
  Workers = ets:lookup_element(ThisEts, workers, ?ETS_KEYVAL_VAL_IDX),
  ets:insert(ThisEts, {workers, Workers++[WorkerName]}).

%% Send updated weights if set
pre_train({GenWorkerEts, WorkerData}) -> ok.

%% calculate avg of weights when set
post_train({GenWorkerEts, WorkerData}) -> 
  ThisEts = get_this_server_ets(GenWorkerEts),
  SyncCount = ets:lookup_element(ThisEts, sync_count, ?ETS_KEYVAL_VAL_IDX),
  if SyncCount == 0 ->
    ModelID = ets:lookup_element(GenWorkerEts, model_id, ?ETS_KEYVAL_VAL_IDX),
    Weights = nerlNIF:call_to_get_weights(ModelID),
    ClientPID = ets:lookup_element(GenWorkerEts, client_pid, ?ETS_KEYVAL_VAL_IDX),
    MyName = ets:lookup_element(GenWorkerEts, worker_name, ?ETS_KEYVAL_VAL_IDX),
    gen_statem:cast(ClientPID, {update, {MyName, MyName, Weights}}),
    MaxSyncCount = ets:lookup_element(ThisEts, sync_max_count, ?ETS_KEYVAL_VAL_IDX),
    ets:update_counter(ThisEts, sync_count, MaxSyncCount),
    ToUpdate = true;
  true ->
    ets:update_counter(ThisEts, sync_count, -1),
    ToUpdate = false
  end.
  % ThisEts = get_this_server_ets(GenWorkerEts),
  % Weights = generate_avg_weights(ThisEts),

  % gen_statem:cast({update, Weights}). %TODO complete send to all workers in lists:foreach

%% nothing?
pre_predict({GenWorkerEts, WorkerData}) -> ok.

%% nothing?
post_predict({GenWorkerEts, WorkerData}) -> ok.

%%  FedServer keeps an ets list of tuples: {WorkerName, worker, WeightsAndBiasNerlTensor}
%%  in update get weights of clients, if got from all => avg and send back
update({GenWorkerEts, WorkerData}) ->
  {WorkerName, Me, NerlTensorWeights} = WorkerData,
  ThisEts = get_this_server_ets(GenWorkerEts),
  %% update weights in ets
  ets:insert(ThisEts, {WorkerName, worker, NerlTensorWeights}),

  %% check if there are queued messages, and treat them accordingly
  Q = ets:lookup_element(GenWorkerEts, message_q, ?ETS_KEYVAL_VAL_IDX),
  [ets:insert(ThisEts, {WorkerName, worker, NerlTensorWeights}) || {Action, WorkerName, To, NerlTensorWeights} <- Q, Action == update],

  %% check if got all weights of workers
  WorkersList = ets:lookup_element(ThisEts, workers, ?ETS_KEYVAL_VAL_IDX),
  GotAll = length(WorkersList) == 
    length([ element(?ETS_WEIGHTS_AND_BIAS_NERLTENSOR_IDX, Attr) || Attr <- ets:tab2list(ThisEts), element(?ETS_TYPE_IDX, Attr) == worker]),
  if GotAll ->
      AvgWeightsNerlTensor = generate_avg_weights(ThisEts),
      io:format("AvgWeights = ~p~n",[AvgWeightsNerlTensor]),
      ModelID = ets:lookup_element(GenWorkerEts, model_id, ?ETS_KEYVAL_VAL_IDX),
      nerlNIF:call_to_set_weights(ModelID, AvgWeightsNerlTensor),     %% update self weights to new model
      [ets:delete(ThisEts, WorkerName) || WorkerName <- WorkersList ],%% delete old tensors for next aggregation phase
      ClientPID = ets:lookup_element(GenWorkerEts, client_pid, ?ETS_KEYVAL_VAL_IDX),
      gen_statem:cast(ClientPID, {custom_worker_message, WorkersList, AvgWeightsNerlTensor}),
      false;
  true -> true end.


generate_avg_weights(FedEts) ->
  BinaryType = ets:lookup_element(FedEts, nerltensor_type, ?ETS_NERLTENSOR_TYPE_IDX),
  ListOfWorkersNerlTensors = [ element(?ETS_WEIGHTS_AND_BIAS_NERLTENSOR_IDX, Attr) || Attr <- ets:tab2list(FedEts), element(?ETS_TYPE_IDX, Attr) == worker],
  io:format("Tensors to sum = ~p~n",[ListOfWorkersNerlTensors]),
  NerlTensors = length(ListOfWorkersNerlTensors),
  FinalSumNerlTensor = nerlNIF:sum_nerltensors_lists(ListOfWorkersNerlTensors, BinaryType),
  io:format("Summed = ~p~n",[FinalSumNerlTensor]),
  nerlNIF:nerltensor_scalar_multiplication_nif(FinalSumNerlTensor, BinaryType, 1.0/NerlTensors).
