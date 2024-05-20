-module(workerFederatedServer).

-export([controller/2]).

-include("workerDefinitions.hrl").
-include("w2wCom.hrl").

-import(nerlNIF,[nerltensor_scalar_multiplication_nif/3, call_to_get_weights/1, call_to_set_weights/2]).
-import(nerlTensor,[sum_nerltensors_lists/2]).
-import(w2wCom,[send_message/3, get_all_messages/0, is_inbox_empty/0]).


-define(ETS_WID_IDX, 1).
-define(ETS_TYPE_IDX, 2).
-define(ETS_WEIGHTS_AND_BIAS_NERLTENSOR_IDX, 3).
-define(ETS_NERLTENSOR_TYPE_IDX, 2).
-define(DEFAULT_SYNC_MAX_COUNT_ARG, 1).
-define(HANDSHAKE_TIMEOUT, 2000). % 2 seconds


controller(FuncName, {GenWorkerEts, WorkerData}) -> 
  case FuncName of
    init -> init({GenWorkerEts, WorkerData});
    pre_idle -> pre_idle({GenWorkerEts, WorkerData});
    post_idle -> post_idle({GenWorkerEts, WorkerData});
    pre_train -> pre_train({GenWorkerEts, WorkerData});
    post_train -> post_train({GenWorkerEts, WorkerData});
    pre_predict -> pre_predict({GenWorkerEts, WorkerData});
    post_predict -> post_predict({GenWorkerEts, WorkerData});
    start_stream -> start_stream({GenWorkerEts, WorkerData});
    end_stream -> end_stream({GenWorkerEts, WorkerData});
    worker_done -> worker_done({GenWorkerEts, WorkerData})
  end.


% After adding put(Ets) to init this function is not needed
get_this_server_ets(GenWorkerEts) -> 
  ets:lookup_element(GenWorkerEts, federated_server_ets, ?ETS_KEYVAL_VAL_IDX).
  
parse_args(Args) -> 
  ArgsList = string:split(Args, "," , all),
  Func = fun(Arg) ->
    [Key, Val] = string:split(Arg, "="),
    {Key, Val}
  end,
  lists:map(Func, ArgsList). % Returns list of tuples [{Key, Val}, ...]

sync_max_count_init(FedClientEts , ArgsList) -> 
  case lists:keyfind("SyncMaxCount", 1, ArgsList) of
    false -> ValInt = ?DEFAULT_SYNC_MAX_COUNT_ARG;
    {_, Val} -> ValInt = list_to_integer(Val)
  end,
  ets:insert(FedClientEts, {sync_max_count, ValInt}).

%% handshake with workers / server
init({GenWorkerEts, WorkerData}) -> 
  FederatedServerEts = ets:new(federated_server,[set]),
  {MyName, Args, Token , WorkersList} = WorkerData,
  BroadcastWorkers = WorkersList -- [MyName],
  ArgsList = parse_args(Args),
  sync_max_count_init(FederatedServerEts, ArgsList),
  ets:insert(GenWorkerEts, {federated_server_ets, FederatedServerEts}),
  W2WPid = ets:lookup_element(GenWorkerEts, w2wcom_pid, ?ETS_KEYVAL_VAL_IDX),
  ets:insert(FederatedServerEts, {w2wcom_pid, W2WPid}),
  ets:insert(FederatedServerEts, {broadcast_workers_list, BroadcastWorkers}),
  ets:insert(FederatedServerEts, {fed_clients, []}),
  ets:insert(FederatedServerEts, {training_workers , []}),
  ets:insert(FederatedServerEts, {sync_count, 0}),
  ets:insert(FederatedServerEts, {my_name, MyName}),
  ets:insert(FederatedServerEts, {token , Token}),
  ets:insert(FederatedServerEts, {weights_list, []}),
  put(fed_server_ets, FederatedServerEts).

  
start_stream({_GenWorkerEts, _WorkerData}) -> ok.

end_stream({_GenWorkerEts, _WorkerData}) -> ok.

pre_idle({_GenWorkerEts, _WorkerName}) -> ok.


worker_done({GenWorkerEts, WorkerData}) -> 
  WorkerName = hd(WorkerData),
  ThisEts = get_this_server_ets(GenWorkerEts),
  TrainingWorkers = ets:lookup_element(ThisEts, training_workers, ?ETS_KEYVAL_VAL_IDX),
  UpdatedTrainingWorkers = lists:delete(WorkerName, TrainingWorkers),
  io:format("Worker ~p Done, UpdatedTrainingWorkers = ~p~n", [WorkerName, UpdatedTrainingWorkers]),
  ets:update_element(ThisEts, training_workers, {?ETS_KEYVAL_VAL_IDX, UpdatedTrainingWorkers}).


% Extract all workers in nerlnet network 
% Send handshake message to all workers
% Wait for all workers to send handshake message back
post_idle({GenWorkerEts, _WorkerName}) -> 
  HandshakeDone = ets:lookup_element(GenWorkerEts, handshake_done, ?ETS_KEYVAL_VAL_IDX),
  case HandshakeDone of
  false -> 
    FedServerEts = get(fed_server_ets),
    FedServerName = ets:lookup_element(FedServerEts, my_name, ?ETS_KEYVAL_VAL_IDX),
    WorkersList = ets:lookup_element(FedServerEts, broadcast_workers_list, ?ETS_KEYVAL_VAL_IDX),
    W2WPid = ets:lookup_element(FedServerEts, w2wcom_pid, ?ETS_KEYVAL_VAL_IDX),
    MyToken = ets:lookup_element(FedServerEts, token, ?ETS_KEYVAL_VAL_IDX),
    Func = fun(FedClient) ->
      w2wCom:send_message(W2WPid, FedServerName, FedClient, {handshake, MyToken})
    end,
    lists:foreach(Func, WorkersList),
    timer:sleep(?HANDSHAKE_TIMEOUT),
    InboxQueue = w2wCom:get_all_messages(W2WPid),
    IsEmpty = queue:len(InboxQueue) == 0,
    if IsEmpty == true -> 
      throw("Handshake failed, none of the workers responded in time");
      true -> ok
    end,
    MessagesList = queue:to_list(InboxQueue),
    MsgFunc = 
      fun({FedClient, {handshake, _Token}}) ->
        FedClients = ets:lookup_element(FedServerEts, fed_clients, ?ETS_KEYVAL_VAL_IDX),
        ets:update_element(FedServerEts, fed_clients, {?ETS_KEYVAL_VAL_IDX , [FedClient] ++ FedClients}),
        w2wCom:send_message(W2WPid, FedServerName, FedClient, {handshake_done, MyToken})
    end,
    lists:foreach(MsgFunc, MessagesList),
    UpdatedTrainingWorkers = ets:lookup_element(FedServerEts, fed_clients, ?ETS_KEYVAL_VAL_IDX),
    ets:update_element(FedServerEts, training_workers, {?ETS_KEYVAL_VAL_IDX, UpdatedTrainingWorkers}),
    ets:update_element(GenWorkerEts, handshake_done, {?ETS_KEYVAL_VAL_IDX, true});
  true -> ok
  end.

%% Send updated weights if set
pre_train({_GenWorkerEts, _WorkerData}) -> ok.

% 1. get weights from all workers
% 2. average them
% 3. set new weights to model
% 4. send new weights to all workers
post_train({GenWorkerEts, WorkerData}) when length(WorkerData) == 0 -> % WorkerData = []
  ThisEts = get_this_server_ets(GenWorkerEts),
  FedServerEts = get(fed_server_ets),
  NumOfTrainingWorkers = length(ets:lookup_element(ThisEts, training_workers, ?ETS_KEYVAL_VAL_IDX)),
  W2WPid = ets:lookup_element(GenWorkerEts, w2wcom_pid, ?ETS_KEYVAL_VAL_IDX),
  InboxQueue = w2wCom:get_all_messages(W2WPid),
  MessagesList = queue:to_list(InboxQueue),
  ReceivedWeights = [WorkersWeights || {_WorkerName, {WorkersWeights, _BinaryType}} <- MessagesList],
  CurrWorkersWeightsList = ets:lookup_element(FedServerEts, weights_list, ?ETS_KEYVAL_VAL_IDX),
  TotalWorkersWeights = CurrWorkersWeightsList ++ ReceivedWeights,
  case length(TotalWorkersWeights) == NumOfTrainingWorkers of % ? Why not timeout
    true -> 
      ModelID = ets:lookup_element(GenWorkerEts, model_id, ?ETS_KEYVAL_VAL_IDX),
      {CurrentModelWeights, BinaryType} = nerlNIF:call_to_get_weights(ModelID),
      FedServerName = ets:lookup_element(FedServerEts, my_name, ?ETS_KEYVAL_VAL_IDX),
      AllWorkersWeightsList = TotalWorkersWeights ++ [CurrentModelWeights],
      AvgWeightsNerlTensor = generate_avg_weights(AllWorkersWeightsList, BinaryType),
      nerlNIF:call_to_set_weights(ModelID, AvgWeightsNerlTensor), %% update self weights to new model
      Func = fun(FedClient) ->
        FedServerName = ets:lookup_element(ThisEts, my_name, ?ETS_KEYVAL_VAL_IDX),
        W2WPid = ets:lookup_element(ThisEts, w2wcom_pid, ?ETS_KEYVAL_VAL_IDX),
        w2wCom:send_message(W2WPid, FedServerName, FedClient, {update_weights, AvgWeightsNerlTensor})
      end,
      WorkersList = ets:lookup_element(ThisEts, training_workers, ?ETS_KEYVAL_VAL_IDX),
      lists:foreach(Func, WorkersList),
      ets:update_element(FedServerEts, weights_list, {?ETS_KEYVAL_VAL_IDX, []});
    false -> ets:update_element(FedServerEts, weights_list, {?ETS_KEYVAL_VAL_IDX, TotalWorkersWeights})
  end.
  
%% nothing?
pre_predict({_GenWorkerEts, _WorkerData}) -> ok.

%% nothing?
post_predict({_GenWorkerEts, _WorkerData}) -> ok.


generate_avg_weights(AllWorkersWeightsList, BinaryType) ->
  % io:format("AllWorkersWeightsList = ~p~n",[AllWorkersWeightsList]),
  NumNerlTensors = length(AllWorkersWeightsList),
  if 
    NumNerlTensors > 1 -> [FinalSumNerlTensor] = nerlTensor:sum_nerltensors_lists(AllWorkersWeightsList, BinaryType);
    true -> FinalSumNerlTensor = hd(AllWorkersWeightsList)
  end,
  % io:format("Summed = ~p~n",[FinalSumNerlTensor]),
  nerlNIF:nerltensor_scalar_multiplication_nif(FinalSumNerlTensor, BinaryType, 1.0/NumNerlTensors).
