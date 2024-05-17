-module(workerFederatedServer).

-export([controller/2]).

-include("workerDefinitions.hrl").
-include("w2wCom.hrl").

-import(nerlNIF,[nerltensor_scalar_multiplication_nif/3]).
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
    update -> update({GenWorkerEts, WorkerData})
  end.

get_this_server_ets(GenWorkerEts) -> 
  ets:lookup_element(GenWorkerEts, federated_server_ets, ?ETS_KEYVAL_VAL_IDX).
  
parse_args(Args) -> 
  ArgsList = string:split(Args, "," , all),
  Func = fun(Arg) ->
    [Key, Val] = string:split(Arg, "="),
    {Key, Val}
  end,
  lists:map(Func, ArgsList). % Returns list of tuples [{Key, Val}, ...]

sync_max_count_init(FedServerEts , ArgsList) -> 
  case lists:keyfind("sync_max_count", 1, ArgsList) of
    false -> Val = ?DEFAULT_SYNC_MAX_COUNT_ARG;
    {_, Val} -> list_to_integer(Val)
  end,
  ets:insert(FedServerEts, {sync_max_count, Val}).

%% handshake with workers / server
init({GenWorkerEts, WorkerData}) -> 
  FederatedServerEts = ets:new(federated_server,[set]),
  {MyName, Args, Token , WorkersList} = WorkerData,
  ArgsList = parse_args(Args),
  sync_max_count_init(FederatedServerEts, ArgsList),
  ets:insert(GenWorkerEts, {federated_server_ets, FederatedServerEts}),
  ets:insert(FederatedServerEts, {workers, WorkersList}),
  ets:insert(FederatedServerEts, {fed_clients, []}),
  ets:insert(FederatedServerEts, {sync_count, 0}),
  ets:insert(FederatedServerEts, {my_name, MyName}),
  ets:insert(FederatedServerEts, {token , Token}),
  put(fed_server_ets, FederatedServerEts).

  

pre_idle({_GenWorkerEts, _WorkerName}) -> ok.


post_idle({_GenWorkerEts, _WorkerName}) -> 
  % Extract all workers in nerlnet network 
  % Send handshake message to all workers
  % Wait for all workers to send handshake message back
  FedServerEts = get(fed_server_ets),
  FedServerName = ets:lookup_element(FedServerEts, my_name, ?ETS_KEYVAL_VAL_IDX),
  WorkersList = ets:lookup_element(FedServerEts, workers, ?ETS_KEYVAL_VAL_IDX),
  MyToken = ets:lookup_element(FedServerEts, token, ?ETS_KEYVAL_VAL_IDX),
  Func = fun(FedClient) ->
    w2wCom:send_message(FedClient, FedServerName, {handshake, MyToken})
  end,
  lists:foreach(Func, WorkersList),
  timer:sleep(?HANDSHAKE_TIMEOUT),
  IsEmpty = w2wCom:is_inbox_empty(),
  if IsEmpty == true -> 
    throw("Handshake failed, none of the workers responded in time");
    true -> ok
  end,
  InboxQueue = w2wCom:get_all_messages(),
  MessagesList = queue:to_list(InboxQueue),
  MsgFunc = 
    fun({?W2WCOM_ATOM, FromWorker, _MyName, {handshake, _WorkerToken}}) ->
      FedWorkers = ets:lookup_element(FedServerEts, fed_clients, ?ETS_KEYVAL_VAL_IDX),
      ets:update_element(FedServerEts, fed_clients, {?ETS_KEYVAL_VAL_IDX , [FromWorker] ++ FedWorkers})
  end,
  lists:foreach(MsgFunc, MessagesList),
  io:format("Handshake done~n").

%% Send updated weights if set
pre_train({_GenWorkerEts, _WorkerData}) -> ok.

%% calculate avg of weights when set
post_train({GenWorkerEts, _WorkerData}) -> 
  ThisEts = get_this_server_ets(GenWorkerEts),
  SyncCount = ets:lookup_element(ThisEts, sync_count, ?ETS_KEYVAL_VAL_IDX),
  if SyncCount == 0 -> 
    ModelID = ets:lookup_element(GenWorkerEts, model_id, ?ETS_KEYVAL_VAL_IDX),
    Weights = nerlNIF:call_to_get_weights(GenWorkerEts, ModelID),
    ClientPID = ets:lookup_element(GenWorkerEts, client_pid, ?ETS_KEYVAL_VAL_IDX),
    MyName = ets:lookup_element(GenWorkerEts, worker_name, ?ETS_KEYVAL_VAL_IDX),
    gen_statem:cast(ClientPID, {update, {MyName, MyName, Weights}}),
    MaxSyncCount = ets:lookup_element(ThisEts, sync_max_count, ?ETS_KEYVAL_VAL_IDX),
    ets:update_counter(ThisEts, sync_count, MaxSyncCount),
    _ToUpdate = true;
  true ->
    ets:update_counter(ThisEts, sync_count, -1),
    _ToUpdate = false
  end.
  % ThisEts = get_this_server_ets(GenWorkerEts),
  % Weights = generate_avg_weights(ThisEts),

  % gen_statem:cast({update, Weights}). %TODO complete send to all workers in lists:foreach

%% nothing?
pre_predict({_GenWorkerEts, _WorkerData}) -> ok.

%% nothing?
post_predict({_GenWorkerEts, _WorkerData}) -> ok.

%%  FedServer keeps an ets list of tuples: {WorkerName, worker, WeightsAndBiasNerlTensor}
%%  in update get weights of clients, if got from all => avg and send back
update({GenWorkerEts, WorkerData}) ->
  {WorkerName, _Me, NerlTensorWeights} = WorkerData,
  ThisEts = get_this_server_ets(GenWorkerEts),
  %% update weights in ets
  ets:insert(ThisEts, {WorkerName, worker, NerlTensorWeights}),

  %% check if there are queued messages, and treat them accordingly
  MessageQueue = ets:lookup_element(GenWorkerEts, controller_message_q, ?ETS_KEYVAL_VAL_IDX),
  % io:format("MessageQ=~p~n",[MessageQueue]),
  [ets:insert(ThisEts, {WorkerName, worker, NerlTensorWeights}) || {Action, WorkerName, To, NerlTensorWeights} <- MessageQueue, Action == update],
  % reset q
  ets:delete(GenWorkerEts, controller_message_q),  
  ets:insert(GenWorkerEts, {controller_message_q, []}),

  %% check if got all weights of workers
  WorkersList = ets:lookup_element(ThisEts, workers, ?ETS_KEYVAL_VAL_IDX),
  GotWorkers = [ element(?ETS_WID_IDX, Attr) || Attr <- ets:tab2list(ThisEts), element(?ETS_TYPE_IDX, Attr) == worker],
  % io:format("My workers=~p, have vectors from=~p~n",[WorkersList,GotWorkers]),
  WaitingFor = WorkersList -- GotWorkers,
    
  if WaitingFor == [] ->
      AvgWeightsNerlTensor = generate_avg_weights(ThisEts),
      % io:format("AvgWeights = ~p~n",[AvgWeightsNerlTensor]),
      ModelID = ets:lookup_element(GenWorkerEts, model_id, ?ETS_KEYVAL_VAL_IDX),
      nerlNIF:call_to_set_weights(ModelID, AvgWeightsNerlTensor),     %% update self weights to new model
      [ets:delete(ThisEts, OldWorkerName) || OldWorkerName <- WorkersList ],%% delete old tensors for next aggregation phase
      ClientPID = ets:lookup_element(GenWorkerEts, client_pid, ?ETS_KEYVAL_VAL_IDX),
      gen_statem:cast(ClientPID, {custom_worker_message, WorkersList, AvgWeightsNerlTensor}),
      false;
  true -> true end. %% return StillUpdate = true


generate_avg_weights(FedEts) ->
  BinaryType = ets:lookup_element(FedEts, nerltensor_type, ?ETS_NERLTENSOR_TYPE_IDX),
  ListOfWorkersNerlTensors = [ element(?TENSOR_DATA_IDX, element(?ETS_WEIGHTS_AND_BIAS_NERLTENSOR_IDX, Attr)) || Attr <- ets:tab2list(FedEts), element(?ETS_TYPE_IDX, Attr) == worker],
  % io:format("Tensors to sum = ~p~n",[ListOfWorkersNerlTensors]),
  NerlTensors = length(ListOfWorkersNerlTensors),
  [FinalSumNerlTensor] = nerlTensor:sum_nerltensors_lists(ListOfWorkersNerlTensors, BinaryType),
  % io:format("Summed = ~p~n",[FinalSumNerlTensor]),
  nerlNIF:nerltensor_scalar_multiplication_nif(FinalSumNerlTensor, BinaryType, 1.0/NerlTensors).
