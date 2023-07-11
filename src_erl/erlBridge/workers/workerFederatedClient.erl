-module(workerFederatedClient).

-export([controller/2]).

-include("/usr/local/lib/nerlnet-lib/NErlNet/src_erl/Communication_Layer/http_Nerlserver/src/nerl_tools.hrl").
-include("workerDefinitions.hrl").

-define(WORKER_FEDERATED_CLIENT_ETS_FIELDS, [my_name, client_pid, server_name, sync_max_count, sync_count]).
-define(FEDERATED_CLIENT_ETS_KEY_IN_GENWORKER_ETS, fedrated_client_ets).

% %% Federated mode
% wait(cast, {loss, {LOSS_FUNC,Time_NIF}}, State = #workerGeneric_state{clientPid = ClientPid,ackClient = AckClient, myName = MyName, nextState = NextState, count = Count, countLimit = CountLimit, modelId = Mid}) ->
%   % {LOSS_FUNC,_TimeCpp} = LossAndTime,
%   if Count == CountLimit ->
%       % Get weights
%       Ret_weights = nerlNIF:call_to_get_weights(Mid),
%       % Ret_weights_tuple = niftest:call_to_get_weights(Mid),
%       % {Weights,Bias,Biases_sizes_list,Wheights_sizes_list} = Ret_weights_tuple,

%       % ListToSend = [Weights,Bias,Biases_sizes_list,Wheights_sizes_list],

%       % Send weights and loss value
%       gen_statem:cast(ClientPid,{loss, federated_weights, MyName, LOSS_FUNC, Ret_weights}), %% TODO Add Time and Time_NIF to the cast
%       checkAndAck(MyName,ClientPid,AckClient),
%       % Reset count and go to state train
%       {next_state, NextState, State#workerNN_state{ackClient = 0, count = 0}};

%     true ->
%       %% Send back the loss value
%       gen_statem:cast(ClientPid,{loss, MyName, LOSS_FUNC,Time_NIF/1000}), %% TODO Add Time and Time_NIF to the cast
%       checkAndAck(MyName,ClientPid,AckClient),
      

%       {next_state, NextState, State#workerNN_state{ackClient = 0, count = Count + 1}}
%   end.

%% Data = -record(workerFederatedClient, {syncCount, syncMaxCount, serverAddr}).

controller(FuncName, {GenWorkerEts, WorkerData}) -> 
  case FuncName of
    init        -> init({GenWorkerEts, WorkerData});
    pre_idle    -> pre_idle({GenWorkerEts, WorkerData});
    post_idle   -> post_idle({GenWorkerEts, WorkerData});
    pre_train   -> pre_train({GenWorkerEts, WorkerData});
    post_train  -> post_train({GenWorkerEts, WorkerData});
    pre_predict -> pre_predict({GenWorkerEts, WorkerData});
    post_predict-> post_predict({GenWorkerEts, WorkerData});
    update      -> update({GenWorkerEts, WorkerData})
  end.

get_this_client_ets(GenWorkerEts) -> 
  ets:lookup_element(GenWorkerEts, federated_client_ets, ?ETS_KEYVAL_VAL_IDX).

%% handshake with workers / server
init({GenWorkerEts, WorkerData}) ->
  % create an ets for this client and save it to generic worker ets
  FedratedClientEts = ets:new(federated_client,[set]),
  ets:insert(GenWorkerEts, {federated_client_ets, FedratedClientEts}),
  {SyncMaxCount, MyName, ServerName} = WorkerData,
  % create fields in this ets
  ets:insert(FedratedClientEts, {my_name, MyName}),
  ets:insert(FedratedClientEts, {server_name, ServerName}),
  ets:insert(FedratedClientEts, {sync_max_count, SyncMaxCount}),
  ets:insert(FedratedClientEts, {sync_count, SyncMaxCount}),
  ets:insert(FedratedClientEts, {server_update, false}),
  io:format("finished init in ~p~n",[MyName]).

pre_idle({GenWorkerEts, _WorkerData}) ->
    ThisEts = get_this_client_ets(GenWorkerEts),
    %% send to server that this worker is part of the federated workers
    ClientPID = ets:lookup_element(GenWorkerEts, client_pid, ?ETS_KEYVAL_VAL_IDX),
    MyName = ets:lookup_element(ThisEts, my_name, ?ETS_KEYVAL_VAL_IDX),
    ServerName = ets:lookup_element(ThisEts, server_name, ?ETS_KEYVAL_VAL_IDX),
    gen_statem:cast(ClientPID,{custom_worker_message,{MyName, ServerName}}),
    io:format("sent ~p init message: ~p~n",[ServerName, {MyName, ServerName}]).

post_idle({_GenWorkerEts, _WorkerData}) -> ok.

%% set weights from fedserver
pre_train({GenWorkerEts, WorkerData}) -> ok.
  % ThisEts = get_this_client_ets(GenWorkerEts),
  % ToUpdate = ets:lookup_element(ThisEts, server_update, ?ETS_KEYVAL_VAL_IDX),
  % if ToUpdate ->
  %   ModelID = ets:lookup_element(GenWorkerEts, model_id, ?ETS_KEYVAL_VAL_IDX),
  %   nerlNIF:call_to_set_weights(ModelID, Weights);
  % true -> nothing
  % end.

%% every countLimit batches, send updated weights
post_train({GenWorkerEts, _WorkerData}) -> 
  ThisEts = get_this_client_ets(GenWorkerEts),
  SyncCount = ets:lookup_element(ThisEts, sync_count, ?ETS_KEYVAL_VAL_IDX),
  if SyncCount == 0 ->
    ModelID = ets:lookup_element(GenWorkerEts, model_id, ?ETS_KEYVAL_VAL_IDX),
    Weights = nerlNIF:call_to_get_weights(ModelID),
    ClientPID = ets:lookup_element(GenWorkerEts, client_pid, ?ETS_KEYVAL_VAL_IDX),
    ServerName = ets:lookup_element(ThisEts, server_name, ?ETS_KEYVAL_VAL_IDX),
    MyName = ets:lookup_element(GenWorkerEts, worker_name, ?ETS_KEYVAL_VAL_IDX),
    MaxSyncCount = ets:lookup_element(ThisEts, sync_max_count, ?ETS_KEYVAL_VAL_IDX),
    % io:format("Worker ~p entering update and got weights ~p~n",[MyName, Weights]),
    ets:update_counter(ThisEts, sync_count, MaxSyncCount),
    % io:format("Worker ~p entering update~n",[MyName]),
    gen_statem:cast(ClientPID, {update, {MyName, ServerName, Weights}}),
    ToUpdate = true;
  true ->
    ets:update_counter(ThisEts, sync_count, -1),
    ToUpdate = false
  end.

%% nothing?
pre_predict({GenWorkerEts, WorkerData}) -> WorkerData.

%% nothing?
post_predict(Data) -> Data.

%% gets weights from federated server
update({GenWorkerEts, NerlTensorWeights}) ->
  ThisEts = get_this_client_ets(GenWorkerEts),
  ModelID = ets:lookup_element(GenWorkerEts, model_id, ?ETS_KEYVAL_VAL_IDX),
  nerlNIF:call_to_set_weights(ModelID, NerlTensorWeights).
  % io:format("updated weights in worker ~p~n",[ets:lookup_element(GenWorkerEts, worker_name, ?ETS_KEYVAL_VAL_IDX)]).

%%------------------------------------------
% worker_event_polling(0) -> ?LOG_ERROR("worker event polling takes too long!");
% worker_event_polling(Weights) ->
%   if length(Weights) == 1 -> Weights;
%     length(Weights) > 1 -> ?LOG_ERROR("more than 1 messages pending!");
%     true -> %% wait for info to update
%       receive _ -> non   
%       after 1 -> worker_event_polling(T-1)
%       end
%   end.