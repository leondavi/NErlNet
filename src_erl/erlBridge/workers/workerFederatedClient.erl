-module(workerFederatedClient).

-export([controller/2]).

-include("/usr/local/lib/nerlnet-lib/NErlNet/src_erl/Communication_Layer/http_Nerlserver/src/nerl_tools.hrl").
-include("workerDefinitions.hrl").

-define(WORKER_FEDERATED_CLIENT_ETS_FIELDS, [my_name, client_pid, server_name, sync_max_count, sync_count]).

% %% Federated mode
% wait(cast, {loss, {LOSS_FUNC,Time_NIF}}, State = #workerGeneric_state{clientPid = ClientPid,ackClient = AckClient, myName = MyName, nextState = NextState, count = Count, countLimit = CountLimit, modelId = Mid}) ->
%   % {LOSS_FUNC,_TimeCpp} = LossAndTime,
%   if Count == CountLimit ->
%       % Get weights
%       Ret_weights = nerlNIF:call_to_get_weights(Mid),
%       % Ret_weights_tuple = niftest:call_to_get_weights(Mid),
%       % {Wheights,Bias,Biases_sizes_list,Wheights_sizes_list} = Ret_weights_tuple,

%       % ListToSend = [Wheights,Bias,Biases_sizes_list,Wheights_sizes_list],

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
    init -> init({GenWorkerEts, WorkerData});
    pre_train -> pre_train({GenWorkerEts, WorkerData});
    post_train -> post_train({GenWorkerEts, WorkerData});
    pre_predict -> pre_predict({GenWorkerEts, WorkerData});
    post_predict -> post_predict({GenWorkerEts, WorkerData})
  end.

get_this_client_ets(GenWorkerEts) -> 
  ets:lookup_element(GenWorkerEts, federated_client_ets, ?ETS_KEYVAL_VAL_IDX).

%% handshake with workers / server
init({GenWorkerEts, WorkerData}) ->
  % create an ets for this client and save it to generic worker ets
  FedratedClientEts = ets:new(federated_client,[set]),
  ets:insert(GenWorkerEts, {fedrated_client_ets, FedratedClientEts}),
  #workerFederatedClient{myName = MyName,clientPid = ClientPid, serverName = ServerName} = WorkerData,
  % create fields in this ets
  ets:insert(my_name, MyName),
  ets:insert(client_pid, ClientPid),
  ets:insert(server_name, ServerName)
  ets:insert(sync_max_count, 0),
  ets:insert(sync_count, 0),
  gen_statem:cast(ClientPID,{custom_worker_message,{MyName, ServerName}}).   %% send to client that this worker is part of the federated workers

%% every countLimit batches, get updated model
pre_train({GenWorkerEts, WorkerData}) -> 
  ThisEts = get_this_client_ets(GenWorkerEts),
  ets:update_counter(ThisEts, sync_count, -1), ok.

%% every countLimit batches, send updated weights
post_train({GenWorkerEts, WorkerData}) -> 
  ThisEts = get_this_client_ets(GenWorkerEts),
  SyncCount = ets:lookup_element(ThisEts, sync_count, ?ETS_KEYVAL_VAL_IDX),
  if SyncCount == 0 ->
      send_my_weights, get_new_weights;
  true -> cont end.

%% nothing?
pre_predict({GenWorkerEts, WorkerData}) -> Data.

%% nothing?
post_predict(Data) -> Data.
