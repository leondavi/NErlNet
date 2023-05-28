-module(workerFederatedClient).

-export([controller/2]).

-include("/usr/local/lib/nerlnet-lib/NErlNet/src_erl/Communication_Layer/http_Nerlserver/src/nerl_tools.hrl").

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

controller(FuncName, Data) -> 
  case FuncName of
    init -> init(Data);
    pre_train -> pre_train(Data);
    post_train -> post_train(Data);
    pre_predict -> pre_predict(Data);
    post_predict -> post_predict(Data)
  end.

%% handshake with workers / server
init(Data) -> 
    ServerName = Data#workerFederatedClient.serverName,
    ClientPID = Data#workerFederatedClient.clientPID,
    MyName = Data#workerFederatedClient.myName,
    gen_statem:cast(ClientPID,{init,MyName, ServerName}).   %% send to client that this worker is part of the federated workers

%% every countLimit batches, get updated model
pre_train(Data) -> 
    NewSyncCount = Data#workerFederatedClient.syncCount - 1,
    Data#workerFederatedClient{syncCount = NewSyncCount}.

%% every countLimit batches, send updated weights
post_train(Data) -> 
    SyncCount = Data#workerFederatedClient.syncCount,
    if SyncCount == 0 ->
        send_my_weights, get_new_weights;
    true -> cont end.

%% nothing?
pre_predict(Data) -> Data.

%% nothing?
post_predict(Data) -> Data.
