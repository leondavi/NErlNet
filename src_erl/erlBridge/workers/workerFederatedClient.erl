-module(workerFederatedClient).


% %% Federated mode
% wait(cast, {loss, {LOSS_FUNC,Time_NIF}}, State = #workerNN_state{clientPid = ClientPid,ackClient = AckClient, myName = MyName, nextState = NextState, count = Count, countLimit = CountLimit, modelId = Mid, federatedMode = ?MODE_FEDERATED}) ->
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
%   end;
