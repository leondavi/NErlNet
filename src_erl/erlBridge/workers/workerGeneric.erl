-module(workerGeneric).
-include("workerDefinitions.hrl").
%%%-------------------------------------------------------------------
%%% @copyright (C) 2023, Nerlnet
%%% @doc
%%%
%%% @end
%%% Created : 07. May 2023 21:58
%%%-------------------------------------------------------------------
-import(nerlNIF,[decode_nif/2, nerltensor_binary_decode/2]).
-import(nerlNIF,[encode_nif/2, nerltensor_encode/5, nerltensor_conversion/2, get_all_binary_types/0]).
-import(nerlNIF,[erl_type_conversion/1]).
-include("/usr/local/lib/nerlnet-lib/NErlNet/src_erl/Communication_Layer/http_Nerlserver/src/nerl_tools.hrl").
-include("/usr/local/lib/nerlnet-lib/NErlNet/src_erl/erlBridge/nerlTensor.hrl").

-behaviour(gen_statem).

%% API
-export([start_link/1]).
%% gen_statem callbacks
-export([init/1, format_status/2, state_name/3, handle_event/4, terminate/3,
  code_change/4, callback_mode/0]).
%% States functions
-export([idle/3, train/3, predict/3, wait/3, update/3]).

%% countLimit - Number of samples to count before sending the weights for averaging. Predifined in the json file.
%% count - Number of samples recieved for training after the last weights sended.

%%%===================================================================
%%% API
%%%===================================================================

%% @doc Creates a gen_statem process which calls Module:init/1 to
%% initialize. To ensure a synchronized start-up procedure, this
%% function does not return until Module:init/1 has returned.
start_link(ARGS) ->
  %{ok,Pid} = gen_statem:start_link({local, element(1, ARGS)}, ?MODULE, ARGS, []),   %% name this machine by unique name
  {ok,Pid} = gen_statem:start_link(?MODULE, ARGS, []),
  Pid.

%%%===================================================================
%%% gen_statem callbacks
%%%===================================================================

%% @private
%% @doc Whenever a gen_statem is started using gen_statem:start/[3,4] or
%% gen_statem:start_link/[3,4], this function is called by the new process to initialize.
init({WorkerName,ModelId, ModelType, ScalingMethod,LayerTypesList,LayersSizes,LayersActivationFunctions,Optimizer, LossMethod, LearningRate, ClientPID, Func, WorkerData}) ->
  nerl_tools:setup_logger(?MODULE),

  GenWorkerEts = ets:new(generic_worker,[set]),
  ets:insert(GenWorkerEts,{worker_name, WorkerName}),
  ets:insert(GenWorkerEts,{client_pid, ClientPID}),
  ets:insert(GenWorkerEts,{model_id, ModelId}),
  ets:insert(GenWorkerEts,{model_type, ModelType}),
  ets:insert(GenWorkerEts,{layer_types_list, LayerTypesList}),
  ets:insert(GenWorkerEts,{layers_sizes, LayersSizes}),
  ets:insert(GenWorkerEts,{layers_activation_functions, LayersActivationFunctions}),
  put(generic_worker_ets, GenWorkerEts),
  put(client_pid, ClientPID),

  Res=nerlNIF:create_nif(ModelId, ModelType , ScalingMethod , LayerTypesList , LayersSizes , LayersActivationFunctions),
  Func(init,{GenWorkerEts, WorkerData}),

  ?LOG_NOTICE("Res = ~p ~n",[Res]),

  {ok, idle, #workerGeneric_state{myName = WorkerName, modelId = ModelId,optimizer = Optimizer,learningRate = LearningRate, lossMethod = LossMethod, customFunc = Func, workerData = WorkerData}}.

%% @private
%% @doc This function is called by a gen_statem when it needs to find out
%% the callback mode of the callback module.
callback_mode() ->
  state_functions.

%% @private
%% @doc Called (1) whenever sys:get_status/1,2 is called by gen_statem or
%% (2) when gen_statem terminates abnormally.
%% This callback is optional.
format_status(_Opt, [_PDict, _StateName, _State]) ->
  Status = some_term,
  Status.

%% @private
%% @doc There should be one instance of this function for each possible
%% state name.  If callback_mode is state_functions, one of these
%% functions is called when gen_statem receives and event from
%% call/2, cast/2, or as a normal process message.
state_name(_EventType, _EventContent, State = #workerGeneric_state{}) ->
  NextStateName = next_state,
  {next_state, NextStateName, State}.

%% @private
%% @doc If callback_mode is handle_event_function, then whenever a
%% gen_statem receives an event from call/2, cast/2, or as a normal
%% process message, this function is called.
handle_event(_EventType, _EventContent, _StateName, State = #workerGeneric_state{}) ->
  NextStateName = the_next_state_name,
  {next_state, NextStateName, State}.

%% @private
%% @doc This function is called by a gen_statem when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_statem terminates with
%% Reason. The return value is ignored.
terminate(_Reason, _StateName, _State) ->
  ok.

%% @private
%% @doc Convert process state when code is changed
code_change(_OldVsn, StateName, State = #workerGeneric_state{}, _Extra) ->
  {ok, StateName, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%% State idle

%% got init from FedWorker, add it to workersList
idle(cast, {pre_idle}, State = #workerGeneric_state{myName = MyName,customFunc = Func}) ->
  io:format("worker ~p got pre_idle signal~n",[MyName]),
  Func(pre_idle, {get(generic_worker_ets), empty}),
  {next_state, idle, State};

idle(cast, {post_idle, From}, State = #workerGeneric_state{myName = MyName,customFunc = Func}) ->
  io:format("worker ~p got post_idle signal~n",[MyName]),
  Func(post_idle, {get(generic_worker_ets), From}),
  {next_state, idle, State};

idle(cast, {training}, State = #workerGeneric_state{myName = MyName,clientPid = ClientPid}) ->
  ?LOG_NOTICE("~p Go from idle to train!\n",[MyName]),
  gen_statem:cast(get(client_pid),{stateChange,MyName}),
  {next_state, train, State};

idle(cast, {predict}, State = #workerGeneric_state{myName = MyName,clientPid = ClientPid}) ->
  ?LOG_NOTICE("Go from idle to predict\n"),
  gen_statem:cast(get(client_pid),{stateChange,MyName}),
  {next_state, predict, State#workerGeneric_state{nextState = predict}};

idle(cast, {set_weights,Ret_weights_list}, State = #workerGeneric_state{modelId=_ModelId}) ->

  ?LOG_NOTICE("Set weights in wait state: \n"),
  nerlNIF:call_to_set_weights(_ModelId, Ret_weights_list),
  
  % %% Set weights TODO maybe send the results of the update
  % [WeightsList, BiasList, Biases_sizes_list, Wheights_sizes_list] = Ret_weights_list,

  % %% Make bias sizes and weights sizes as integer 
  % NewBiases_sizes_list = [round(X)||X<-Biases_sizes_list],
  % NewWheights_sizes_list = [round(X)||X<-Wheights_sizes_list],
  % _Result_set_weights = niftest:set_weights_nif(WeightsList, BiasList, NewBiases_sizes_list, NewWheights_sizes_list, ModelId),
  % _Result_set_weights2 = niftest:set_weights_nif(WeightsList, BiasList, Biases_sizes_list, Wheights_sizes_list, ModelId),
 %io:format("####sending new weights to workers####~n"),
  %niftest:call_to_set_weights(ModelId, Ret_weights_list), niftest is depracated - use nerlNIF instead
  ?LOG_NOTICE("####end set weights idle####~n"),

  {next_state, idle, State};
 
idle(cast, _Param, State) ->
  % io:fwrite("Same state idle, command: ~p\n",[Param]),
  {next_state, idle, State}.

%% Waiting for receiving results or loss function
%% Got nan or inf from loss function - Error, loss function too big for double
wait(cast, {loss,nan,Time_NIF}, State = #workerGeneric_state{clientPid = ClientPid, myName = MyName, nextState = NextState,ackClient = AckClient}) ->
  ?LOG_NOTICE("Loss func in wait: nan (Loss function too big for double)\n"),
  gen_statem:cast(get(client_pid),{loss, MyName, nan,Time_NIF}), %% TODO send to tal stop casting request with error desc
  checkAndAck(MyName,get(client_pid),AckClient),

  {next_state, NextState, State#workerGeneric_state{ackClient = 0}};


wait(cast, {loss, {LossVal,Time}}, State = #workerGeneric_state{clientPid = ClientPid, myName = MyName, nextState = NextState, modelId=_ModelID,ackClient = AckClient, customFunc = CustomFunc, workerData = WorkerData}) ->
  gen_statem:cast(get(client_pid),{loss, MyName, LossVal,Time/1000}), %% TODO Add Time and Time_NIF to the cast
  Update = CustomFunc(post_train, {get(generic_worker_ets),WorkerData}),
  checkAndAck(MyName,get(client_pid),AckClient),
  if Update -> 
    {next_state, update, State#workerGeneric_state{ackClient = 0, nextState=NextState}};
  true ->
    {next_state, NextState, State#workerGeneric_state{ackClient = 0}}
  end;

wait(cast, {predictRes,NerlTensor, Type, TimeTook, CSVname,BatchID}, State = #workerGeneric_state{myName = MyName, clientPid = ClientPid, nextState = NextState,ackClient = AckClient, customFunc = CustomFunc, workerData = WorkerData}) ->
  gen_statem:cast(get(client_pid),{predictRes,MyName, CSVname,BatchID, NerlTensor, Type, TimeTook}), %% TODO TODO change csv name and batch id(1)
  Update = CustomFunc(post_predict, {get(generic_worker_ets),WorkerData}),
  checkAndAck(MyName,get(client_pid),AckClient),
  if Update -> 
    {next_state, update, State#workerGeneric_state{ackClient = 0, nextState=NextState}};
  true ->
    {next_state, NextState, State#workerGeneric_state{ackClient = 0}}
  end;

wait(cast, {idle}, State) ->
  %logger:notice("Waiting, next state - idle"),
  {next_state, wait, State#workerGeneric_state{nextState = idle,ackClient=1}};

wait(cast, {training}, State) ->
  %logger:notice("Waiting, next state - train"),
  % gen_statem:cast(ClientPid,{stateChange,WorkerName}),
  {next_state, wait, State#workerGeneric_state{nextState = train,ackClient=1}};

wait(cast, {predict}, State) ->
  %logger:notice("Waiting, next state - predict"),
  {next_state, wait, State#workerGeneric_state{nextState = predict,ackClient=1}};

wait(cast, {sample, _SampleListTrain}, State = #workerGeneric_state{missedSamplesCount = MissedSamplesCount, missedTrainSamples = _MissedTrainSamples}) ->
  ?LOG_NOTICE(?LOG_HEADER++"Missed in pid: ~p, Missed batches count: ~p\n",[self(), MissedSamplesCount]),
  % Miss = MissedTrainSamples++SampleListTrain,
  {next_state, wait, State#workerGeneric_state{missedSamplesCount = MissedSamplesCount+1}};

wait(cast, {sample,_CSVname, _BatchID, _SampleListPredict}, State = #workerGeneric_state{missedSamplesCount = MissedSamplesCount, missedTrainSamples = _MissedTrainSamples}) ->
  % throw("got sample while calculating"),
  ?LOG_NOTICE(?LOG_HEADER++"Missed in pid: ~p, Missed batches count: ~p\n",[self(), MissedSamplesCount]),
  % ?LOG_NOTICE(?LOG_HEADER++"Missed in pid: ~p, Missed Samples: ~p\n",[self(), SampleListPredict]),
  
  % Miss = MissedTrainSamples++SampleListTrain,
  {next_state, wait, State#workerGeneric_state{missedSamplesCount = MissedSamplesCount+1}};

wait(cast, Param, State) ->
  logger:notice("worker Not supposed to be. Got: ~p\n",[Param]),
  {next_state, wait, State}.

update(cast, Data, State = #workerGeneric_state{modelId = ModelId, customFunc = CustomFunc, nextState = NextState}) ->
  CustomFunc(update, {get(generic_worker_ets), Data}),
{next_state, NextState, State}.

%% State train
train(cast, {sample, {<<>>, _Type}}, State ) ->
  ?LOG_ERROR("Empty sample received"),
  {next_state, train, State#workerGeneric_state{nextState = train}};
  
%% Change SampleListTrain to NerlTensor
train(cast, {sample, {NerlTensorOfSamples, Type}}, State = #workerGeneric_state{modelId = ModelId, optimizer = Optimizer, lossMethod = LossMethod, learningRate = LearningRate, customFunc = CustomFunc, workerData = WorkerData}) ->
    % io:format("Got Tensor: {~p, ~p}~n",[NerlTensorOfSamples, Type]),
    % NerlTensor = nerltensor_conversion({NerlTensorOfSamples, Type}, erl_float),
    % io:format("Got NerlTensor: ~p~n",[NerlTensor]),
    MyPid = self(),
    NewWorkerData = CustomFunc(pre_train, {get(generic_worker_ets),WorkerData}),
    _Pid = spawn(fun()-> nerlNIF:call_to_train(ModelId, Optimizer , LossMethod , LearningRate , {NerlTensorOfSamples, Type} ,MyPid) end),
    {next_state, wait, State#workerGeneric_state{nextState = train, workerData = NewWorkerData}};
  
%% TODO: implement send model and weights by demand (Tensor / XML)
train(cast, {set_weights,Ret_weights_list}, State = #workerGeneric_state{modelId = ModelId}) ->
  %% Set weights
  %io:format("####sending new weights to workers####~n"),
  nerlNIF:call_to_set_weights(ModelId, Ret_weights_list),
  %logger:notice("####end set weights train####~n"),
  {next_state, train, State};


train(cast, {idle}, State = #workerGeneric_state{myName = MyName, clientPid = ClientPid}) ->
  %logger:notice("Go from train to idle\n"),
  gen_statem:cast(get(client_pid),{stateChange,MyName}),
  {next_state, idle, State};

train(cast, {predict}, State = #workerGeneric_state{myName = MyName, clientPid = ClientPid}) ->
  %logger:notice("Go from train to predict\n"),
  gen_statem:cast(get(client_pid),{stateChange,MyName}),
  {next_state, predict, State};

train(cast, _Param, State) ->
  % io:fwrite("Same state train, not supposed to be, command: ~p\n",[Param]),
  {next_state, train, State}.

%% State predict
predict(cast, {sample,_CSVname, _BatchID, {<<>>, _Type}}, State) ->
  ?LOG_ERROR("Received empty tensor"),
  {next_state, predict, State#workerGeneric_state{nextState = predict}};

% send predict sample to worker
predict(cast, {sample,CSVname, BatchID, {PredictBatchTensor, Type}}, State = #workerGeneric_state{ modelId = ModelId, customFunc = CustomFunc, workerData = WorkerData}) ->
    CurrPID = self(),
    CustomFunc(pre_predict, {get(generic_worker_ets),WorkerData}),
    _Pid = spawn(fun()-> nerlNIF:call_to_predict(ModelId,PredictBatchTensor, Type,CurrPID,CSVname, BatchID) end),
    {next_state, wait, State#workerGeneric_state{nextState = predict}};
  
predict(cast, {idle}, State = #workerGeneric_state{myName = MyName, clientPid = ClientPid}) ->
  %logger:notice("Go from predict to idle\n"),
  gen_statem:cast(get(client_pid),{stateChange,MyName}),

  {next_state, idle, State};

predict(cast, {training}, State = #workerGeneric_state{myName = MyName, clientPid = ClientPid}) ->
  %logger:notice("Go from predict to train\n"),
  gen_statem:cast(get(client_pid),{stateChange,MyName}),

  {next_state, train, State};

predict(cast, _Param, State) ->
  %logger:notice("Same state Predict, command: ~p\n",[Param]),
  {next_state, predict, State}.

%% Updates the client that worker is available
checkAndAck(_MyName,_ClientPid,0) -> ok_no_need;
checkAndAck(MyName, ClientPid, 1) -> gen_statem:cast(get(client_pid),{stateChange,MyName}).