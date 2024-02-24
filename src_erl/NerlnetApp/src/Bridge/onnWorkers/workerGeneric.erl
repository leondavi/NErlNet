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
-include("/usr/local/lib/nerlnet-lib/NErlNet/src_erl/NerlnetApp/src/nerl_tools.hrl").
-include("/usr/local/lib/nerlnet-lib/NErlNet/src_erl/NerlnetApp/src/Bridge/nerlTensor.hrl").

-behaviour(gen_statem).

%% API
-export([start_link/1]).
%% gen_statem callbacks
-export([init/1, format_status/2, state_name/3, handle_event/4, terminate/3,
  code_change/4, callback_mode/0]).
%% States functions
-export([idle/3, train/3, predict/3, wait/3, update/3]).

%% ackClient :: need to tell mainserver that worker is safe and going to new state after wait state 

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
%% distributedBehaviorFunc is the special behavior of the worker regrading the distributed system e.g. federated client/server
init({WorkerName , WorkerArgs , DistributedBehaviorFunc , DistributedWorkerData , ClientPid , WorkerStatsEts}) -> 
  nerl_tools:setup_logger(?MODULE),
  {ModelID , ModelType , LayersSizes, LayersTypes, LayersFunctionalityCodes, LearningRate , Epochs, 
   OptimizerType, OptimizerArgs , LossMethod , DistributedSystemType , DistributedSystemArgs} = WorkerArgs,
  GenWorkerEts = ets:new(generic_worker,[set]),
  put(generic_worker_ets, GenWorkerEts),
  put(client_pid, ClientPid),
  put(worker_stats_ets , WorkerStatsEts),
  SourceBatchesEts = ets:new(source_batches,[set]),
  put(source_batches_ets, SourceBatchesEts),
  ets:insert(GenWorkerEts,{worker_name, WorkerName}),
  ets:insert(GenWorkerEts,{model_id, ModelID}),
  ets:insert(GenWorkerEts,{model_type, ModelType}),
  ets:insert(GenWorkerEts,{layers_types, LayersTypes}),
  ets:insert(GenWorkerEts,{layers_sizes, LayersSizes}),
  ets:insert(GenWorkerEts,{layers_functionality_codes, LayersFunctionalityCodes}),
  ets:insert(GenWorkerEts,{loss_method, LossMethod}),
  ets:insert(GenWorkerEts,{learning_rate, LearningRate}),
  ets:insert(GenWorkerEts,{epochs, Epochs}),
  ets:insert(GenWorkerEts,{optimizer, OptimizerType}),
  ets:insert(GenWorkerEts,{optimizer_args, OptimizerArgs}),
  ets:insert(GenWorkerEts,{distributed_system_args, DistributedSystemArgs}),
  ets:insert(GenWorkerEts,{distributed_system_type, DistributedSystemType}),
  ets:insert(GenWorkerEts,{controller_message_q, []}), %% empty Queue

  Res = nerlNIF:new_nerlworker_nif(ModelID , ModelType, LayersSizes, LayersTypes, LayersFunctionalityCodes, LearningRate, Epochs, OptimizerType,
                                OptimizerArgs, LossMethod , DistributedSystemType , DistributedSystemArgs),
  DistributedBehaviorFunc(init,{GenWorkerEts, DistributedWorkerData}),

  if 
    Res == ok -> nif_created_successfully;
    true -> nif_failed_to_create,
            ?LOG_ERROR("Failed to create worker ~p\n",[WorkerName]),
            exit(nif_failed_to_create)
  end,

  {ok, idle, #workerGeneric_state{myName = WorkerName , modelID = ModelID , distributedBehaviorFunc = DistributedBehaviorFunc , distributedWorkerData = DistributedWorkerData}}.

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

%% Event from clientStatem
idle(cast, {pre_idle}, State = #workerGeneric_state{myName = _MyName,distributedBehaviorFunc = DistributedBehaviorFunc}) ->
  DistributedBehaviorFunc(pre_idle, {get(generic_worker_ets), empty}),
  {next_state, idle, State};

%% Event from clientStatem
idle(cast, {post_idle, From}, State = #workerGeneric_state{myName = _MyName,distributedBehaviorFunc = DistributedBehaviorFunc}) ->
  DistributedBehaviorFunc(post_idle, {get(generic_worker_ets), From}),
  {next_state, idle, State};

% Go from idle to train
idle(cast, {training}, State = #workerGeneric_state{myName = MyName}) ->
  worker_controller_empty_message_queue(),
  update_client_avilable_worker(MyName),
  {next_state, train, State#workerGeneric_state{lastPhase = train}};

% Go from idle to predict
idle(cast, {predict}, State = #workerGeneric_state{myName = MyName}) ->
  worker_controller_empty_message_queue(),
  update_client_avilable_worker(MyName),
  {next_state, predict, State#workerGeneric_state{lastPhase = predict}};

idle(cast, _Param, State) ->
  % io:fwrite("Same state idle, command: ~p\n",[Param]),
  {next_state, idle, State}.

%% Waiting for receiving results or loss function
%% Got nan or inf from loss function - Error, loss function too big for double
wait(cast, {loss , nan , TimeNIF , BatchID , SourceName}, State = #workerGeneric_state{myName = MyName, nextState = NextState}) ->
  ?LOG_WARNING("~p , BatchID ~p training loss value in nan" , [MyName, BatchID]),
  stats:increment_by_value(get(worker_stats_ets), nan_loss_count, 1),
  gen_statem:cast(get(client_pid),{loss, MyName , SourceName ,nan , TimeNIF ,BatchID}), %% TODO send to tal stop casting request with error desc
  {next_state, NextState, State};

wait(cast, {loss, LossVal , TimeNIF , BatchID , SourceName}, State = #workerGeneric_state{myName = MyName, nextState = NextState, modelID=_ModelID, distributedBehaviorFunc = DistributedBehaviorFunc, distributedWorkerData = DistributedWorkerData}) ->
  gen_statem:cast(get(client_pid),{loss, MyName, SourceName ,LossVal , TimeNIF , BatchID}), %% TODO Add Time and Time_NIF to the cast
  ToUpdate = DistributedBehaviorFunc(post_train, {get(generic_worker_ets),DistributedWorkerData}),
  if  ToUpdate -> {next_state, update, State#workerGeneric_state{nextState=NextState}};
      true ->     {next_state, NextState, State}
  end;

wait(cast, {predictRes,PredNerlTensor, Type, TimeNIF, BatchID , SourceName}, State = #workerGeneric_state{myName = MyName, nextState = NextState, distributedBehaviorFunc = DistributedBehaviorFunc, distributedWorkerData = DistributedWorkerData}) ->
  BatchTimeStamp = erlang:system_time(nanosecond),
  gen_statem:cast(get(client_pid),{predictRes,MyName,SourceName, {PredNerlTensor, Type}, TimeNIF , BatchID , BatchTimeStamp}), %% TODO TODO change csv name and batch id(1)
  Update = DistributedBehaviorFunc(post_predict, {get(generic_worker_ets),DistributedWorkerData}),
  if Update -> 
    {next_state, update, State#workerGeneric_state{nextState=NextState}};
  true ->
    {next_state, NextState, State}
  end;

wait(cast, {idle}, State) ->
  %logger:notice("Waiting, next state - idle"),
  {next_state, wait, State#workerGeneric_state{nextState = idle}};

wait(cast, {training}, State) ->
  %logger:notice("Waiting, next state - train"),
  % gen_statem:cast(ClientPid,{stateChange,WorkerName}),
  {next_state, wait, State#workerGeneric_state{nextState = train}};

wait(cast, {predict}, State) ->
  %logger:notice("Waiting, next state - predict"),
  {next_state, wait, State#workerGeneric_state{nextState = predict}};

%% Worker in wait can't treat incoming message 
wait(cast, _BatchData , State = #workerGeneric_state{lastPhase = LastPhase}) ->
  case LastPhase of
    train -> 
      ets:update_counter(get(worker_stats_ets), batches_dropped_train , 1);
    predict -> 
      ets:update_counter(get(worker_stats_ets), batches_dropped_predict , 1)
  end,
  {next_state, wait, State};

wait(cast, Data, State) ->
  % logger:notice("worker ~p in wait cant treat message: ~p\n",[ets:lookup_element(get(generic_worker_ets), worker_name, ?ETS_KEYVAL_VAL_IDX), Data]),
  worker_controller_message_queue(Data),
  {keep_state, State}.

%% treated runaway message in nerlNIF:call_to_fet_weights
% update(info, Data, State) ->
%   ?LOG_NOTICE(?LOG_HEADER++"Worker ~p got data thru info: ~p\n",[ets:lookup_element(get(generic_worker_ets), worker_name, ?ETS_KEYVAL_VAL_IDX), Data]),
%   ?LOG_INFO("Worker ets is: ~p",[ets:match_object(get(generic_worker_ets), {'$0', '$1'})]),
%   {keep_state, State};

%% TODO FIX CONTROLLER
update(cast, {update, _From, NerltensorWeights}, State = #workerGeneric_state{distributedBehaviorFunc = DistributedBehaviorFunc, nextState = NextState}) ->
  ?LOG_WARNING("************* Unrecognized update method , next state: ~p **************" , [NextState]),
  DistributedBehaviorFunc(update, {get(generic_worker_ets), NerltensorWeights}),
  {next_state, NextState, State};

%% Worker updates its' client that it is available (in idle state)
update(cast, {idle}, State = #workerGeneric_state{myName = MyName}) ->
  update_client_avilable_worker(MyName),
  {next_state, idle, State#workerGeneric_state{nextState = idle}};
    

%% TODO Guy MOVE THIS FUNCTION TO CONTROLLER
update(cast, Data, State = #workerGeneric_state{distributedBehaviorFunc = DistributedBehaviorFunc, nextState = NextState}) ->
  % io:format("worker ~p got ~p~n",[ets:lookup_element(get(generic_worker_ets), worker_name, ?ETS_KEYVAL_VAL_IDX), Data]),
  case Data of
    %% FedClient update avg weights
    {update, "server", _Me, NerltensorWeights} -> 
      DistributedBehaviorFunc(update, {get(generic_worker_ets), NerltensorWeights}),
      % io:format("worker ~p updated model and going to ~p state~n",[ets:lookup_element(get(generic_worker_ets), worker_name, ?ETS_KEYVAL_VAL_IDX), NextState]),
      {next_state, NextState, State};
    %% FedServer get weights from clients
    {update, WorkerName, Me, NerlTensorWeights} ->
      StillUpdate = DistributedBehaviorFunc(update, {get(generic_worker_ets), {WorkerName, Me, NerlTensorWeights}}),
      if StillUpdate -> 
        % io:format("worker ~p in update waiting to go to ~p state~n",[ets:lookup_element(get(generic_worker_ets), worker_name, ?ETS_KEYVAL_VAL_IDX), NextState]),
        {keep_state, State#workerGeneric_state{nextState=NextState}};
      true ->
        {next_state, NextState, State#workerGeneric_state{}}
      end;
    %% got sample from source. discard and add missed count TODO: add to Q
    {sample, _Tensor} ->  
        %%ets:update_counter(get(generic_worker_ets), missedBatches, 1),
        {keep_state, State}
  end.


%% State train
train(cast, {sample, BatchID ,{<<>>, _Type}}, State) ->
  ?LOG_WARNING("Empty sample received , batch id: ~p",[BatchID]),
  WorkerStatsEts = get(worker_stats_ets),
  stats:increment_by_value(WorkerStatsEts , empty_batches , 1),
  {next_state, train, State#workerGeneric_state{nextState = train , currentBatchID = BatchID}};
  
%% Change SampleListTrain to NerlTensor
train(cast, {sample, SourceName ,BatchID ,{NerlTensorOfSamples, NerlTensorType}}, State = #workerGeneric_state{modelID = ModelId, distributedBehaviorFunc = DistributedBehaviorFunc, distributedWorkerData = DistributedWorkerData}) ->
    % NerlTensor = nerltensor_conversion({NerlTensorOfSamples, Type}, erl_float),
    MyPid = self(),
    NewWorkerData = DistributedBehaviorFunc(pre_train, {get(generic_worker_ets),DistributedWorkerData}),
    WorkersStatsEts = get(worker_stats_ets),
    stats:increment_by_value(WorkersStatsEts , batches_received_train , 1),
    _Pid = spawn(fun()-> nerlNIF:call_to_train(ModelId , {NerlTensorOfSamples, NerlTensorType} ,MyPid , BatchID , SourceName) end),
    {next_state, wait, State#workerGeneric_state{nextState = train, distributedWorkerData = NewWorkerData , currentBatchID = BatchID}};
  
%% TODO: implement send model and weights by demand (Tensor / XML)
train(cast, {set_weights,Ret_weights_list}, State = #workerGeneric_state{modelID = ModelId}) ->
  %% Set weights
  %io:format("####sending new weights to workers####~n"),
  nerlNIF:call_to_set_weights(ModelId, Ret_weights_list), %% TODO wrong usage
  %logger:notice("####end set weights train####~n"),
  {next_state, train, State};


train(cast, {idle}, State = #workerGeneric_state{myName = MyName}) ->
  update_client_avilable_worker(MyName),
  {next_state, idle, State};

train(cast, Data, State) ->
  % logger:notice("worker ~p in wait cant treat message: ~p\n",[ets:lookup_element(get(generic_worker_ets), worker_name, ?ETS_KEYVAL_VAL_IDX), Data]),
  worker_controller_message_queue(Data),
  {keep_state, State}.

%% State predict
predict(cast, {sample,_CSVname, BatchID, {<<>>, _Type}}, State) ->
  ?LOG_WARNING("Received empty tensor , batch id: ~p",[BatchID]),
  WorkersStatsEts = get(worker_stats_ets),
  stats:increment_bad_messages(WorkersStatsEts),
  {next_state, predict, State#workerGeneric_state{nextState = predict , currentBatchID = BatchID}};

% send predict sample to worker
predict(cast, {sample , SourceName , BatchID , {PredictBatchTensor, Type}}, State = #workerGeneric_state{modelID = ModelId, distributedBehaviorFunc = DistributedBehaviorFunc, distributedWorkerData = DistributedWorkerData}) ->
    CurrPID = self(),
    DistributedBehaviorFunc(pre_predict, {get(generic_worker_ets),DistributedWorkerData}),
    WorkersStatsEts = get(worker_stats_ets),
    stats:increment_by_value(WorkersStatsEts , batches_received_predict , 1),
    %% io:format("Pred Tensor: ~p~n",[nerlNIF:nerltensor_conversion({PredictBatchTensor , Type} , nerlNIF:erl_type_conversion(Type))]),
    _Pid = spawn(fun()-> nerlNIF:call_to_predict(ModelId , {PredictBatchTensor, Type} , CurrPID , BatchID, SourceName) end),
    {next_state, wait, State#workerGeneric_state{nextState = predict , currentBatchID = BatchID}};
  
predict(cast, {idle}, State = #workerGeneric_state{myName = MyName}) ->
  update_client_avilable_worker(MyName),
  {next_state, idle, State};

predict(cast, Data, State) ->
  worker_controller_message_queue(Data),
  {next_state, predict, State}.

%% Updates the client that worker is available
update_client_avilable_worker(MyName) -> 
  gen_statem:cast(get(client_pid),{stateChange,MyName}).

worker_controller_message_queue(ReceiveData) ->
    Queue = ets:lookup_element(get(generic_worker_ets), controller_message_q, ?ETS_KEYVAL_VAL_IDX),
    ets:update_element(get(generic_worker_ets), controller_message_q, {?ETS_KEYVAL_VAL_IDX , Queue++[ReceiveData]}).

worker_controller_empty_message_queue() ->
    ets:update_element(get(generic_worker_ets), controller_message_q, {?ETS_KEYVAL_VAL_IDX , []}).
