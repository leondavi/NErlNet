-module(workerGeneric).
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
-import(w2wCom,[send_message/3, get_inbox_queue/1]).

% includes
-include("/usr/local/lib/nerlnet-lib/NErlNet/src_erl/NerlnetApp/src/Bridge/Common/workerDefinitions.hrl").
-include("/usr/local/lib/nerlnet-lib/NErlNet/src_erl/NerlnetApp/src/nerl_tools.hrl").
-include("/usr/local/lib/nerlnet-lib/NErlNet/src_erl/NerlnetApp/src/Bridge/nerlTensor.hrl").

-behaviour(gen_statem).

%% API
-export([start_link/1]).
%% gen_statem callbacks
-export([init/1, format_status/2, state_name/3, handle_event/4, terminate/3,
  code_change/4, callback_mode/0]).
%% States functions
-export([idle/3, train/3, predict/3, wait/3]).

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
init({WorkerName , WorkerArgs , DistributedBehaviorFunc , DistributedWorkerData , ClientPid , WorkerStatsEts , W2WPid}) -> 
  nerl_tools:setup_logger(?MODULE),
  {ModelID , ModelType , ModelArgs , LayersSizes,
  LayersTypes, LayersFunctionalityCodes, LearningRate , Epochs, 
  OptimizerType, OptimizerArgs , LossMethod , LossArgs, InfraType, DistributedSystemType ,
  DistributedSystemToken, DistributedSystemArgs, TrainParams} = WorkerArgs,
  GenWorkerEts = ets:new(generic_worker,[set, public]),
  put(generic_worker_ets, GenWorkerEts),
  put(client_pid, ClientPid),
  put(worker_stats_ets , WorkerStatsEts),
  ets:insert(GenWorkerEts,{client_pid, ClientPid}),
  ets:insert(GenWorkerEts,{w2wcom_pid, W2WPid}),
  ets:insert(GenWorkerEts,{worker_name, WorkerName}),
  ets:insert(GenWorkerEts,{model_id, ModelID}),
  ets:insert(GenWorkerEts,{model_type, ModelType}),
  ets:insert(GenWorkerEts,{model_args, ModelArgs}),
  ets:insert(GenWorkerEts,{layers_types, LayersTypes}),
  ets:insert(GenWorkerEts,{layers_sizes, LayersSizes}),
  ets:insert(GenWorkerEts,{layers_functionality_codes, LayersFunctionalityCodes}),
  ets:insert(GenWorkerEts,{loss_method, LossMethod}),
  ets:insert(GenWorkerEts,{loss_args, LossArgs}),
  ets:insert(GenWorkerEts,{learning_rate, LearningRate}),
  ets:insert(GenWorkerEts,{epochs, Epochs}),
  ets:insert(GenWorkerEts,{optimizer, OptimizerType}),
  ets:insert(GenWorkerEts,{optimizer_args, OptimizerArgs}),
  ets:insert(GenWorkerEts,{distributed_system_args, DistributedSystemArgs}),
  ets:insert(GenWorkerEts,{distributed_system_token, DistributedSystemToken}),
  ets:insert(GenWorkerEts,{distributed_system_type, DistributedSystemType}),
  InfraModule = select_infra_module(InfraType),
  NormalizedInfraType = normalize_infra_type(InfraType),
  ets:insert(GenWorkerEts,{infra_type, InfraType}),
  ets:insert(GenWorkerEts,{nif_module, InfraModule}),
  ets:insert(GenWorkerEts,{train_params, TrainParams}),
  put(nif_module, InfraModule),
  ets:insert(GenWorkerEts,{controller_message_q, []}), %% TODO Deprecated
  ets:insert(GenWorkerEts,{handshake_done, false}),
  ets:insert(GenWorkerEts,{active_streams, []}),
  ets:insert(GenWorkerEts,{stream_occuring, false}),
  ets:insert(GenWorkerEts,{end_streams_waiting_list, []}), % Waiting list of messages from client to end_stream with source
  % Worker to Worker communication module - this is a gen_server

  Res = case NormalizedInfraType of
          torch ->
            nif_call(new_nerlworker_nif, [ModelID, DistributedSystemType, DistributedSystemArgs, TrainParams]);
          _ ->
            nif_call(new_nerlworker_nif, [ModelID , ModelType, ModelArgs, LayersSizes, LayersTypes, LayersFunctionalityCodes, LearningRate, Epochs, OptimizerType,
                                OptimizerArgs, LossMethod , LossArgs, DistributedSystemType , DistributedSystemArgs])
        end,
  DistributedBehaviorFunc(init,{GenWorkerEts, DistributedWorkerData}),

  if 
    Res == ok -> nif_created_successfully;
    true -> nif_failed_to_create,
            ?LOG_ERROR("Failed to create worker ~p\n",[WorkerName]),
            exit(nif_failed_to_create)
  end,
  DistributedBehaviorFunc(pre_idle,{GenWorkerEts, DistributedWorkerData}),
  %% Starting negotiators processes for train and predict
  WorkerPid = self(),
  nif_call(start_train_negotiator, [ModelID, WorkerPid]),
  nif_call(start_predict_negotiator, [ModelID, WorkerPid]),
  {ok, idle, #workerGeneric_state{myName = WorkerName , modelID = ModelID , distributedBehaviorFunc = DistributedBehaviorFunc , distributedWorkerData = DistributedWorkerData, postBatchFunc = ?EMPTY_FUNC, nifModule = InfraModule}}.

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
  % stop negotiators
  nif_call(stop_train_negotiator, []),
  nif_call(stop_predict_negotiator, []),
  ok.

%% @private
%% @doc Convert process state when code is changed
code_change(_OldVsn, StateName, State = #workerGeneric_state{}, _Extra) ->
  {ok, StateName, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%% State idle

% Go from idle to train
idle(cast, {training}, State = #workerGeneric_state{myName = MyName , distributedBehaviorFunc = DistributedBehaviorFunc}) ->
  % io:format("@idle got training , Worker ~p is going to state idle...~n",[MyName]),
  % update the phase in registry
  put(phase, training),
  ets:update_element(get(generic_worker_ets), active_streams, {?ETS_KEYVAL_VAL_IDX, []}),
  DistributedBehaviorFunc(post_idle, {get(generic_worker_ets), train}),
  update_client_avilable_worker(MyName),
  ?LOG_INFO("Worker ~p is switching to phase train",[MyName]),
  {next_state, train, State#workerGeneric_state{lastPhase = train}};

% Go from idle to predict
idle(cast, {predict}, State = #workerGeneric_state{myName = MyName , distributedBehaviorFunc = DistributedBehaviorFunc}) ->
  % worker_controller_empty_message_queue(),
  put(phase, predict),
  ets:update_element(get(generic_worker_ets), active_streams, {?ETS_KEYVAL_VAL_IDX, []}),
  DistributedBehaviorFunc(post_idle, {get(generic_worker_ets), predict}),
  update_client_avilable_worker(MyName),
  ?LOG_INFO("Worker ~p is switching to phase predict",[MyName]),
  {next_state, predict, State#workerGeneric_state{lastPhase = predict}};

idle(cast, _Param, State = #workerGeneric_state{myName = _MyName}) ->
  % io:format("@idle Worker ~p is going to state idle...~n",[MyName]),
  {next_state, idle, State}.

%% Waiting for receiving results or loss function
%% Got nan or inf from loss function - Error, loss function too big for double
wait(cast, {loss, nan , TrainTime , BatchID , SourceName}, State = #workerGeneric_state{myName = MyName, distributedBehaviorFunc = DistributedBehaviorFunc}) ->
  stats:increment_by_value(get(worker_stats_ets), nan_loss_count, 1),
  WorkerToken = ets:lookup_element(get(generic_worker_ets), distributed_system_token, ?ETS_KEYVAL_VAL_IDX),
  gen_statem:cast(get(client_pid),{loss, MyName , SourceName ,nan , TrainTime, WorkerToken ,BatchID}),
  NextStateBehavior = DistributedBehaviorFunc(post_train, {get(generic_worker_ets),[]}), %% First call sends empty list , then it will be updated by the federated server and clients
  handle_end_stream_waiting_list(DistributedBehaviorFunc, train),
  {next_state, NextStateBehavior, State};


wait(cast, {loss, {LossTensor, LossTensorType} , TrainTime , BatchID , SourceName}, State = #workerGeneric_state{myName = MyName, modelID=_ModelID, distributedBehaviorFunc = DistributedBehaviorFunc}) ->
  BatchTimeStamp = erlang:system_time(nanosecond),
  WorkerToken = ets:lookup_element(get(generic_worker_ets), distributed_system_token, ?ETS_KEYVAL_VAL_IDX),
  gen_statem:cast(get(client_pid),{loss, MyName, SourceName ,{LossTensor, LossTensorType} , TrainTime , WorkerToken, BatchID , BatchTimeStamp}),
  NextStateBehavior = DistributedBehaviorFunc(post_train, {get(generic_worker_ets),[]}), %% First call sends empty list , then it will be updated by the federated server and clients
  handle_end_stream_waiting_list(DistributedBehaviorFunc, train),
  {next_state, NextStateBehavior, State};

wait(cast, {predictRes, PredNerlTensor, PredNerlTensorType, TimeNif, BatchID , SourceName}, State = #workerGeneric_state{myName = MyName, nextState = NextState, distributedBehaviorFunc = DistributedBehaviorFunc, distributedWorkerData = DistributedWorkerData}) ->
  BatchTimeStamp = erlang:system_time(nanosecond),
  WorkerToken = ets:lookup_element(get(generic_worker_ets), distributed_system_token, ?ETS_KEYVAL_VAL_IDX),
  gen_statem:cast(get(client_pid),{predictRes,MyName, SourceName, {PredNerlTensor, PredNerlTensorType}, TimeNif , WorkerToken, BatchID , BatchTimeStamp}), 
  DistributedBehaviorFunc(post_predict, {get(generic_worker_ets),DistributedWorkerData}),
  handle_end_stream_waiting_list(DistributedBehaviorFunc, predict),
  {next_state, NextState, State};

wait(cast, {end_stream , StreamName}, State = #workerGeneric_state{myName = _MyName, distributedBehaviorFunc = DistributedBehaviorFunc}) ->
  %logger:notice("Waiting, next state - idle"),
  CurrentEndStreamWaitingList = ets:lookup_element(get(generic_worker_ets), end_streams_waiting_list, ?ETS_KEYVAL_VAL_IDX),
  NewEndStreamWaitingList = CurrentEndStreamWaitingList ++ [StreamName],
  % io:format("Got end_stream @wait: NewWaitingList: ~p~n",[NewEndStreamWaitingList]),
  ets:update_element(get(generic_worker_ets), end_streams_waiting_list, {?ETS_KEYVAL_VAL_IDX, NewEndStreamWaitingList}),
  % get phase from registry
  Phase = get(phase),
  handle_end_stream_waiting_list(DistributedBehaviorFunc, Phase),
  % io:format("@wait ~p got end stream from ~p~n",[MyName, StreamName]),
  {next_state, wait, State};

wait(cast, {post_train_update, Data}, State = #workerGeneric_state{myName = _MyName, distributedBehaviorFunc = DistributedBehaviorFunc}) ->
  NextStateBehavior = DistributedBehaviorFunc(post_train, {get(generic_worker_ets), {post_train_update, Data}}),
  if 
    NextStateBehavior == train -> 
      ok;
    true -> 
      ?LOG_ERROR("@wait: post_train controller method must return train atom!"),
      throw("@wait: post_train controller method must return train atom!")
  end,
  handle_end_stream_waiting_list(DistributedBehaviorFunc, train),
  {next_state, NextStateBehavior, State};

% This state happens when worker is busy with sample that sent by source X but source Y just its stream
wait(cast,  {start_stream , StreamName}, State = #workerGeneric_state{lastPhase = LastPhase, distributedBehaviorFunc = DistributedBehaviorFunc}) ->
    stream_handler(start_stream, LastPhase, StreamName, DistributedBehaviorFunc),
{keep_state, State};

% CANNOT HAPPEN 
wait(cast, {idle}, State= #workerGeneric_state{myName = MyName, distributedBehaviorFunc = DistributedBehaviorFunc}) ->
  %logger:notice("Waiting, next state - idle"),
  % io:format("@wait: Got idle message, next state - idle~n"),
  logger:warning("Worker ~p got idle message in wait state, going to idle state but this is an unexpected behavior",[MyName]),
  Phase = get(phase),
  DistributedBehaviorFunc(pre_idle, {get(generic_worker_ets), Phase}),
  update_client_avilable_worker(MyName),
  {next_state, idle, State#workerGeneric_state{nextState = idle}};

wait(cast, {training}, State) ->
  %logger:notice("Waiting, next state - train"),
  % gen_statem:cast(ClientPid,{stateChange,WorkerName}),
  {next_state, wait, State#workerGeneric_state{nextState = train}};

wait(cast, {predict}, State) ->
  %logger:notice("Waiting, next state - predict"),
  {next_state, wait, State#workerGeneric_state{nextState = predict}};

%% Worker in wait can't treat incoming message 
wait(cast, BatchTuple , State = #workerGeneric_state{lastPhase = LastPhase, myName= _MyName}) when element(1, BatchTuple) == sample ->
  % io:format("@wait: Dropped batch state...~n"),
  case LastPhase of
    train -> 
      ets:update_counter(get(worker_stats_ets), batches_dropped_train , 1);
    predict -> 
      ets:update_counter(get(worker_stats_ets), batches_dropped_predict , 1)
  end,
  {next_state, wait, State};

wait(cast, Data, State) ->
  % logger:notice("worker ~p in wait cant treat message: ~p\n",[ets:lookup_element(get(generic_worker_ets), worker_name, ?ETS_KEYVAL_VAL_IDX), Data]),
  ?LOG_ERROR("Got unknown message in wait state: ~p~n",[Data]),
  throw("Got unknown message in wait state"),
  {keep_state, State}.


%% State train
train(cast, {sample, BatchID ,{<<>>, _Type}}, State) ->
  ?LOG_WARNING("Empty sample received , batch id: ~p~n",[BatchID]),
  WorkerStatsEts = get(worker_stats_ets),
  stats:increment_by_value(WorkerStatsEts , empty_batches , 1),
  {next_state, train, State#workerGeneric_state{nextState = train , currentBatchID = BatchID}};
  
%% Change SampleListTrain to NerlTensor
train(cast, {sample, SourceName ,BatchID ,{NerlTensorOfSamples, NerlTensorType}}, State = #workerGeneric_state{modelID = ModelId, distributedBehaviorFunc = DistributedBehaviorFunc, distributedWorkerData = DistributedWorkerData, myName = _MyName}) ->
    % NerlTensor = nerltensor_conversion({NerlTensorOfSamples, Type}, erl_float),
    DistributedBehaviorFunc(pre_train, {get(generic_worker_ets),DistributedWorkerData}), % Here the model can be updated by the federated server
    WorkersStatsEts = get(worker_stats_ets),
    stats:increment_by_value(WorkersStatsEts , batches_received_train , 1),
    nif_call(call_to_train, [ModelId , {NerlTensorOfSamples, NerlTensorType} , BatchID , SourceName]),
    {next_state, wait, State#workerGeneric_state{nextState = train, currentBatchID = BatchID}};
  
%% TODO: implement send model and weights by demand (Tensor / XML)
train(cast, {set_weights,Ret_weights_list}, State = #workerGeneric_state{modelID = ModelId}) ->
  %% Set weights
  nif_call(call_to_set_weights, [ModelId, Ret_weights_list]), %% TODO wrong usage
  %logger:notice("####end set weights train####~n"),
  {next_state, train, State};

train(cast, {post_train_update , Weights}, State = #workerGeneric_state{myName = _MyName, distributedBehaviorFunc = DistributedBehaviorFunc}) ->
  DistributedBehaviorFunc(post_train, {get(generic_worker_ets), Weights}),
  {next_state, train, State};

train(cast, {start_stream , StreamName}, State = #workerGeneric_state{myName = _MyName , distributedBehaviorFunc = DistributedBehaviorFunc}) ->
  stream_handler(start_stream, train, StreamName, DistributedBehaviorFunc),
  {next_state, train, State};

train(cast, {end_stream , StreamName}, State = #workerGeneric_state{myName = _MyName , distributedBehaviorFunc = DistributedBehaviorFunc}) ->
  stream_handler(end_stream, train, StreamName, DistributedBehaviorFunc),
  {next_state, train, State};

train(cast, {idle}, State = #workerGeneric_state{myName = MyName , distributedBehaviorFunc = DistributedBehaviorFunc}) ->
  update_client_avilable_worker(MyName),
  DistributedBehaviorFunc(pre_idle, {get(generic_worker_ets), train}),
  erlang:garbage_collect(), % free memory when phase is changed to idle
  {next_state, idle, State};

train(cast, Data, State = #workerGeneric_state{myName = _MyName}) ->
  % logger:notice("worker ~p in wait cant treat message: ~p\n",[ets:lookup_element(get(generic_worker_ets), worker_name, ?ETS_KEYVAL_VAL_IDX), Data]),
  ?LOG_ERROR("Got unknown message in train state: ~p~n",[Data]),
  throw("Got unknown message in train state"),
  {keep_state, State}.

%% State predict
predict(cast, {sample,_CSVname, BatchID, {<<>>, _Type}}, State) ->
  ?LOG_WARNING("Received empty tensor , batch id: ~p",[BatchID]),
  WorkersStatsEts = get(worker_stats_ets),
  stats:increment_bad_messages(WorkersStatsEts),
  {next_state, predict, State#workerGeneric_state{nextState = predict , currentBatchID = BatchID}};

% send predict sample to worker
predict(cast, {sample , SourceName , BatchID , {PredictBatchTensor, Type}}, State = #workerGeneric_state{modelID = ModelId, distributedBehaviorFunc = DistributedBehaviorFunc, distributedWorkerData = DistributedWorkerData}) ->
    DistributedBehaviorFunc(pre_predict, {get(generic_worker_ets),DistributedWorkerData}),
    WorkersStatsEts = get(worker_stats_ets),
    stats:increment_by_value(WorkersStatsEts , batches_received_predict , 1),
    nif_call(call_to_predict, [ModelId , {PredictBatchTensor, Type} , BatchID, SourceName]),
    {next_state, wait, State#workerGeneric_state{nextState = predict , currentBatchID = BatchID}};

predict(cast, {start_stream , SourceName}, State = #workerGeneric_state{myName = _MyName , distributedBehaviorFunc = DistributedBehaviorFunc}) ->
  stream_handler(start_stream, predict, SourceName, DistributedBehaviorFunc),
  {next_state, predict, State};

predict(cast, {end_stream , SourceName}, State = #workerGeneric_state{myName = _MyName , distributedBehaviorFunc = DistributedBehaviorFunc}) ->
  stream_handler(end_stream, predict, SourceName, DistributedBehaviorFunc),
  {next_state, predict, State};

predict(cast, {idle}, State = #workerGeneric_state{myName = MyName , distributedBehaviorFunc = DistributedBehaviorFunc}) ->
  update_client_avilable_worker(MyName),
  DistributedBehaviorFunc(pre_idle, {get(generic_worker_ets), predict}),
  erlang:garbage_collect(), % free memory when phase is changed to idle
  {next_state, idle, State};

predict(cast, Data, State) ->
  ?LOG_ERROR("Got unknown message in predict state: ~p~n",[Data]),
  throw("Got unknown message in predict state"),
  {next_state, predict, State}.

%% Updates the client that worker is available
update_client_avilable_worker(MyName) -> 
  gen_statem:cast(get(client_pid),{stateChange,MyName}).

stream_handler(StreamPhase , ModelPhase , StreamName , DistributedBehaviorFunc) -> 
  GenWorkerEts = get(generic_worker_ets),
  MyName = ets:lookup_element(GenWorkerEts, worker_name, ?ETS_KEYVAL_VAL_IDX),
  % io:format("~p got ~p from ~p~n",[MyName, StreamPhase, StreamName]),
  ClientPid = ets:lookup_element(GenWorkerEts, client_pid, ?ETS_KEYVAL_VAL_IDX),
  ActiveStreams = ets:lookup_element(GenWorkerEts, active_streams, ?ETS_KEYVAL_VAL_IDX),
  % io:format("~p ActiveStreams: ~p~n",[MyName, ActiveStreams]),
  NewActiveStreams = 
      case StreamPhase of
          start_stream -> ActiveStreams ++ [{MyName, StreamName}];
          end_stream -> gen_statem:cast(ClientPid, {stream_ended, {MyName, StreamName}}),
                        ActiveStreams -- [{MyName, StreamName}]
                        
      end,
  ets:update_element(GenWorkerEts, active_streams, {?ETS_KEYVAL_VAL_IDX, NewActiveStreams}),
  DistributedBehaviorFunc(StreamPhase, {GenWorkerEts, [StreamName , ModelPhase]}).

handle_end_stream_waiting_list(DistributedBehaviorFunc, ModelPhase) ->
  EndStreamWaitingList = ets:lookup_element(get(generic_worker_ets), end_streams_waiting_list, ?ETS_KEYVAL_VAL_IDX),
  % io:format("EndStreamWaitingList: ~p~n",[EndStreamWaitingList]),
  case length(EndStreamWaitingList) of
    0 -> ok;
    _ -> 
      % io:format("Removing from waiting list...~n"),
      Func = fun(StreamName) -> 
                stream_handler(end_stream, ModelPhase, StreamName, DistributedBehaviorFunc),
                CurrentEndStreamWaitingList = ets:lookup_element(get(generic_worker_ets), end_streams_waiting_list, ?ETS_KEYVAL_VAL_IDX),
                NewEndStreamWaitingList = CurrentEndStreamWaitingList -- [StreamName],
                ets:update_element(get(generic_worker_ets), end_streams_waiting_list, {?ETS_KEYVAL_VAL_IDX, NewEndStreamWaitingList})
              end,
      lists:foreach(Func, EndStreamWaitingList)
  end.


get_backend_module() ->
  case get(nif_module) of
    undefined -> ets:lookup_element(get(generic_worker_ets), nif_module, ?ETS_KEYVAL_VAL_IDX);
    Module -> Module
  end.

nif_call(Function, Args) when is_atom(Function), is_list(Args) ->
  Module = get_backend_module(),
  erlang:apply(Module, Function, Args).

select_infra_module(InfraType) ->
  case normalize_infra_type(InfraType) of
    opennn -> nerlNIF;
    torch -> nerlTorchNIF;
    Other ->
      ?LOG_ERROR("Unsupported infrastructure type ~p~n", [InfraType]),
      throw({unsupported_infra_type, Other})
  end.

normalize_infra_type(Value) when is_list(Value) ->
  Lower = string:lowercase(Value),
  case Lower of
    "" -> opennn;
    "0" -> opennn;
    "opennn" -> opennn;
    "onn" -> opennn;
    "torch" -> torch;
    "2" -> torch;
    _ -> Lower
  end;
normalize_infra_type(Value) when is_atom(Value) -> normalize_infra_type(atom_to_list(Value));
normalize_infra_type(Value) when is_integer(Value) -> normalize_infra_type(integer_to_list(Value));
normalize_infra_type(Other) -> Other.

