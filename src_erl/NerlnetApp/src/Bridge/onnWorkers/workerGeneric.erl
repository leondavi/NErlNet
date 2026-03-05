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
-import(w2wCom,[send_message/4]).

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
  WorkerModelSha = get_worker_model_sha(TrainParams),
  WorkerTpPlan = get_worker_tp_plan(WorkerModelSha),
  WorkerParallelCfg = get_worker_parallel_cfg(WorkerName),
  PreparedTrainParams = augment_train_params_with_parallel_cfg(TrainParams, WorkerParallelCfg),
  TpGroupState = build_worker_tp_group_state(WorkerName, WorkerParallelCfg),
  WorkerTpPlanFiltered = filter_tp_plan_for_worker(WorkerTpPlan, WorkerParallelCfg),
  ets:insert(GenWorkerEts,{parallel_mode, legacy}),
  ets:insert(GenWorkerEts,{parallel_execution, #{}}),
  ets:insert(GenWorkerEts,{parallel_super_authority, false}),
  ets:insert(GenWorkerEts,{parallel_scheduler_grants, []}),
  ets:insert(GenWorkerEts,{parallel_pending_losses, 0}),
  ets:insert(GenWorkerEts,{parallel_total_microbatches, 0}),
  ets:insert(GenWorkerEts,{parallel_loss_acc, undefined}),
  ets:insert(GenWorkerEts,{parallel_time_acc, 0.0}),
  ets:insert(GenWorkerEts,{parallel_microbatch_queue, []}),
  ets:insert(GenWorkerEts,{parallel_pending_backward_events, []}),
  ets:insert(GenWorkerEts,{parallel_active_batch_ctx, undefined}),
  ets:insert(GenWorkerEts,{parallel_deferred_samples, []}),
  ets:insert(GenWorkerEts,{parallel_congestion_signaled, false}),
  ets:insert(GenWorkerEts,{congestion_drop_enabled, false}),
  ets:insert(GenWorkerEts,{deferred_sample_soft_limit, 256}),
  ets:insert(GenWorkerEts,{pipeline_buffer_cap, 0}),
  ets:insert(GenWorkerEts,{parallel_microbatches_skipped, 0}),
  ets:insert(GenWorkerEts,{parallel_pipeline_forward_buffer, []}),
  ets:insert(GenWorkerEts,{parallel_pipeline_backward_buffer, []}),
  ets:insert(GenWorkerEts,{parallel_pipeline_predict_buffer, []}),
  ets:insert(GenWorkerEts,{worker_parallel_cfg, WorkerParallelCfg}),
  ets:insert(GenWorkerEts,{worker_model_sha, WorkerModelSha}),
  ets:insert(GenWorkerEts,{worker_tp_plan, WorkerTpPlanFiltered}),
  ets:insert(GenWorkerEts,{worker_tp_group_state, TpGroupState}),
  ets:insert(GenWorkerEts,{tp_collective_inbox_buffer, []}),
  ets:insert(GenWorkerEts,{pipeline_nif_worker, false}),
  ets:insert(GenWorkerEts,{parallel_nif_epoch, 0}),
  InfraModule = select_infra_module(InfraType),
  NormalizedInfraType = normalize_infra_type(InfraType),
  ets:insert(GenWorkerEts,{infra_type, InfraType}),
  ets:insert(GenWorkerEts,{nif_module, InfraModule}),
  ets:insert(GenWorkerEts,{train_params, PreparedTrainParams}),
  put(nif_module, InfraModule),
  ets:insert(GenWorkerEts,{controller_message_q, []}), %% TODO Deprecated
  ets:insert(GenWorkerEts,{handshake_done, false}),
  ets:insert(GenWorkerEts,{active_streams, []}),
  ets:insert(GenWorkerEts,{stream_occuring, false}),
  ets:insert(GenWorkerEts,{end_streams_waiting_list, []}), % Waiting list of messages from client to end_stream with source
  ets:insert(GenWorkerEts,{end_stream_flush_retries, 0}),
  % Worker to Worker communication module - this is a gen_server

  Res = case NormalizedInfraType of
          torch ->
            nif_call(new_nerlworker_nif, [ModelID, DistributedSystemType, DistributedSystemArgs, PreparedTrainParams]);
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

idle(cast, {set_parallel_mode, Mode}, State) ->
  set_worker_parallel_mode(get(generic_worker_ets), Mode),
  {keep_state, State};

idle(cast, {set_parallel_execution, ParallelExecution}, State) ->
  set_worker_parallel_execution(get(generic_worker_ets), ParallelExecution),
  {keep_state, State};

idle(cast, {set_parallel_authority, Enabled}, State) ->
  set_worker_parallel_authority(get(generic_worker_ets), Enabled),
  {keep_state, State};

idle(cast, {parallel_scheduler_grant, Direction, BatchID, MicrobatchID, StageID}, State) ->
  append_parallel_scheduler_grant(get(generic_worker_ets), Direction, BatchID, MicrobatchID, StageID),
  {keep_state, State};
idle(cast, {parallel_scheduler_grant, Direction, MicrobatchID, StageID}, State) ->
  idle(cast, {parallel_scheduler_grant, Direction, any, MicrobatchID, StageID}, State);
idle(cast, {parallel_skip_work_item, Direction, BatchID, MicrobatchID, StageID, Reason, EventID, PhaseEpoch}, State = #workerGeneric_state{myName = WorkerName}) ->
  handle_parallel_skip_work_item(get(generic_worker_ets), WorkerName, Direction, BatchID, MicrobatchID, StageID, Reason, EventID, PhaseEpoch),
  {keep_state, State};

% Go from idle to train
idle(cast, {training}, State = #workerGeneric_state{myName = MyName , distributedBehaviorFunc = DistributedBehaviorFunc}) ->
  % io:format("@idle got training , Worker ~p is going to state idle...~n",[MyName]),
  % update the phase in registry
  reset_parallel_loss_context(get(generic_worker_ets)),
  put(phase, training),
  ets:update_element(get(generic_worker_ets), active_streams, {?ETS_KEYVAL_VAL_IDX, []}),
  DistributedBehaviorFunc(post_idle, {get(generic_worker_ets), train}),
  update_client_avilable_worker(MyName),
  ?LOG_INFO("Worker ~p is switching to phase train",[MyName]),
  {next_state, train, State#workerGeneric_state{lastPhase = train}};

% Go from idle to predict
idle(cast, {predict}, State = #workerGeneric_state{myName = MyName , distributedBehaviorFunc = DistributedBehaviorFunc}) ->
  % worker_controller_empty_message_queue(),
  reset_parallel_loss_context(get(generic_worker_ets)),
  put(phase, predict),
  ets:update_element(get(generic_worker_ets), active_streams, {?ETS_KEYVAL_VAL_IDX, []}),
  DistributedBehaviorFunc(post_idle, {get(generic_worker_ets), predict}),
  update_client_avilable_worker(MyName),
  ?LOG_INFO("Worker ~p is switching to phase predict",[MyName]),
  {next_state, predict, State#workerGeneric_state{lastPhase = predict}};

idle(cast, {pipeline_nif_result, _Continuation, _NifResult}, State) ->
  GenWorkerEts = get(generic_worker_ets),
  ets:update_element(GenWorkerEts, pipeline_nif_worker, {?ETS_KEYVAL_VAL_IDX, false}),
  WorkerName = ets:lookup_element(GenWorkerEts, worker_name, ?ETS_KEYVAL_VAL_IDX),
  ?LOG_WARNING("Worker ~p received stale pipeline NIF result in idle state, discarding", [WorkerName]),
  {keep_state, State};

idle(cast, _Param, State = #workerGeneric_state{myName = _MyName}) ->
  % io:format("@idle Worker ~p is going to state idle...~n",[MyName]),
  {next_state, idle, State}.

%% Waiting for receiving results or loss function
%% Got nan or inf from loss function - Error, loss function too big for double
wait(cast, {set_parallel_mode, Mode}, State) ->
  set_worker_parallel_mode(get(generic_worker_ets), Mode),
  {keep_state, State};

wait(cast, {set_parallel_execution, ParallelExecution}, State) ->
  set_worker_parallel_execution(get(generic_worker_ets), ParallelExecution),
  {keep_state, State};

wait(cast, {set_parallel_authority, Enabled}, State) ->
  set_worker_parallel_authority(get(generic_worker_ets), Enabled),
  {keep_state, State};

wait(
  cast,
  {parallel_scheduler_grant, Direction, BatchID, MicrobatchID, StageID},
  State = #workerGeneric_state{lastPhase = LastPhase, nextState = NextState}
) ->
  GenWorkerEts = get(generic_worker_ets),
  append_parallel_scheduler_grant(GenWorkerEts, Direction, BatchID, MicrobatchID, StageID),
  ParallelMode = normalize_parallel_mode_atom(
                   ets:lookup_element(GenWorkerEts, parallel_mode, ?ETS_KEYVAL_VAL_IDX)
                 ),
  case is_pipeline_mode_atom(ParallelMode) of
    true ->
      case get(phase) of
        predict ->
          case dispatch_pipeline_predict_buffers(GenWorkerEts, State#workerGeneric_state.modelID, State#workerGeneric_state.myName) of
            ok ->
              maybe_replay_grant_matched_deferred_sample_in_wait(
                GenWorkerEts,
                NextState,
                LastPhase,
                State
              );
            {abort, PipelineReason} ->
              notify_worker_parallel_abort(GenWorkerEts, {pipeline_predict_dispatch_failed, PipelineReason}),
              reset_parallel_loss_context(GenWorkerEts),
              {keep_state, State}
          end;
        _ ->
          case maybe_dispatch_pending_parallel_backward_events(GenWorkerEts) of
            ok ->
              case dispatch_pipeline_buffers(GenWorkerEts, State#workerGeneric_state.modelID, State#workerGeneric_state.myName) of
                ok ->
                  maybe_replay_grant_matched_deferred_sample_in_wait(
                    GenWorkerEts,
                    NextState,
                    LastPhase,
                    State
                  );
                {abort, PipelineReason} ->
                  notify_worker_parallel_abort(GenWorkerEts, {pipeline_dispatch_failed, PipelineReason}),
                  reset_parallel_loss_context(GenWorkerEts),
                  {keep_state, State}
              end;
            {abort, PendingDispatchReason} ->
              notify_worker_parallel_abort(
                GenWorkerEts,
                {pending_parallel_backward_dispatch_failed, PendingDispatchReason}
              ),
              reset_parallel_loss_context(GenWorkerEts),
              {keep_state, State}
          end
      end;
    false ->
      case maybe_dispatch_pending_parallel_backward_events(GenWorkerEts) of
        ok ->
          case dispatch_queued_parallel_microbatches(GenWorkerEts) of
            ok ->
              case maybe_pop_matching_deferred_sample_for_grants(GenWorkerEts) of
                {ok, SampleTuple} ->
                  ?LOG_INFO(
                    "Worker ~p replaying grant-matched deferred sample while leaving wait state",
                    [ets:lookup_element(GenWorkerEts, worker_name, ?ETS_KEYVAL_VAL_IDX)]
                  ),
                  {next_state, normalize_parallel_next_state(NextState, LastPhase), State, [{next_event, cast, SampleTuple}]};
                none ->
                  {keep_state, State}
              end;
            {abort, DispatchReason} ->
              case is_nonfatal_tp_batch_abort(DispatchReason) of
                true ->
                  handle_nonfatal_tp_batch_abort(
                    GenWorkerEts,
                    State#workerGeneric_state.myName,
                    State#workerGeneric_state.currentBatchID,
                    undefined,
                    current_worker_phase(),
                    DispatchReason
                  ),
                  reset_parallel_loss_context(GenWorkerEts);
                false ->
                  notify_worker_parallel_abort(
                    GenWorkerEts,
                    {queued_parallel_dispatch_failed, DispatchReason}
                  ),
                  reset_parallel_loss_context(GenWorkerEts)
              end,
              {keep_state, State}
          end;
        {abort, PendingDispatchReason} ->
          notify_worker_parallel_abort(
            GenWorkerEts,
            {pending_parallel_backward_dispatch_failed, PendingDispatchReason}
          ),
          reset_parallel_loss_context(GenWorkerEts),
          {keep_state, State}
      end
  end;
wait(cast, {parallel_scheduler_grant, Direction, MicrobatchID, StageID}, State) ->
  wait(cast, {parallel_scheduler_grant, Direction, any, MicrobatchID, StageID}, State);
wait(cast, {parallel_skip_work_item, Direction, BatchID, MicrobatchID, StageID, Reason, EventID, PhaseEpoch}, State = #workerGeneric_state{myName = WorkerName}) ->
  handle_parallel_skip_work_item(get(generic_worker_ets), WorkerName, Direction, BatchID, MicrobatchID, StageID, Reason, EventID, PhaseEpoch),
  {keep_state, State};

wait(cast, {parallel_pipeline_inbox, FromWorker, Payload}, State = #workerGeneric_state{modelID = ModelId, myName = MyName}) ->
  GenWorkerEts = get(generic_worker_ets),
  case handle_parallel_pipeline_inbox(GenWorkerEts, ModelId, MyName, FromWorker, Payload) of
    ok ->
      {keep_state, State};
    {abort, Reason} ->
      notify_worker_parallel_abort(GenWorkerEts, {pipeline_inbox_rejected, Reason}),
      reset_parallel_loss_context(GenWorkerEts),
      {keep_state, State}
  end;

wait(cast, {pipeline_nif_result, Continuation, NifResult}, State) ->
  GenWorkerEts = get(generic_worker_ets),
  ets:update_element(GenWorkerEts, pipeline_nif_worker, {?ETS_KEYVAL_VAL_IDX, false}),
  ContinuationResult = Continuation(NifResult),
  case ContinuationResult of
    processed ->
      case State#workerGeneric_state.nextState of
        predict ->
          case dispatch_pipeline_predict_buffers(GenWorkerEts, State#workerGeneric_state.modelID, State#workerGeneric_state.myName) of
            ok -> {keep_state, State};
            {abort, Reason} ->
              notify_worker_parallel_abort(GenWorkerEts, {pipeline_nif_predict_dispatch_failed, Reason}),
              reset_parallel_loss_context(GenWorkerEts),
              {keep_state, State}
          end;
        _ ->
          case maybe_dispatch_pending_parallel_backward_events(GenWorkerEts) of
            ok ->
              case dispatch_pipeline_buffers(GenWorkerEts, State#workerGeneric_state.modelID, State#workerGeneric_state.myName) of
                ok -> {keep_state, State};
                {abort, Reason} ->
                  notify_worker_parallel_abort(GenWorkerEts, {pipeline_nif_dispatch_failed, Reason}),
                  reset_parallel_loss_context(GenWorkerEts),
                  {keep_state, State}
              end;
            {abort, PendingReason} ->
              notify_worker_parallel_abort(GenWorkerEts, {pipeline_nif_pending_dispatch_failed, PendingReason}),
              reset_parallel_loss_context(GenWorkerEts),
              {keep_state, State}
          end
      end;
    stale ->
      {keep_state, State};
    {abort, Reason} ->
      notify_worker_parallel_abort(GenWorkerEts, Reason),
      reset_parallel_loss_context(GenWorkerEts),
      {keep_state, State}
  end;

wait(cast, {parallel_check_end_stream_flush, ModelPhase},
     State = #workerGeneric_state{distributedBehaviorFunc = DistributedBehaviorFunc}) ->
  maybe_retry_pending_end_stream_flush(get(generic_worker_ets), ModelPhase, DistributedBehaviorFunc),
  {keep_state, State};

wait(cast, {loss_microbatch, {LossTensor, LossTensorType}, TrainTime, BatchID, SourceName, MicrobatchID},
     State = #workerGeneric_state{myName = MyName, distributedBehaviorFunc = DistributedBehaviorFunc}) ->
  GenWorkerEts = get(generic_worker_ets),
  PendingLosses = ets:lookup_element(GenWorkerEts, parallel_pending_losses, ?ETS_KEYVAL_VAL_IDX),
  case PendingLosses > 0 of
    false ->
      handle_unexpected_microbatch_loss(
        GenWorkerEts,
        MyName,
        BatchID,
        SourceName,
        MicrobatchID
      ),
      reset_parallel_loss_context(GenWorkerEts),
      {next_state, wait, State};
    true ->
      StageID = get_worker_pipeline_stage(GenWorkerEts),
      ParallelMode = ets:lookup_element(GenWorkerEts, parallel_mode, ?ETS_KEYVAL_VAL_IDX),
      case maybe_emit_parallel_backward_event(
             ParallelMode,
             MyName,
             BatchID,
             MicrobatchID,
             StageID,
             TrainTime
           ) of
        {error, EmitReason} ->
          notify_worker_parallel_abort(
            GenWorkerEts,
            {parallel_backward_event_rejected, MyName, BatchID, MicrobatchID, StageID, EmitReason}
          ),
          reset_parallel_loss_context(GenWorkerEts),
          {next_state, wait, State};
        ok ->
          UpdatedLossAcc = accumulate_parallel_loss(
                             ets:lookup_element(GenWorkerEts, parallel_loss_acc, ?ETS_KEYVAL_VAL_IDX),
                             {LossTensor, LossTensorType}
                           ),
          UpdatedTimeAcc = ets:lookup_element(GenWorkerEts, parallel_time_acc, ?ETS_KEYVAL_VAL_IDX) + TrainTime,
          Remaining = PendingLosses - 1,
          ets:update_element(GenWorkerEts, parallel_loss_acc, {?ETS_KEYVAL_VAL_IDX, UpdatedLossAcc}),
          ets:update_element(GenWorkerEts, parallel_time_acc, {?ETS_KEYVAL_VAL_IDX, UpdatedTimeAcc}),
          stats:increment_by_value(get(worker_stats_ets), acc_time_training, trunc(TrainTime)),
          ets:update_element(GenWorkerEts, parallel_pending_losses, {?ETS_KEYVAL_VAL_IDX, Remaining}),
          case Remaining of
            0 ->
              case maybe_call_optimizer_barrier(State#workerGeneric_state.modelID) of
                ok ->
                  TotalMicrobatchesRaw = erlang:max(1, ets:lookup_element(GenWorkerEts, parallel_total_microbatches, ?ETS_KEYVAL_VAL_IDX)),
                  SkippedMicrobatches = ets:lookup_element(GenWorkerEts, parallel_microbatches_skipped, ?ETS_KEYVAL_VAL_IDX),
                  TotalMicrobatches = erlang:max(1, TotalMicrobatchesRaw - SkippedMicrobatches),
                  FinalLossTensor = finalize_parallel_loss(UpdatedLossAcc, TotalMicrobatches),
                  FinalTrainTime = UpdatedTimeAcc / TotalMicrobatches,
                  BatchTimeStamp = erlang:system_time(nanosecond),
                  WorkerToken = ets:lookup_element(GenWorkerEts, distributed_system_token, ?ETS_KEYVAL_VAL_IDX),
                  stats:increment_by_value(get(worker_stats_ets), batches_sent_train, 1),
                  gen_statem:cast(get(client_pid),{loss, MyName, SourceName ,FinalLossTensor , FinalTrainTime , WorkerToken, BatchID , BatchTimeStamp}),
                  NextStateBehavior = DistributedBehaviorFunc(post_train, {GenWorkerEts,[]}),
                  reset_parallel_batch_context(GenWorkerEts),
                  maybe_finalize_pending_end_streams(DistributedBehaviorFunc, train),
                  case NextStateBehavior =:= train orelse NextStateBehavior =:= predict of
                    true ->
                      case pop_deferred_parallel_sample(GenWorkerEts) of
                        {ok, DeferredSample} ->
                          {next_state, NextStateBehavior, State, [{next_event, cast, DeferredSample}]};
                        empty ->
                          {next_state, NextStateBehavior, State}
                      end;
                    false ->
                      {next_state, NextStateBehavior, State}
                  end;
                {error, BarrierReason} ->
                  notify_worker_parallel_abort(
                    GenWorkerEts,
                    {optimizer_barrier_failed, BarrierReason}
                  ),
                  reset_parallel_loss_context(GenWorkerEts),
                  {next_state, wait, State}
              end;
            _ ->
              case dispatch_queued_parallel_microbatches(GenWorkerEts) of
                ok ->
                  {next_state, wait, State};
                {abort, DispatchReason} ->
                  case is_nonfatal_tp_batch_abort(DispatchReason) of
                    true ->
                      handle_nonfatal_tp_batch_abort(
                        GenWorkerEts,
                        MyName,
                        BatchID,
                        SourceName,
                        train,
                        DispatchReason
                      ),
                      reset_parallel_loss_context(GenWorkerEts);
                    false ->
                      notify_worker_parallel_abort(
                        GenWorkerEts,
                        {queued_parallel_dispatch_failed, DispatchReason}
                      ),
                      reset_parallel_loss_context(GenWorkerEts)
                  end,
                  {next_state, wait, State}
              end
          end
      end
  end;

wait(cast, {loss, nan , TrainTime , BatchID , SourceName}, State = #workerGeneric_state{myName = MyName, distributedBehaviorFunc = DistributedBehaviorFunc}) ->
  stats:increment_by_value(get(worker_stats_ets), nan_loss_count, 1),
  WorkerToken = ets:lookup_element(get(generic_worker_ets), distributed_system_token, ?ETS_KEYVAL_VAL_IDX),
  stats:increment_by_value(get(worker_stats_ets), batches_sent_train, 1),
  gen_statem:cast(get(client_pid),{loss, MyName , SourceName ,nan , TrainTime, WorkerToken ,BatchID}),
  NextStateBehavior = DistributedBehaviorFunc(post_train, {get(generic_worker_ets),[]}), %% First call sends empty list , then it will be updated by the federated server and clients
  maybe_finalize_pending_end_streams(DistributedBehaviorFunc, train),
  {next_state, NextStateBehavior, State};


wait(cast, {loss, {LossTensor, LossTensorType} , TrainTime , BatchID , SourceName}, State = #workerGeneric_state{myName = MyName, modelID=_ModelID, distributedBehaviorFunc = DistributedBehaviorFunc}) ->
  BatchTimeStamp = erlang:system_time(nanosecond),
  WorkerToken = ets:lookup_element(get(generic_worker_ets), distributed_system_token, ?ETS_KEYVAL_VAL_IDX),
  stats:increment_by_value(get(worker_stats_ets), batches_sent_train, 1),
  gen_statem:cast(get(client_pid),{loss, MyName, SourceName ,{LossTensor, LossTensorType} , TrainTime , WorkerToken, BatchID , BatchTimeStamp}),
  stats:increment_by_value(get(worker_stats_ets), acc_time_training, trunc(TrainTime)),
  NextStateBehavior = DistributedBehaviorFunc(post_train, {get(generic_worker_ets),[]}), %% First call sends empty list , then it will be updated by the federated server and clients
  maybe_finalize_pending_end_streams(DistributedBehaviorFunc, train),
  {next_state, NextStateBehavior, State};

wait(cast, {predictRes, PredNerlTensor, PredNerlTensorType, TimeNif, BatchID , SourceName}, State = #workerGeneric_state{myName = MyName, nextState = NextState, distributedBehaviorFunc = DistributedBehaviorFunc, distributedWorkerData = DistributedWorkerData}) ->
  GenWorkerEts = get(generic_worker_ets),
  ActiveCtx = ets:lookup_element(GenWorkerEts, parallel_active_batch_ctx, ?ETS_KEYVAL_VAL_IDX),
  case ActiveCtx of
    Ctx when is_map(Ctx) andalso is_map_key(mode, Ctx) andalso map_get(mode, Ctx) =:= tensor_predict ->
      %% Tensor predict mode: accumulate microbatch results
      ExistingPred = maps:get(predict_acc, Ctx, []),
      MicrobatchIdx = length(ExistingPred),
      PendingPred = ets:lookup_element(GenWorkerEts, parallel_pending_losses, ?ETS_KEYVAL_VAL_IDX),
      NewCtx = Ctx#{
        predict_acc => ExistingPred ++ [{MicrobatchIdx, {PredNerlTensor, PredNerlTensorType}}],
        forward_completed => maps:get(forward_completed, Ctx, 0) + 1
      },
      ets:update_element(GenWorkerEts, parallel_active_batch_ctx, {?ETS_KEYVAL_VAL_IDX, NewCtx}),
      ets:update_element(GenWorkerEts, parallel_time_acc, {?ETS_KEYVAL_VAL_IDX,
        ets:lookup_element(GenWorkerEts, parallel_time_acc, ?ETS_KEYVAL_VAL_IDX) + TimeNif}),
      stats:increment_by_value(get(worker_stats_ets), acc_time_prediction, trunc(TimeNif)),
      Remaining = PendingPred - 1,
      ets:update_element(GenWorkerEts, parallel_pending_losses, {?ETS_KEYVAL_VAL_IDX, Remaining}),
      case Remaining =< 0 of
        false ->
          %% More microbatch predictions pending, try dispatch remaining queued microbatches
          case dispatch_queued_parallel_microbatches(GenWorkerEts) of
            ok ->
              {keep_state, State};
            {abort, DispatchReason} ->
              case is_nonfatal_tp_batch_abort(DispatchReason) of
                true ->
                  handle_nonfatal_tp_batch_abort(
                    GenWorkerEts,
                    MyName,
                    BatchID,
                    SourceName,
                    predict,
                    DispatchReason
                  ),
                  reset_parallel_loss_context(GenWorkerEts),
                  {keep_state, State};
                false ->
                  notify_worker_parallel_abort(
                    GenWorkerEts,
                    {queued_parallel_dispatch_failed, DispatchReason}
                  ),
                  reset_parallel_loss_context(GenWorkerEts),
                  {keep_state, State}
              end
          end;
        true ->
          %% All microbatch predictions received, concatenate and send combined result
          WorkerName = ets:lookup_element(GenWorkerEts, worker_name, ?ETS_KEYVAL_VAL_IDX),
          FinalCtx = ets:lookup_element(GenWorkerEts, parallel_active_batch_ctx, ?ETS_KEYVAL_VAL_IDX),
          PredAcc = maps:get(predict_acc, FinalCtx, []),
          TotalMicrobatches = maps:get(total_microbatches, FinalCtx, 0),
          maybe_send_pipeline_last_stage_prediction(GenWorkerEts, WorkerName, FinalCtx, PredAcc, TotalMicrobatches),
          DistributedBehaviorFunc(post_predict, {GenWorkerEts, DistributedWorkerData}),
          reset_parallel_batch_context(GenWorkerEts),
          maybe_finalize_pending_end_streams(DistributedBehaviorFunc, predict),
          case NextState =:= train orelse NextState =:= predict of
            true ->
              case pop_deferred_parallel_sample(GenWorkerEts) of
                {ok, DeferredSample} ->
                  {next_state, NextState, State, [{next_event, cast, DeferredSample}]};
                empty ->
                  {next_state, NextState, State}
              end;
            false ->
              {next_state, NextState, State}
          end
      end;
    _ ->
      %% Legacy predict result handling
      BatchTimeStamp = erlang:system_time(nanosecond),
      WorkerToken = ets:lookup_element(GenWorkerEts, distributed_system_token, ?ETS_KEYVAL_VAL_IDX),
      stats:increment_by_value(get(worker_stats_ets), batches_sent_predict, 1),
      gen_statem:cast(get(client_pid),{predictRes,MyName, SourceName, {PredNerlTensor, PredNerlTensorType}, TimeNif , WorkerToken, BatchID , BatchTimeStamp}),
      stats:increment_by_value(get(worker_stats_ets), acc_time_prediction, trunc(TimeNif)),
      DistributedBehaviorFunc(post_predict, {GenWorkerEts,DistributedWorkerData}),
      maybe_finalize_pending_end_streams(DistributedBehaviorFunc, predict),
      case NextState =:= train orelse NextState =:= predict of
        true ->
          case pop_deferred_parallel_sample(GenWorkerEts) of
            {ok, DeferredSample} ->
              {next_state, NextState, State, [{next_event, cast, DeferredSample}]};
            empty ->
              {next_state, NextState, State}
          end;
        false ->
          {next_state, NextState, State}
      end
  end;

wait(cast, {end_stream , StreamName}, State = #workerGeneric_state{myName = _MyName, distributedBehaviorFunc = DistributedBehaviorFunc}) ->
  Phase = get(phase),
  handle_end_stream_event(DistributedBehaviorFunc, Phase, StreamName),
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
  maybe_finalize_pending_end_streams(DistributedBehaviorFunc, train),
  {next_state, NextStateBehavior, State};

% This state happens when worker is busy with sample that sent by source X but source Y just its stream
wait(cast,  {start_stream , StreamName}, State = #workerGeneric_state{lastPhase = LastPhase, distributedBehaviorFunc = DistributedBehaviorFunc}) ->
    stream_handler(start_stream, LastPhase, StreamName, DistributedBehaviorFunc),
{keep_state, State};

% CANNOT HAPPEN 
wait(cast, {idle}, State= #workerGeneric_state{myName = MyName, distributedBehaviorFunc = DistributedBehaviorFunc}) ->
  GenWorkerEts = get(generic_worker_ets),
  case has_pending_runtime_excluding_scheduler_grants(GenWorkerEts) of
    true ->
      case has_orphaned_runtime_after_phase_close(GenWorkerEts) of
        true ->
          logger:warning(
            "Worker ~p got idle in wait with orphaned runtime after stream close; dropping stale pending state",
            [MyName]
          ),
          put(idle_defer_count, 0),
          Phase = get(phase),
          reset_parallel_runtime_for_idle(GenWorkerEts),
          DistributedBehaviorFunc(pre_idle, {GenWorkerEts, Phase}),
          update_client_avilable_worker(MyName),
          {next_state, idle, State#workerGeneric_state{nextState = idle}};
        false ->
          DeferCount = case get(idle_defer_count) of undefined -> 0; N -> N end,
          case DeferCount >= 50 of
            true ->
              logger:warning("Worker ~p forcing idle after ~p idle deferrals with pending runtime", [MyName, DeferCount]),
              put(idle_defer_count, 0),
              Phase = get(phase),
              reset_parallel_runtime_for_idle(GenWorkerEts),
              DistributedBehaviorFunc(pre_idle, {GenWorkerEts, Phase}),
              update_client_avilable_worker(MyName),
              {next_state, idle, State#workerGeneric_state{nextState = idle}};
            false ->
              put(idle_defer_count, DeferCount + 1),
              erlang:send_after(10, self(), {'$gen_cast', {idle}}),
              {keep_state, State}
          end
      end;
    false ->
      logger:warning("Worker ~p got idle message in wait state, going to idle state but this is an unexpected behavior",[MyName]),
      put(idle_defer_count, 0),
      Phase = get(phase),
      reset_parallel_runtime_for_idle(GenWorkerEts),
      DistributedBehaviorFunc(pre_idle, {GenWorkerEts, Phase}),
      update_client_avilable_worker(MyName),
      {next_state, idle, State#workerGeneric_state{nextState = idle}}
  end;

wait(cast, {training}, State) ->
  %logger:notice("Waiting, next state - train"),
  % gen_statem:cast(ClientPid,{stateChange,WorkerName}),
  {next_state, wait, State#workerGeneric_state{nextState = train}};

wait(cast, {predict}, State) ->
  %logger:notice("Waiting, next state - predict"),
  {next_state, wait, State#workerGeneric_state{nextState = predict}};

%% Worker in wait can't treat incoming message 
wait(cast, BatchTuple , State = #workerGeneric_state{lastPhase = LastPhase, nextState = NextState, myName= _MyName}) when element(1, BatchTuple) == sample ->
  GenWorkerEts = get(generic_worker_ets),
  ParallelMode = normalize_parallel_mode_atom(
                   ets:lookup_element(GenWorkerEts, parallel_mode, ?ETS_KEYVAL_VAL_IDX)
                 ),
  case ParallelMode of
    legacy ->
      case LastPhase of
        train ->
          ets:update_counter(get(worker_stats_ets), batches_dropped_train , 1),
          ets:update_counter(get(worker_stats_ets), drop_legacy_busy_train , 1);
        predict ->
          ets:update_counter(get(worker_stats_ets), batches_dropped_predict , 1),
          ets:update_counter(get(worker_stats_ets), drop_legacy_busy_predict , 1)
      end,
      {next_state, wait, State};
    _ ->
      case should_replay_parallel_sample_immediately(GenWorkerEts, NextState, BatchTuple) of
        true ->
          ?LOG_INFO(
            "Worker ~p replaying grant-matched incoming sample immediately by transitioning wait->~p",
            [ets:lookup_element(GenWorkerEts, worker_name, ?ETS_KEYVAL_VAL_IDX), NextState]
          ),
          {next_state, normalize_parallel_next_state(NextState, LastPhase), State, [{next_event, cast, BatchTuple}]};
        false ->
          queue_deferred_parallel_sample(GenWorkerEts, BatchTuple),
          {keep_state, State}
      end
  end;

wait(cast, Data, State) ->
  % logger:notice("worker ~p in wait cant treat message: ~p\n",[ets:lookup_element(get(generic_worker_ets), worker_name, ?ETS_KEYVAL_VAL_IDX), Data]),
  ?LOG_ERROR("Got unknown message in wait state: ~p~n",[Data]),
  throw("Got unknown message in wait state"),
  {keep_state, State}.


%% State train
train(cast, {set_parallel_mode, Mode}, State) ->
  set_worker_parallel_mode(get(generic_worker_ets), Mode),
  {keep_state, State};

train(cast, {set_parallel_execution, ParallelExecution}, State) ->
  set_worker_parallel_execution(get(generic_worker_ets), ParallelExecution),
  {keep_state, State};

train(cast, {set_parallel_authority, Enabled}, State) ->
  set_worker_parallel_authority(get(generic_worker_ets), Enabled),
  {keep_state, State};

train(cast, {parallel_scheduler_grant, Direction, BatchID, MicrobatchID, StageID}, State) ->
  GenWorkerEts = get(generic_worker_ets),
  case is_buffer_over_cap(GenWorkerEts) of
    true ->
      reject_grant_for_congestion(GenWorkerEts, State#workerGeneric_state.myName, Direction, BatchID, MicrobatchID, StageID),
      {keep_state, State};
    false ->
      append_parallel_scheduler_grant(GenWorkerEts, Direction, BatchID, MicrobatchID, StageID),
      ParallelMode = normalize_parallel_mode_atom(
                       ets:lookup_element(GenWorkerEts, parallel_mode, ?ETS_KEYVAL_VAL_IDX)
                     ),
      case is_pipeline_mode_atom(ParallelMode) of
        true ->
          case maybe_dispatch_pending_parallel_backward_events(GenWorkerEts) of
            ok ->
              case dispatch_pipeline_buffers(GenWorkerEts, State#workerGeneric_state.modelID, State#workerGeneric_state.myName) of
                ok -> {keep_state, State};
                {abort, Reason} ->
                  notify_worker_parallel_abort(GenWorkerEts, {pipeline_dispatch_failed, Reason}),
                  reset_parallel_loss_context(GenWorkerEts),
                  {keep_state, State}
              end;
            {abort, PendingDispatchReason} ->
              notify_worker_parallel_abort(
                GenWorkerEts,
                {pending_parallel_backward_dispatch_failed, PendingDispatchReason}
              ),
              reset_parallel_loss_context(GenWorkerEts),
              {keep_state, State}
          end;
        false ->
          {keep_state, State}
      end
  end;
train(cast, {parallel_scheduler_grant, Direction, MicrobatchID, StageID}, State) ->
  train(cast, {parallel_scheduler_grant, Direction, any, MicrobatchID, StageID}, State);
train(cast, {parallel_skip_work_item, Direction, BatchID, MicrobatchID, StageID, Reason, EventID, PhaseEpoch}, State = #workerGeneric_state{myName = WorkerName}) ->
  handle_parallel_skip_work_item(get(generic_worker_ets), WorkerName, Direction, BatchID, MicrobatchID, StageID, Reason, EventID, PhaseEpoch),
  {keep_state, State};

train(cast, {parallel_pipeline_inbox, FromWorker, Payload}, State = #workerGeneric_state{modelID = ModelId, myName = MyName}) ->
  GenWorkerEts = get(generic_worker_ets),
  case handle_parallel_pipeline_inbox(GenWorkerEts, ModelId, MyName, FromWorker, Payload) of
    ok ->
      {keep_state, State};
    {abort, Reason} ->
      notify_worker_parallel_abort(GenWorkerEts, {pipeline_inbox_rejected, Reason}),
      reset_parallel_loss_context(GenWorkerEts),
      {keep_state, State}
  end;

train(cast, {pipeline_nif_result, Continuation, NifResult}, State) ->
  GenWorkerEts = get(generic_worker_ets),
  ets:update_element(GenWorkerEts, pipeline_nif_worker, {?ETS_KEYVAL_VAL_IDX, false}),
  case Continuation(NifResult) of
    processed ->
      case maybe_dispatch_pending_parallel_backward_events(GenWorkerEts) of
        ok ->
          case dispatch_pipeline_buffers(GenWorkerEts, State#workerGeneric_state.modelID, State#workerGeneric_state.myName) of
            ok -> {keep_state, State};
            {abort, Reason} ->
              notify_worker_parallel_abort(GenWorkerEts, {pipeline_nif_dispatch_failed, Reason}),
              reset_parallel_loss_context(GenWorkerEts),
              {keep_state, State}
          end;
        {abort, PendingReason} ->
          notify_worker_parallel_abort(GenWorkerEts, {pipeline_nif_pending_dispatch_failed, PendingReason}),
          reset_parallel_loss_context(GenWorkerEts),
          {keep_state, State}
      end;
    stale ->
      {keep_state, State};
    {abort, Reason} ->
      notify_worker_parallel_abort(GenWorkerEts, Reason),
      reset_parallel_loss_context(GenWorkerEts),
      {keep_state, State}
  end;

train(cast, {sample, BatchID ,{<<>>, _Type}}, State) ->
  ?LOG_WARNING("Empty sample received , batch id: ~p~n",[BatchID]),
  WorkerStatsEts = get(worker_stats_ets),
  stats:increment_by_value(WorkerStatsEts , empty_batches , 1),
  {next_state, train, State#workerGeneric_state{nextState = train , currentBatchID = BatchID}};
  
%% Change SampleListTrain to NerlTensor
train(cast, {sample, SourceName ,BatchID ,{NerlTensorOfSamples, NerlTensorType}}, State = #workerGeneric_state{modelID = ModelId, distributedBehaviorFunc = DistributedBehaviorFunc, distributedWorkerData = DistributedWorkerData, myName = _MyName}) ->
    GenWorkerEts = get(generic_worker_ets),
    ParallelMode = normalize_parallel_mode_atom(
                     ets:lookup_element(GenWorkerEts, parallel_mode, ?ETS_KEYVAL_VAL_IDX)
                   ),
    case ParallelMode of
      legacy ->
        DistributedBehaviorFunc(pre_train, {GenWorkerEts,DistributedWorkerData}), % Here the model can be updated by the federated server
        WorkersStatsEts = get(worker_stats_ets),
        stats:increment_by_value(WorkersStatsEts , batches_received_train , 1),
        nif_call(call_to_train, [ModelId , {NerlTensorOfSamples, NerlTensorType} , BatchID , SourceName]),
        {next_state, wait, State#workerGeneric_state{nextState = train, currentBatchID = BatchID}};
      _ ->
        case is_pipeline_mode_atom(ParallelMode) of
          true ->
            DistributedBehaviorFunc(pre_train, {GenWorkerEts,DistributedWorkerData}),
            WorkersStatsEts = get(worker_stats_ets),
            stats:increment_by_value(WorkersStatsEts , batches_received_train , 1),
            StageID = get_worker_pipeline_stage(GenWorkerEts),
            case StageID of
              0 ->
                case prepare_pipeline_stage0_microbatches(
                       GenWorkerEts,
                       SourceName,
                       BatchID,
                       {NerlTensorOfSamples, NerlTensorType},
                       ParallelMode
                     ) of
                  {ok, NumDispatched, RemainingQueue, TotalMicrobatches} ->
                    ActiveBatchCtx = #{
                      model_id => ModelId,
                      source_name => SourceName,
                      batch_id => BatchID,
                      mode => pipeline,
                      stage => StageID,
                      total_microbatches => TotalMicrobatches,
                      forward_dispatched => NumDispatched,
                      backward_completed => 0
                    },
                    ets:update_element(GenWorkerEts, parallel_pending_losses, {?ETS_KEYVAL_VAL_IDX, TotalMicrobatches}),
                    ets:update_element(GenWorkerEts, parallel_total_microbatches, {?ETS_KEYVAL_VAL_IDX, TotalMicrobatches}),
                    ets:update_element(GenWorkerEts, parallel_loss_acc, {?ETS_KEYVAL_VAL_IDX, undefined}),
                    ets:update_element(GenWorkerEts, parallel_time_acc, {?ETS_KEYVAL_VAL_IDX, 0.0}),
                    ets:update_element(GenWorkerEts, parallel_microbatch_queue, {?ETS_KEYVAL_VAL_IDX, RemainingQueue}),
                    ets:update_element(GenWorkerEts, parallel_active_batch_ctx, {?ETS_KEYVAL_VAL_IDX, ActiveBatchCtx}),
                    ?LOG_INFO(
                      "Worker ~p stage0 pipeline batch=~p source=~p total_microbatches=~p dispatched=~p queued=~p",
                      [State#workerGeneric_state.myName, BatchID, SourceName, TotalMicrobatches, NumDispatched, length(RemainingQueue)]
                    ),
                    {next_state, wait, State#workerGeneric_state{nextState = train, currentBatchID = BatchID}};
                  {abort, Stage0Reason} ->
                    ?LOG_ERROR(
                      "Worker ~p stage0 pipeline prepare failed batch=~p source=~p reason=~p",
                      [State#workerGeneric_state.myName, BatchID, SourceName, Stage0Reason]
                    ),
                    notify_worker_parallel_abort(
                      GenWorkerEts,
                      {pipeline_stage0_prepare_failed, BatchID, SourceName, Stage0Reason}
                    ),
                    reset_parallel_loss_context(GenWorkerEts),
                    {next_state, train, State#workerGeneric_state{nextState = train, currentBatchID = BatchID}}
                end;
              _NonZeroStage ->
                ?LOG_INFO(
                  "Worker ~p pipeline stage ~p ignored source batch=~p from ~p; waiting for upstream activation",
                  [State#workerGeneric_state.myName, StageID, BatchID, SourceName]
                ),
                {next_state, train, State#workerGeneric_state{nextState = train, currentBatchID = BatchID}}
            end;
          false ->
            DistributedBehaviorFunc(pre_train, {GenWorkerEts,DistributedWorkerData}),
            WorkersStatsEts = get(worker_stats_ets),
            stats:increment_by_value(WorkersStatsEts , batches_received_train , 1),
            case maybe_parallel_train_microbatch_path(GenWorkerEts, ModelId, SourceName, BatchID, {NerlTensorOfSamples, NerlTensorType}) of
          {ok, ParallelMode, NumDispatched, RemainingQueue} ->
            NumMicrobatches = NumDispatched + length(RemainingQueue),
            ActiveBatchCtx = #{
              model_id => ModelId,
              source_name => SourceName,
              batch_id => BatchID,
              mode => ParallelMode
            },
            ets:update_element(GenWorkerEts, parallel_pending_losses, {?ETS_KEYVAL_VAL_IDX, NumMicrobatches}),
            ets:update_element(GenWorkerEts, parallel_total_microbatches, {?ETS_KEYVAL_VAL_IDX, NumMicrobatches}),
            ets:update_element(GenWorkerEts, parallel_loss_acc, {?ETS_KEYVAL_VAL_IDX, undefined}),
            ets:update_element(GenWorkerEts, parallel_time_acc, {?ETS_KEYVAL_VAL_IDX, 0.0}),
            ets:update_element(GenWorkerEts, parallel_microbatch_queue, {?ETS_KEYVAL_VAL_IDX, RemainingQueue}),
            ets:update_element(GenWorkerEts, parallel_active_batch_ctx, {?ETS_KEYVAL_VAL_IDX, ActiveBatchCtx}),
            case NumMicrobatches > 0 of
              true ->
                {next_state, wait, State#workerGeneric_state{nextState = train, currentBatchID = BatchID}};
              false ->
                notify_worker_parallel_abort(
                  GenWorkerEts,
                  {parallel_train_path_rejected, BatchID, SourceName, no_microbatches_dispatched}
                ),
                reset_parallel_loss_context(GenWorkerEts),
                {next_state, train, State#workerGeneric_state{nextState = train, currentBatchID = BatchID}}
            end;
          {abort, AbortReason} ->
            case is_nonfatal_tp_batch_abort(AbortReason) of
              true ->
                handle_nonfatal_tp_batch_abort(
                  GenWorkerEts,
                  State#workerGeneric_state.myName,
                  BatchID,
                  SourceName,
                  train,
                  AbortReason
                ),
                reset_parallel_loss_context(GenWorkerEts),
                {next_state, train, State#workerGeneric_state{nextState = train, currentBatchID = BatchID}};
              false ->
                notify_worker_parallel_abort(
                  GenWorkerEts,
                  {parallel_train_path_rejected, BatchID, SourceName, AbortReason}
                ),
                reset_parallel_loss_context(GenWorkerEts),
                {next_state, train, State#workerGeneric_state{nextState = train, currentBatchID = BatchID}}
            end
            end
        end
    end;

train(cast, LossMicrobatch = {loss_microbatch, _LossTensor, _TrainTime, _BatchID, _SourceName, _MicrobatchID}, State) ->
  wait(cast, LossMicrobatch, State);
  
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

train(cast, {parallel_check_end_stream_flush, ModelPhase},
      State = #workerGeneric_state{distributedBehaviorFunc = DistributedBehaviorFunc}) ->
  maybe_retry_pending_end_stream_flush(get(generic_worker_ets), ModelPhase, DistributedBehaviorFunc),
  {keep_state, State};

train(cast, {end_stream , StreamName}, State = #workerGeneric_state{myName = _MyName , distributedBehaviorFunc = DistributedBehaviorFunc}) ->
  handle_end_stream_event(DistributedBehaviorFunc, train, StreamName),
  {next_state, train, State};

train(cast, {idle}, State = #workerGeneric_state{myName = MyName , distributedBehaviorFunc = DistributedBehaviorFunc}) ->
  reset_parallel_runtime_for_idle(get(generic_worker_ets)),
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
predict(cast, {set_parallel_mode, Mode}, State) ->
  set_worker_parallel_mode(get(generic_worker_ets), Mode),
  {keep_state, State};

predict(cast, {set_parallel_execution, ParallelExecution}, State) ->
  set_worker_parallel_execution(get(generic_worker_ets), ParallelExecution),
  {keep_state, State};

predict(cast, {set_parallel_authority, Enabled}, State) ->
  set_worker_parallel_authority(get(generic_worker_ets), Enabled),
  {keep_state, State};

predict(cast, {parallel_scheduler_grant, Direction, BatchID, MicrobatchID, StageID}, State) ->
  GenWorkerEts = get(generic_worker_ets),
  case is_buffer_over_cap(GenWorkerEts) of
    true ->
      reject_grant_for_congestion(GenWorkerEts, State#workerGeneric_state.myName, Direction, BatchID, MicrobatchID, StageID),
      {keep_state, State};
    false ->
      append_parallel_scheduler_grant(GenWorkerEts, Direction, BatchID, MicrobatchID, StageID),
      ParallelMode = normalize_parallel_mode_atom(
                       ets:lookup_element(GenWorkerEts, parallel_mode, ?ETS_KEYVAL_VAL_IDX)
                     ),
      case is_pipeline_mode_atom(ParallelMode) of
        true ->
          case dispatch_pipeline_predict_buffers(GenWorkerEts, State#workerGeneric_state.modelID, State#workerGeneric_state.myName) of
            ok ->
              {keep_state, State};
            {abort, Reason} ->
              notify_worker_parallel_abort(GenWorkerEts, {pipeline_predict_dispatch_failed, Reason}),
              reset_parallel_loss_context(GenWorkerEts),
            {keep_state, State}
          end;
        false ->
          {keep_state, State}
      end
  end;
predict(cast, {parallel_scheduler_grant, Direction, MicrobatchID, StageID}, State) ->
  predict(cast, {parallel_scheduler_grant, Direction, any, MicrobatchID, StageID}, State);
predict(cast, {parallel_skip_work_item, Direction, BatchID, MicrobatchID, StageID, Reason, EventID, PhaseEpoch}, State = #workerGeneric_state{myName = WorkerName}) ->
  handle_parallel_skip_work_item(get(generic_worker_ets), WorkerName, Direction, BatchID, MicrobatchID, StageID, Reason, EventID, PhaseEpoch),
  {keep_state, State};

predict(cast, {parallel_pipeline_inbox, FromWorker, Payload}, State = #workerGeneric_state{modelID = ModelId, myName = MyName}) ->
  GenWorkerEts = get(generic_worker_ets),
  case handle_parallel_pipeline_predict_inbox(GenWorkerEts, ModelId, MyName, FromWorker, Payload) of
    ok ->
      {keep_state, State};
    {abort, Reason} ->
      notify_worker_parallel_abort(GenWorkerEts, {pipeline_predict_inbox_rejected, Reason}),
      reset_parallel_loss_context(GenWorkerEts),
      {keep_state, State}
  end;

predict(cast, {pipeline_nif_result, Continuation, NifResult}, State) ->
  GenWorkerEts = get(generic_worker_ets),
  ets:update_element(GenWorkerEts, pipeline_nif_worker, {?ETS_KEYVAL_VAL_IDX, false}),
  case Continuation(NifResult) of
    processed ->
      case dispatch_pipeline_predict_buffers(GenWorkerEts, State#workerGeneric_state.modelID, State#workerGeneric_state.myName) of
        ok -> {keep_state, State};
        {abort, Reason} ->
          notify_worker_parallel_abort(GenWorkerEts, {pipeline_nif_predict_dispatch_failed, Reason}),
          reset_parallel_loss_context(GenWorkerEts),
          {keep_state, State}
      end;
    stale ->
      {keep_state, State};
    {abort, Reason} ->
      notify_worker_parallel_abort(GenWorkerEts, Reason),
      reset_parallel_loss_context(GenWorkerEts),
      {keep_state, State}
  end;

predict(cast, {sample,_CSVname, BatchID, {<<>>, _Type}}, State) ->
  ?LOG_WARNING("Received empty tensor , batch id: ~p",[BatchID]),
  WorkersStatsEts = get(worker_stats_ets),
  stats:increment_bad_messages(WorkersStatsEts),
  {next_state, predict, State#workerGeneric_state{nextState = predict , currentBatchID = BatchID}};

% send predict sample to worker
predict(cast, {sample , SourceName , BatchID , {PredictBatchTensor, Type}}, State = #workerGeneric_state{modelID = ModelId, distributedBehaviorFunc = DistributedBehaviorFunc, distributedWorkerData = DistributedWorkerData}) ->
    GenWorkerEts = get(generic_worker_ets),
    ParallelMode = normalize_parallel_mode_atom(
                     ets:lookup_element(GenWorkerEts, parallel_mode, ?ETS_KEYVAL_VAL_IDX)
                   ),
    DistributedBehaviorFunc(pre_predict, {GenWorkerEts,DistributedWorkerData}),
    WorkersStatsEts = get(worker_stats_ets),
    stats:increment_by_value(WorkersStatsEts , batches_received_predict , 1),
    case is_pipeline_mode_atom(ParallelMode) of
      true ->
        StageID = get_worker_pipeline_stage(GenWorkerEts),
        case StageID of
          0 ->
            case prepare_pipeline_predict_stage0_microbatches(
                   GenWorkerEts,
                   SourceName,
                   BatchID,
                   {PredictBatchTensor, Type},
                   ParallelMode
                 ) of
              {ok, NumDispatched, RemainingQueue, TotalMicrobatches} ->
                ActiveBatchCtx = #{
                  model_id => ModelId,
                  source_name => SourceName,
                  batch_id => BatchID,
                  mode => pipeline_predict,
                  stage => StageID,
                  total_microbatches => TotalMicrobatches,
                  forward_dispatched => NumDispatched,
                  forward_completed => 0,
                  predict_acc => []
                },
                ets:update_element(GenWorkerEts, parallel_total_microbatches, {?ETS_KEYVAL_VAL_IDX, TotalMicrobatches}),
                ets:update_element(GenWorkerEts, parallel_pending_losses, {?ETS_KEYVAL_VAL_IDX, 0}),
                ets:update_element(GenWorkerEts, parallel_time_acc, {?ETS_KEYVAL_VAL_IDX, 0.0}),
                ets:update_element(GenWorkerEts, parallel_microbatch_queue, {?ETS_KEYVAL_VAL_IDX, RemainingQueue}),
                ets:update_element(GenWorkerEts, parallel_active_batch_ctx, {?ETS_KEYVAL_VAL_IDX, ActiveBatchCtx}),
                ?LOG_INFO(
                  "Worker ~p stage0 pipeline predict batch=~p source=~p total_microbatches=~p dispatched=~p queued=~p",
                  [State#workerGeneric_state.myName, BatchID, SourceName, TotalMicrobatches, NumDispatched, length(RemainingQueue)]
                ),
                {next_state, wait, State#workerGeneric_state{nextState = predict , currentBatchID = BatchID}};
              {abort, PredictReason} ->
                notify_worker_parallel_abort(GenWorkerEts, {pipeline_predict_stage0_prepare_failed, BatchID, SourceName, PredictReason}),
                reset_parallel_loss_context(GenWorkerEts),
                {next_state, predict, State#workerGeneric_state{nextState = predict , currentBatchID = BatchID}}
            end;
          _ ->
            ?LOG_INFO(
              "Worker ~p pipeline predict stage ~p ignored source batch=~p from ~p; waiting for upstream activation",
              [State#workerGeneric_state.myName, StageID, BatchID, SourceName]
            ),
            {next_state, predict, State#workerGeneric_state{nextState = predict , currentBatchID = BatchID}}
        end;
      false ->
        case maybe_parallel_predict_microbatch_path(GenWorkerEts, ModelId, SourceName, BatchID, {PredictBatchTensor, Type}) of
          {ok, _ParMode, NumDispatched, RemainingQueue} ->
            NumMicrobatches = NumDispatched + length(RemainingQueue),
            ActiveBatchCtx = #{
              model_id => ModelId,
              source_name => SourceName,
              batch_id => BatchID,
              mode => tensor_predict,
              total_microbatches => NumMicrobatches,
              forward_dispatched => NumDispatched,
              forward_completed => 0,
              predict_acc => []
            },
            ets:update_element(GenWorkerEts, parallel_pending_losses, {?ETS_KEYVAL_VAL_IDX, NumMicrobatches}),
            ets:update_element(GenWorkerEts, parallel_total_microbatches, {?ETS_KEYVAL_VAL_IDX, NumMicrobatches}),
            ets:update_element(GenWorkerEts, parallel_time_acc, {?ETS_KEYVAL_VAL_IDX, 0.0}),
            ets:update_element(GenWorkerEts, parallel_microbatch_queue, {?ETS_KEYVAL_VAL_IDX, RemainingQueue}),
            ets:update_element(GenWorkerEts, parallel_active_batch_ctx, {?ETS_KEYVAL_VAL_IDX, ActiveBatchCtx}),
            case NumMicrobatches > 0 of
              true ->
                {next_state, wait, State#workerGeneric_state{nextState = predict, currentBatchID = BatchID}};
              false ->
                handle_nonfatal_tp_batch_abort(
                  GenWorkerEts,
                  State#workerGeneric_state.myName,
                  BatchID,
                  SourceName,
                  predict,
                  no_microbatches_dispatched
                ),
                reset_parallel_loss_context(GenWorkerEts),
                {next_state, predict, State#workerGeneric_state{nextState = predict, currentBatchID = BatchID}}
            end;
          {abort, AbortReason} ->
            case is_nonfatal_tp_batch_abort(AbortReason) of
              true ->
                handle_nonfatal_tp_batch_abort(
                  GenWorkerEts,
                  State#workerGeneric_state.myName,
                  BatchID,
                  SourceName,
                  predict,
                  AbortReason
                ),
                reset_parallel_loss_context(GenWorkerEts),
                {next_state, predict, State#workerGeneric_state{nextState = predict, currentBatchID = BatchID}};
              false ->
                ?LOG_WARNING("Worker ~p parallel predict microbatch path aborted batch=~p reason=~p, falling back to legacy predict",
                  [State#workerGeneric_state.myName, BatchID, AbortReason]),
                nif_call(call_to_predict, [ModelId, {PredictBatchTensor, Type}, BatchID, SourceName]),
                {next_state, wait, State#workerGeneric_state{nextState = predict, currentBatchID = BatchID}}
            end
        end
    end;

predict(cast, PredictResMsg = {predictRes, PredNerlTensor, PredNerlTensorType, TimeNif, BatchID , SourceName},
        State = #workerGeneric_state{myName = MyName, distributedBehaviorFunc = DistributedBehaviorFunc, distributedWorkerData = DistributedWorkerData}) ->
  GenWorkerEts = get(generic_worker_ets),
  ActiveCtx = ets:lookup_element(GenWorkerEts, parallel_active_batch_ctx, ?ETS_KEYVAL_VAL_IDX),
  case ActiveCtx of
    Ctx when is_map(Ctx) andalso is_map_key(mode, Ctx) andalso map_get(mode, Ctx) =:= tensor_predict ->
      %% Late tensor microbatch results can arrive while we already transitioned back to predict.
      %% Reuse wait-state accumulation logic instead of crashing the worker.
      ?LOG_WARNING(
        "Worker ~p received late tensor predict result while in predict state; delegating to wait handler batch=~p source=~p",
        [MyName, BatchID, SourceName]
      ),
      wait(cast, PredictResMsg, State);
    _ ->
      %% Legacy fallback predict path can complete after the worker already re-entered predict.
      %% Accept and forward this result instead of treating it as an unknown message.
      ?LOG_WARNING(
        "Worker ~p received late predictRes while in predict state; forwarding result batch=~p source=~p",
        [MyName, BatchID, SourceName]
      ),
      BatchTimeStamp = erlang:system_time(nanosecond),
      WorkerToken = ets:lookup_element(GenWorkerEts, distributed_system_token, ?ETS_KEYVAL_VAL_IDX),
      gen_statem:cast(
        get(client_pid),
        {predictRes, MyName, SourceName, {PredNerlTensor, PredNerlTensorType}, TimeNif, WorkerToken, BatchID, BatchTimeStamp}
      ),
      stats:increment_by_value(get(worker_stats_ets), acc_time_prediction, trunc(TimeNif)),
      NextStateBehavior = DistributedBehaviorFunc(post_predict, {GenWorkerEts, DistributedWorkerData}),
      maybe_finalize_pending_end_streams(DistributedBehaviorFunc, predict),
      {next_state, NextStateBehavior, State#workerGeneric_state{nextState = NextStateBehavior}}
  end;

predict(cast, {start_stream , SourceName}, State = #workerGeneric_state{myName = _MyName , distributedBehaviorFunc = DistributedBehaviorFunc}) ->
  stream_handler(start_stream, predict, SourceName, DistributedBehaviorFunc),
  {next_state, predict, State};

predict(cast, {parallel_check_end_stream_flush, ModelPhase},
        State = #workerGeneric_state{distributedBehaviorFunc = DistributedBehaviorFunc}) ->
  maybe_retry_pending_end_stream_flush(get(generic_worker_ets), ModelPhase, DistributedBehaviorFunc),
  {keep_state, State};

predict(cast, {end_stream , SourceName}, State = #workerGeneric_state{myName = _MyName , distributedBehaviorFunc = DistributedBehaviorFunc}) ->
  handle_end_stream_event(DistributedBehaviorFunc, predict, SourceName),
  {next_state, predict, State};

predict(cast, {idle}, State = #workerGeneric_state{myName = MyName , distributedBehaviorFunc = DistributedBehaviorFunc}) ->
  reset_parallel_runtime_for_idle(get(generic_worker_ets)),
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
  ActiveStreams = ets:lookup_element(GenWorkerEts, active_streams, ?ETS_KEYVAL_VAL_IDX),
  StreamTuple = {MyName, StreamName},
  NewActiveStreams = 
      case StreamPhase of
          start_stream ->
            case lists:member(StreamTuple, ActiveStreams) of
              true -> ActiveStreams;
              false -> ActiveStreams ++ [StreamTuple]
            end;
          end_stream ->
            [Tuple || Tuple <- ActiveStreams, Tuple =/= StreamTuple]
      end,
  ets:update_element(GenWorkerEts, active_streams, {?ETS_KEYVAL_VAL_IDX, NewActiveStreams}),
  DistributedBehaviorFunc(StreamPhase, {GenWorkerEts, [StreamName , ModelPhase]}).

handle_end_stream_event(DistributedBehaviorFunc, ModelPhase, StreamName) ->
  GenWorkerEts = get(generic_worker_ets),
  queue_end_stream_waiting(GenWorkerEts, StreamName),
  case can_flush_pending_end_streams(GenWorkerEts) of
    true ->
      maybe_finalize_pending_end_streams(DistributedBehaviorFunc, ModelPhase);
    false ->
      WorkerName = ets:lookup_element(GenWorkerEts, worker_name, ?ETS_KEYVAL_VAL_IDX),
      ?LOG_INFO(
        "Worker ~p queued end_stream for source=~p and deferred stream_ended until pipeline drain",
        [WorkerName, StreamName]
      ),
      schedule_end_stream_flush_retry(ModelPhase),
      ok
  end.

maybe_finalize_pending_end_streams(DistributedBehaviorFunc, ModelPhase) ->
  GenWorkerEts = get(generic_worker_ets),
  case can_flush_pending_end_streams(GenWorkerEts) of
    false ->
      ok;
    true ->
      EndStreamWaitingList = ets:lookup_element(GenWorkerEts, end_streams_waiting_list, ?ETS_KEYVAL_VAL_IDX),
      case EndStreamWaitingList of
        [] ->
          ok;
        _ ->
          lists:foreach(
            fun(StreamName) ->
              flush_end_stream(GenWorkerEts, ModelPhase, StreamName, DistributedBehaviorFunc)
            end,
            EndStreamWaitingList
          ),
          ets:update_element(GenWorkerEts, end_streams_waiting_list, {?ETS_KEYVAL_VAL_IDX, []})
      end
  end.

flush_end_stream(GenWorkerEts, ModelPhase, StreamName, DistributedBehaviorFunc) ->
  MyName = ets:lookup_element(GenWorkerEts, worker_name, ?ETS_KEYVAL_VAL_IDX),
  ClientPid = ets:lookup_element(GenWorkerEts, client_pid, ?ETS_KEYVAL_VAL_IDX),
  DrainPhase = normalize_phase_for_client(ModelPhase),
  gen_statem:cast(ClientPid, {stream_ended, {MyName, StreamName}}),
  try stream_handler(end_stream, ModelPhase, StreamName, DistributedBehaviorFunc) of
    _ -> ok
  catch
    Class:Reason:Stacktrace ->
      ?LOG_WARNING(
        "Worker ~p failed stream_handler(end_stream) source=~p phase=~p error=~p stack=~p",
        [MyName, StreamName, ModelPhase, {Class, Reason}, Stacktrace]
      )
  end,
  gen_statem:cast(ClientPid, {parallel_worker_drain_ready, MyName, DrainPhase}).

queue_end_stream_waiting(GenWorkerEts, StreamName) ->
  CurrentEndStreamWaitingList = ets:lookup_element(GenWorkerEts, end_streams_waiting_list, ?ETS_KEYVAL_VAL_IDX),
  NewEndStreamWaitingList =
    case lists:member(StreamName, CurrentEndStreamWaitingList) of
      true -> CurrentEndStreamWaitingList;
      false -> CurrentEndStreamWaitingList ++ [StreamName]
    end,
  ets:update_element(GenWorkerEts, end_streams_waiting_list, {?ETS_KEYVAL_VAL_IDX, NewEndStreamWaitingList}).

can_flush_pending_end_streams(GenWorkerEts) ->
  EndStreamWaitingList = ets:lookup_element(GenWorkerEts, end_streams_waiting_list, ?ETS_KEYVAL_VAL_IDX),
  case EndStreamWaitingList =/= [] of
    false ->
      false;
    true ->
      maybe_drop_stale_scheduler_grants_for_stream_end(GenWorkerEts),
      is_pipeline_stream_end_flush_ready(GenWorkerEts)
  end.

schedule_end_stream_flush_retry(ModelPhase) ->
  erlang:send_after(10, self(), {'$gen_cast', {parallel_check_end_stream_flush, ModelPhase}}).

maybe_retry_pending_end_stream_flush(GenWorkerEts, ModelPhase, DistributedBehaviorFunc) ->
  EndStreamWaitingList = ets:lookup_element(GenWorkerEts, end_streams_waiting_list, ?ETS_KEYVAL_VAL_IDX),
  case EndStreamWaitingList of
    [] ->
      ets:update_element(GenWorkerEts, end_stream_flush_retries, {?ETS_KEYVAL_VAL_IDX, 0}),
      ok;
    _ ->
      case can_flush_pending_end_streams(GenWorkerEts) of
        true ->
          ets:update_element(GenWorkerEts, end_stream_flush_retries, {?ETS_KEYVAL_VAL_IDX, 0}),
          maybe_finalize_pending_end_streams(DistributedBehaviorFunc, ModelPhase);
        false ->
          Retries = ets:lookup_element(GenWorkerEts, end_stream_flush_retries, ?ETS_KEYVAL_VAL_IDX),
          case Retries >= 100 of
            true ->
              WorkerName = ets:lookup_element(GenWorkerEts, worker_name, ?ETS_KEYVAL_VAL_IDX),
              ?LOG_WARNING(
                "Worker ~p force-flushing end_stream after ~p retries (1s drain timeout) waiting_list=~p",
                [WorkerName, Retries, EndStreamWaitingList]
              ),
              ets:update_element(GenWorkerEts, end_stream_flush_retries, {?ETS_KEYVAL_VAL_IDX, 0}),
              maybe_drop_stale_scheduler_grants_for_stream_end(GenWorkerEts),
              lists:foreach(
                fun(StreamName) ->
                  flush_end_stream(GenWorkerEts, ModelPhase, StreamName, DistributedBehaviorFunc)
                end,
                EndStreamWaitingList
              ),
              ets:update_element(GenWorkerEts, end_streams_waiting_list, {?ETS_KEYVAL_VAL_IDX, []});
            false ->
              ets:update_element(GenWorkerEts, end_stream_flush_retries, {?ETS_KEYVAL_VAL_IDX, Retries + 1}),
              schedule_end_stream_flush_retry(ModelPhase)
          end
      end
  end.

is_pipeline_stream_end_flush_ready(GenWorkerEts) ->
  Mode = normalize_parallel_mode_atom(
           ets:lookup_element(GenWorkerEts, parallel_mode, ?ETS_KEYVAL_VAL_IDX)
         ),
  case Mode of
    pipeline ->
      not has_pending_parallel_runtime_for_stream_end(GenWorkerEts);
    pipeline_tensor ->
      not has_pending_parallel_runtime_for_stream_end(GenWorkerEts);
    _ ->
      true
  end.

has_pending_parallel_runtime_for_stream_end(GenWorkerEts) ->
  HasPendingNonGrantRuntime = has_pending_runtime_excluding_scheduler_grants(GenWorkerEts),
  SchedulerGrants = ets:lookup_element(GenWorkerEts, parallel_scheduler_grants, ?ETS_KEYVAL_VAL_IDX),
  HasPendingNonGrantRuntime orelse SchedulerGrants =/= [].

has_pending_runtime_excluding_scheduler_grants(GenWorkerEts) ->
  ActiveCtx = ets:lookup_element(GenWorkerEts, parallel_active_batch_ctx, ?ETS_KEYVAL_VAL_IDX),
  PendingLosses = ets:lookup_element(GenWorkerEts, parallel_pending_losses, ?ETS_KEYVAL_VAL_IDX),
  MicrobatchQueue = ets:lookup_element(GenWorkerEts, parallel_microbatch_queue, ?ETS_KEYVAL_VAL_IDX),
  DeferredSamples = ets:lookup_element(GenWorkerEts, parallel_deferred_samples, ?ETS_KEYVAL_VAL_IDX),
  ForwardBuffer = ets:lookup_element(GenWorkerEts, parallel_pipeline_forward_buffer, ?ETS_KEYVAL_VAL_IDX),
  BackwardBuffer = ets:lookup_element(GenWorkerEts, parallel_pipeline_backward_buffer, ?ETS_KEYVAL_VAL_IDX),
  PredictBuffer = ets:lookup_element(GenWorkerEts, parallel_pipeline_predict_buffer, ?ETS_KEYVAL_VAL_IDX),
  PendingBackwardEvents = ets:lookup_element(GenWorkerEts, parallel_pending_backward_events, ?ETS_KEYVAL_VAL_IDX),
  CollectiveInbox = ets:lookup_element(GenWorkerEts, tp_collective_inbox_buffer, ?ETS_KEYVAL_VAL_IDX),
  ActiveCtx =/= undefined orelse
    PendingLosses > 0 orelse
    MicrobatchQueue =/= [] orelse
    DeferredSamples =/= [] orelse
    ForwardBuffer =/= [] orelse
    BackwardBuffer =/= [] orelse
    PredictBuffer =/= [] orelse
    PendingBackwardEvents =/= [] orelse
    CollectiveInbox =/= [].

maybe_drop_stale_scheduler_grants_for_stream_end(GenWorkerEts) ->
  HasPendingNonGrantRuntime = has_pending_runtime_excluding_scheduler_grants(GenWorkerEts),
  SchedulerGrants = ets:lookup_element(GenWorkerEts, parallel_scheduler_grants, ?ETS_KEYVAL_VAL_IDX),
  case {HasPendingNonGrantRuntime, SchedulerGrants =/= []} of
    {false, true} ->
      WorkerName = ets:lookup_element(GenWorkerEts, worker_name, ?ETS_KEYVAL_VAL_IDX),
      ?LOG_WARNING(
        "Worker ~p dropping stale scheduler grants during end_stream drain grants=~p",
        [WorkerName, SchedulerGrants]
      ),
      ets:update_counter(get(worker_stats_ets), skip_phase_close_drain, length(SchedulerGrants)),
      ets:update_element(GenWorkerEts, parallel_scheduler_grants, {?ETS_KEYVAL_VAL_IDX, []});
    _ ->
      ok
  end.

has_orphaned_runtime_after_phase_close(GenWorkerEts) ->
  ActiveStreams = ets:lookup_element(GenWorkerEts, active_streams, ?ETS_KEYVAL_VAL_IDX),
  EndStreamWaitingList = ets:lookup_element(GenWorkerEts, end_streams_waiting_list, ?ETS_KEYVAL_VAL_IDX),
  SchedulerGrants = ets:lookup_element(GenWorkerEts, parallel_scheduler_grants, ?ETS_KEYVAL_VAL_IDX),
  DeferredSamples = ets:lookup_element(GenWorkerEts, parallel_deferred_samples, ?ETS_KEYVAL_VAL_IDX),
  ForwardBuffer = ets:lookup_element(GenWorkerEts, parallel_pipeline_forward_buffer, ?ETS_KEYVAL_VAL_IDX),
  BackwardBuffer = ets:lookup_element(GenWorkerEts, parallel_pipeline_backward_buffer, ?ETS_KEYVAL_VAL_IDX),
  PredictBuffer = ets:lookup_element(GenWorkerEts, parallel_pipeline_predict_buffer, ?ETS_KEYVAL_VAL_IDX),
  PendingBackwardEvents = ets:lookup_element(GenWorkerEts, parallel_pending_backward_events, ?ETS_KEYVAL_VAL_IDX),
  CollectiveInbox = ets:lookup_element(GenWorkerEts, tp_collective_inbox_buffer, ?ETS_KEYVAL_VAL_IDX),
  ActiveCtx = ets:lookup_element(GenWorkerEts, parallel_active_batch_ctx, ?ETS_KEYVAL_VAL_IDX),
  MicrobatchQueue = ets:lookup_element(GenWorkerEts, parallel_microbatch_queue, ?ETS_KEYVAL_VAL_IDX),
  PendingLosses = ets:lookup_element(GenWorkerEts, parallel_pending_losses, ?ETS_KEYVAL_VAL_IDX),
  ActiveStreams =:= [] andalso
    EndStreamWaitingList =:= [] andalso
    SchedulerGrants =:= [] andalso
    ForwardBuffer =:= [] andalso
    BackwardBuffer =:= [] andalso
    PredictBuffer =:= [] andalso
    PendingBackwardEvents =:= [] andalso
    CollectiveInbox =:= [] andalso
    (ActiveCtx =/= undefined orelse MicrobatchQueue =/= [] orelse
     PendingLosses > 0 orelse DeferredSamples =/= []).

normalize_phase_for_client(training) -> training;
normalize_phase_for_client(train) -> training;
normalize_phase_for_client(prediction) -> prediction;
normalize_phase_for_client(predict) -> prediction;
normalize_phase_for_client(Value) when is_binary(Value) ->
  normalize_phase_for_client(binary_to_list(Value));
normalize_phase_for_client(Value) when is_list(Value) ->
  case string:lowercase(string:trim(Value)) of
    "training" -> training;
    "train" -> training;
    "prediction" -> prediction;
    "predict" -> prediction;
    _ -> training
  end;
normalize_phase_for_client(_) ->
  training.

get_worker_parallel_cfg(WorkerName) ->
  case catch ets:lookup_element(nerlnet_data, workers_parallel, ?ETS_KEYVAL_VAL_IDX) of
    {'EXIT', _} -> #{};
    WorkerParallelMap when is_map(WorkerParallelMap) -> maps:get(WorkerName, WorkerParallelMap, #{});
    _ -> #{}
  end.

get_worker_pipeline_stage(GenWorkerEts) ->
  WorkerParallelCfg = ets:lookup_element(GenWorkerEts, worker_parallel_cfg, ?ETS_KEYVAL_VAL_IDX),
  case maps:get(pipeline_stage, WorkerParallelCfg, 0) of
    Stage when is_integer(Stage) -> Stage;
    _ -> 0
  end.

get_worker_model_sha(TrainParams) when is_map(TrainParams) ->
  case maps:get("model_sha", TrainParams, maps:get(model_sha, TrainParams, "")) of
    Value when is_list(Value) -> Value;
    Value when is_binary(Value) -> binary_to_list(Value);
    Value when is_atom(Value) -> atom_to_list(Value);
    _ -> ""
  end;
get_worker_model_sha(_TrainParams) ->
  "".

get_worker_tp_plan("") ->
  [];
get_worker_tp_plan(ModelSha) ->
  case catch ets:lookup_element(nerlnet_data, model_tp_plan_map, ?ETS_KEYVAL_VAL_IDX) of
    {'EXIT', _} ->
      [];
    ModelTpPlanMap when is_map(ModelTpPlanMap) ->
      RawTpPlan = maps:get(ModelSha, ModelTpPlanMap, []),
      normalize_worker_tp_plan(RawTpPlan);
    _ ->
      []
  end.

normalize_worker_tp_plan(RawTpPlan) when is_list(RawTpPlan) ->
  lists:map(fun normalize_worker_tp_plan_entry/1, RawTpPlan);
normalize_worker_tp_plan(_) ->
  [].

normalize_worker_tp_plan_entry(Entry) when is_map(Entry) ->
  Layer = normalize_tp_string(maps:get(layer, Entry, maps:get(<<"layer">>, Entry, ""))),
  Mode = string:lowercase(normalize_tp_string(maps:get(mode, Entry, maps:get(<<"mode">>, Entry, "column")))),
  Group = normalize_tp_string(maps:get(group, Entry, maps:get(<<"group">>, Entry, ""))),
  ShardAxis = normalize_tp_int(
                maps:get(shard_axis, Entry, maps:get(shardAxis, Entry, maps:get(<<"shardAxis">>, Entry, 0))),
                0
              ),
  #{
    layer => Layer,
    mode => Mode,
    group => Group,
    shard_axis => ShardAxis
  };
normalize_worker_tp_plan_entry(_) ->
  #{
    layer => "",
    mode => "column",
    group => "",
    shard_axis => 0
  }.

build_worker_tp_group_state(WorkerName, WorkerParallelCfg) ->
  TpGroup = normalize_tp_string(maps:get(tp_group, WorkerParallelCfg, "")),
  TpRank = normalize_tp_int(maps:get(tp_rank, WorkerParallelCfg, 0), 0),
  TpWorldSizeCfg = normalize_tp_int(maps:get(tp_world_size, WorkerParallelCfg, 1), 1),
  case TpGroup of
    "" ->
      #{
        tp_group => "",
        tp_rank => TpRank,
        tp_world_size => 1,
        ordered_workers => [WorkerName],
        worker_to_rank => #{WorkerName => TpRank},
        rank_to_worker => #{TpRank => WorkerName}
      };
    _ ->
      WorkersParallelMap =
        case catch ets:lookup_element(nerlnet_data, workers_parallel, ?ETS_KEYVAL_VAL_IDX) of
          {'EXIT', _} -> #{};
          Map when is_map(Map) -> Map;
          _ -> #{}
        end,
      Members =
        maps:fold(
          fun(CandidateWorker, CandidateCfg, Acc) ->
            CandidateGroup = normalize_tp_string(maps:get(tp_group, CandidateCfg, "")),
            case CandidateGroup =:= TpGroup of
              true ->
                CandidateRank = normalize_tp_int(maps:get(tp_rank, CandidateCfg, 0), 0),
                [{CandidateRank, CandidateWorker} | Acc];
              false ->
                Acc
            end
          end,
          [],
          WorkersParallelMap
        ),
      SortedMembers = lists:keysort(1, Members),
      OrderedWorkers = [CandidateWorker || {_Rank, CandidateWorker} <- SortedMembers],
      WorkerToRank = maps:from_list([{CandidateWorker, Rank} || {Rank, CandidateWorker} <- SortedMembers]),
      RankToWorker = maps:from_list(SortedMembers),
      InferredWorldSize = erlang:max(1, erlang:max(TpWorldSizeCfg, length(OrderedWorkers))),
      #{
        tp_group => TpGroup,
        tp_rank => TpRank,
        tp_world_size => InferredWorldSize,
        ordered_workers => OrderedWorkers,
        worker_to_rank => WorkerToRank,
        rank_to_worker => RankToWorker
      }
  end.

filter_tp_plan_for_worker(TpPlan, WorkerParallelCfg) ->
  TpGroup = normalize_tp_string(maps:get(tp_group, WorkerParallelCfg, "")),
  case TpGroup of
    "" ->
      [];
    _ ->
      [PlanEntry || PlanEntry <- TpPlan, normalize_tp_string(maps:get(group, PlanEntry, "")) =:= TpGroup]
  end.

normalize_tp_int(Value, _Default) when is_integer(Value) ->
  Value;
normalize_tp_int(Value, Default) when is_binary(Value) ->
  normalize_tp_int(binary_to_list(Value), Default);
normalize_tp_int(Value, Default) when is_list(Value) ->
  try
    list_to_integer(string:trim(Value))
  catch
    _:_ -> Default
  end;
normalize_tp_int(_Value, Default) ->
  Default.

normalize_tp_string(Value) when is_binary(Value) -> binary_to_list(Value);
normalize_tp_string(Value) when is_list(Value) -> string:trim(Value);
normalize_tp_string(Value) when is_atom(Value) -> atom_to_list(Value);
normalize_tp_string(Value) when is_integer(Value) -> integer_to_list(Value);
normalize_tp_string(_Value) -> "".

augment_train_params_with_parallel_cfg(TrainParams, WorkerParallelCfg) ->
  BaseParams =
    case TrainParams of
      Map when is_map(Map) -> Map;
      _ -> #{}
    end,
  PipelineStage = normalize_tp_int(maps:get(pipeline_stage, WorkerParallelCfg, 0), 0),
  PipelineWorldSize = normalize_tp_int(maps:get(pipeline_world_size, WorkerParallelCfg, 1), 1),
  maps:merge(
    BaseParams,
    #{
      "pipeline_stage" => integer_to_list(PipelineStage),
      "pipeline_world_size" => integer_to_list(PipelineWorldSize)
    }
  ).

emit_parallel_event(WorkerName, Direction, BatchID, MicrobatchID, StageID, Meta) ->
  GenWorkerEts = get(generic_worker_ets),
  case maybe_consume_parallel_scheduler_grant(GenWorkerEts, Direction, BatchID, MicrobatchID, StageID) of
    {ok, GrantedBatchID} ->
      EmittedBatchID = resolve_emitted_parallel_batch_id(BatchID, GrantedBatchID),
      EventID = parallel_event_id(WorkerName, Direction, EmittedBatchID, MicrobatchID, StageID),
      ?LOG_INFO(
        "Worker ~p emits parallel event direction=~p requested_batch=~p emitted_batch=~p microbatch=~p stage=~p event_id=~p (grant-consumed)",
        [WorkerName, Direction, BatchID, EmittedBatchID, MicrobatchID, StageID, EventID]
      ),
      AugmentedMeta = maybe_augment_meta_with_congestion(GenWorkerEts, Meta),
      gen_statem:cast(get(client_pid), {parallel_event, WorkerName, Direction, EmittedBatchID, MicrobatchID, StageID, AugmentedMeta}),
      {ok, EmittedBatchID};
    {error, no_scheduler_grant} ->
      EventID = parallel_event_id(WorkerName, Direction, BatchID, MicrobatchID, StageID),
      ?LOG_INFO(
        "Worker ~p waits for super-node scheduler grant direction=~p batch=~p microbatch=~p stage=~p event_id=~p",
        [WorkerName, Direction, BatchID, MicrobatchID, StageID, EventID]
      ),
      {error, no_scheduler_grant};
    {error, GrantReason} ->
      notify_worker_parallel_abort(
        GenWorkerEts,
        {scheduler_grant_rejected, WorkerName, Direction, BatchID, MicrobatchID, StageID, GrantReason}
      ),
      {error, GrantReason}
  end.

resolve_emitted_parallel_batch_id(RequestedBatchID, GrantedBatchID) ->
  case normalize_parallel_batch_id(GrantedBatchID) of
    any ->
      normalize_parallel_batch_id(RequestedBatchID);
    NormalizedGrantedBatchID ->
      NormalizedGrantedBatchID
  end.

parallel_event_id(WorkerName, Direction, BatchID, MicrobatchID, StageID) ->
  {parallel_event, WorkerName, Direction, BatchID, MicrobatchID, StageID}.

maybe_augment_meta_with_congestion(GenWorkerEts, Meta) ->
  case ets:lookup_element(GenWorkerEts, congestion_drop_enabled, ?ETS_KEYVAL_VAL_IDX) of
    true ->
      case ets:lookup_element(GenWorkerEts, parallel_congestion_signaled, ?ETS_KEYVAL_VAL_IDX) of
        true ->
          DeferredLen = length(ets:lookup_element(GenWorkerEts, parallel_deferred_samples, ?ETS_KEYVAL_VAL_IDX)),
          #{meta => Meta, congestion => DeferredLen};
        false -> Meta
      end;
    false -> Meta
  end.

handle_parallel_skip_work_item(
  GenWorkerEts,
  WorkerName,
  Direction,
  BatchID,
  MicrobatchID,
  StageID,
  Reason,
  EventID,
  PhaseEpoch
) ->
  NormalizedDirection = normalize_parallel_direction_atom(Direction),
  NormalizedBatchID = normalize_parallel_batch_id(BatchID),
  drop_matching_scheduler_grants_for_skip(GenWorkerEts, NormalizedDirection, NormalizedBatchID, MicrobatchID, StageID),
  prune_runtime_buffers_for_skip(GenWorkerEts, NormalizedBatchID, MicrobatchID, StageID),
  increment_skip_reason_counter(get(worker_stats_ets), Reason),
  case NormalizedDirection of
    forward ->
      ets:update_counter(GenWorkerEts, parallel_microbatches_skipped, 1);
    _ ->
      ok
  end,
  SkipMeta = #{
    reason => Reason,
    event_id => EventID,
    phase_epoch => PhaseEpoch
  },
  gen_statem:cast(
    get(client_pid),
    {parallel_skip_event, WorkerName, NormalizedDirection, NormalizedBatchID, MicrobatchID, StageID, SkipMeta}
  ).

increment_skip_reason_counter(WorkerStatsEts, Reason) ->
  case skip_reason_counter_key(Reason) of
    none ->
      ok;
    CounterKey ->
      ets:update_counter(WorkerStatsEts, CounterKey, 1)
  end.

skip_reason_counter_key({Tag, _}) when is_atom(Tag) ->
  skip_reason_counter_key(Tag);
skip_reason_counter_key(skip_grant_accept_timeout) ->
  skip_grant_accept_timeout;
skip_reason_counter_key(skip_payload_delivery_timeout) ->
  skip_payload_delivery_timeout;
skip_reason_counter_key(skip_completion_timeout) ->
  skip_completion_timeout;
skip_reason_counter_key(skip_phase_close_drain) ->
  skip_phase_close_drain;
skip_reason_counter_key(stale_event_after_skip) ->
  stale_event_after_skip;
skip_reason_counter_key(skip_congestion_drop) ->
  skip_congestion_drop;
skip_reason_counter_key(buffer_congestion) ->
  congestion_grant_rejected;
skip_reason_counter_key(_) ->
  none.

drop_matching_scheduler_grants_for_skip(GenWorkerEts, Direction, BatchID, MicrobatchID, StageID) ->
  Grants = ets:lookup_element(GenWorkerEts, parallel_scheduler_grants, ?ETS_KEYVAL_VAL_IDX),
  RemainingGrants =
    lists:filter(
      fun(Grant) ->
        case normalize_parallel_scheduler_grant(Grant) of
          {ok, {GrantDirection, GrantBatchID, GrantMicrobatchID, GrantStageID}} ->
            not skip_grant_matches(
                  Direction,
                  BatchID,
                  MicrobatchID,
                  StageID,
                  GrantDirection,
                  GrantBatchID,
                  GrantMicrobatchID,
                  GrantStageID
                );
          _ ->
            true
        end
      end,
      Grants
    ),
  ets:update_element(GenWorkerEts, parallel_scheduler_grants, {?ETS_KEYVAL_VAL_IDX, RemainingGrants}).

skip_grant_matches(Direction, BatchID, MicrobatchID, StageID, GrantDirection, GrantBatchID, GrantMicrobatchID, GrantStageID) ->
  Direction =:= GrantDirection andalso
    MicrobatchID =:= GrantMicrobatchID andalso
    StageID =:= GrantStageID andalso
    skip_batch_matches(BatchID, GrantBatchID).

skip_batch_matches(any, _GrantBatchID) ->
  true;
skip_batch_matches(_BatchID, any) ->
  true;
skip_batch_matches(BatchID, GrantBatchID) ->
  BatchID =:= GrantBatchID.

prune_runtime_buffers_for_skip(GenWorkerEts, BatchID, MicrobatchID, StageID) ->
  prune_active_batch_state_for_skip(GenWorkerEts, BatchID),
  prune_pending_backward_events_for_skip(GenWorkerEts, BatchID, MicrobatchID, StageID),
  prune_pipeline_buffers_for_skip(GenWorkerEts, BatchID, MicrobatchID),
  prune_deferred_samples_for_skip(GenWorkerEts, BatchID).

prune_active_batch_state_for_skip(GenWorkerEts, BatchID) ->
  ActiveCtx = ets:lookup_element(GenWorkerEts, parallel_active_batch_ctx, ?ETS_KEYVAL_VAL_IDX),
  case ActiveCtx of
    Ctx when is_map(Ctx) ->
      ActiveBatchID = normalize_parallel_batch_id(maps:get(batch_id, Ctx, any)),
      case skip_batch_matches(BatchID, ActiveBatchID) of
        false ->
          ok;
        true ->
          %% Clear the local batch context and invalidate in-flight stage
          %% continuations for the skipped scheduler batch.
          ets:update_counter(GenWorkerEts, parallel_nif_epoch, 1),
          ets:update_element(GenWorkerEts, pipeline_nif_worker, {?ETS_KEYVAL_VAL_IDX, false}),
          ets:update_element(GenWorkerEts, parallel_pending_losses, {?ETS_KEYVAL_VAL_IDX, 0}),
          ets:update_element(GenWorkerEts, parallel_total_microbatches, {?ETS_KEYVAL_VAL_IDX, 0}),
          ets:update_element(GenWorkerEts, parallel_loss_acc, {?ETS_KEYVAL_VAL_IDX, undefined}),
          ets:update_element(GenWorkerEts, parallel_time_acc, {?ETS_KEYVAL_VAL_IDX, 0.0}),
          ets:update_element(GenWorkerEts, parallel_microbatch_queue, {?ETS_KEYVAL_VAL_IDX, []}),
          ets:update_element(GenWorkerEts, parallel_active_batch_ctx, {?ETS_KEYVAL_VAL_IDX, undefined})
      end;
    _ ->
      ok
  end.

prune_pending_backward_events_for_skip(GenWorkerEts, BatchID, MicrobatchID, StageID) ->
  PendingEvents = ets:lookup_element(GenWorkerEts, parallel_pending_backward_events, ?ETS_KEYVAL_VAL_IDX),
  RemainingEvents =
    lists:filter(
      fun({_WorkerName, PendingBatchID, PendingMicrobatchID, PendingStageID, _TrainTime}) ->
        not (skip_batch_matches(BatchID, PendingBatchID) andalso PendingMicrobatchID =:= MicrobatchID andalso PendingStageID =:= StageID)
      end,
      PendingEvents
    ),
  ets:update_element(GenWorkerEts, parallel_pending_backward_events, {?ETS_KEYVAL_VAL_IDX, RemainingEvents}).

prune_pipeline_buffers_for_skip(GenWorkerEts, BatchID, MicrobatchID) ->
  ForwardBuffer = ets:lookup_element(GenWorkerEts, parallel_pipeline_forward_buffer, ?ETS_KEYVAL_VAL_IDX),
  BackwardBuffer = ets:lookup_element(GenWorkerEts, parallel_pipeline_backward_buffer, ?ETS_KEYVAL_VAL_IDX),
  PredictBuffer = ets:lookup_element(GenWorkerEts, parallel_pipeline_predict_buffer, ?ETS_KEYVAL_VAL_IDX),
  RemainingForward =
    lists:filter(
      fun(Payload) ->
        case Payload of
          {pipeline_forward_payload, PayloadBatchID, _SourceName, _TotalMicrobatches, PayloadMicrobatchID, _Activation, _Labels} ->
            not (skip_batch_matches(BatchID, PayloadBatchID) andalso PayloadMicrobatchID =:= MicrobatchID);
          _ ->
            true
        end
      end,
      ForwardBuffer
    ),
  RemainingBackward =
    lists:filter(
      fun(Payload) ->
        case Payload of
          {pipeline_backward_payload, PayloadBatchID, _SourceName, _TotalMicrobatches, PayloadMicrobatchID, _Grad} ->
            not (skip_batch_matches(BatchID, PayloadBatchID) andalso PayloadMicrobatchID =:= MicrobatchID);
          _ ->
            true
        end
      end,
      BackwardBuffer
    ),
  RemainingPredict =
    lists:filter(
      fun(Payload) ->
        case Payload of
          {pipeline_predict_payload, PayloadBatchID, _SourceName, _TotalMicrobatches, PayloadMicrobatchID, _Activation} ->
            not (skip_batch_matches(BatchID, PayloadBatchID) andalso PayloadMicrobatchID =:= MicrobatchID);
          _ ->
            true
        end
      end,
      PredictBuffer
    ),
  ets:update_element(GenWorkerEts, parallel_pipeline_forward_buffer, {?ETS_KEYVAL_VAL_IDX, RemainingForward}),
  ets:update_element(GenWorkerEts, parallel_pipeline_backward_buffer, {?ETS_KEYVAL_VAL_IDX, RemainingBackward}),
  ets:update_element(GenWorkerEts, parallel_pipeline_predict_buffer, {?ETS_KEYVAL_VAL_IDX, RemainingPredict}).

prune_deferred_samples_for_skip(GenWorkerEts, BatchID) ->
  DeferredSamples = ets:lookup_element(GenWorkerEts, parallel_deferred_samples, ?ETS_KEYVAL_VAL_IDX),
  RemainingSamples =
    lists:filter(
      fun(SampleTuple) ->
        case SampleTuple of
          {sample, _SourceName, SampleBatchID, _BatchPayload} ->
            not skip_batch_matches(BatchID, SampleBatchID);
          _ ->
            true
        end
      end,
      DeferredSamples
    ),
  ets:update_element(GenWorkerEts, parallel_deferred_samples, {?ETS_KEYVAL_VAL_IDX, RemainingSamples}).

maybe_emit_parallel_forward_event(tensor, _WorkerName, _BatchID, _MicrobatchID, _StageID) ->
  ok;
maybe_emit_parallel_forward_event(legacy, _WorkerName, _BatchID, _MicrobatchID, _StageID) ->
  ok;
maybe_emit_parallel_forward_event(Mode, WorkerName, BatchID, MicrobatchID, StageID) ->
  case maybe_emit_parallel_forward_event_with_batch(
         Mode,
         WorkerName,
         BatchID,
         MicrobatchID,
         StageID
       ) of
    {ok, _EmittedBatchID} ->
      ok;
    Error ->
      Error
  end.

maybe_emit_parallel_forward_event_with_batch(tensor, _WorkerName, BatchID, _MicrobatchID, _StageID) ->
  {ok, normalize_parallel_batch_id(BatchID)};
maybe_emit_parallel_forward_event_with_batch(legacy, _WorkerName, BatchID, _MicrobatchID, _StageID) ->
  {ok, normalize_parallel_batch_id(BatchID)};
maybe_emit_parallel_forward_event_with_batch(Mode, WorkerName, BatchID, MicrobatchID, StageID) ->
  GenWorkerEts = get(generic_worker_ets),
  case can_emit_parallel_event_now(GenWorkerEts, forward, BatchID, MicrobatchID, StageID) of
    ready ->
      emit_parallel_event(
        WorkerName,
        forward,
        BatchID,
        MicrobatchID,
        StageID,
        parallel_forward_event_meta(Mode)
      );
    wait_for_grant ->
      {error, no_scheduler_grant};
    {error, Reason} ->
      {error, Reason}
  end.

parallel_forward_event_meta(pipeline_predict) ->
  predict;
parallel_forward_event_meta(pipeline_tensor_predict) ->
  predict;
parallel_forward_event_meta(_) ->
  training.

parallel_predict_event_mode(pipeline_tensor) ->
  pipeline_tensor_predict;
parallel_predict_event_mode(_) ->
  pipeline_predict.

maybe_emit_parallel_backward_event(tensor, _WorkerName, _BatchID, _MicrobatchID, _StageID, _TrainTime) ->
  ok;
maybe_emit_parallel_backward_event(pipeline_tensor, _WorkerName, _BatchID, _MicrobatchID, _StageID, _TrainTime) ->
  ok;
maybe_emit_parallel_backward_event(legacy, _WorkerName, _BatchID, _MicrobatchID, _StageID, _TrainTime) ->
  ok;
maybe_emit_parallel_backward_event(_Mode, WorkerName, BatchID, MicrobatchID, StageID, TrainTime) ->
  GenWorkerEts = get(generic_worker_ets),
  case can_emit_parallel_event_now(GenWorkerEts, backward, BatchID, MicrobatchID, StageID) of
    ready ->
      case emit_parallel_event(WorkerName, backward, BatchID, MicrobatchID, StageID, TrainTime) of
        {ok, _EmittedBatchID} ->
          ok;
        Error ->
          Error
      end;
    wait_for_grant ->
      queue_parallel_backward_event(
        GenWorkerEts,
        WorkerName,
        BatchID,
        MicrobatchID,
        StageID,
        TrainTime
      ),
      case maybe_dispatch_pending_parallel_backward_events(GenWorkerEts) of
        ok ->
          ok;
        {abort, PendingDispatchReason} ->
          {error, {pending_backward_dispatch_failed, PendingDispatchReason}}
      end;
    {error, Reason} ->
      {error, Reason}
  end.

maybe_apply_tensor_parallel_collectives(
  _GenWorkerEts,
  Mode,
  _BatchID,
  _MicrobatchID,
  BatchTensor
) when Mode =/= tensor, Mode =/= pipeline_tensor ->
  {ok, BatchTensor};
maybe_apply_tensor_parallel_collectives(
  GenWorkerEts,
  _Mode,
  BatchID,
  MicrobatchID,
  BatchTensor
) ->
  TpPlan = ets:lookup_element(GenWorkerEts, worker_tp_plan, ?ETS_KEYVAL_VAL_IDX),
  TpGroupState = ets:lookup_element(GenWorkerEts, worker_tp_group_state, ?ETS_KEYVAL_VAL_IDX),
  TpWorldSize = normalize_tp_int(maps:get(tp_world_size, TpGroupState, 1), 1),
  case {TpPlan, TpWorldSize} of
    {[], _} ->
      {ok, BatchTensor};
    {_Plan, World} when World =< 1 ->
      {ok, BatchTensor};
    _ ->
      apply_tensor_parallel_tp_plan(GenWorkerEts, TpPlan, BatchID, MicrobatchID, BatchTensor, 0)
  end.

apply_tensor_parallel_tp_plan(
  _GenWorkerEts,
  [],
  _BatchID,
  _MicrobatchID,
  BatchTensor,
  _LayerIndex
) ->
  {ok, BatchTensor};
apply_tensor_parallel_tp_plan(
  GenWorkerEts,
  [PlanEntry | Rest],
  BatchID,
  MicrobatchID,
  BatchTensor,
  LayerIndex
) ->
  case execute_tp_collective_step(GenWorkerEts, PlanEntry, BatchID, MicrobatchID, LayerIndex, BatchTensor) of
    {ok, UpdatedTensor} ->
      apply_tensor_parallel_tp_plan(
        GenWorkerEts,
        Rest,
        BatchID,
        MicrobatchID,
        UpdatedTensor,
        LayerIndex + 1
      );
    {abort, _Reason} = Abort ->
      Abort
  end.

execute_tp_collective_step(
  GenWorkerEts,
  PlanEntry,
  BatchID,
  MicrobatchID,
  LayerIndex,
  {TensorBin, TensorType}
) ->
  TpGroupState = ets:lookup_element(GenWorkerEts, worker_tp_group_state, ?ETS_KEYVAL_VAL_IDX),
  WorkerName = ets:lookup_element(GenWorkerEts, worker_name, ?ETS_KEYVAL_VAL_IDX),
  LocalRank = normalize_tp_int(maps:get(tp_rank, TpGroupState, 0), 0),
  TpWorldSize = normalize_tp_int(maps:get(tp_world_size, TpGroupState, 1), 1),
  OrderedWorkers = maps:get(ordered_workers, TpGroupState, [WorkerName]),
  PeerWorkers = [PeerWorker || PeerWorker <- OrderedWorkers, PeerWorker =/= WorkerName],
  LayerName = normalize_tp_string(maps:get(layer, PlanEntry, "")),
  Mode = string:lowercase(normalize_tp_string(maps:get(mode, PlanEntry, "column"))),
  ShardAxis = normalize_tp_int(maps:get(shard_axis, PlanEntry, 0), 0),
  CollectiveToken = {tp_collective, BatchID, MicrobatchID, LayerIndex, LayerName, Mode},
  StartUs = erlang:monotonic_time(microsecond),
  Result =
    case Mode of
      "column" ->
        execute_tp_column_collective(
          GenWorkerEts,
          WorkerName,
          PeerWorkers,
          CollectiveToken,
          LocalRank,
          TpWorldSize,
          ShardAxis,
          {TensorBin, TensorType}
        );
      "row" ->
        execute_tp_row_collective(
          GenWorkerEts,
          WorkerName,
          PeerWorkers,
          CollectiveToken,
          LocalRank,
          TpWorldSize,
          {TensorBin, TensorType}
        );
      Unsupported ->
        {abort, {unsupported_tp_mode, Unsupported, LayerName}}
    end,
  EndUs = erlang:monotonic_time(microsecond),
  record_tp_collective_stats(EndUs - StartUs),
  Result.

execute_tp_column_collective(
  GenWorkerEts,
  WorkerName,
  PeerWorkers,
  CollectiveToken,
  LocalRank,
  TpWorldSize,
  ShardAxis,
  {TensorBin, TensorType}
) ->
  try nif_call(nerltensor_split_nif, [TensorBin, TensorType, TpWorldSize, ShardAxis]) of
    Shards when is_list(Shards) ->
      case safe_list_nth(LocalRank + 1, Shards) of
        undefined ->
          {abort, {invalid_tp_local_rank, LocalRank, TpWorldSize}};
        LocalShard ->
          CollectivePayload = {tp_collective_payload, CollectiveToken, LocalRank, LocalShard},
          broadcast_tp_collective_payload(GenWorkerEts, WorkerName, PeerWorkers, CollectivePayload),
          case collect_tp_peer_payloads(GenWorkerEts, CollectiveToken, PeerWorkers) of
            {error, Reason} ->
              {abort, {tp_collective_timeout, Reason}};
            {ok, PeerPayloadMap} ->
              RankToShardWithPeers = maps:fold(
                fun(_PeerWorker, {PeerRank, PeerShard}, AccMap) ->
                  maps:put(PeerRank, PeerShard, AccMap)
                end,
                #{LocalRank => LocalShard},
                PeerPayloadMap
              ),
              RequiredRanks = lists:seq(0, TpWorldSize - 1),
              case all_tp_ranks_present(RequiredRanks, RankToShardWithPeers) of
                false ->
                  {abort, {tp_collective_missing_rank, RequiredRanks, maps:keys(RankToShardWithPeers)}};
                true ->
                  OrderedShards = [maps:get(Rank, RankToShardWithPeers) || Rank <- RequiredRanks],
                  try nif_call(nerltensor_concat_nif, [OrderedShards, TensorType, ShardAxis]) of
                    {ConcatTensor, ConcatType} ->
                      {ok, {ConcatTensor, ConcatType}};
                    _ ->
                      {abort, tp_collective_concat_failed}
                  catch
                    ErrConcat:ReasonConcat ->
                      {abort, {tp_collective_concat_exception, ErrConcat, ReasonConcat}}
                  end
              end
          end
      end;
    _ ->
      {abort, tp_collective_split_failed}
  catch
    ErrSplit:ReasonSplit ->
      {abort, {tp_collective_split_exception, ErrSplit, ReasonSplit}}
  end.

execute_tp_row_collective(
  GenWorkerEts,
  WorkerName,
  PeerWorkers,
  CollectiveToken,
  LocalRank,
  TpWorldSize,
  {TensorBin, TensorType}
) ->
  CollectivePayload = {tp_collective_payload, CollectiveToken, LocalRank, {TensorBin, TensorType}},
  broadcast_tp_collective_payload(GenWorkerEts, WorkerName, PeerWorkers, CollectivePayload),
  case collect_tp_peer_payloads(GenWorkerEts, CollectiveToken, PeerWorkers) of
    {error, Reason} ->
      {abort, {tp_collective_timeout, Reason}};
    {ok, PeerPayloadMap} ->
      TensorList =
        [{TensorBin, TensorType}] ++
        [PeerTensor || {_PeerWorker, {_PeerRank, PeerTensor}} <- maps:to_list(PeerPayloadMap)],
      try nif_call(nerltensor_reduce_sum_list_nif, [TensorList, TensorType]) of
        {ReducedTensor, ReducedType} ->
          Scale = 1.0 / erlang:max(1, TpWorldSize),
          try nif_call(nerltensor_scalar_multiplication_nif, [ReducedTensor, ReducedType, Scale]) of
            {ScaledTensor, ScaledType} ->
              {ok, {ScaledTensor, ScaledType}};
            _ ->
              {abort, tp_collective_row_scale_failed}
          catch
            ErrScale:ReasonScale ->
              {abort, {tp_collective_row_scale_exception, ErrScale, ReasonScale}}
          end;
        _ ->
          {abort, tp_collective_row_reduce_failed}
      catch
        ErrReduce:ReasonReduce ->
          {abort, {tp_collective_row_reduce_exception, ErrReduce, ReasonReduce}}
      end
  end.

broadcast_tp_collective_payload(_GenWorkerEts, _WorkerName, [], _Payload) ->
  ok;
broadcast_tp_collective_payload(GenWorkerEts, WorkerName, [PeerWorker | Rest], Payload) ->
  W2WPid = ets:lookup_element(GenWorkerEts, w2wcom_pid, ?ETS_KEYVAL_VAL_IDX),
  send_message(W2WPid, WorkerName, PeerWorker, Payload),
  broadcast_tp_collective_payload(GenWorkerEts, WorkerName, Rest, Payload).

collect_tp_peer_payloads(GenWorkerEts, CollectiveToken, PeerWorkers) ->
  TimeoutMs = get_parallel_execution_int(
                ets:lookup_element(GenWorkerEts, parallel_execution, ?ETS_KEYVAL_VAL_IDX),
                [<<"collectiveTimeoutMs">>, collectiveTimeoutMs],
                5000
              ),
  ExpectedWorkers = lists:usort(PeerWorkers),
  InitialBuffer = ets:lookup_element(GenWorkerEts, tp_collective_inbox_buffer, ?ETS_KEYVAL_VAL_IDX),
  collect_tp_peer_payloads_loop(
    GenWorkerEts,
    CollectiveToken,
    ExpectedWorkers,
    #{},
    InitialBuffer,
    erlang:monotonic_time(millisecond),
    TimeoutMs
  ).

collect_tp_peer_payloads_loop(
  GenWorkerEts,
  CollectiveToken,
  ExpectedWorkers,
  CollectedMap,
  InboxBuffer,
  StartMs,
  TimeoutMs
) ->
  {CollectedAfterBuffer, RemainingBuffer} =
    consume_tp_collective_buffer(CollectiveToken, ExpectedWorkers, CollectedMap, InboxBuffer, []),
  case tp_collective_collection_complete(ExpectedWorkers, CollectedAfterBuffer) of
    true ->
      ets:update_element(GenWorkerEts, tp_collective_inbox_buffer, {?ETS_KEYVAL_VAL_IDX, RemainingBuffer}),
      {ok, CollectedAfterBuffer};
    false ->
      NowMs = erlang:monotonic_time(millisecond),
      case (NowMs - StartMs) >= TimeoutMs of
        true ->
          ets:update_element(GenWorkerEts, tp_collective_inbox_buffer, {?ETS_KEYVAL_VAL_IDX, RemainingBuffer}),
          MissingWorkers = [Worker || Worker <- ExpectedWorkers, not maps:is_key(Worker, CollectedAfterBuffer)],
          WorkerName = ets:lookup_element(GenWorkerEts, worker_name, ?ETS_KEYVAL_VAL_IDX),
          ?LOG_ERROR(
            "Worker ~p TP collective timeout after ~pms token=~p missing_workers=~p",
            [WorkerName, TimeoutMs, CollectiveToken, MissingWorkers]
          ),
          {error, {collective_timeout, CollectiveToken, MissingWorkers}};
        false ->
          receive after 10 -> ok end,
          FreshMessages = fetch_worker_inbox_messages(GenWorkerEts),
          collect_tp_peer_payloads_loop(
            GenWorkerEts,
            CollectiveToken,
            ExpectedWorkers,
            CollectedAfterBuffer,
            RemainingBuffer ++ FreshMessages,
            StartMs,
            TimeoutMs
          )
      end
  end.

consume_tp_collective_buffer(
  _CollectiveToken,
  _ExpectedWorkers,
  CollectedMap,
  [],
  RemainingAcc
) ->
  {CollectedMap, lists:reverse(RemainingAcc)};
consume_tp_collective_buffer(
  CollectiveToken,
  ExpectedWorkers,
  CollectedMap,
  [{FromWorker, Data} = Message | Rest],
  RemainingAcc
) ->
  case parse_tp_collective_payload(CollectiveToken, FromWorker, Data) of
    skip ->
      consume_tp_collective_buffer(
        CollectiveToken,
        ExpectedWorkers,
        CollectedMap,
        Rest,
        [Message | RemainingAcc]
      );
    {collect, Rank, TensorPayload} ->
      case lists:member(FromWorker, ExpectedWorkers) andalso not maps:is_key(FromWorker, CollectedMap) of
        true ->
          consume_tp_collective_buffer(
            CollectiveToken,
            ExpectedWorkers,
            maps:put(FromWorker, {Rank, TensorPayload}, CollectedMap),
            Rest,
            RemainingAcc
          );
        false ->
          consume_tp_collective_buffer(
            CollectiveToken,
            ExpectedWorkers,
            CollectedMap,
            Rest,
            [Message | RemainingAcc]
          )
      end
  end;
consume_tp_collective_buffer(
  CollectiveToken,
  ExpectedWorkers,
  CollectedMap,
  [UnknownMessage | Rest],
  RemainingAcc
) ->
  consume_tp_collective_buffer(
    CollectiveToken,
    ExpectedWorkers,
    CollectedMap,
    Rest,
    [UnknownMessage | RemainingAcc]
  ).

parse_tp_collective_payload(
  CollectiveToken,
  _FromWorker,
  {tp_collective_payload, PayloadToken, Rank, TensorPayload}
) ->
  case PayloadToken =:= CollectiveToken of
    true -> {collect, Rank, TensorPayload};
    false -> skip
  end;
parse_tp_collective_payload(_CollectiveToken, _FromWorker, _Data) ->
  skip.

tp_collective_collection_complete(ExpectedWorkers, CollectedMap) ->
  lists:all(fun(WorkerName) -> maps:is_key(WorkerName, CollectedMap) end, ExpectedWorkers).

fetch_worker_inbox_messages(GenWorkerEts) ->
  W2WPid = ets:lookup_element(GenWorkerEts, w2wcom_pid, ?ETS_KEYVAL_VAL_IDX),
  case gen_server:call(W2WPid, {get_inbox_queue}) of
    {ok, Queue} ->
      queue:to_list(Queue);
    _ ->
      []
  end.

all_tp_ranks_present([], _RankToPayloadMap) ->
  true;
all_tp_ranks_present([Rank | Rest], RankToPayloadMap) ->
  maps:is_key(Rank, RankToPayloadMap) andalso all_tp_ranks_present(Rest, RankToPayloadMap).

safe_list_nth(Index, List) when is_list(List), Index >= 1 ->
  case length(List) >= Index of
    true -> lists:nth(Index, List);
    false -> undefined
  end;
safe_list_nth(_Index, _List) ->
  undefined.

record_tp_collective_stats(DurationUs) ->
  WorkerStatsEts = get(worker_stats_ets),
  stats:increment_by_value(WorkerStatsEts, tp_collective_count, 1),
  stats:increment_by_value(WorkerStatsEts, tp_collective_latency_us, erlang:max(0, round(DurationUs))).

maybe_parallel_train_microbatch_path(GenWorkerEts, ModelId, SourceName, BatchID, {NerlTensorOfSamples, NerlTensorType}) ->
  ParallelExecution = ets:lookup_element(GenWorkerEts, parallel_execution, ?ETS_KEYVAL_VAL_IDX),
  NumMicrobatches = get_parallel_execution_int(ParallelExecution, [<<"numMicroBatches">>, numMicroBatches], 1),
  MicroBatchSize = get_parallel_execution_int(ParallelExecution, [<<"microBatchSize">>, microBatchSize], 0),
  ModeValue = maps:get(<<"mode">>, ParallelExecution, maps:get(mode, ParallelExecution, <<"legacy">>)),
  ModeLower = string:lowercase(normalize_parallel_mode_value(ModeValue)),
  NifModule = get_backend_module(),
  SupportsMicrobatch = (NifModule =:= nerlTorchNIF) andalso erlang:function_exported(NifModule, call_to_train_microbatch, 5),
  HasSuperAuthority = ets:lookup_element(GenWorkerEts, parallel_super_authority, ?ETS_KEYVAL_VAL_IDX),
  case {ModeLower, SupportsMicrobatch, NumMicrobatches > 0, HasSuperAuthority} of
    {"pipeline_tensor", true, true, true} ->
      prepare_and_dispatch_parallel_microbatches(
        GenWorkerEts,
        ModelId,
        SourceName,
        BatchID,
        {NerlTensorOfSamples, NerlTensorType},
        NumMicrobatches,
        MicroBatchSize,
        pipeline_tensor
      );
    {"tensor", true, true, true} ->
      prepare_and_dispatch_parallel_microbatches(
        GenWorkerEts,
        ModelId,
        SourceName,
        BatchID,
        {NerlTensorOfSamples, NerlTensorType},
        NumMicrobatches,
        MicroBatchSize,
        tensor
      );
    {_AnyNonLegacyMode, _Supports, _NumValid, false} ->
      {abort, super_node_authority_required};
    {_AnyMode, false, _NumValid, _AnyAuthority} ->
      {abort, torch_microbatch_api_missing};
    {_AnyMode, _Supports, false, _AnyAuthority} ->
      {abort, invalid_num_microbatches};
    {_, true, true, true} ->
      {abort, unsupported_parallel_mode};
    _ ->
      {abort, unsupported_parallel_mode}
  end.

prepare_and_dispatch_parallel_microbatches(
  GenWorkerEts,
  ModelId,
  SourceName,
  BatchID,
  BatchTensor,
  NumMicrobatches,
  MicroBatchSize,
  ParallelMode
) ->
  case split_batch_into_microbatches(BatchTensor, NumMicrobatches, MicroBatchSize) of
    [] ->
      {abort, microbatch_split_failed};
    MicrobatchList ->
      WorkerName = ets:lookup_element(GenWorkerEts, worker_name, ?ETS_KEYVAL_VAL_IDX),
      StageID = get_worker_pipeline_stage(GenWorkerEts),
      case dispatch_parallel_microbatch_list_loop(
        GenWorkerEts,
        WorkerName,
        StageID,
        ModelId,
        SourceName,
        BatchID,
        MicrobatchList,
        0,
        ParallelMode
      ) of
        {ok, NumDispatched, RemainingQueue} ->
          {ok, ParallelMode, NumDispatched, RemainingQueue};
        {abort, _Reason} = Abort ->
          Abort
      end
  end.

dispatch_parallel_microbatch_list_loop(
  _GenWorkerEts,
  _WorkerName,
  _StageID,
  _ModelId,
  _SourceName,
  _BatchID,
  [],
  NumDispatched,
  _ParallelMode
) ->
  {ok, NumDispatched, []};
dispatch_parallel_microbatch_list_loop(
  GenWorkerEts,
  WorkerName,
  StageID,
  ModelId,
  SourceName,
  BatchID,
  [{MicrobatchID, {MicrobatchTensor, MicrobatchType}} | Rest],
  NumDispatched,
  ParallelMode
) ->
  case maybe_emit_parallel_forward_event(ParallelMode, WorkerName, BatchID, MicrobatchID, StageID) of
    {error, no_scheduler_grant} ->
      {ok, NumDispatched, [{MicrobatchID, {MicrobatchTensor, MicrobatchType}} | Rest]};
    {error, EmitReason} ->
      {abort, {forward_event_rejected, EmitReason}};
    ok ->
      case maybe_apply_tensor_parallel_collectives(
             GenWorkerEts,
             ParallelMode,
             BatchID,
             MicrobatchID,
             {MicrobatchTensor, MicrobatchType}
           ) of
        {abort, TpAbortReason} ->
          {abort, {tensor_collective_failed, TpAbortReason}};
        {ok, {PreparedTensor, PreparedType}} ->
          nif_call(call_to_train_microbatch, [ModelId, {PreparedTensor, PreparedType}, BatchID, SourceName, MicrobatchID]),
          dispatch_parallel_microbatch_list_loop(
            GenWorkerEts,
            WorkerName,
            StageID,
            ModelId,
            SourceName,
            BatchID,
            Rest,
            NumDispatched + 1,
            ParallelMode
          )
      end
  end.

%% Predict microbatch path for tensor/pipeline_tensor modes
maybe_parallel_predict_microbatch_path(GenWorkerEts, ModelId, SourceName, BatchID, {PredictBatchTensor, PredictBatchType}) ->
  ParallelExecution = ets:lookup_element(GenWorkerEts, parallel_execution, ?ETS_KEYVAL_VAL_IDX),
  NumMicrobatches = get_parallel_execution_int(ParallelExecution, [<<"numMicroBatches">>, numMicroBatches], 1),
  MicroBatchSize = get_parallel_execution_int(ParallelExecution, [<<"microBatchSize">>, microBatchSize], 0),
  ModeValue = maps:get(<<"mode">>, ParallelExecution, maps:get(mode, ParallelExecution, <<"legacy">>)),
  ModeLower = string:lowercase(normalize_parallel_mode_value(ModeValue)),
  NifModule = get_backend_module(),
  SupportsTorch = (NifModule =:= nerlTorchNIF),
  HasSuperAuthority = ets:lookup_element(GenWorkerEts, parallel_super_authority, ?ETS_KEYVAL_VAL_IDX),
  case {ModeLower, SupportsTorch, NumMicrobatches > 0, HasSuperAuthority} of
    {"pipeline_tensor", true, true, true} ->
      prepare_and_dispatch_parallel_predict_microbatches(
        GenWorkerEts, ModelId, SourceName, BatchID,
        {PredictBatchTensor, PredictBatchType},
        NumMicrobatches, MicroBatchSize, pipeline_tensor
      );
    {"tensor", true, true, true} ->
      prepare_and_dispatch_parallel_predict_microbatches(
        GenWorkerEts, ModelId, SourceName, BatchID,
        {PredictBatchTensor, PredictBatchType},
        NumMicrobatches, MicroBatchSize, tensor
      );
    {_AnyNonLegacyMode, _Supports, _NumValid, false} ->
      {abort, super_node_authority_required};
    {_AnyMode, false, _NumValid, _AnyAuthority} ->
      {abort, torch_backend_required};
    _ ->
      {abort, unsupported_parallel_mode}
  end.

prepare_and_dispatch_parallel_predict_microbatches(
  GenWorkerEts, ModelId, SourceName, BatchID,
  BatchTensor, NumMicrobatches, MicroBatchSize, ParallelMode
) ->
  case split_batch_into_microbatches(BatchTensor, NumMicrobatches, MicroBatchSize) of
    [] ->
      {abort, microbatch_split_failed};
    MicrobatchList ->
      WorkerName = ets:lookup_element(GenWorkerEts, worker_name, ?ETS_KEYVAL_VAL_IDX),
      StageID = get_worker_pipeline_stage(GenWorkerEts),
      case dispatch_parallel_predict_microbatch_list_loop(
        GenWorkerEts, WorkerName, StageID, ModelId,
        SourceName, BatchID, MicrobatchList, 0, ParallelMode
      ) of
        {ok, NumDispatched, RemainingQueue} ->
          {ok, ParallelMode, NumDispatched, RemainingQueue};
        {abort, _Reason} = Abort ->
          Abort
      end
  end.

dispatch_parallel_predict_microbatch_list_loop(
  _GenWorkerEts, _WorkerName, _StageID, _ModelId,
  _SourceName, _BatchID, [], NumDispatched, _ParallelMode
) ->
  {ok, NumDispatched, []};
dispatch_parallel_predict_microbatch_list_loop(
  GenWorkerEts, WorkerName, StageID, ModelId,
  SourceName, BatchID,
  [{MicrobatchID, {MicrobatchTensor, MicrobatchType}} | Rest],
  NumDispatched, ParallelMode
) ->
  case maybe_emit_parallel_forward_event(ParallelMode, WorkerName, BatchID, MicrobatchID, StageID) of
    {error, no_scheduler_grant} ->
      {ok, NumDispatched, [{MicrobatchID, {MicrobatchTensor, MicrobatchType}} | Rest]};
    {error, EmitReason} ->
      {abort, {forward_event_rejected, EmitReason}};
    ok ->
      case maybe_apply_tensor_parallel_collectives(
             GenWorkerEts, ParallelMode, BatchID, MicrobatchID,
             {MicrobatchTensor, MicrobatchType}
           ) of
        {abort, TpAbortReason} ->
          {abort, {tensor_collective_failed, TpAbortReason}};
        {ok, {PreparedTensor, PreparedType}} ->
          nif_call(call_to_predict, [ModelId, {PreparedTensor, PreparedType}, BatchID, SourceName]),
          dispatch_parallel_predict_microbatch_list_loop(
            GenWorkerEts, WorkerName, StageID, ModelId,
            SourceName, BatchID, Rest, NumDispatched + 1, ParallelMode
          )
      end
  end.

dispatch_queued_parallel_microbatches(GenWorkerEts) ->
  ActiveBatchCtx = ets:lookup_element(GenWorkerEts, parallel_active_batch_ctx, ?ETS_KEYVAL_VAL_IDX),
  case ActiveBatchCtx of
    undefined ->
      ok;
    Ctx when is_map(Ctx) ->
      case ets:lookup_element(GenWorkerEts, parallel_microbatch_queue, ?ETS_KEYVAL_VAL_IDX) of
        [] ->
          ok;
        Queue ->
          WorkerName = ets:lookup_element(GenWorkerEts, worker_name, ?ETS_KEYVAL_VAL_IDX),
          StageID = get_worker_pipeline_stage(GenWorkerEts),
          ModelId = maps:get(model_id, Ctx, ets:lookup_element(GenWorkerEts, model_id, ?ETS_KEYVAL_VAL_IDX)),
          SourceName = maps:get(source_name, Ctx, undefined),
          BatchID = maps:get(batch_id, Ctx, undefined),
          ParallelMode = maps:get(mode, Ctx, legacy),
          case {SourceName, BatchID} of
            {undefined, _} ->
              {abort, missing_parallel_active_source_name};
            {_, undefined} ->
              {abort, missing_parallel_active_batch_id};
            _ ->
              DispatchFun = case ParallelMode of
                tensor_predict ->
                  fun(GWE, WN, SID, MID, SN, BID, Q, ND, PM) ->
                    dispatch_parallel_predict_microbatch_list_loop(GWE, WN, SID, MID, SN, BID, Q, ND, PM)
                  end;
                _ ->
                  fun(GWE, WN, SID, MID, SN, BID, Q, ND, PM) ->
                    dispatch_parallel_microbatch_list_loop(GWE, WN, SID, MID, SN, BID, Q, ND, PM)
                  end
              end,
              %% For tensor_predict, use the actual TP parallel mode for grant matching
              DispatchMode = case ParallelMode of
                tensor_predict -> pipeline_tensor;
                _ -> ParallelMode
              end,
              case DispatchFun(
                     GenWorkerEts,
                     WorkerName,
                     StageID,
                     ModelId,
                     SourceName,
                     BatchID,
                     Queue,
                     0,
                     DispatchMode
                   ) of
                {ok, _NumDispatched, RemainingQueue} ->
                  ets:update_element(GenWorkerEts, parallel_microbatch_queue, {?ETS_KEYVAL_VAL_IDX, RemainingQueue}),
                  ok;
                {abort, _Reason} = Abort ->
                  Abort
              end
          end
      end;
    _ ->
      ok
  end.

prepare_pipeline_stage0_microbatches(
  GenWorkerEts,
  SourceName,
  BatchID,
  BatchTensor,
  PipelineMode
) ->
  case pipeline_torch_backend_supported(GenWorkerEts) of
    false ->
      {abort, pipeline_torch_backend_required};
    true ->
  ParallelExecution = ets:lookup_element(GenWorkerEts, parallel_execution, ?ETS_KEYVAL_VAL_IDX),
  NumMicrobatches = get_parallel_execution_int(ParallelExecution, [<<"numMicroBatches">>, numMicroBatches], 1),
  MicroBatchSize = get_parallel_execution_int(ParallelExecution, [<<"microBatchSize">>, microBatchSize], 0),
  case split_batch_into_microbatches(BatchTensor, NumMicrobatches, MicroBatchSize) of
    [] ->
      {abort, microbatch_split_failed};
    MicrobatchList ->
      ModelId = ets:lookup_element(GenWorkerEts, model_id, ?ETS_KEYVAL_VAL_IDX),
      WorkerName = ets:lookup_element(GenWorkerEts, worker_name, ?ETS_KEYVAL_VAL_IDX),
      StageID = get_worker_pipeline_stage(GenWorkerEts),
      TotalMicrobatches = length(MicrobatchList),
      dispatch_pipeline_stage0_forward_microbatch_loop(
        GenWorkerEts,
        ModelId,
        WorkerName,
        SourceName,
        BatchID,
        PipelineMode,
        StageID,
        TotalMicrobatches,
        MicrobatchList,
        0
      )
  end
  end.

dispatch_pipeline_stage0_forward_microbatch_loop(
  _GenWorkerEts,
  _ModelId,
  _WorkerName,
  _SourceName,
  _BatchID,
  _PipelineMode,
  _StageID,
  _TotalMicrobatches,
  [],
  NumDispatched
) ->
  {ok, NumDispatched, [], NumDispatched};
dispatch_pipeline_stage0_forward_microbatch_loop(
  GenWorkerEts,
  ModelId,
  WorkerName,
  SourceName,
  BatchID,
  PipelineMode,
  StageID,
  TotalMicrobatches,
  [{MicrobatchID, {MicrobatchTensor, MicrobatchType}} | Rest],
  NumDispatched
) ->
  GrantResult = maybe_emit_parallel_forward_event_with_batch(
                  PipelineMode,
                  WorkerName,
                  BatchID,
                  MicrobatchID,
                  StageID
                ),
  case GrantResult of
    {error, no_scheduler_grant} ->
      {ok, NumDispatched, [{MicrobatchID, {MicrobatchTensor, MicrobatchType}} | Rest], TotalMicrobatches};
    {error, EmitReason} ->
      {abort, {pipeline_stage0_forward_grant_rejected, EmitReason}};
    {ok, RuntimeBatchID} ->
      bind_stage0_runtime_batch_id(GenWorkerEts, RuntimeBatchID),
      case maybe_apply_tensor_parallel_collectives(
             GenWorkerEts,
             PipelineMode,
             RuntimeBatchID,
             MicrobatchID,
             {MicrobatchTensor, MicrobatchType}
           ) of
        {abort, TpAbortReason} ->
          {abort, {tensor_collective_failed, TpAbortReason}};
        {ok, {PreparedTensor, PreparedType}} ->
          Continuation = fun(NifResult) ->
            case NifResult of
              {ok, pipeline_stage0_forward, ActivationTensor, ActivationType, LabelsTensor, LabelsType, StageTime} ->
                case resolve_pipeline_adjacent_worker(GenWorkerEts, next) of
                  {ok, NextWorker} ->
                    FwdPayload = {
                      pipeline_forward_payload,
                      RuntimeBatchID,
                      SourceName,
                      TotalMicrobatches,
                      MicrobatchID,
                      {ActivationTensor, ActivationType},
                      {LabelsTensor, LabelsType}
                    },
                    route_pipeline_payload_to_worker(GenWorkerEts, WorkerName, NextWorker, FwdPayload),
                    ets:update_element(
                      GenWorkerEts,
                      parallel_time_acc,
                      {
                        ?ETS_KEYVAL_VAL_IDX,
                        ets:lookup_element(GenWorkerEts, parallel_time_acc, ?ETS_KEYVAL_VAL_IDX) + StageTime
                      }
                    ),
                    stats:increment_by_value(get(worker_stats_ets), acc_time_training, trunc(StageTime)),
                    processed;
                  {error, AdjacentReason} ->
                    {abort, {pipeline_next_stage_resolution_failed, AdjacentReason}}
                end;
              {nerlnif, error, Reason} ->
                ?LOG_ERROR(
                  "Worker ~p stage0 pipeline NIF failed source_batch=~p runtime_batch=~p microbatch=~p stage=~p reason=~p",
                  [WorkerName, BatchID, RuntimeBatchID, MicrobatchID, StageID, Reason]
                ),
                {abort, {pipeline_stage0_forward_nif_error, Reason}};
              Unexpected ->
                {abort, {pipeline_stage0_forward_unexpected, Unexpected}}
            end
          end,
          async_pipeline_nif(
            GenWorkerEts,
            call_to_pipeline_stage0_forward,
            [ModelId, {PreparedTensor, PreparedType}, RuntimeBatchID, MicrobatchID],
            Continuation
          ),
          {ok, NumDispatched + 1, Rest, TotalMicrobatches}
      end
  end.

prepare_pipeline_predict_stage0_microbatches(
  GenWorkerEts,
  SourceName,
  BatchID,
  BatchTensor,
  PipelineMode
) ->
  case pipeline_torch_backend_supported(GenWorkerEts) of
    false ->
      {abort, pipeline_torch_backend_required};
    true ->
  ParallelExecution = ets:lookup_element(GenWorkerEts, parallel_execution, ?ETS_KEYVAL_VAL_IDX),
  NumMicrobatches = get_parallel_execution_int(ParallelExecution, [<<"numMicroBatches">>, numMicroBatches], 1),
  MicroBatchSize = get_parallel_execution_int(ParallelExecution, [<<"microBatchSize">>, microBatchSize], 0),
  case split_batch_into_microbatches(BatchTensor, NumMicrobatches, MicroBatchSize) of
    [] ->
      {abort, microbatch_split_failed};
    MicrobatchList ->
      ModelId = ets:lookup_element(GenWorkerEts, model_id, ?ETS_KEYVAL_VAL_IDX),
      WorkerName = ets:lookup_element(GenWorkerEts, worker_name, ?ETS_KEYVAL_VAL_IDX),
      StageID = get_worker_pipeline_stage(GenWorkerEts),
      TotalMicrobatches = length(MicrobatchList),
      dispatch_pipeline_stage0_predict_microbatch_loop(
        GenWorkerEts,
        ModelId,
        WorkerName,
        SourceName,
        BatchID,
        PipelineMode,
        StageID,
        TotalMicrobatches,
        MicrobatchList,
        0
      )
  end
  end.

dispatch_pipeline_stage0_predict_microbatch_loop(
  _GenWorkerEts,
  _ModelId,
  _WorkerName,
  _SourceName,
  _BatchID,
  _PipelineMode,
  _StageID,
  _TotalMicrobatches,
  [],
  NumDispatched
) ->
  {ok, NumDispatched, [], NumDispatched};
dispatch_pipeline_stage0_predict_microbatch_loop(
  GenWorkerEts,
  ModelId,
  WorkerName,
  SourceName,
  BatchID,
  PipelineMode,
  StageID,
  TotalMicrobatches,
  [{MicrobatchID, {MicrobatchTensor, MicrobatchType}} | Rest],
  NumDispatched
) ->
  case maybe_emit_parallel_forward_event_with_batch(
         pipeline_predict,
         WorkerName,
         BatchID,
         MicrobatchID,
         StageID
       ) of
    {error, no_scheduler_grant} ->
      {ok, NumDispatched, [{MicrobatchID, {MicrobatchTensor, MicrobatchType}} | Rest], TotalMicrobatches};
    {error, EmitReason} ->
      {abort, {pipeline_predict_stage0_grant_rejected, EmitReason}};
    {ok, RuntimeBatchID} ->
      bind_stage0_runtime_batch_id(GenWorkerEts, RuntimeBatchID),
      case maybe_apply_tensor_parallel_collectives(
             GenWorkerEts,
             PipelineMode,
             RuntimeBatchID,
             MicrobatchID,
             {MicrobatchTensor, MicrobatchType}
           ) of
        {abort, TpAbortReason} ->
          {abort, {tensor_collective_failed, TpAbortReason}};
        {ok, {PreparedTensor, PreparedType}} ->
          PredStage0Continuation = fun(NifResult) ->
            case NifResult of
              {ok, pipeline_predict_stage0, ActivationTensor, ActivationType, StageTime} ->
                case resolve_pipeline_adjacent_worker(GenWorkerEts, next) of
                  {ok, NextWorker} ->
                    PredPayload = {
                      pipeline_predict_payload,
                      RuntimeBatchID,
                      SourceName,
                      TotalMicrobatches,
                      MicrobatchID,
                      {ActivationTensor, ActivationType}
                    },
                    route_pipeline_payload_to_worker(GenWorkerEts, WorkerName, NextWorker, PredPayload),
                    ets:update_element(
                      GenWorkerEts,
                      parallel_time_acc,
                      {
                        ?ETS_KEYVAL_VAL_IDX,
                        ets:lookup_element(GenWorkerEts, parallel_time_acc, ?ETS_KEYVAL_VAL_IDX) + StageTime
                      }
                    ),
                    stats:increment_by_value(get(worker_stats_ets), acc_time_prediction, trunc(StageTime)),
                    processed;
                  {error, AdjacentReason} ->
                    {abort, {pipeline_predict_next_stage_resolution_failed, AdjacentReason}}
                end;
              {nerlnif, error, Reason} ->
                {abort, {pipeline_predict_stage0_nif_error, Reason}};
              Unexpected ->
                {abort, {pipeline_predict_stage0_unexpected, Unexpected}}
            end
          end,
          async_pipeline_nif(
            GenWorkerEts,
            call_to_pipeline_predict_stage0_forward,
            [ModelId, {PreparedTensor, PreparedType}, RuntimeBatchID],
            PredStage0Continuation
          ),
          {ok, NumDispatched + 1, Rest, TotalMicrobatches}
      end
  end.

handle_parallel_pipeline_inbox(GenWorkerEts, ModelId, WorkerName, _FromWorker, Payload) ->
  case Payload of
    {pipeline_forward_payload, BatchID, SourceName, TotalMicrobatches, _MicrobatchID, _Activation, _Labels} ->
      ForwardBuffer = ets:lookup_element(GenWorkerEts, parallel_pipeline_forward_buffer, ?ETS_KEYVAL_VAL_IDX),
      ets:update_element(
        GenWorkerEts,
        parallel_pipeline_forward_buffer,
        {?ETS_KEYVAL_VAL_IDX, ForwardBuffer ++ [Payload]}
      ),
      case ensure_active_pipeline_training_ctx(
             GenWorkerEts,
             ModelId,
             SourceName,
             BatchID,
             TotalMicrobatches
           ) of
        ok ->
          dispatch_pipeline_buffers(GenWorkerEts, ModelId, WorkerName);
        {error, Reason} ->
          {abort, Reason}
      end;
    {pipeline_backward_payload, BatchID, SourceName, TotalMicrobatches, _MicrobatchID, _Grad} ->
      BackwardBuffer = ets:lookup_element(GenWorkerEts, parallel_pipeline_backward_buffer, ?ETS_KEYVAL_VAL_IDX),
      ets:update_element(
        GenWorkerEts,
        parallel_pipeline_backward_buffer,
        {?ETS_KEYVAL_VAL_IDX, BackwardBuffer ++ [Payload]}
      ),
      case ensure_active_pipeline_training_ctx(
             GenWorkerEts,
             ModelId,
             SourceName,
             BatchID,
             TotalMicrobatches
           ) of
        ok ->
          dispatch_pipeline_buffers(GenWorkerEts, ModelId, WorkerName);
        {error, Reason} ->
          {abort, Reason}
      end;
    _ ->
      {abort, {unsupported_pipeline_train_payload, Payload}}
  end.

handle_parallel_pipeline_predict_inbox(GenWorkerEts, ModelId, WorkerName, _FromWorker, Payload) ->
  case Payload of
    {pipeline_predict_payload, BatchID, SourceName, TotalMicrobatches, _MicrobatchID, _Activation} ->
      PredictBuffer = ets:lookup_element(GenWorkerEts, parallel_pipeline_predict_buffer, ?ETS_KEYVAL_VAL_IDX),
      ets:update_element(
        GenWorkerEts,
        parallel_pipeline_predict_buffer,
        {?ETS_KEYVAL_VAL_IDX, PredictBuffer ++ [Payload]}
      ),
      ensure_active_pipeline_predict_ctx(
        GenWorkerEts,
        ModelId,
        SourceName,
        BatchID,
        TotalMicrobatches
      ),
      dispatch_pipeline_predict_buffers(GenWorkerEts, ModelId, WorkerName);
    _ ->
      {abort, {unsupported_pipeline_predict_payload, Payload}}
  end.

dispatch_pipeline_buffers(GenWorkerEts, ModelId, WorkerName) ->
  NifFlight = is_pipeline_nif_in_flight(GenWorkerEts),
  case NifFlight of
    true -> ok;
    false ->
      case dispatch_pipeline_stage0_queue(GenWorkerEts, ModelId, WorkerName) of
        {abort, _Reason} = Abort ->
          Abort;
        ok ->
          case is_pipeline_nif_in_flight(GenWorkerEts) of
            true -> ok;
            false ->
              case dispatch_pipeline_forward_buffer(GenWorkerEts, ModelId, WorkerName) of
                {abort, _Reason} = Abort ->
                  Abort;
                ok ->
                  case is_pipeline_nif_in_flight(GenWorkerEts) of
                    true -> ok;
                    false ->
                      case dispatch_pipeline_backward_buffer(GenWorkerEts, ModelId, WorkerName) of
                        {abort, _Reason} = Abort ->
                          Abort;
                        ok ->
                          case is_pipeline_nif_in_flight(GenWorkerEts) of
                            true -> ok;
                            false ->
                              maybe_finalize_pipeline_training_batch(GenWorkerEts, WorkerName)
                          end
                      end
                  end
              end
          end
      end
  end.

dispatch_pipeline_stage0_queue(GenWorkerEts, ModelId, WorkerName) ->
  StageID = get_worker_pipeline_stage(GenWorkerEts),
  case StageID of
    0 ->
      ActiveCtx = ets:lookup_element(GenWorkerEts, parallel_active_batch_ctx, ?ETS_KEYVAL_VAL_IDX),
      Queue = ets:lookup_element(GenWorkerEts, parallel_microbatch_queue, ?ETS_KEYVAL_VAL_IDX),
      case {ActiveCtx, Queue} of
        {Ctx, [_ | _]} when is_map(Ctx) ->
          Mode = maps:get(mode, Ctx, legacy),
          RuntimePipelineMode = normalize_parallel_mode_atom(
                                  ets:lookup_element(GenWorkerEts, parallel_mode, ?ETS_KEYVAL_VAL_IDX)
                                ),
          SourceName = maps:get(source_name, Ctx, undefined),
          BatchID = maps:get(batch_id, Ctx, undefined),
          TotalMicrobatches = maps:get(total_microbatches, Ctx, length(Queue)),
          Dispatched = maps:get(forward_dispatched, Ctx, 0),
          case Mode of
            pipeline ->
              case dispatch_pipeline_stage0_forward_microbatch_loop(
                     GenWorkerEts,
                     ModelId,
                     WorkerName,
                     SourceName,
                     BatchID,
                     RuntimePipelineMode,
                     StageID,
                     TotalMicrobatches,
                     Queue,
                     Dispatched
                   ) of
                {ok, NewDispatched, RemainingQueue, _Total} ->
                  ets:update_element(GenWorkerEts, parallel_microbatch_queue, {?ETS_KEYVAL_VAL_IDX, RemainingQueue}),
                  update_active_pipeline_batch_ctx(
                    GenWorkerEts,
                    fun(Ctx0) ->
                      update_stage0_forward_progress_ctx(Ctx0, NewDispatched, TotalMicrobatches)
                    end
                  ),
                  ok;
                {abort, _Reason} = Abort ->
                  Abort
              end;
            pipeline_predict ->
              case dispatch_pipeline_stage0_predict_microbatch_loop(
                     GenWorkerEts,
                     ModelId,
                     WorkerName,
                     SourceName,
                     BatchID,
                     RuntimePipelineMode,
                     StageID,
                     TotalMicrobatches,
                     Queue,
                     Dispatched
                   ) of
                {ok, NewDispatched, RemainingQueue, _Total} ->
                  ets:update_element(GenWorkerEts, parallel_microbatch_queue, {?ETS_KEYVAL_VAL_IDX, RemainingQueue}),
                  update_active_pipeline_batch_ctx(
                    GenWorkerEts,
                    fun(Ctx0) ->
                      update_stage0_forward_progress_ctx(Ctx0, NewDispatched, TotalMicrobatches)
                    end
                  ),
                  ok;
                {abort, _Reason} = Abort ->
                  Abort
              end;
            _ ->
              ok
          end;
        _ ->
          ok
      end;
    _ ->
      ok
  end.

dispatch_pipeline_forward_buffer(GenWorkerEts, ModelId, WorkerName) ->
  ForwardBuffer = ets:lookup_element(GenWorkerEts, parallel_pipeline_forward_buffer, ?ETS_KEYVAL_VAL_IDX),
  dispatch_pipeline_forward_buffer_loop(GenWorkerEts, ModelId, WorkerName, ForwardBuffer, []).

dispatch_pipeline_forward_buffer_loop(GenWorkerEts, _ModelId, _WorkerName, [], RemainingAcc) ->
  ets:update_element(
    GenWorkerEts,
    parallel_pipeline_forward_buffer,
    {?ETS_KEYVAL_VAL_IDX, lists:reverse(RemainingAcc)}
  ),
  ok;
dispatch_pipeline_forward_buffer_loop(
  GenWorkerEts,
  ModelId,
  WorkerName,
  [Payload | Rest],
  RemainingAcc
) ->
  case maybe_process_pipeline_forward_payload(GenWorkerEts, ModelId, WorkerName, Payload) of
    processed ->
      case is_pipeline_nif_in_flight(GenWorkerEts) of
        true ->
          ets:update_element(
            GenWorkerEts,
            parallel_pipeline_forward_buffer,
            {?ETS_KEYVAL_VAL_IDX, lists:reverse(RemainingAcc) ++ Rest}
          ),
          ok;
        false ->
          dispatch_pipeline_forward_buffer_loop(GenWorkerEts, ModelId, WorkerName, Rest, RemainingAcc)
      end;
    wait_for_batch ->
      ets:update_element(
        GenWorkerEts,
        parallel_pipeline_forward_buffer,
        {?ETS_KEYVAL_VAL_IDX, lists:reverse(RemainingAcc) ++ [Payload | Rest]}
      ),
      ok;
    wait_for_grant ->
      ets:update_element(
        GenWorkerEts,
        parallel_pipeline_forward_buffer,
        {?ETS_KEYVAL_VAL_IDX, lists:reverse(RemainingAcc) ++ [Payload | Rest]}
      ),
      ok;
    {skip, Reason} ->
      ets:update_element(
        GenWorkerEts,
        parallel_pipeline_forward_buffer,
        {?ETS_KEYVAL_VAL_IDX, lists:reverse([Payload | RemainingAcc]) ++ Rest}
      ),
      {abort, {pipeline_forward_payload_skipped, Reason}};
    {abort, _Reason} = Abort ->
      Abort
  end.

maybe_process_pipeline_forward_payload(
  GenWorkerEts,
  ModelId,
  WorkerName,
  {pipeline_forward_payload, BatchID, SourceName, TotalMicrobatches, MicrobatchID, Activation, Labels}
) ->
  case pipeline_torch_backend_supported(GenWorkerEts) of
    false ->
      {abort, pipeline_torch_backend_required};
    true ->
      case pipeline_payload_batch_status(GenWorkerEts, BatchID) of
        wait_for_batch ->
          wait_for_batch;
        ready ->
          StageID = get_worker_pipeline_stage(GenWorkerEts),
          PipelineMode = normalize_parallel_mode_atom(
                           ets:lookup_element(GenWorkerEts, parallel_mode, ?ETS_KEYVAL_VAL_IDX)
                         ),
          case maybe_emit_parallel_forward_event(PipelineMode, WorkerName, BatchID, MicrobatchID, StageID) of
            {error, no_scheduler_grant} ->
              wait_for_grant;
            {error, EmitReason} ->
              {abort, {pipeline_forward_event_rejected, EmitReason}};
            ok ->
              case maybe_apply_tensor_parallel_collectives(
                     GenWorkerEts,
                     PipelineMode,
                     BatchID,
                     MicrobatchID,
                     Activation
                   ) of
                {abort, TpAbortReason} ->
                  {abort, {tensor_collective_failed, TpAbortReason}};
                {ok, PreparedActivation} ->
                  case is_last_pipeline_stage(GenWorkerEts) of
                    true ->
                      LastContinuation = fun(NifResult) ->
                        case NifResult of
                          {ok, pipeline_stage_last, LossTensor, LossType, GradTensor, GradType, StageTime} ->
                            UpdatedLossAcc = accumulate_parallel_loss(
                                               ets:lookup_element(GenWorkerEts, parallel_loss_acc, ?ETS_KEYVAL_VAL_IDX),
                                               {LossTensor, LossType}
                                             ),
                            ets:update_element(GenWorkerEts, parallel_loss_acc, {?ETS_KEYVAL_VAL_IDX, UpdatedLossAcc}),
                            ets:update_element(
                              GenWorkerEts,
                              parallel_time_acc,
                              {
                                ?ETS_KEYVAL_VAL_IDX,
                                ets:lookup_element(GenWorkerEts, parallel_time_acc, ?ETS_KEYVAL_VAL_IDX) + StageTime
                              }
                            ),
                            stats:increment_by_value(get(worker_stats_ets), acc_time_training, trunc(StageTime)),
                            update_active_pipeline_batch_ctx(
                              GenWorkerEts,
                              fun(Ctx) ->
                                Ctx#{
                                  forward_completed => maps:get(forward_completed, Ctx, 0) + 1,
                                  backward_completed => maps:get(backward_completed, Ctx, 0) + 1,
                                  total_microbatches => TotalMicrobatches
                                }
                              end
                            ),
                            case maybe_emit_parallel_backward_event(
                                   PipelineMode,
                                   WorkerName,
                                   BatchID,
                                   MicrobatchID,
                                   StageID,
                                   StageTime
                                 ) of
                              {error, LastEmitReason} ->
                                {abort, {pipeline_last_stage_backward_event_rejected, LastEmitReason}};
                              ok ->
                                case resolve_pipeline_adjacent_worker(GenWorkerEts, prev) of
                                  {ok, PrevWorker} ->
                                    BackwardPayload = {
                                      pipeline_backward_payload,
                                      BatchID,
                                      SourceName,
                                      TotalMicrobatches,
                                      MicrobatchID,
                                      {GradTensor, GradType}
                                    },
                                    route_pipeline_payload_to_worker(GenWorkerEts, WorkerName, PrevWorker, BackwardPayload),
                                    processed;
                                  {error, no_adjacent_stage} ->
                                    processed;
                                  {error, AdjacentReason} ->
                                    {abort, {pipeline_prev_stage_resolution_failed, AdjacentReason}}
                                end
                            end;
                          {nerlnif, error, Reason} ->
                            {abort, {pipeline_stage_last_nif_error, Reason}};
                          Unexpected ->
                            {abort, {pipeline_stage_last_unexpected, Unexpected}}
                        end
                      end,
                      async_pipeline_nif(
                        GenWorkerEts,
                        call_to_pipeline_stage_last_forward_backward,
                        [ModelId, PreparedActivation, Labels, BatchID, SourceName, MicrobatchID],
                        LastContinuation
                      );
                    false ->
                      FwdContinuation = fun(NifResult) ->
                        case NifResult of
                          {ok, pipeline_stage_forward, StageTensor, StageType, ForwardLabelsTensor, ForwardLabelsType, StageTime} ->
                            update_active_pipeline_batch_ctx(
                              GenWorkerEts,
                              fun(Ctx) ->
                                Ctx#{
                                  forward_completed => maps:get(forward_completed, Ctx, 0) + 1,
                                  total_microbatches => TotalMicrobatches
                                }
                              end
                            ),
                            ets:update_element(
                              GenWorkerEts,
                              parallel_time_acc,
                              {
                                ?ETS_KEYVAL_VAL_IDX,
                                ets:lookup_element(GenWorkerEts, parallel_time_acc, ?ETS_KEYVAL_VAL_IDX) + StageTime
                              }
                            ),
                            stats:increment_by_value(get(worker_stats_ets), acc_time_training, trunc(StageTime)),
                            case resolve_pipeline_adjacent_worker(GenWorkerEts, next) of
                              {ok, NextWorker} ->
                                ForwardPayload = {
                                  pipeline_forward_payload,
                                  BatchID,
                                  SourceName,
                                  TotalMicrobatches,
                                  MicrobatchID,
                                  {StageTensor, StageType},
                                  {ForwardLabelsTensor, ForwardLabelsType}
                                },
                                route_pipeline_payload_to_worker(GenWorkerEts, WorkerName, NextWorker, ForwardPayload),
                                processed;
                              {error, AdjacentReason} ->
                                {abort, {pipeline_next_stage_resolution_failed, AdjacentReason}}
                            end;
                          {nerlnif, error, Reason} ->
                            {abort, {pipeline_stage_forward_nif_error, Reason}};
                          Unexpected ->
                            {abort, {pipeline_stage_forward_unexpected, Unexpected}}
                        end
                      end,
                      async_pipeline_nif(
                        GenWorkerEts,
                        call_to_pipeline_stage_forward,
                        [ModelId, PreparedActivation, Labels, BatchID, SourceName, MicrobatchID],
                        FwdContinuation
                      )
                  end
              end
          end
      end
  end;
maybe_process_pipeline_forward_payload(_GenWorkerEts, _ModelId, _WorkerName, Payload) ->
  {skip, {invalid_pipeline_forward_payload, Payload}}.

dispatch_pipeline_backward_buffer(GenWorkerEts, ModelId, WorkerName) ->
  BackwardBuffer = ets:lookup_element(GenWorkerEts, parallel_pipeline_backward_buffer, ?ETS_KEYVAL_VAL_IDX),
  dispatch_pipeline_backward_buffer_by_grant(GenWorkerEts, ModelId, WorkerName, BackwardBuffer).

dispatch_pipeline_backward_buffer_by_grant(GenWorkerEts, _ModelId, _WorkerName, []) ->
  ets:update_element(GenWorkerEts, parallel_pipeline_backward_buffer, {?ETS_KEYVAL_VAL_IDX, []}),
  ok;
dispatch_pipeline_backward_buffer_by_grant(GenWorkerEts, ModelId, WorkerName, BackwardBuffer) ->
  case peek_pipeline_backward_grant(GenWorkerEts) of
    none ->
      ets:update_element(
        GenWorkerEts,
        parallel_pipeline_backward_buffer,
        {?ETS_KEYVAL_VAL_IDX, BackwardBuffer}
      ),
      ok;
    {GrantBatchID, GrantMicrobatchID, _GrantStageID} ->
      case pop_pipeline_backward_payload_for_grant(GenWorkerEts, BackwardBuffer, GrantBatchID, GrantMicrobatchID, []) of
        not_found ->
          ets:update_element(
            GenWorkerEts,
            parallel_pipeline_backward_buffer,
            {?ETS_KEYVAL_VAL_IDX, BackwardBuffer}
          ),
          ok;
        {ok, Payload, RemainingBuffer} ->
          case maybe_process_pipeline_backward_payload(GenWorkerEts, ModelId, WorkerName, Payload) of
            processed ->
              case is_pipeline_nif_in_flight(GenWorkerEts) of
                true ->
                  ets:update_element(
                    GenWorkerEts,
                    parallel_pipeline_backward_buffer,
                    {?ETS_KEYVAL_VAL_IDX, RemainingBuffer}
                  ),
                  ok;
                false ->
                  dispatch_pipeline_backward_buffer_by_grant(
                    GenWorkerEts,
                    ModelId,
                    WorkerName,
                    RemainingBuffer
                  )
              end;
            wait_for_batch ->
              ets:update_element(
                GenWorkerEts,
                parallel_pipeline_backward_buffer,
                {?ETS_KEYVAL_VAL_IDX, [Payload | RemainingBuffer]}
              ),
              ok;
            wait_for_grant ->
              ets:update_element(
                GenWorkerEts,
                parallel_pipeline_backward_buffer,
                {?ETS_KEYVAL_VAL_IDX, [Payload | RemainingBuffer]}
              ),
              ok;
            {skip, Reason} ->
              ets:update_element(
                GenWorkerEts,
                parallel_pipeline_backward_buffer,
                {?ETS_KEYVAL_VAL_IDX, [Payload | RemainingBuffer]}
              ),
              {abort, {pipeline_backward_payload_skipped, Reason}};
            {abort, _Reason} = Abort ->
              ets:update_element(
                GenWorkerEts,
                parallel_pipeline_backward_buffer,
                {?ETS_KEYVAL_VAL_IDX, [Payload | RemainingBuffer]}
              ),
              Abort
          end
      end
  end.

peek_pipeline_backward_grant(GenWorkerEts) ->
  case ets:lookup_element(GenWorkerEts, parallel_scheduler_grants, ?ETS_KEYVAL_VAL_IDX) of
    [HeadGrant | _Rest] ->
      case normalize_parallel_scheduler_grant(HeadGrant) of
        {ok, {backward, GrantBatchID, MicrobatchID, StageID}} ->
          {GrantBatchID, MicrobatchID, StageID};
        _ ->
          none
      end;
    _ ->
      none
  end.

pop_pipeline_backward_payload_for_grant(GenWorkerEts, BackwardBuffer, RequestedBatchID, MicrobatchID, Acc) ->
  EffectiveBatchID =
    case RequestedBatchID of
      any ->
        case active_pipeline_batch_id(GenWorkerEts) of
          {ok, ActiveBatchID} -> ActiveBatchID;
          _ -> any
        end;
      _ -> RequestedBatchID
    end,
  pop_pipeline_backward_payload_for_grant_loop(BackwardBuffer, EffectiveBatchID, MicrobatchID, Acc).

pop_pipeline_backward_payload_for_grant_loop([], _BatchID, _MicrobatchID, _Acc) ->
  not_found;
pop_pipeline_backward_payload_for_grant_loop(
  [Payload = {pipeline_backward_payload, PayloadBatchID, _SourceName, _TotalMicrobatches, PayloadMicrobatchID, _Grad} | Rest],
  BatchID,
  MicrobatchID,
  Acc
) ->
  case (PayloadMicrobatchID =:= MicrobatchID) andalso payload_batch_matches(BatchID, PayloadBatchID) of
    true ->
      {ok, Payload, lists:reverse(Acc) ++ Rest};
    false ->
      pop_pipeline_backward_payload_for_grant_loop(Rest, BatchID, MicrobatchID, [Payload | Acc])
  end;
pop_pipeline_backward_payload_for_grant_loop([Payload | Rest], BatchID, MicrobatchID, Acc) ->
  pop_pipeline_backward_payload_for_grant_loop(Rest, BatchID, MicrobatchID, [Payload | Acc]).

payload_batch_matches(any, _PayloadBatchID) ->
  true;
payload_batch_matches(ExpectedBatchID, PayloadBatchID) ->
  ExpectedBatchID =:= PayloadBatchID.

maybe_process_pipeline_backward_payload(
  GenWorkerEts,
  ModelId,
  WorkerName,
  {pipeline_backward_payload, BatchID, SourceName, TotalMicrobatches, MicrobatchID, Grad}
) ->
  case pipeline_torch_backend_supported(GenWorkerEts) of
    false ->
      {abort, pipeline_torch_backend_required};
    true ->
      case pipeline_payload_batch_status(GenWorkerEts, BatchID) of
        wait_for_batch ->
          wait_for_batch;
        ready ->
          StageID = get_worker_pipeline_stage(GenWorkerEts),
          PipelineMode = normalize_parallel_mode_atom(
                           ets:lookup_element(GenWorkerEts, parallel_mode, ?ETS_KEYVAL_VAL_IDX)
                         ),
          case maybe_emit_parallel_backward_event(
                 PipelineMode,
                 WorkerName,
                 BatchID,
                 MicrobatchID,
                 StageID,
                 0.0
               ) of
            {error, no_scheduler_grant} ->
              wait_for_grant;
            {error, EmitReason} ->
              {abort, {pipeline_backward_event_rejected, EmitReason}};
            ok ->
              case maybe_apply_tensor_parallel_collectives(
                     GenWorkerEts,
                     PipelineMode,
                     BatchID,
                     MicrobatchID,
                     Grad
                   ) of
                {abort, TpAbortReason} ->
                  {abort, {tensor_collective_failed, TpAbortReason}};
                {ok, PreparedGrad} ->
                  BwdContinuation = fun(NifResult) ->
                    case NifResult of
                      {ok, pipeline_stage_backward, PrevGradTensor, PrevGradType, StageTime} ->
                        update_active_pipeline_batch_ctx(
                          GenWorkerEts,
                          fun(Ctx) ->
                            Ctx#{
                              backward_completed => maps:get(backward_completed, Ctx, 0) + 1,
                              total_microbatches => TotalMicrobatches
                            }
                          end
                        ),
                        ets:update_element(
                          GenWorkerEts,
                          parallel_time_acc,
                          {
                            ?ETS_KEYVAL_VAL_IDX,
                            ets:lookup_element(GenWorkerEts, parallel_time_acc, ?ETS_KEYVAL_VAL_IDX) + StageTime
                          }
                        ),
                        stats:increment_by_value(get(worker_stats_ets), acc_time_training, trunc(StageTime)),
                        case resolve_pipeline_adjacent_worker(GenWorkerEts, prev) of
                          {ok, PrevWorker} ->
                            PrevPayload = {
                              pipeline_backward_payload,
                              BatchID,
                              SourceName,
                              TotalMicrobatches,
                              MicrobatchID,
                              {PrevGradTensor, PrevGradType}
                            },
                            route_pipeline_payload_to_worker(GenWorkerEts, WorkerName, PrevWorker, PrevPayload),
                            processed;
                          {error, no_adjacent_stage} ->
                            processed;
                          {error, AdjacentReason} ->
                            {abort, {pipeline_prev_stage_resolution_failed, AdjacentReason}}
                        end;
                      {nerlnif, error, Reason} ->
                        {abort, {pipeline_stage_backward_nif_error, Reason}};
                      Unexpected ->
                        {abort, {pipeline_stage_backward_unexpected, Unexpected}}
                    end
                  end,
                  async_pipeline_nif(
                    GenWorkerEts,
                    call_to_pipeline_stage_backward,
                    [ModelId, PreparedGrad, BatchID, MicrobatchID],
                    BwdContinuation
                  )
              end
          end
      end
  end;
maybe_process_pipeline_backward_payload(_GenWorkerEts, _ModelId, _WorkerName, Payload) ->
  {skip, {invalid_pipeline_backward_payload, Payload}}.

maybe_finalize_pipeline_training_batch(GenWorkerEts, WorkerName) ->
  ActiveCtx = ets:lookup_element(GenWorkerEts, parallel_active_batch_ctx, ?ETS_KEYVAL_VAL_IDX),
  case ActiveCtx of
    undefined ->
      ok;
    Ctx when is_map(Ctx) ->
      Mode = maps:get(mode, Ctx, legacy),
      case Mode of
        pipeline ->
          TotalMicrobatches = maps:get(total_microbatches, Ctx, 0),
          ForwardCompleted = maps:get(forward_completed, Ctx, maps:get(forward_dispatched, Ctx, 0)),
          BackwardCompleted = maps:get(backward_completed, Ctx, 0),
          IsLastStage = is_last_pipeline_stage(GenWorkerEts),
          PendingBackwardEvents = ets:lookup_element(GenWorkerEts, parallel_pending_backward_events, ?ETS_KEYVAL_VAL_IDX),
          Done =
            case IsLastStage of
              true ->
                ForwardCompleted >= TotalMicrobatches andalso
                TotalMicrobatches > 0 andalso
                PendingBackwardEvents =:= [];
              false -> ForwardCompleted >= TotalMicrobatches andalso BackwardCompleted >= TotalMicrobatches andalso TotalMicrobatches > 0
            end,
          case Done of
            false ->
              ok;
            true ->
              ModelId = maps:get(model_id, Ctx, ets:lookup_element(GenWorkerEts, model_id, ?ETS_KEYVAL_VAL_IDX)),
              SourceName = maps:get(source_name, Ctx, undefined),
              BatchID = maps:get(batch_id, Ctx, undefined),
              case maybe_call_optimizer_barrier(ModelId) of
                ok ->
                  case IsLastStage of
                    true ->
                      maybe_send_pipeline_last_stage_loss(GenWorkerEts, WorkerName, SourceName, BatchID, TotalMicrobatches);
                    false ->
                      ok
                  end,
                  reset_parallel_batch_context(GenWorkerEts),
                  maybe_dispatch_deferred_parallel_sample(GenWorkerEts, train),
                  ok;
                {error, BarrierReason} ->
                  {abort, {optimizer_barrier_failed, BarrierReason}}
              end
          end;
        _ ->
          ok
      end;
    _ ->
      ok
  end.

maybe_send_pipeline_last_stage_loss(GenWorkerEts, WorkerName, SourceName, BatchID, TotalMicrobatches) ->
  SkippedMicrobatches = ets:lookup_element(GenWorkerEts, parallel_microbatches_skipped, ?ETS_KEYVAL_VAL_IDX),
  EffectiveMicrobatches = erlang:max(1, TotalMicrobatches - SkippedMicrobatches),
  LossAcc = ets:lookup_element(GenWorkerEts, parallel_loss_acc, ?ETS_KEYVAL_VAL_IDX),
  AvgLossTensor = finalize_parallel_loss(LossAcc, EffectiveMicrobatches),
  TimeAcc = ets:lookup_element(GenWorkerEts, parallel_time_acc, ?ETS_KEYVAL_VAL_IDX),
  AvgTime = TimeAcc / EffectiveMicrobatches,
  BatchTimeStamp = erlang:system_time(nanosecond),
  WorkerToken = ets:lookup_element(GenWorkerEts, distributed_system_token, ?ETS_KEYVAL_VAL_IDX),
  ?LOG_INFO(
    "Worker ~p pipeline last-stage completed batch=~p source=~p microbatches=~p",
    [WorkerName, BatchID, SourceName, TotalMicrobatches]
  ),
  gen_statem:cast(
    get(client_pid),
    {loss, WorkerName, SourceName, AvgLossTensor, AvgTime, WorkerToken, BatchID, BatchTimeStamp}
  ).

ensure_active_pipeline_training_ctx(GenWorkerEts, ModelId, SourceName, BatchID, TotalMicrobatches) ->
  ActiveCtx = ets:lookup_element(GenWorkerEts, parallel_active_batch_ctx, ?ETS_KEYVAL_VAL_IDX),
  case ActiveCtx of
    undefined ->
      StageID = get_worker_pipeline_stage(GenWorkerEts),
      NewCtx = #{
        model_id => ModelId,
        source_name => SourceName,
        batch_id => BatchID,
        source_batch_id => BatchID,
        mode => pipeline,
        stage => StageID,
        total_microbatches => TotalMicrobatches,
        forward_completed => 0,
        backward_completed => 0
      },
      ets:update_element(GenWorkerEts, parallel_active_batch_ctx, {?ETS_KEYVAL_VAL_IDX, NewCtx}),
      ets:update_element(GenWorkerEts, parallel_total_microbatches, {?ETS_KEYVAL_VAL_IDX, TotalMicrobatches}),
      ets:update_element(GenWorkerEts, parallel_loss_acc, {?ETS_KEYVAL_VAL_IDX, undefined}),
      ets:update_element(GenWorkerEts, parallel_time_acc, {?ETS_KEYVAL_VAL_IDX, 0.0}),
      ok;
    Ctx when is_map(Ctx) ->
      case maps:get(batch_id, Ctx, BatchID) of
        BatchID ->
          ok;
        ActiveBatchID ->
          case has_pending_runtime_excluding_scheduler_grants(GenWorkerEts) of
            false ->
              StageID = get_worker_pipeline_stage(GenWorkerEts),
              WorkerName = ets:lookup_element(GenWorkerEts, worker_name, ?ETS_KEYVAL_VAL_IDX),
              ?LOG_WARNING(
                "Worker ~p replacing stale active pipeline context batch=~p with new batch=~p",
                [WorkerName, ActiveBatchID, BatchID]
              ),
              NewCtx = #{
                model_id => ModelId,
                source_name => SourceName,
                batch_id => BatchID,
                source_batch_id => BatchID,
                mode => pipeline,
                stage => StageID,
                total_microbatches => TotalMicrobatches,
                forward_completed => 0,
                backward_completed => 0
              },
              ets:update_element(GenWorkerEts, parallel_active_batch_ctx, {?ETS_KEYVAL_VAL_IDX, NewCtx}),
              ets:update_element(GenWorkerEts, parallel_total_microbatches, {?ETS_KEYVAL_VAL_IDX, TotalMicrobatches}),
              ets:update_element(GenWorkerEts, parallel_loss_acc, {?ETS_KEYVAL_VAL_IDX, undefined}),
              ets:update_element(GenWorkerEts, parallel_time_acc, {?ETS_KEYVAL_VAL_IDX, 0.0}),
              ok;
            true ->
              {error, {pipeline_batch_context_mismatch, ActiveBatchID, BatchID}}
          end
      end;
    _ ->
      ok
  end.

active_pipeline_batch_id(GenWorkerEts) ->
  case ets:lookup_element(GenWorkerEts, parallel_active_batch_ctx, ?ETS_KEYVAL_VAL_IDX) of
    Ctx when is_map(Ctx) ->
      case maps:get(batch_id, Ctx, undefined) of
        BatchID when is_integer(BatchID) ->
          {ok, BatchID};
        _ ->
          undefined
      end;
    _ ->
      undefined
  end.

pipeline_payload_batch_status(GenWorkerEts, BatchID) when is_integer(BatchID) ->
  case active_pipeline_batch_id(GenWorkerEts) of
    undefined ->
      ready;
    {ok, BatchID} ->
      ready;
    {ok, ActiveBatchID} ->
      ?LOG_INFO(
        "Worker ~p deferring payload for batch=~p while active batch=~p",
        [ets:lookup_element(GenWorkerEts, worker_name, ?ETS_KEYVAL_VAL_IDX), BatchID, ActiveBatchID]
      ),
      wait_for_batch
  end;
pipeline_payload_batch_status(_GenWorkerEts, _BatchID) ->
  ready.

update_active_pipeline_batch_ctx(GenWorkerEts, UpdaterFun) ->
  ActiveCtx = ets:lookup_element(GenWorkerEts, parallel_active_batch_ctx, ?ETS_KEYVAL_VAL_IDX),
  case ActiveCtx of
    Ctx when is_map(Ctx) ->
      UpdatedCtx = UpdaterFun(Ctx),
      ets:update_element(GenWorkerEts, parallel_active_batch_ctx, {?ETS_KEYVAL_VAL_IDX, UpdatedCtx});
    _ ->
      ok
  end.

update_stage0_forward_progress_ctx(Ctx, NewDispatched, TotalMicrobatches) ->
  ExistingForwardCompleted = maps:get(forward_completed, Ctx, 0),
  Ctx#{
    forward_dispatched => NewDispatched,
    forward_completed => erlang:max(ExistingForwardCompleted, NewDispatched),
    total_microbatches => TotalMicrobatches
  }.

bind_stage0_runtime_batch_id(GenWorkerEts, RuntimeBatchID) ->
  update_active_pipeline_batch_ctx(
    GenWorkerEts,
    fun(Ctx) ->
      SourceBatchID = maps:get(source_batch_id, Ctx, maps:get(batch_id, Ctx, RuntimeBatchID)),
      Ctx#{
        source_batch_id => SourceBatchID,
        batch_id => RuntimeBatchID
      }
    end
  ).

dispatch_pipeline_predict_buffers(GenWorkerEts, ModelId, WorkerName) ->
  case is_pipeline_nif_in_flight(GenWorkerEts) of
    true -> ok;
    false ->
      case dispatch_pipeline_stage0_queue(GenWorkerEts, ModelId, WorkerName) of
        {abort, _Reason} = Abort ->
          Abort;
        ok ->
          case is_pipeline_nif_in_flight(GenWorkerEts) of
            true -> ok;
            false ->
              PredictBuffer = ets:lookup_element(GenWorkerEts, parallel_pipeline_predict_buffer, ?ETS_KEYVAL_VAL_IDX),
              case dispatch_pipeline_predict_buffer_loop(GenWorkerEts, ModelId, WorkerName, PredictBuffer, []) of
                {abort, _Reason} = Abort ->
                  Abort;
                ok ->
                  case is_pipeline_nif_in_flight(GenWorkerEts) of
                    true -> ok;
                    false ->
                      maybe_finalize_pipeline_predict_batch(GenWorkerEts, WorkerName)
                  end
              end
          end
      end
  end.

dispatch_pipeline_predict_buffer_loop(GenWorkerEts, _ModelId, _WorkerName, [], RemainingAcc) ->
  ets:update_element(
    GenWorkerEts,
    parallel_pipeline_predict_buffer,
    {?ETS_KEYVAL_VAL_IDX, lists:reverse(RemainingAcc)}
  ),
  ok;
dispatch_pipeline_predict_buffer_loop(
  GenWorkerEts,
  ModelId,
  WorkerName,
  [Payload | Rest],
  RemainingAcc
) ->
  case maybe_process_pipeline_predict_payload(GenWorkerEts, ModelId, WorkerName, Payload) of
    processed ->
      case is_pipeline_nif_in_flight(GenWorkerEts) of
        true ->
          ets:update_element(
            GenWorkerEts,
            parallel_pipeline_predict_buffer,
            {?ETS_KEYVAL_VAL_IDX, lists:reverse(RemainingAcc) ++ Rest}
          ),
          ok;
        false ->
          dispatch_pipeline_predict_buffer_loop(GenWorkerEts, ModelId, WorkerName, Rest, RemainingAcc)
      end;
    wait_for_batch ->
      ets:update_element(
        GenWorkerEts,
        parallel_pipeline_predict_buffer,
        {?ETS_KEYVAL_VAL_IDX, lists:reverse(RemainingAcc) ++ [Payload | Rest]}
      ),
      ok;
    wait_for_grant ->
      ets:update_element(
        GenWorkerEts,
        parallel_pipeline_predict_buffer,
        {?ETS_KEYVAL_VAL_IDX, lists:reverse(RemainingAcc) ++ [Payload | Rest]}
      ),
      ok;
    {skip, Reason} ->
      ets:update_element(
        GenWorkerEts,
        parallel_pipeline_predict_buffer,
        {?ETS_KEYVAL_VAL_IDX, lists:reverse([Payload | RemainingAcc]) ++ Rest}
      ),
      {abort, {pipeline_predict_payload_skipped, Reason}};
    {abort, _Reason} = Abort ->
      Abort
  end.

maybe_process_pipeline_predict_payload(
  GenWorkerEts,
  ModelId,
  WorkerName,
  {pipeline_predict_payload, BatchID, SourceName, TotalMicrobatches, MicrobatchID, Activation}
) ->
  case pipeline_torch_backend_supported(GenWorkerEts) of
    false ->
      {abort, pipeline_torch_backend_required};
    true ->
      case pipeline_payload_batch_status(GenWorkerEts, BatchID) of
        wait_for_batch ->
          wait_for_batch;
        ready ->
          StageID = get_worker_pipeline_stage(GenWorkerEts),
          PipelineMode = normalize_parallel_mode_atom(
                           ets:lookup_element(GenWorkerEts, parallel_mode, ?ETS_KEYVAL_VAL_IDX)
                         ),
          case maybe_emit_parallel_forward_event(pipeline_predict, WorkerName, BatchID, MicrobatchID, StageID) of
            {error, no_scheduler_grant} ->
              wait_for_grant;
            {error, EmitReason} ->
              {abort, {pipeline_predict_event_rejected, EmitReason}};
            ok ->
              case maybe_apply_tensor_parallel_collectives(
                     GenWorkerEts,
                     PipelineMode,
                     BatchID,
                     MicrobatchID,
                     Activation
                   ) of
                {abort, TpAbortReason} ->
                  {abort, {tensor_collective_failed, TpAbortReason}};
                {ok, PreparedActivation} ->
                  PredStageContinuation = fun(NifResult) ->
                    case NifResult of
                      {ok, pipeline_predict_stage, StageTensor, StageType, StageTime} ->
                        update_active_pipeline_batch_ctx(
                          GenWorkerEts,
                          fun(Ctx) ->
                            ExistingPred = maps:get(predict_acc, Ctx, []),
                            Ctx#{
                              forward_completed => maps:get(forward_completed, Ctx, 0) + 1,
                              total_microbatches => TotalMicrobatches,
                              predict_acc => ExistingPred ++ [{MicrobatchID, {StageTensor, StageType}}]
                            }
                          end
                        ),
                        ets:update_element(
                          GenWorkerEts,
                          parallel_time_acc,
                          {
                            ?ETS_KEYVAL_VAL_IDX,
                            ets:lookup_element(GenWorkerEts, parallel_time_acc, ?ETS_KEYVAL_VAL_IDX) + StageTime
                          }
                        ),
                        stats:increment_by_value(get(worker_stats_ets), acc_time_prediction, trunc(StageTime)),
                        case is_last_pipeline_stage(GenWorkerEts) of
                          true ->
                            processed;
                          false ->
                            case resolve_pipeline_adjacent_worker(GenWorkerEts, next) of
                              {ok, NextWorker} ->
                                ForwardPayload = {
                                  pipeline_predict_payload,
                                  BatchID,
                                  SourceName,
                                  TotalMicrobatches,
                                  MicrobatchID,
                                  {StageTensor, StageType}
                                },
                                route_pipeline_payload_to_worker(GenWorkerEts, WorkerName, NextWorker, ForwardPayload),
                                processed;
                              {error, AdjacentReason} ->
                                {abort, {pipeline_predict_next_stage_resolution_failed, AdjacentReason}}
                            end
                        end;
                      {nerlnif, error, Reason} ->
                        {abort, {pipeline_predict_stage_nif_error, Reason}};
                      Unexpected ->
                        {abort, {pipeline_predict_stage_unexpected, Unexpected}}
                    end
                  end,
                  async_pipeline_nif(
                    GenWorkerEts,
                    call_to_pipeline_predict_stage_forward,
                    [ModelId, PreparedActivation, BatchID],
                    PredStageContinuation
                  )
              end
          end
      end
  end;
maybe_process_pipeline_predict_payload(_GenWorkerEts, _ModelId, _WorkerName, Payload) ->
  {skip, {invalid_pipeline_predict_payload, Payload}}.

maybe_finalize_pipeline_predict_batch(GenWorkerEts, WorkerName) ->
  ActiveCtx = ets:lookup_element(GenWorkerEts, parallel_active_batch_ctx, ?ETS_KEYVAL_VAL_IDX),
  case ActiveCtx of
    Ctx when is_map(Ctx) ->
      Mode = maps:get(mode, Ctx, legacy),
      case Mode of
        pipeline_predict ->
          TotalMicrobatches = maps:get(total_microbatches, Ctx, 0),
          ForwardCompleted = maps:get(forward_completed, Ctx, maps:get(forward_dispatched, Ctx, 0)),
          StageID = get_worker_pipeline_stage(GenWorkerEts),
          IsLastStage = is_last_pipeline_stage(GenWorkerEts),
          IsBatchComplete = ForwardCompleted >= TotalMicrobatches andalso TotalMicrobatches > 0,
          case IsBatchComplete of
            false ->
              ok;
            true ->
              case IsLastStage of
                true ->
                  ModelPredAcc = maps:get(predict_acc, Ctx, []),
                  maybe_send_pipeline_last_stage_prediction(GenWorkerEts, WorkerName, Ctx, ModelPredAcc, TotalMicrobatches);
                false ->
                  ?LOG_INFO(
                    "Worker ~p pipeline predict stage ~p completed local batch=~p source=~p microbatches=~p",
                    [WorkerName, StageID, maps:get(batch_id, Ctx, undefined), maps:get(source_name, Ctx, undefined), TotalMicrobatches]
                  )
              end,
              reset_parallel_batch_context(GenWorkerEts),
              maybe_dispatch_deferred_parallel_sample(GenWorkerEts, predict),
              ok
          end;
        _ ->
          ok
      end;
    _ ->
      ok
  end.

maybe_send_pipeline_last_stage_prediction(
  GenWorkerEts,
  WorkerName,
  Ctx,
  PredictAcc,
  TotalMicrobatches
) ->
  SortedPredictions = lists:keysort(1, PredictAcc),
  PayloadList = [TensorPayload || {_MicrobatchID, TensorPayload} <- SortedPredictions],
  case PayloadList of
    [] ->
      ok;
    [SinglePayload] ->
      send_pipeline_prediction_to_client(GenWorkerEts, WorkerName, Ctx, SinglePayload, TotalMicrobatches);
    _ ->
      case PayloadList of
        [{_TensorBin, TensorType} | _] ->
          case nif_call(nerltensor_concat_nif, [PayloadList, TensorType, 0]) of
            {PredTensor, PredType} ->
              send_pipeline_prediction_to_client(
                GenWorkerEts,
                WorkerName,
                Ctx,
                {PredTensor, PredType},
                TotalMicrobatches
              );
            _ ->
              ok
          end;
        _ ->
          ok
      end
  end.

send_pipeline_prediction_to_client(GenWorkerEts, WorkerName, Ctx, {PredTensor, PredType}, TotalMicrobatches) ->
  SourceName = maps:get(source_name, Ctx, undefined),
  BatchID = maps:get(batch_id, Ctx, undefined),
  BatchTimeStamp = erlang:system_time(nanosecond),
  WorkerToken = ets:lookup_element(GenWorkerEts, distributed_system_token, ?ETS_KEYVAL_VAL_IDX),
  TimeAcc = ets:lookup_element(GenWorkerEts, parallel_time_acc, ?ETS_KEYVAL_VAL_IDX),
  AvgTime = TimeAcc / erlang:max(1, TotalMicrobatches),
  ?LOG_INFO(
    "Worker ~p pipeline last-stage predict completed batch=~p source=~p microbatches=~p",
    [WorkerName, BatchID, SourceName, TotalMicrobatches]
  ),
  stats:increment_by_value(get(worker_stats_ets), batches_sent_predict, 1),
  gen_statem:cast(
    get(client_pid),
    {predictRes, WorkerName, SourceName, {PredTensor, PredType}, AvgTime, WorkerToken, BatchID, BatchTimeStamp}
  ).

ensure_active_pipeline_predict_ctx(GenWorkerEts, ModelId, SourceName, BatchID, TotalMicrobatches) ->
  ActiveCtx = ets:lookup_element(GenWorkerEts, parallel_active_batch_ctx, ?ETS_KEYVAL_VAL_IDX),
  case ActiveCtx of
    undefined ->
      StageID = get_worker_pipeline_stage(GenWorkerEts),
      NewCtx = #{
        model_id => ModelId,
        source_name => SourceName,
        batch_id => BatchID,
        source_batch_id => BatchID,
        mode => pipeline_predict,
        stage => StageID,
        total_microbatches => TotalMicrobatches,
        forward_completed => 0,
        predict_acc => []
      },
      ets:update_element(GenWorkerEts, parallel_active_batch_ctx, {?ETS_KEYVAL_VAL_IDX, NewCtx}),
      ets:update_element(GenWorkerEts, parallel_total_microbatches, {?ETS_KEYVAL_VAL_IDX, TotalMicrobatches}),
      ets:update_element(GenWorkerEts, parallel_time_acc, {?ETS_KEYVAL_VAL_IDX, 0.0}),
      ok;
    _ ->
      ok
  end.

resolve_pipeline_adjacent_worker(GenWorkerEts, next) ->
  WorkerParallelCfg = ets:lookup_element(GenWorkerEts, worker_parallel_cfg, ?ETS_KEYVAL_VAL_IDX),
  Stage = normalize_tp_int(maps:get(pipeline_stage, WorkerParallelCfg, 0), 0),
  WorldSize = normalize_tp_int(maps:get(pipeline_world_size, WorkerParallelCfg, 1), 1),
  PreferredTpRank = normalize_tp_int(maps:get(tp_rank, WorkerParallelCfg, 0), 0),
  TargetStage = Stage + 1,
  case TargetStage >= WorldSize of
    true -> {error, no_adjacent_stage};
    false -> pick_pipeline_stage_worker(TargetStage, WorldSize, PreferredTpRank)
  end;
resolve_pipeline_adjacent_worker(GenWorkerEts, prev) ->
  WorkerParallelCfg = ets:lookup_element(GenWorkerEts, worker_parallel_cfg, ?ETS_KEYVAL_VAL_IDX),
  Stage = normalize_tp_int(maps:get(pipeline_stage, WorkerParallelCfg, 0), 0),
  WorldSize = normalize_tp_int(maps:get(pipeline_world_size, WorkerParallelCfg, 1), 1),
  PreferredTpRank = normalize_tp_int(maps:get(tp_rank, WorkerParallelCfg, 0), 0),
  TargetStage = Stage - 1,
  case TargetStage < 0 of
    true -> {error, no_adjacent_stage};
    false -> pick_pipeline_stage_worker(TargetStage, WorldSize, PreferredTpRank)
  end.

pick_pipeline_stage_worker(TargetStage, WorldSize, PreferredTpRank) ->
  WorkersParallelMap =
    case catch ets:lookup_element(nerlnet_data, workers_parallel, ?ETS_KEYVAL_VAL_IDX) of
      {'EXIT', _} -> #{};
      Map when is_map(Map) -> Map;
      _ -> #{}
    end,
  MatchingWorkers =
    maps:fold(
      fun(CandidateWorker, CandidateCfg, Acc) ->
        CandidateStage = normalize_tp_int(maps:get(pipeline_stage, CandidateCfg, -1), -1),
        CandidateWorld = normalize_tp_int(maps:get(pipeline_world_size, CandidateCfg, 1), 1),
        case (CandidateStage =:= TargetStage) andalso (CandidateWorld =:= WorldSize) of
          true ->
            CandidateTpRank = normalize_tp_int(maps:get(tp_rank, CandidateCfg, 0), 0),
            [{CandidateWorker, CandidateTpRank} | Acc];
          false -> Acc
        end
      end,
      [],
      WorkersParallelMap
    ),
  SortedWorkers =
    lists:sort(
      fun({WorkerA, _}, {WorkerB, _}) ->
        WorkerA =< WorkerB
      end,
      MatchingWorkers
    ),
  case SortedWorkers of
    [] -> {error, {missing_pipeline_stage_worker, TargetStage}};
    _ ->
      case [Worker || {Worker, CandidateTpRank} <- SortedWorkers, CandidateTpRank =:= PreferredTpRank] of
        [RankMatchedWorker | _] ->
          {ok, RankMatchedWorker};
        [] ->
          [{FallbackWorker, _} | _] = SortedWorkers,
          {ok, FallbackWorker}
      end
  end.

is_last_pipeline_stage(GenWorkerEts) ->
  WorkerParallelCfg = ets:lookup_element(GenWorkerEts, worker_parallel_cfg, ?ETS_KEYVAL_VAL_IDX),
  Stage = normalize_tp_int(maps:get(pipeline_stage, WorkerParallelCfg, 0), 0),
  WorldSize = normalize_tp_int(maps:get(pipeline_world_size, WorkerParallelCfg, 1), 1),
  Stage + 1 >= WorldSize.

route_pipeline_payload_to_worker(GenWorkerEts, FromWorker, ToWorker, Payload) ->
  W2WPid = ets:lookup_element(GenWorkerEts, w2wcom_pid, ?ETS_KEYVAL_VAL_IDX),
  ?LOG_INFO("Worker ~p routes pipeline payload to ~p payload_tag=~p", [FromWorker, ToWorker, element(1, Payload)]),
  send_message(W2WPid, FromWorker, ToWorker, Payload).

split_batch_into_microbatches({_TensorBin, _TensorType}, NumMicrobatches, _MicroBatchSize)
when NumMicrobatches =< 0 ->
  [];
split_batch_into_microbatches({TensorBin, TensorType}, 1, _MicroBatchSize) ->
  [{0, {TensorBin, TensorType}}];
split_batch_into_microbatches({TensorBin, TensorType}, NumMicrobatches, MicroBatchSize) ->
  case nif_call(decode_nif, [TensorBin, TensorType]) of
    {DecodedTensor, TensorListType} when is_list(DecodedTensor) ->
      build_microbatches_from_decoded(DecodedTensor, TensorListType, TensorType, NumMicrobatches, MicroBatchSize);
    _ ->
      []
  end.

build_microbatches_from_decoded([DimXRaw, DimYRaw, DimZRaw | Data], TensorListType, TensorType, NumMicrobatches, MicroBatchSize) ->
  DimX = round(DimXRaw),
  DimY = round(DimYRaw),
  DimZ = round(DimZRaw),
  SampleSpan = DimY * DimZ,
  case {DimX > 0, SampleSpan > 0, length(Data) == DimX * SampleSpan} of
    {true, true, true} ->
      RequestedMicroBatchSize = case MicroBatchSize > 0 of
                                  true -> MicroBatchSize;
                                  false -> erlang:max(1, DimX div NumMicrobatches)
                                end,
      build_microbatches_loop(Data, DimX, DimY, DimZ, SampleSpan, RequestedMicroBatchSize, NumMicrobatches, TensorListType, TensorType, 0, []);
    _ ->
      []
  end;
build_microbatches_from_decoded(_, _TensorListType, _TensorType, _NumMicrobatches, _MicroBatchSize) ->
  [].

build_microbatches_loop(_Data, _DimX, _DimY, _DimZ, _SampleSpan, _MicroBatchSize, NumMicrobatches, _TensorListType, _TensorType, MicrobatchID, Acc)
when MicrobatchID >= NumMicrobatches ->
  lists:reverse(Acc);
build_microbatches_loop([], _DimX, _DimY, _DimZ, _SampleSpan, _MicroBatchSize, _NumMicrobatches, _TensorListType, _TensorType, _MicrobatchID, Acc) ->
  lists:reverse(Acc);
build_microbatches_loop(Data, DimX, DimY, DimZ, SampleSpan, MicroBatchSize, NumMicrobatches, TensorListType, TensorType, MicrobatchID, Acc) ->
  RemainingSamples = length(Data) div SampleSpan,
  RemainingMicrobatches = NumMicrobatches - MicrobatchID,
  SamplesForThisMicrobatch = case RemainingMicrobatches =< 1 of
                               true -> RemainingSamples;
                               false -> erlang:min(MicroBatchSize, RemainingSamples - (RemainingMicrobatches - 1))
                             end,
  case SamplesForThisMicrobatch =< 0 of
    true ->
      lists:reverse(Acc);
    false ->
      ValuesCount = SamplesForThisMicrobatch * SampleSpan,
      {BatchData, RestData} = lists:split(ValuesCount, Data),
      DimPrefix = case TensorListType of
                    erl_float -> [float(SamplesForThisMicrobatch), float(DimY), float(DimZ)];
                    _ -> [SamplesForThisMicrobatch, DimY, DimZ]
                  end,
      MicrobatchList = DimPrefix ++ BatchData,
      case nif_call(encode_nif, [MicrobatchList, TensorType]) of
        {MicrobatchTensor, TensorType} ->
          build_microbatches_loop(
            RestData, DimX, DimY, DimZ, SampleSpan, MicroBatchSize, NumMicrobatches,
            TensorListType, TensorType, MicrobatchID + 1,
            [{MicrobatchID, {MicrobatchTensor, TensorType}} | Acc]
          );
        _ ->
          lists:reverse(Acc)
      end
  end.

normalize_parallel_mode_value(Value) when is_binary(Value) -> binary_to_list(Value);
normalize_parallel_mode_value(Value) when is_list(Value) -> Value;
normalize_parallel_mode_value(Value) when is_atom(Value) -> atom_to_list(Value);
normalize_parallel_mode_value(_) -> "legacy".

get_parallel_execution_int(ParallelExecution, [Key | Rest], Default) ->
  case maps:get(Key, ParallelExecution, undefined) of
    undefined -> get_parallel_execution_int(ParallelExecution, Rest, Default);
    Value ->
      try
        case Value of
          V when is_integer(V) -> V;
          V when is_binary(V) -> list_to_integer(binary_to_list(V));
          V when is_list(V) -> list_to_integer(V);
          _ -> Default
        end
      catch
        _:_ -> Default
      end
  end;
get_parallel_execution_int(_ParallelExecution, [], Default) ->
  Default.

accumulate_parallel_loss(undefined, LossTensor) ->
  LossTensor;
accumulate_parallel_loss({AccTensor, AccType}, {LossTensor, LossType}) ->
  SumType = case AccType of
              undefined -> LossType;
              _ -> AccType
            end,
  case nif_call(nerltensor_sum_nif, [AccTensor, LossTensor, SumType]) of
    {SummedTensor, SumType} -> {SummedTensor, SumType};
    _ -> {LossTensor, LossType}
  end.

finalize_parallel_loss(undefined, _TotalMicrobatches) ->
  {<<>>, float};
finalize_parallel_loss({LossTensor, LossType}, TotalMicrobatches) ->
  Scale = 1.0 / erlang:max(1, TotalMicrobatches),
  case nif_call(nerltensor_scalar_multiplication_nif, [LossTensor, LossType, Scale]) of
    {AvgTensor, AvgType} -> {AvgTensor, AvgType};
    _ -> {LossTensor, LossType}
  end.

maybe_call_optimizer_barrier(ModelID) ->
  NifModule = get_backend_module(),
  case erlang:function_exported(NifModule, call_to_optimizer_barrier, 1) of
    true ->
      try nif_call(call_to_optimizer_barrier, [ModelID]) of
        ok ->
          ok;
        {error, Reason} ->
          {error, {nif_error, Reason}};
        {nerlnif, error, Reason} ->
          {error, {nerlnif_error, Reason}};
        Unexpected ->
          ?LOG_WARNING("Unexpected optimizer barrier response from ~p: ~p", [NifModule, Unexpected]),
          ok
      catch
        Class:Reason:Stacktrace ->
          ?LOG_ERROR(
            "Optimizer barrier call failed in ~p with class=~p reason=~p stack=~p",
            [NifModule, Class, Reason, Stacktrace]
          ),
          {error, {Class, Reason}}
      end;
    false ->
      ok
  end.

set_worker_parallel_mode(GenWorkerEts, Mode) ->
  NormalizedMode = normalize_parallel_mode_atom(Mode),
  ets:update_element(GenWorkerEts, parallel_mode, {?ETS_KEYVAL_VAL_IDX, NormalizedMode}),
  case NormalizedMode of
    legacy ->
      ets:update_element(GenWorkerEts, parallel_execution, {?ETS_KEYVAL_VAL_IDX, #{}}),
      set_worker_parallel_authority(GenWorkerEts, false),
      reset_parallel_loss_context(GenWorkerEts);
    _ ->
      case is_pipeline_mode_atom(NormalizedMode) of
        true -> ok;
        false -> reset_parallel_loss_context(GenWorkerEts)
      end
  end.

set_worker_parallel_execution(GenWorkerEts, ParallelExecution) ->
  NormalizedExecution = case is_map(ParallelExecution) of
                          true -> ParallelExecution;
                          false -> #{}
                        end,
  Mode = normalize_parallel_mode_atom(
           ets:lookup_element(GenWorkerEts, parallel_mode, ?ETS_KEYVAL_VAL_IDX)
         ),
  StoredExecution = case Mode of
                      legacy -> #{};
                      _ -> NormalizedExecution
                    end,
  ets:update_element(GenWorkerEts, parallel_execution, {?ETS_KEYVAL_VAL_IDX, StoredExecution}),
  ets:update_element(GenWorkerEts, parallel_scheduler_grants, {?ETS_KEYVAL_VAL_IDX, []}),
  apply_congestion_config(GenWorkerEts, StoredExecution),
  case should_preserve_parallel_loss_context(Mode, StoredExecution) of
    true -> ok;
    false -> reset_parallel_loss_context(GenWorkerEts)
  end.

set_worker_parallel_authority(GenWorkerEts, EnabledRaw) ->
  Enabled = case EnabledRaw of
              true -> true;
              _ -> false
            end,
  ets:update_element(GenWorkerEts, parallel_super_authority, {?ETS_KEYVAL_VAL_IDX, Enabled}),
  case Enabled of
    true -> ok;
    false ->
      ets:update_element(GenWorkerEts, parallel_scheduler_grants, {?ETS_KEYVAL_VAL_IDX, []}),
      ets:update_element(GenWorkerEts, parallel_pending_backward_events, {?ETS_KEYVAL_VAL_IDX, []}),
      ets:update_element(GenWorkerEts, tp_collective_inbox_buffer, {?ETS_KEYVAL_VAL_IDX, []}),
      ets:update_element(GenWorkerEts, pipeline_nif_worker, {?ETS_KEYVAL_VAL_IDX, false})
  end.

apply_congestion_config(GenWorkerEts, Execution) when is_map(Execution) ->
  CongestionEnabled = resolve_bool_config(Execution,
    [<<"congestionDropEnabled">>, congestionDropEnabled, <<"congestion_drop_enabled">>, congestion_drop_enabled],
    false),
  SoftLimit = resolve_int_config(Execution,
    [<<"deferredSampleSoftLimit">>, deferredSampleSoftLimit, <<"deferred_sample_soft_limit">>, deferred_sample_soft_limit],
    256),
  BufferCap = resolve_int_config(Execution,
    [<<"pipelineBufferCap">>, pipelineBufferCap, <<"pipeline_buffer_cap">>, pipeline_buffer_cap],
    0),
  ets:update_element(GenWorkerEts, congestion_drop_enabled, {?ETS_KEYVAL_VAL_IDX, CongestionEnabled}),
  ets:update_element(GenWorkerEts, deferred_sample_soft_limit, {?ETS_KEYVAL_VAL_IDX, erlang:max(1, SoftLimit)}),
  ets:update_element(GenWorkerEts, pipeline_buffer_cap, {?ETS_KEYVAL_VAL_IDX, erlang:max(0, BufferCap)}),
  ets:update_element(GenWorkerEts, parallel_congestion_signaled, {?ETS_KEYVAL_VAL_IDX, false}),
  ets:update_element(GenWorkerEts, parallel_microbatches_skipped, {?ETS_KEYVAL_VAL_IDX, 0}),
  ok;
apply_congestion_config(_GenWorkerEts, _Execution) ->
  ok.

resolve_bool_config(_Map, [], Default) -> Default;
resolve_bool_config(Map, [Key | Rest], Default) ->
  case maps:find(Key, Map) of
    {ok, true} -> true;
    {ok, false} -> false;
    {ok, <<"true">>} -> true;
    {ok, <<"false">>} -> false;
    {ok, "true"} -> true;
    {ok, "false"} -> false;
    {ok, 1} -> true;
    {ok, 0} -> false;
    _ -> resolve_bool_config(Map, Rest, Default)
  end.

resolve_int_config(_Map, [], Default) -> Default;
resolve_int_config(Map, [Key | Rest], Default) ->
  case maps:find(Key, Map) of
    {ok, V} when is_integer(V) -> V;
    {ok, V} when is_binary(V) ->
      try binary_to_integer(V) catch _:_ -> resolve_int_config(Map, Rest, Default) end;
    {ok, V} when is_list(V) ->
      try list_to_integer(V) catch _:_ -> resolve_int_config(Map, Rest, Default) end;
    _ -> resolve_int_config(Map, Rest, Default)
  end.

is_buffer_over_cap(GenWorkerEts) ->
  CongestionEnabled = ets:lookup_element(GenWorkerEts, congestion_drop_enabled, ?ETS_KEYVAL_VAL_IDX),
  Cap = ets:lookup_element(GenWorkerEts, pipeline_buffer_cap, ?ETS_KEYVAL_VAL_IDX),
  case {CongestionEnabled, Cap > 0} of
    {true, true} ->
      FwdBuf = ets:lookup_element(GenWorkerEts, parallel_pipeline_forward_buffer, ?ETS_KEYVAL_VAL_IDX),
      BwdBuf = ets:lookup_element(GenWorkerEts, parallel_pipeline_backward_buffer, ?ETS_KEYVAL_VAL_IDX),
      PredBuf = ets:lookup_element(GenWorkerEts, parallel_pipeline_predict_buffer, ?ETS_KEYVAL_VAL_IDX),
      (length(FwdBuf) + length(BwdBuf) + length(PredBuf)) >= Cap;
    _ -> false
  end.

reject_grant_for_congestion(GenWorkerEts, WorkerName, Direction, BatchID, MicrobatchID, StageID) ->
  NormalizedDirection = normalize_parallel_direction_atom(Direction),
  NormalizedBatchID = normalize_parallel_batch_id(BatchID),
  FwdBuf = ets:lookup_element(GenWorkerEts, parallel_pipeline_forward_buffer, ?ETS_KEYVAL_VAL_IDX),
  BwdBuf = ets:lookup_element(GenWorkerEts, parallel_pipeline_backward_buffer, ?ETS_KEYVAL_VAL_IDX),
  PredBuf = ets:lookup_element(GenWorkerEts, parallel_pipeline_predict_buffer, ?ETS_KEYVAL_VAL_IDX),
  Cap = ets:lookup_element(GenWorkerEts, pipeline_buffer_cap, ?ETS_KEYVAL_VAL_IDX),
  ?LOG_WARNING(
    "Worker ~p rejecting grant for buffer congestion direction=~p batch=~p microbatch=~p stage=~p fwd=~p bwd=~p pred=~p cap=~p",
    [WorkerName, NormalizedDirection, NormalizedBatchID, MicrobatchID, StageID,
     length(FwdBuf), length(BwdBuf), length(PredBuf), Cap]
  ),
  ets:update_counter(get(worker_stats_ets), congestion_grant_rejected, 1),
  ets:update_element(GenWorkerEts, parallel_congestion_signaled, {?ETS_KEYVAL_VAL_IDX, true}),
  SkipMeta = #{
    reason => buffer_congestion,
    event_id => {buffer_congestion, WorkerName, NormalizedDirection, NormalizedBatchID, MicrobatchID, StageID}
  },
  gen_statem:cast(
    get(client_pid),
    {parallel_skip_event, WorkerName, NormalizedDirection, NormalizedBatchID, MicrobatchID, StageID, SkipMeta}
  ).

append_parallel_scheduler_grant(GenWorkerEts, Direction, BatchID, MicrobatchID, StageID) ->
  Grants = ets:lookup_element(GenWorkerEts, parallel_scheduler_grants, ?ETS_KEYVAL_VAL_IDX),
  NormalizedGrant = {
    normalize_parallel_direction_atom(Direction),
    normalize_parallel_batch_id(BatchID),
    MicrobatchID,
    StageID
  },
  case lists:member(NormalizedGrant, Grants) of
    true ->
      ok;
    false ->
      ets:update_element(GenWorkerEts, parallel_scheduler_grants, {?ETS_KEYVAL_VAL_IDX, Grants ++ [NormalizedGrant]})
  end.

maybe_consume_parallel_scheduler_grant(GenWorkerEts, Direction, BatchID, MicrobatchID, StageID) ->
  Mode = normalize_parallel_mode_atom(
           ets:lookup_element(GenWorkerEts, parallel_mode, ?ETS_KEYVAL_VAL_IDX)
         ),
  case is_pipeline_mode_atom(Mode) of
    false ->
      {ok, normalize_parallel_batch_id(BatchID)};
    true ->
      HasSuperAuthority = ets:lookup_element(GenWorkerEts, parallel_super_authority, ?ETS_KEYVAL_VAL_IDX),
      case HasSuperAuthority of
        false ->
          {error, super_authority_disabled};
        true ->
          ExpectedGrant = {
            normalize_parallel_direction_atom(Direction),
            expected_parallel_scheduler_batch_id(Mode, Direction, BatchID, StageID),
            MicrobatchID,
            StageID
          },
          Grants = ets:lookup_element(GenWorkerEts, parallel_scheduler_grants, ?ETS_KEYVAL_VAL_IDX),
          case pop_matching_scheduler_grant(ExpectedGrant, Grants, []) of
            {ok, {_, MatchedBatchID, _, _}, RemainingGrants, _MatchedAtHead} ->
              ets:update_element(GenWorkerEts, parallel_scheduler_grants, {?ETS_KEYVAL_VAL_IDX, RemainingGrants}),
              {ok, MatchedBatchID};
            not_found ->
              {error, no_scheduler_grant}
          end
      end
  end.

can_emit_parallel_event_now(GenWorkerEts, Direction, BatchID, MicrobatchID, StageID) ->
  Mode = normalize_parallel_mode_atom(
           ets:lookup_element(GenWorkerEts, parallel_mode, ?ETS_KEYVAL_VAL_IDX)
         ),
  case is_pipeline_mode_atom(Mode) of
    false ->
      ready;
    true ->
      HasSuperAuthority = ets:lookup_element(GenWorkerEts, parallel_super_authority, ?ETS_KEYVAL_VAL_IDX),
      case HasSuperAuthority of
        false ->
          {error, super_authority_disabled};
        true ->
          ExpectedGrant = {
            normalize_parallel_direction_atom(Direction),
            expected_parallel_scheduler_batch_id(Mode, Direction, BatchID, StageID),
            MicrobatchID,
            StageID
          },
          Grants = ets:lookup_element(GenWorkerEts, parallel_scheduler_grants, ?ETS_KEYVAL_VAL_IDX),
          case has_matching_scheduler_grant(ExpectedGrant, Grants) of
            true ->
              ready;
            false ->
              wait_for_grant
          end
      end
  end.

queue_parallel_backward_event(GenWorkerEts, WorkerName, BatchID, MicrobatchID, StageID, TrainTime) ->
  PendingEvents = ets:lookup_element(GenWorkerEts, parallel_pending_backward_events, ?ETS_KEYVAL_VAL_IDX),
  Event = {WorkerName, BatchID, MicrobatchID, StageID, TrainTime},
  case lists:any(
         fun({_W, PendingBatchID, PendingMicrobatchID, PendingStageID, _T}) ->
           PendingBatchID =:= BatchID andalso
             PendingMicrobatchID =:= MicrobatchID andalso
             PendingStageID =:= StageID
         end,
         PendingEvents
       ) of
    true ->
      ok;
    false ->
      ets:update_element(
        GenWorkerEts,
        parallel_pending_backward_events,
        {?ETS_KEYVAL_VAL_IDX, PendingEvents ++ [Event]}
      )
  end.

maybe_dispatch_pending_parallel_backward_events(GenWorkerEts) ->
  Grants = ets:lookup_element(GenWorkerEts, parallel_scheduler_grants, ?ETS_KEYVAL_VAL_IDX),
  PendingEvents = ets:lookup_element(GenWorkerEts, parallel_pending_backward_events, ?ETS_KEYVAL_VAL_IDX),
  maybe_dispatch_pending_parallel_backward_events_loop(GenWorkerEts, Grants, PendingEvents).

maybe_dispatch_pending_parallel_backward_events_loop(_GenWorkerEts, [], _PendingEvents) ->
  ok;
maybe_dispatch_pending_parallel_backward_events_loop(_GenWorkerEts, _Grants, []) ->
  ok;
maybe_dispatch_pending_parallel_backward_events_loop(
  GenWorkerEts,
  Grants,
  PendingEvents
) ->
  case find_dispatchable_pending_backward_event(Grants, PendingEvents) of
    not_found ->
      ok;
    {ok, {WorkerName, EventBatchID, MicrobatchID, StageID, TrainTime}, RemainingPendingEvents} ->
      case emit_parallel_event(WorkerName, backward, EventBatchID, MicrobatchID, StageID, TrainTime) of
        {ok, _EmittedBatchID} ->
          ets:update_element(
            GenWorkerEts,
            parallel_pending_backward_events,
            {?ETS_KEYVAL_VAL_IDX, RemainingPendingEvents}
          ),
          UpdatedGrants = ets:lookup_element(GenWorkerEts, parallel_scheduler_grants, ?ETS_KEYVAL_VAL_IDX),
          maybe_dispatch_pending_parallel_backward_events_loop(
            GenWorkerEts,
            UpdatedGrants,
            RemainingPendingEvents
          );
        {error, no_scheduler_grant} ->
          ok;
        {error, EmitReason} ->
          {abort, {pending_backward_event_emit_rejected, WorkerName, EventBatchID, MicrobatchID, StageID, EmitReason}}
      end
  end.

find_dispatchable_pending_backward_event([], _PendingEvents) ->
  not_found;
find_dispatchable_pending_backward_event([Grant | Rest], PendingEvents) ->
  case normalize_parallel_scheduler_grant(Grant) of
    {ok, {backward, GrantBatchID, MicrobatchID, StageID}} ->
      case pop_pending_parallel_backward_event(PendingEvents, GrantBatchID, MicrobatchID, StageID, []) of
        not_found ->
          find_dispatchable_pending_backward_event(Rest, PendingEvents);
        {ok, Event, RemainingPendingEvents} ->
          {ok, Event, RemainingPendingEvents}
      end;
    _ ->
      find_dispatchable_pending_backward_event(Rest, PendingEvents)
  end.

pop_matching_scheduler_grant(_ExpectedGrant, [], _Acc) ->
  not_found;
pop_matching_scheduler_grant(ExpectedGrant, [Grant | Rest], Acc) ->
  case normalize_parallel_scheduler_grant(Grant) of
    {ok, NormalizedGrant} ->
      case scheduler_grant_matches(ExpectedGrant, NormalizedGrant) of
        true ->
          {ok, NormalizedGrant, lists:reverse(Acc) ++ Rest, Acc =:= []};
        false ->
          pop_matching_scheduler_grant(ExpectedGrant, Rest, [Grant | Acc])
      end;
    {error, _Reason} ->
      pop_matching_scheduler_grant(ExpectedGrant, Rest, Acc)
  end.

has_matching_scheduler_grant(_ExpectedGrant, []) ->
  false;
has_matching_scheduler_grant(ExpectedGrant, [Grant | Rest]) ->
  case normalize_parallel_scheduler_grant(Grant) of
    {ok, NormalizedGrant} ->
      scheduler_grant_matches(ExpectedGrant, NormalizedGrant) orelse
        has_matching_scheduler_grant(ExpectedGrant, Rest);
    {error, _Reason} ->
      has_matching_scheduler_grant(ExpectedGrant, Rest)
  end.

pop_pending_parallel_backward_event([], _BatchID, _MicrobatchID, _StageID, _Acc) ->
  not_found;
pop_pending_parallel_backward_event(
  [Event = {_WorkerName, BatchID, MicrobatchID, StageID, _TrainTime} | Rest],
  BatchID,
  MicrobatchID,
  StageID,
  Acc
) ->
  {ok, Event, lists:reverse(Acc) ++ Rest};
pop_pending_parallel_backward_event(
  [Event = {_WorkerName, _EventBatchID, MicrobatchID, StageID, _TrainTime} | Rest],
  any,
  MicrobatchID,
  StageID,
  Acc
) ->
  {ok, Event, lists:reverse(Acc) ++ Rest};
pop_pending_parallel_backward_event([Event | Rest], BatchID, MicrobatchID, StageID, Acc) ->
  pop_pending_parallel_backward_event(Rest, BatchID, MicrobatchID, StageID, [Event | Acc]).

normalize_parallel_batch_id(any) ->
  any;
normalize_parallel_batch_id(BatchID) when is_integer(BatchID) ->
  BatchID;
normalize_parallel_batch_id(BatchID) when is_binary(BatchID) ->
  normalize_parallel_batch_id(binary_to_list(BatchID));
normalize_parallel_batch_id(BatchID) when is_list(BatchID) ->
  try list_to_integer(string:trim(BatchID)) of
    ParsedBatchID ->
      ParsedBatchID
  catch
    _:_ ->
      any
  end;
normalize_parallel_batch_id(_) ->
  any.

normalize_parallel_scheduler_grant({Direction, BatchID, MicrobatchID, StageID}) ->
  {ok, {normalize_parallel_direction_atom(Direction), normalize_parallel_batch_id(BatchID), MicrobatchID, StageID}};
normalize_parallel_scheduler_grant({Direction, MicrobatchID, StageID}) ->
  {ok, {normalize_parallel_direction_atom(Direction), any, MicrobatchID, StageID}};
normalize_parallel_scheduler_grant(Unexpected) ->
  {error, {invalid_grant_shape, Unexpected}}.

scheduler_grant_matches({ExpectedDirection, ExpectedBatchID, ExpectedMicrobatchID, ExpectedStageID},
                        {GrantDirection, GrantBatchID, GrantMicrobatchID, GrantStageID}) ->
  ExpectedDirection =:= GrantDirection andalso
    batch_id_matches(ExpectedBatchID, GrantBatchID) andalso
    ExpectedMicrobatchID =:= GrantMicrobatchID andalso
    ExpectedStageID =:= GrantStageID.

expected_parallel_scheduler_batch_id(Mode, Direction, BatchID, StageID) ->
  NormalizedBatchID = normalize_parallel_batch_id(BatchID),
  NormalizedDirection = normalize_parallel_direction_atom(Direction),
  case {Mode, NormalizedDirection, StageID} of
    {pipeline, forward, 0} ->
      %% Pure pipeline stage-0 can decouple source ingress batch ids from
      %% scheduler batch ids and then rewrite event/payload/runtime tags to the
      %% consumed concrete grant batch id.
      %%
      %% pipeline_tensor must stay strict-batch to preserve TP collective token
      %% alignment across same-stage ranks.
      any;
    _ ->
      NormalizedBatchID
  end.

batch_id_matches(any, _GrantBatchID) ->
  true;

batch_id_matches(_ExpectedBatchID, any) ->
  %% Keep legacy/bootstrapping compatibility: super node can seed the first
  %% batch with grant batch=any, then resolve to a concrete batch after event 1.
  true;
batch_id_matches(ExpectedBatchID, GrantBatchID) ->
  ExpectedBatchID =:= GrantBatchID.

normalize_parallel_direction_atom(Direction) when is_atom(Direction) ->
  Direction;
normalize_parallel_direction_atom(Direction) when is_binary(Direction) ->
  normalize_parallel_direction_atom(binary_to_list(Direction));
normalize_parallel_direction_atom(Direction) when is_list(Direction) ->
  case string:lowercase(string:trim(Direction)) of
    "backward" -> backward;
    _ -> forward
  end;
normalize_parallel_direction_atom(_) ->
  forward.

notify_worker_parallel_abort(GenWorkerEts, Reason) ->
  WorkerName = ets:lookup_element(GenWorkerEts, worker_name, ?ETS_KEYVAL_VAL_IDX),
  ?LOG_ERROR("Worker ~p sending parallel_abort reason=~p", [WorkerName, Reason]),
  gen_statem:cast(get(client_pid), {worker_parallel_abort, WorkerName, Reason}).

should_preserve_parallel_loss_context(Mode, ParallelExecution) ->
  case is_pipeline_mode_atom(Mode) of
    false -> false;
    true ->
      NumMicrobatches = get_parallel_execution_int(ParallelExecution, [<<"numMicroBatches">>, numMicroBatches], 1),
      NumMicrobatches > 0
  end.

is_pipeline_mode_atom(pipeline) -> true;
is_pipeline_mode_atom(pipeline_tensor) -> true;
is_pipeline_mode_atom(_) -> false.

normalize_parallel_mode_atom(Mode) when is_atom(Mode) ->
  normalize_parallel_mode_atom(atom_to_list(Mode));
normalize_parallel_mode_atom(Mode) when is_binary(Mode) ->
  normalize_parallel_mode_atom(binary_to_list(Mode));
normalize_parallel_mode_atom(Mode) when is_list(Mode) ->
  case string:lowercase(string:trim(Mode)) of
    "pipeline" -> pipeline;
    "tensor" -> tensor;
    "pipeline_tensor" -> pipeline_tensor;
    _ -> legacy
  end;
normalize_parallel_mode_atom(_) ->
  legacy.

reset_parallel_runtime_for_idle(GenWorkerEts) ->
  reset_parallel_loss_context(GenWorkerEts),
  ets:update_element(GenWorkerEts, pipeline_nif_worker, {?ETS_KEYVAL_VAL_IDX, false}),
  ets:update_counter(GenWorkerEts, parallel_nif_epoch, 1),
  ets:update_element(GenWorkerEts, parallel_scheduler_grants, {?ETS_KEYVAL_VAL_IDX, []}),
  ets:update_element(GenWorkerEts, tp_collective_inbox_buffer, {?ETS_KEYVAL_VAL_IDX, []}),
  Mode = normalize_parallel_mode_atom(
           ets:lookup_element(GenWorkerEts, parallel_mode, ?ETS_KEYVAL_VAL_IDX)
         ),
  case Mode of
    legacy ->
      ets:update_element(GenWorkerEts, parallel_execution, {?ETS_KEYVAL_VAL_IDX, #{}}),
      set_worker_parallel_authority(GenWorkerEts, false);
    _ ->
      ok
  end.

reset_parallel_loss_context(GenWorkerEts) ->
  DeferredSamples = ets:lookup_element(GenWorkerEts, parallel_deferred_samples, ?ETS_KEYVAL_VAL_IDX),
  maybe_account_deferred_sample_drop(DeferredSamples),
  reset_parallel_batch_context(GenWorkerEts),
  ets:update_element(GenWorkerEts, parallel_deferred_samples, {?ETS_KEYVAL_VAL_IDX, []}).

reset_parallel_batch_context(GenWorkerEts) ->
  ets:update_element(GenWorkerEts, parallel_pending_losses, {?ETS_KEYVAL_VAL_IDX, 0}),
  ets:update_element(GenWorkerEts, parallel_total_microbatches, {?ETS_KEYVAL_VAL_IDX, 0}),
  ets:update_element(GenWorkerEts, parallel_microbatches_skipped, {?ETS_KEYVAL_VAL_IDX, 0}),
  ets:update_element(GenWorkerEts, parallel_loss_acc, {?ETS_KEYVAL_VAL_IDX, undefined}),
  ets:update_element(GenWorkerEts, parallel_time_acc, {?ETS_KEYVAL_VAL_IDX, 0.0}),
  ets:update_element(GenWorkerEts, parallel_microbatch_queue, {?ETS_KEYVAL_VAL_IDX, []}),
  ets:update_element(GenWorkerEts, parallel_pending_backward_events, {?ETS_KEYVAL_VAL_IDX, []}),
  ets:update_element(GenWorkerEts, parallel_pipeline_forward_buffer, {?ETS_KEYVAL_VAL_IDX, []}),
  ets:update_element(GenWorkerEts, parallel_pipeline_backward_buffer, {?ETS_KEYVAL_VAL_IDX, []}),
  ets:update_element(GenWorkerEts, parallel_pipeline_predict_buffer, {?ETS_KEYVAL_VAL_IDX, []}),
  ets:update_element(GenWorkerEts, parallel_active_batch_ctx, {?ETS_KEYVAL_VAL_IDX, undefined}).

queue_deferred_parallel_sample(GenWorkerEts, SampleTuple) ->
  DeferredSamples = ets:lookup_element(GenWorkerEts, parallel_deferred_samples, ?ETS_KEYVAL_VAL_IDX),
  QueueLen = length(DeferredSamples),
  CongestionEnabled = ets:lookup_element(GenWorkerEts, congestion_drop_enabled, ?ETS_KEYVAL_VAL_IDX),
  SoftLimit = ets:lookup_element(GenWorkerEts, deferred_sample_soft_limit, ?ETS_KEYVAL_VAL_IDX),
  MaxDeferred = 256,
  case CongestionEnabled of
    true ->
      case QueueLen >= SoftLimit of
        true ->
          WorkerName = ets:lookup_element(GenWorkerEts, worker_name, ?ETS_KEYVAL_VAL_IDX),
          ?LOG_WARNING("Worker ~p congestion drop: deferred queue ~p >= soft_limit ~p, dropping incoming sample", [WorkerName, QueueLen, SoftLimit]),
          increment_phase_drop_counter(current_worker_phase(), 1),
          ets:update_counter(get(worker_stats_ets), congestion_drop_deferred_sample, 1),
          ets:update_element(GenWorkerEts, parallel_congestion_signaled, {?ETS_KEYVAL_VAL_IDX, true}),
          ets:update_counter(get(worker_stats_ets), congestion_signal_emitted, 1),
          ok;
        false ->
          case QueueLen < (SoftLimit div 2) of
            true ->
              ets:update_element(GenWorkerEts, parallel_congestion_signaled, {?ETS_KEYVAL_VAL_IDX, false});
            false ->
              ok
          end,
          ets:update_element(GenWorkerEts, parallel_deferred_samples, {?ETS_KEYVAL_VAL_IDX, DeferredSamples ++ [SampleTuple]})
      end;
    false ->
      case QueueLen >= MaxDeferred of
        true ->
          WorkerName = ets:lookup_element(GenWorkerEts, worker_name, ?ETS_KEYVAL_VAL_IDX),
          ?LOG_WARNING("Worker ~p deferred samples queue full (~p), dropping oldest sample", [WorkerName, MaxDeferred]),
          increment_phase_drop_counter(current_worker_phase(), 1),
          [_Oldest | Rest] = DeferredSamples,
          ets:update_element(GenWorkerEts, parallel_deferred_samples, {?ETS_KEYVAL_VAL_IDX, Rest ++ [SampleTuple]});
        false ->
          ets:update_element(GenWorkerEts, parallel_deferred_samples, {?ETS_KEYVAL_VAL_IDX, DeferredSamples ++ [SampleTuple]})
      end
  end.

pop_deferred_parallel_sample(GenWorkerEts) ->
  DeferredSamples = ets:lookup_element(GenWorkerEts, parallel_deferred_samples, ?ETS_KEYVAL_VAL_IDX),
  case DeferredSamples of
    [] ->
      empty;
    [SampleTuple | Rest] ->
      ets:update_element(GenWorkerEts, parallel_deferred_samples, {?ETS_KEYVAL_VAL_IDX, Rest}),
      {ok, SampleTuple}
  end.

maybe_pop_matching_deferred_sample_for_grants(GenWorkerEts) ->
  case has_active_parallel_runtime_work(GenWorkerEts) of
    true ->
      none;
    false ->
  Mode = normalize_parallel_mode_atom(
           ets:lookup_element(GenWorkerEts, parallel_mode, ?ETS_KEYVAL_VAL_IDX)
         ),
      case is_pipeline_mode_atom(Mode) of
        false ->
          none;
        true ->
          StageID = get_worker_pipeline_stage(GenWorkerEts),
          case StageID of
            0 ->
              Grants = ets:lookup_element(GenWorkerEts, parallel_scheduler_grants, ?ETS_KEYVAL_VAL_IDX),
              DeferredSamples = ets:lookup_element(GenWorkerEts, parallel_deferred_samples, ?ETS_KEYVAL_VAL_IDX),
              case pop_matching_deferred_sample_for_grants(Mode, Grants, StageID, DeferredSamples, []) of
                {ok, SampleTuple, RemainingSamples} ->
                  ets:update_element(GenWorkerEts, parallel_deferred_samples, {?ETS_KEYVAL_VAL_IDX, RemainingSamples}),
                  {ok, SampleTuple};
                none ->
                  none
              end;
            _ ->
              none
          end
      end
  end.

maybe_replay_grant_matched_deferred_sample_in_wait(GenWorkerEts, NextState, LastPhase, State) ->
  case maybe_pop_matching_deferred_sample_for_grants(GenWorkerEts) of
    {ok, SampleTuple} ->
      ?LOG_INFO(
        "Worker ~p replaying grant-matched deferred sample while leaving wait state",
        [ets:lookup_element(GenWorkerEts, worker_name, ?ETS_KEYVAL_VAL_IDX)]
      ),
      {next_state, normalize_parallel_next_state(NextState, LastPhase), State, [{next_event, cast, SampleTuple}]};
    none ->
      {keep_state, State}
  end.

has_active_parallel_runtime_work(GenWorkerEts) ->
  ActiveCtx = ets:lookup_element(GenWorkerEts, parallel_active_batch_ctx, ?ETS_KEYVAL_VAL_IDX),
  MicrobatchQueue = ets:lookup_element(GenWorkerEts, parallel_microbatch_queue, ?ETS_KEYVAL_VAL_IDX),
  ForwardBuffer = ets:lookup_element(GenWorkerEts, parallel_pipeline_forward_buffer, ?ETS_KEYVAL_VAL_IDX),
  BackwardBuffer = ets:lookup_element(GenWorkerEts, parallel_pipeline_backward_buffer, ?ETS_KEYVAL_VAL_IDX),
  PredictBuffer = ets:lookup_element(GenWorkerEts, parallel_pipeline_predict_buffer, ?ETS_KEYVAL_VAL_IDX),
  PendingBackwardEvents = ets:lookup_element(GenWorkerEts, parallel_pending_backward_events, ?ETS_KEYVAL_VAL_IDX),
  NifFlight = is_pipeline_nif_in_flight(GenWorkerEts),
  ActiveCtx =/= undefined orelse
    MicrobatchQueue =/= [] orelse
    ForwardBuffer =/= [] orelse
    BackwardBuffer =/= [] orelse
    PredictBuffer =/= [] orelse
    PendingBackwardEvents =/= [] orelse
    NifFlight.

pop_matching_deferred_sample_for_grants(_Mode, _Grants, _StageID, [], _Acc) ->
  none;
pop_matching_deferred_sample_for_grants(Mode, Grants, StageID, [SampleTuple | Rest], Acc) ->
  case sample_tuple_batch_id(SampleTuple) of
    undefined ->
      pop_matching_deferred_sample_for_grants(Mode, Grants, StageID, Rest, [SampleTuple | Acc]);
    SampleBatchID ->
      case has_matching_forward_grant_for_sample(Mode, Grants, StageID, SampleBatchID) of
        true ->
          {ok, SampleTuple, lists:reverse(Acc) ++ Rest};
        false ->
          pop_matching_deferred_sample_for_grants(Mode, Grants, StageID, Rest, [SampleTuple | Acc])
      end
  end.

sample_tuple_batch_id({sample, BatchID, _Payload}) ->
  normalize_parallel_batch_id(BatchID);
sample_tuple_batch_id({sample, _SourceName, BatchID, _Payload}) ->
  normalize_parallel_batch_id(BatchID);
sample_tuple_batch_id(_) ->
  undefined.

has_matching_forward_grant_for_sample(_Mode, [], _StageID, _SampleBatchID) ->
  false;
has_matching_forward_grant_for_sample(Mode, [{Direction, GrantBatchID, _MicrobatchID, GrantStageID} | Rest], StageID, SampleBatchID) ->
  case Direction =:= forward andalso GrantStageID =:= StageID andalso grant_batch_matches_sample(Mode, SampleBatchID, GrantBatchID) of
    true -> true;
    false -> has_matching_forward_grant_for_sample(Mode, Rest, StageID, SampleBatchID)
  end.

grant_batch_matches_sample(pipeline_tensor, SampleBatchID, GrantBatchID) ->
  normalize_parallel_batch_id(SampleBatchID) =:= normalize_parallel_batch_id(GrantBatchID);
grant_batch_matches_sample(_Mode, SampleBatchID, GrantBatchID) ->
  batch_id_matches(normalize_parallel_batch_id(SampleBatchID), normalize_parallel_batch_id(GrantBatchID)).

maybe_dispatch_deferred_parallel_sample(_GenWorkerEts, NextState)
when NextState =/= train, NextState =/= predict ->
  ok;
maybe_dispatch_deferred_parallel_sample(GenWorkerEts, _NextState) ->
  case pop_deferred_parallel_sample(GenWorkerEts) of
    {ok, SampleTuple} ->
      gen_statem:cast(self(), SampleTuple),
      ok;
    empty ->
      ok
  end.

normalize_parallel_next_state(train, _LastPhase) ->
  train;
normalize_parallel_next_state(predict, _LastPhase) ->
  predict;
normalize_parallel_next_state(_, LastPhase) ->
  case LastPhase of
    train -> train;
    predict -> predict;
    _ -> train
  end.

should_replay_parallel_sample_immediately(GenWorkerEts, NextState, BatchTuple) ->
  case NextState =:= train orelse NextState =:= predict of
    false ->
      false;
    true ->
      Mode = normalize_parallel_mode_atom(
               ets:lookup_element(GenWorkerEts, parallel_mode, ?ETS_KEYVAL_VAL_IDX)
             ),
      ActiveCtx = ets:lookup_element(GenWorkerEts, parallel_active_batch_ctx, ?ETS_KEYVAL_VAL_IDX),
      MicrobatchQueue = ets:lookup_element(GenWorkerEts, parallel_microbatch_queue, ?ETS_KEYVAL_VAL_IDX),
      ForwardBuffer = ets:lookup_element(GenWorkerEts, parallel_pipeline_forward_buffer, ?ETS_KEYVAL_VAL_IDX),
      BackwardBuffer = ets:lookup_element(GenWorkerEts, parallel_pipeline_backward_buffer, ?ETS_KEYVAL_VAL_IDX),
      PredictBuffer = ets:lookup_element(GenWorkerEts, parallel_pipeline_predict_buffer, ?ETS_KEYVAL_VAL_IDX),
      PendingBackwardEvents = ets:lookup_element(GenWorkerEts, parallel_pending_backward_events, ?ETS_KEYVAL_VAL_IDX),
      BaseReady = ActiveCtx =:= undefined andalso
        MicrobatchQueue =:= [] andalso
        ForwardBuffer =:= [] andalso
        BackwardBuffer =:= [] andalso
        PredictBuffer =:= [] andalso
        PendingBackwardEvents =:= [],
      case BaseReady of
        false ->
          false;
        true ->
          case is_pipeline_mode_atom(Mode) of
            false ->
              true;
            true ->
              StageID = get_worker_pipeline_stage(GenWorkerEts),
              SampleBatchID = sample_tuple_batch_id(BatchTuple),
              Grants = ets:lookup_element(GenWorkerEts, parallel_scheduler_grants, ?ETS_KEYVAL_VAL_IDX),
              StageID =:= 0 andalso
                SampleBatchID =/= undefined andalso
                has_matching_forward_grant_for_sample(Mode, Grants, StageID, SampleBatchID)
          end
      end
  end.

count_deferred_samples(Samples) ->
  length([
    S
    || S <- Samples,
       is_tuple(S),
       tuple_size(S) >= 1,
       element(1, S) =:= sample
  ]).

maybe_account_deferred_sample_drop(DeferredSamples) ->
  DeferredCount = count_deferred_samples(DeferredSamples),
  case DeferredCount > 0 of
    true ->
      increment_phase_drop_counter(current_worker_phase(), DeferredCount);
    false ->
      ok
  end.

is_nonfatal_tp_batch_abort({tensor_collective_failed, _Reason}) ->
  true;
is_nonfatal_tp_batch_abort({tp_collective_timeout, _Reason}) ->
  true;
is_nonfatal_tp_batch_abort(_Reason) ->
  false.

handle_nonfatal_tp_batch_abort(
  GenWorkerEts,
  WorkerName,
  BatchID,
  SourceName,
  Phase,
  AbortReason
) ->
  StageID = get_worker_pipeline_stage(GenWorkerEts),
  NormalizedBatchID = normalize_parallel_batch_id(BatchID),
  MicrobatchIDs = skip_microbatch_ids_for_local_tp_abort(GenWorkerEts, AbortReason),
  ?LOG_WARNING(
    "Worker ~p non-fatal TP abort: phase=~p batch=~p source=~p stage=~p reason=~p; emitting local skip events microbatches=~p and continuing",
    [WorkerName, Phase, NormalizedBatchID, SourceName, StageID, AbortReason, MicrobatchIDs]
  ),
  increment_phase_drop_counter(Phase, 1),
  lists:foreach(
    fun(MicrobatchID) ->
      SkipMeta = #{
        reason => {tp_collective_failed, AbortReason},
        event_id => {tp_collective_failed, WorkerName, NormalizedBatchID, MicrobatchID, StageID}
      },
      gen_statem:cast(
        get(client_pid),
        {parallel_skip_event, WorkerName, forward, NormalizedBatchID, MicrobatchID, StageID, SkipMeta}
      )
    end,
    MicrobatchIDs
  ),
  ets:update_element(GenWorkerEts, tp_collective_inbox_buffer, {?ETS_KEYVAL_VAL_IDX, []}),
  ok.

skip_microbatch_ids_for_local_tp_abort(GenWorkerEts, AbortReason) ->
  case extract_tp_abort_microbatch_id(AbortReason) of
    MicrobatchID when is_integer(MicrobatchID), MicrobatchID >= 0 ->
      [MicrobatchID];
    _ ->
      ActiveCtx = ets:lookup_element(GenWorkerEts, parallel_active_batch_ctx, ?ETS_KEYVAL_VAL_IDX),
      NumMicrobatches =
        case ActiveCtx of
          Ctx when is_map(Ctx) ->
            erlang:max(
              0,
              normalize_tp_int(
                maps:get(total_microbatches, Ctx, ets:lookup_element(GenWorkerEts, parallel_total_microbatches, ?ETS_KEYVAL_VAL_IDX)),
                0
              )
            );
          _ ->
            erlang:max(
              0,
              normalize_tp_int(ets:lookup_element(GenWorkerEts, parallel_total_microbatches, ?ETS_KEYVAL_VAL_IDX), 0)
            )
        end,
      case NumMicrobatches > 0 of
        true -> lists:seq(0, NumMicrobatches - 1);
        false -> [0]
      end
  end.

extract_tp_abort_microbatch_id({tensor_collective_failed, Reason}) ->
  extract_tp_abort_microbatch_id(Reason);
extract_tp_abort_microbatch_id({tp_collective_timeout, Reason}) ->
  extract_tp_abort_microbatch_id(Reason);
extract_tp_abort_microbatch_id({collective_timeout, CollectiveToken, _MissingWorkers}) ->
  extract_tp_abort_microbatch_id(CollectiveToken);
extract_tp_abort_microbatch_id({tp_collective, _BatchID, MicrobatchID, _LayerIndex, _LayerName, _CollectiveMode})
when is_integer(MicrobatchID), MicrobatchID >= 0 ->
  MicrobatchID;
extract_tp_abort_microbatch_id(_Reason) ->
  undefined.

handle_unexpected_microbatch_loss(
  GenWorkerEts,
  WorkerName,
  BatchID,
  SourceName,
  MicrobatchID
) ->
  NormalizedBatchID = normalize_parallel_batch_id(BatchID),
  StageID = get_worker_pipeline_stage(GenWorkerEts),
  Phase = current_worker_phase(),
  ?LOG_WARNING(
    "Worker ~p unexpected microbatch loss treated as non-fatal skip: phase=~p batch=~p source=~p microbatch=~p stage=~p",
    [WorkerName, Phase, NormalizedBatchID, SourceName, MicrobatchID, StageID]
  ),
  increment_phase_drop_counter(Phase, 1),
  SkipMeta = #{
    reason => unexpected_microbatch_loss,
    event_id => {unexpected_microbatch_loss, WorkerName, NormalizedBatchID, MicrobatchID, StageID}
  },
  gen_statem:cast(
    get(client_pid),
    {parallel_skip_event, WorkerName, forward, NormalizedBatchID, MicrobatchID, StageID, SkipMeta}
  ),
  ok.

current_worker_phase() ->
  case get(phase) of
    training -> train;
    train -> train;
    prediction -> predict;
    predict -> predict;
    _ -> train
  end.

increment_phase_drop_counter(train, Value) when Value > 0 ->
  stats:increment_by_value(get(worker_stats_ets), batches_dropped_train, Value);
increment_phase_drop_counter(predict, Value) when Value > 0 ->
  stats:increment_by_value(get(worker_stats_ets), batches_dropped_predict, Value);
increment_phase_drop_counter(_Phase, _Value) ->
  ok.


get_backend_module() ->
  case get(nif_module) of
    undefined -> ets:lookup_element(get(generic_worker_ets), nif_module, ?ETS_KEYVAL_VAL_IDX);
    Module -> Module
  end.

pipeline_torch_backend_supported(GenWorkerEts) ->
  NifModule =
    case ets:lookup(GenWorkerEts, nif_module) of
      [{nif_module, Module}] -> Module;
      _ -> get_backend_module()
    end,
  NifModule =:= nerlTorchNIF andalso
    erlang:function_exported(NifModule, call_to_pipeline_stage0_forward, 4) andalso
    erlang:function_exported(NifModule, call_to_pipeline_stage_forward, 6) andalso
    erlang:function_exported(NifModule, call_to_pipeline_stage_last_forward_backward, 6) andalso
    erlang:function_exported(NifModule, call_to_pipeline_stage_backward, 4).

nif_call(Function, Args) when is_atom(Function), is_list(Args) ->
  Module = get_backend_module(),
  erlang:apply(Module, Function, Args).

%% Calls a pipeline NIF synchronously on the calling process thread and then
%% casts a pre-applied result to self so the existing pipeline_nif_result
%% handlers process post-NIF dispatch logic.
%%
%% PyTorch models are NOT thread-safe, so the NIF must run on the same Erlang
%% scheduler thread that owns the model state.
%%
%% An epoch guard ensures stale results from a previous phase are discarded.
%% The continuation itself is evaluated immediately to avoid mailbox-order
%% starvation when many source samples are queued ahead of pipeline_nif_result.
async_pipeline_nif(GenWorkerEts, Function, Args, UserContinuation) ->
  ets:update_element(GenWorkerEts, pipeline_nif_worker, {?ETS_KEYVAL_VAL_IDX, true}),
  SpawnEpoch = ets:lookup_element(GenWorkerEts, parallel_nif_epoch, ?ETS_KEYVAL_VAL_IDX),
  NifModule = get_backend_module(),
  SafeContinuation = fun(NifResult) ->
    CurrentEpoch = ets:lookup_element(GenWorkerEts, parallel_nif_epoch, ?ETS_KEYVAL_VAL_IDX),
    case SpawnEpoch =:= CurrentEpoch of
      false ->
        WorkerName = ets:lookup_element(GenWorkerEts, worker_name, ?ETS_KEYVAL_VAL_IDX),
        ?LOG_WARNING(
          "Worker ~p discarding stale pipeline NIF result spawn_epoch=~p current_epoch=~p",
          [WorkerName, SpawnEpoch, CurrentEpoch]
        ),
        stale;
      true ->
        UserContinuation(NifResult)
    end
  end,
  Result = try erlang:apply(NifModule, Function, Args)
  catch
    error:Err -> {nerlnif, error, Err}
  end,
  ContinuationResult = SafeContinuation(Result),
  %% Mark NIF flight complete immediately so newly-arriving grants can trigger
  %% dispatch even if the self-cast is delayed by queued source sample traffic.
  ets:update_element(GenWorkerEts, pipeline_nif_worker, {?ETS_KEYVAL_VAL_IDX, false}),
  AppliedContinuation = fun(_IgnoredNifResult) -> ContinuationResult end,
  gen_statem:cast(self(), {pipeline_nif_result, AppliedContinuation, already_applied}),
  processed.

is_pipeline_nif_in_flight(GenWorkerEts) ->
  ets:lookup_element(GenWorkerEts, pipeline_nif_worker, ?ETS_KEYVAL_VAL_IDX) =/= false.

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
