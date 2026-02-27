%%%-------------------------------------------------------------------
%%% @author Nerlnet
%%% @doc Super Node control-plane process for parallel orchestration.
%%%-------------------------------------------------------------------
-module(superNodeGenserver).

-behaviour(gen_server).

-include("../nerl_tools.hrl").

%% API
-export([start_link/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(super_node_state, {
  my_name,
  managed_clients = [],
  client_workers = #{},
  worker_to_client = #{},
  heartbeat_ms = 1000,
  max_inflight = 1,
  nerlnet_graph,
  my_router = {undefined, 0},
  last_heartbeat = #{},
  parallel_phase = none,
  parallel_mode = legacy,
  parallel_execution = #{},
  worker_parallel = #{},
  stage_workers = #{},
  stage_rr = #{},
  scheduler_trace = [],
  scheduler_cursor = 0,
  pending_grant = none,
  pending_grant_issued_ms = 0,
  grant_timeout_ms = 5000,
  parallel_active = false,
  phase_start_ms = 0,
  last_parallel_event = none,
  last_abort = {none, 0}
}).

-define(HEARTBEAT_MISS_FACTOR, 3).
-define(GRANT_TIMEOUT_FACTOR, 2).

start_link(Args = {MyName, _ManagedClients, _HeartbeatMs, _MaxInflight, _NerlnetGraph}) ->
  gen_server:start_link({local, MyName}, ?MODULE, Args, []).

init({MyName, ManagedClients, HeartbeatMs, MaxInflight, NerlnetGraph}) ->
  nerl_tools:setup_logger(?MODULE),
  inets:start(),
  GrantTimeoutMs = erlang:max(5000, HeartbeatMs * ?HEARTBEAT_MISS_FACTOR * ?GRANT_TIMEOUT_FACTOR),
  WorkerToClientMap = ets:lookup_element(nerlnet_data, workers, ?DATA_IDX),
  RouterProbeDestination = case ManagedClients of
                             [Client | _] -> Client;
                             [] -> ?MAIN_SERVER_ATOM
                           end,
  {RouterHost, RouterPort} = nerl_tools:getShortPath(MyName, RouterProbeDestination, NerlnetGraph),
  erlang:send_after(HeartbeatMs, self(), check_heartbeats),
  ?LOG_NOTICE("Super node ~p starts with ~p managed clients", [MyName, length(ManagedClients)]),
  {ok, #super_node_state{
    my_name = MyName,
    managed_clients = ManagedClients,
    worker_to_client = WorkerToClientMap,
    heartbeat_ms = HeartbeatMs,
    grant_timeout_ms = GrantTimeoutMs,
    max_inflight = MaxInflight,
    nerlnet_graph = NerlnetGraph,
    my_router = {RouterHost, RouterPort}
  }}.

handle_call(get_state, _From, State = #super_node_state{}) ->
  {reply, State, State};
handle_call(
  {parallel_phase_update, PhaseName, ParallelMode, ParallelExecution, WorkerParallelMap},
  _From,
  State = #super_node_state{}
) ->
  {Reply, UpdatedState} =
    apply_parallel_phase_update(PhaseName, ParallelMode, ParallelExecution, WorkerParallelMap, State),
  {reply, Reply, UpdatedState};
handle_call(_Request, _From, State = #super_node_state{}) ->
  {reply, ok, State}.

handle_cast({register_client, ClientName, Workers}, State = #super_node_state{
  managed_clients = ManagedClients,
  client_workers = ClientWorkers
}) ->
  case lists:member(ClientName, ManagedClients) of
    true ->
      ?LOG_INFO("Super node registered client ~p workers ~p", [ClientName, Workers]),
      {noreply, State#super_node_state{client_workers = maps:put(ClientName, Workers, ClientWorkers)}};
    false ->
      ?LOG_WARNING("Client ~p is not managed by this super node", [ClientName]),
      {noreply, State}
  end;

handle_cast({super_heartbeat, ClientName, TsMs}, State = #super_node_state{
  last_heartbeat = LastHeartbeat
}) ->
  ?LOG_INFO("Super node heartbeat received from ~p ts_ms=~p", [ClientName, TsMs]),
  UpdatedHeartbeats = maps:put(ClientName, TsMs, LastHeartbeat),
  {noreply, State#super_node_state{last_heartbeat = UpdatedHeartbeats}};

handle_cast({parallel_worker_message, FromWorker, ToWorker, Data}, State = #super_node_state{
  worker_to_client = WorkerToClientMap,
  my_router = {RouterHost, RouterPort}
}) ->
  ?LOG_INFO("Super node routing parallel worker message from ~p to ~p", [FromWorker, ToWorker]),
  FinalState =
    case maps:get(ToWorker, WorkerToClientMap, undefined) of
      undefined ->
        maybe_notify_parallel_abort(State, {unknown_target_worker, ToWorker, FromWorker});
      DestClient ->
        MessageBody = {parallel_deliver, FromWorker, ToWorker, Data},
        try
          nerl_tools:http_router_request(
            RouterHost,
            RouterPort,
            [DestClient],
            atom_to_list(parallelDeliver),
            MessageBody
          ),
          State
        catch
          Err:Reason ->
            maybe_notify_parallel_abort(
              State,
              {route_failed, DestClient, ToWorker, {Err, Reason}}
            )
        end
    end,
  {noreply, FinalState};

handle_cast(
  {parallel_event, FromWorker, Direction, BatchID, MicrobatchID, StageID, Meta},
  State = #super_node_state{}
) ->
  EventID = parallel_event_id(FromWorker, Direction, BatchID, MicrobatchID, StageID),
  ?LOG_INFO(
    "Super node received parallel event worker=~p direction=~p batch=~p microbatch=~p stage=~p event_id=~p meta=~p",
    [FromWorker, Direction, BatchID, MicrobatchID, StageID, EventID, Meta]
  ),
  EventTsMs = erlang:system_time(millisecond),
  StateWithLastEvent = State#super_node_state{
    last_parallel_event = #{
      event_id => EventID,
      worker => FromWorker,
      direction => Direction,
      batch_id => BatchID,
      microbatch_id => MicrobatchID,
      stage_id => StageID,
      at_ms => EventTsMs
    }
  },
  EventPayload = {parallel_event, FromWorker, Direction, MicrobatchID, StageID, {BatchID, Meta}},
  case maybe_advance_scheduler(EventPayload, StateWithLastEvent) of
    {abort, AbortReason, UpdatedState} ->
      {noreply, maybe_notify_parallel_abort(UpdatedState, AbortReason)};
    {ok, UpdatedState} ->
      {noreply, UpdatedState}
  end;

handle_cast(
  {parallel_phase_update, PhaseName, ParallelMode, ParallelExecution, WorkerParallelMap},
  State = #super_node_state{}
) ->
  {_, UpdatedState} =
    apply_parallel_phase_update(PhaseName, ParallelMode, ParallelExecution, WorkerParallelMap, State),
  {noreply, UpdatedState};

handle_cast(_Request, State = #super_node_state{}) ->
  {noreply, State}.

handle_info(check_heartbeats, State = #super_node_state{
  managed_clients = ManagedClients,
  last_heartbeat = LastHeartbeat,
  heartbeat_ms = HeartbeatMs,
  phase_start_ms = PhaseStartMs,
  parallel_active = ParallelActive
}) ->
  NowMs = erlang:system_time(millisecond),
  TimeoutMs = HeartbeatMs * ?HEARTBEAT_MISS_FACTOR,
  UpdatedState =
    case ParallelActive of
      false ->
        State;
      true ->
        lists:foldl(
          fun(ClientName, AccState) ->
            maybe_check_client_heartbeat(
              ClientName,
              LastHeartbeat,
              NowMs,
              TimeoutMs,
              PhaseStartMs,
              AccState
            )
          end,
          State,
            ManagedClients
        )
    end,
  StateAfterGrantTimeoutCheck = maybe_check_pending_grant_timeout(UpdatedState, NowMs),
  erlang:send_after(HeartbeatMs, self(), check_heartbeats),
  {noreply, StateAfterGrantTimeoutCheck};

handle_info(_Info, State = #super_node_state{}) ->
  {noreply, State}.

terminate(_Reason, _State = #super_node_state{}) ->
  ok.

code_change(_OldVsn, State = #super_node_state{}, _Extra) ->
  {ok, State}.

notify_parallel_abort(#super_node_state{
  my_name = MyName,
  my_router = {RouterHost, RouterPort}
}, Reason) ->
  MessageBody = {MyName, Reason},
  try
    nerl_tools:http_router_request(RouterHost, RouterPort, [?MAIN_SERVER_ATOM], atom_to_list(parallelAbort), MessageBody)
  catch
    _:_ -> ok
  end.

is_parallel_mode_active(legacy) -> false;
is_parallel_mode_active(_) -> true.

maybe_check_client_heartbeat(
  ClientName,
  LastHeartbeatMap,
  NowMs,
  TimeoutMs,
  PhaseStartMs,
  State
) ->
  LastTs = maps:get(ClientName, LastHeartbeatMap, 0),
  case LastTs >= PhaseStartMs of
    false ->
      case (NowMs - PhaseStartMs) > TimeoutMs of
        true -> maybe_notify_parallel_abort(State, {missing_heartbeat, ClientName});
        false -> State
      end;
    true ->
      case (NowMs - LastTs) > TimeoutMs of
        true -> maybe_notify_parallel_abort(State, {heartbeat_timeout, ClientName, NowMs - LastTs});
        false -> State
      end
  end.

maybe_check_pending_grant_timeout(
  State = #super_node_state{
    parallel_active = ParallelActive,
    pending_grant = PendingGrant,
    pending_grant_issued_ms = PendingGrantIssuedMs,
    grant_timeout_ms = GrantTimeoutMs,
    last_parallel_event = LastParallelEvent
  },
  NowMs
) ->
  case {ParallelActive, PendingGrant, PendingGrantIssuedMs > 0} of
    {true, Grant, true} when Grant =/= none ->
      WaitMs = NowMs - PendingGrantIssuedMs,
      case WaitMs > GrantTimeoutMs of
        false ->
          State;
        true ->
          PendingGrantSummary = pending_grant_summary(PendingGrant),
          TimeoutReason = {
            scheduler_grant_timeout,
            PendingGrantSummary,
            GrantTimeoutMs,
            LastParallelEvent
          },
          ?LOG_WARNING(
            "Super node grant timeout after ~p ms (threshold=~p) pending=~p last_event=~p",
            [WaitMs, GrantTimeoutMs, PendingGrantSummary, LastParallelEvent]
          ),
          maybe_notify_parallel_abort(State, TimeoutReason)
      end;
    _ ->
      State
  end.

pending_grant_summary(PendingGrant) ->
  case normalize_pending_grant(PendingGrant) of
    {ok, Direction, MicrobatchId, StageId, GrantedWorkers, AckedWorkers, EventID} ->
      #{
        direction => Direction,
        microbatch_id => MicrobatchId,
        stage_id => StageId,
        workers => GrantedWorkers,
        acked_workers => AckedWorkers,
        event_id => EventID
      };
    {error, _Reason} ->
      PendingGrant
  end.

maybe_notify_parallel_abort(State = #super_node_state{
  last_abort = {LastReason, LastAbortTs},
  heartbeat_ms = HeartbeatMs
}, Reason) ->
  NowMs = erlang:system_time(millisecond),
  SuppressWindow = erlang:max(200, HeartbeatMs),
  case (Reason =:= LastReason) andalso ((NowMs - LastAbortTs) < SuppressWindow) of
    true ->
      State;
    false ->
      notify_parallel_abort(State, Reason),
      State#super_node_state{last_abort = {Reason, NowMs}}
  end.

normalize_parallel_mode(Mode) when is_atom(Mode) ->
  normalize_parallel_mode(atom_to_list(Mode));
normalize_parallel_mode(Mode) when is_binary(Mode) ->
  normalize_parallel_mode(binary_to_list(Mode));
normalize_parallel_mode(Mode) when is_list(Mode) ->
  case string:lowercase(string:trim(Mode)) of
    "pipeline" -> pipeline;
    "tensor" -> tensor;
    "pipeline_tensor" -> pipeline_tensor;
    _ -> legacy
  end;
normalize_parallel_mode(_) ->
  legacy.

apply_parallel_phase_update(
  PhaseName,
  ParallelMode,
  ParallelExecution,
  WorkerParallelMap,
  State = #super_node_state{
    my_name = MyName,
    managed_clients = ManagedClients,
    worker_to_client = WorkerToClientMap
  }
) ->
  NormalizedMode = normalize_parallel_mode(ParallelMode),
  SchedulerTrace = build_scheduler_trace(PhaseName, NormalizedMode, ParallelExecution, WorkerParallelMap),
  ParallelActive = is_parallel_mode_active(NormalizedMode),
  PhaseStartMs =
    case ParallelActive of
      true -> erlang:system_time(millisecond);
      false -> 0
    end,
  StageWorkers = build_stage_workers(WorkerParallelMap, ManagedClients, WorkerToClientMap),
  ?LOG_INFO(
    "Super node ~p updated parallel phase ~p mode=~p trace_length=~p active=~p",
    [MyName, PhaseName, NormalizedMode, length(SchedulerTrace), ParallelActive]
  ),
  ?LOG_INFO(
    "Super node ~p phase stage-worker map: ~p",
    [MyName, StageWorkers]
  ),
  StateAfterPhaseUpdate = State#super_node_state{
    parallel_phase = PhaseName,
    parallel_mode = NormalizedMode,
    parallel_execution = ParallelExecution,
    worker_parallel = WorkerParallelMap,
    stage_workers = StageWorkers,
    stage_rr = #{},
    scheduler_trace = SchedulerTrace,
    scheduler_cursor = 0,
    pending_grant = none,
    pending_grant_issued_ms = 0,
    parallel_active = ParallelActive,
    phase_start_ms = PhaseStartMs,
    last_parallel_event = none
  },
  StateAfterConfig = push_parallel_config_to_managed_clients(StateAfterPhaseUpdate),
  case maybe_send_next_scheduler_grant(StateAfterConfig) of
    {ok, StateAfterGrant} ->
      {ok, StateAfterGrant};
    {abort, AbortReason, StateOnError} ->
      {ok, maybe_notify_parallel_abort(StateOnError, AbortReason)}
  end.

build_stage_workers(WorkerParallelMap, ManagedClients, WorkerToClientMap) ->
  maps:fold(
    fun(WorkerName, WorkerCfg, AccMap) ->
      case is_pipeline_worker_cfg(WorkerCfg) of
        false ->
          AccMap;
        true ->
          ClientName = maps:get(WorkerName, WorkerToClientMap, undefined),
          case lists:member(ClientName, ManagedClients) of
            false ->
              AccMap;
            true ->
              Stage = normalize_non_negative_int(maps:get(pipeline_stage, WorkerCfg, 0), 0),
              CurrentWorkers = maps:get(Stage, AccMap, []),
              maps:put(Stage, lists:usort([WorkerName | CurrentWorkers]), AccMap)
          end
      end
    end,
    #{},
    WorkerParallelMap
  ).

is_pipeline_worker_cfg(WorkerCfg) when is_map(WorkerCfg) ->
  maps:is_key(pipeline_stage, WorkerCfg) orelse maps:is_key(pipeline_world_size, WorkerCfg);
is_pipeline_worker_cfg(_) ->
  false.

push_parallel_config_to_managed_clients(
  State = #super_node_state{
    managed_clients = ManagedClients,
    parallel_mode = ParallelMode,
    parallel_execution = ParallelExecution
  }
) ->
  lists:foldl(
    fun(ClientName, AccState) ->
      Command = {parallel_super_command, configure_parallel, ParallelMode, ParallelExecution},
      ?LOG_INFO(
        "Super node pushing parallel config to client ~p mode=~p",
        [ClientName, ParallelMode]
      ),
      case send_super_command_to_client(AccState, ClientName, Command) of
        ok ->
          AccState;
        {error, RouteReason} ->
          maybe_notify_parallel_abort(
            AccState,
            {parallel_super_config_route_failed, ClientName, RouteReason}
          )
      end
    end,
    State,
    ManagedClients
  ).

send_super_command_to_client(
  #super_node_state{my_router = {RouterHost, RouterPort}},
  ClientName,
  Command
) ->
  try
    nerl_tools:http_router_request(
      RouterHost,
      RouterPort,
      [ClientName],
      atom_to_list(parallelSuperCommand),
      Command
    ),
    ok
  catch
    Err:Reason ->
      {error, {Err, Reason}}
  end.

maybe_send_next_scheduler_grant(
  State = #super_node_state{
    parallel_active = ParallelActive,
    scheduler_trace = Trace,
    pending_grant = PendingGrant
  }
) ->
  case {ParallelActive, Trace, PendingGrant} of
    {false, _AnyTrace, _AnyGrant} ->
      {ok, State};
    {_True, [], _AnyGrant} ->
      {ok, State};
    {_True, _NonEmptyTrace, none} ->
      issue_next_scheduler_grant(State);
    {_True, _NonEmptyTrace, _ExistingGrant} ->
      {ok, State}
  end.

issue_next_scheduler_grant(
  State = #super_node_state{
    scheduler_trace = Trace,
    scheduler_cursor = Cursor,
    parallel_mode = ParallelMode,
    stage_workers = StageWorkers,
    stage_rr = StageRoundRobin
  }
) ->
  TraceLen = length(Trace),
  case TraceLen of
    0 ->
      {ok, State};
    _ ->
      EffectiveState =
        case Cursor >= TraceLen of
          true ->
            State#super_node_state{
              scheduler_cursor = 0,
              pending_grant = none,
              pending_grant_issued_ms = 0
            };
          false ->
            State
        end,
      EffectiveCursor = EffectiveState#super_node_state.scheduler_cursor,
      {Direction, MicrobatchId, StageId} = lists:nth(EffectiveCursor + 1, Trace),
      case choose_stage_workers(ParallelMode, StageId, StageWorkers, StageRoundRobin) of
        {error, Reason} ->
          {abort, Reason, EffectiveState};
        {ok, TargetWorkers, UpdatedStageRoundRobin} ->
          EventID = grant_event_id(Direction, MicrobatchId, StageId, TargetWorkers),
          GrantIssuedMs = erlang:system_time(millisecond),
          ?LOG_INFO(
            "Super node issuing scheduler grant direction=~p microbatch=~p stage=~p targets=~p cursor=~p event_id=~p",
            [Direction, MicrobatchId, StageId, TargetWorkers, EffectiveCursor, EventID]
          ),
          case send_scheduler_grants(EffectiveState, TargetWorkers, Direction, MicrobatchId, StageId) of
            ok ->
              {ok, EffectiveState#super_node_state{
                stage_rr = UpdatedStageRoundRobin,
                pending_grant = #{
                  direction => Direction,
                  microbatch_id => MicrobatchId,
                  stage_id => StageId,
                  workers => TargetWorkers,
                  acked_workers => [],
                  event_id => EventID
                },
                pending_grant_issued_ms = GrantIssuedMs
              }};
            {error, RouteReason} ->
              {abort,
               {scheduler_grant_route_failed, TargetWorkers, {Direction, MicrobatchId, StageId}, RouteReason},
               EffectiveState}
          end
      end
  end.

choose_stage_workers(ParallelMode, StageId, StageWorkers, StageRoundRobin) ->
  Workers = maps:get(StageId, StageWorkers, []),
  case Workers of
    [] ->
      {error, {missing_stage_worker, StageId}};
    _ ->
      case ParallelMode of
        pipeline_tensor ->
          {ok, lists:sort(Workers), StageRoundRobin};
        _ ->
          Cursor = maps:get(StageId, StageRoundRobin, 0),
          WorkerPos = (Cursor rem length(Workers)) + 1,
          WorkerName = lists:nth(WorkerPos, Workers),
          UpdatedRoundRobin = maps:put(StageId, Cursor + 1, StageRoundRobin),
          {ok, [WorkerName], UpdatedRoundRobin}
      end
  end.

send_scheduler_grants(
  _State,
  [],
  _Direction,
  _MicrobatchId,
  _StageId
) ->
  ok;
send_scheduler_grants(
  State = #super_node_state{worker_to_client = WorkerToClientMap},
  [TargetWorker | Rest],
  Direction,
  MicrobatchId,
  StageId
) ->
  case maps:get(TargetWorker, WorkerToClientMap, undefined) of
    undefined ->
      {error, {unknown_grant_target_worker, TargetWorker}};
    TargetClient ->
      ?LOG_INFO(
        "Super node delivering grant to worker=~p via client=~p direction=~p microbatch=~p stage=~p",
        [TargetWorker, TargetClient, Direction, MicrobatchId, StageId]
      ),
      Command = {
        parallel_super_command,
        grant_scheduler_event,
        Direction,
        MicrobatchId,
        StageId,
        TargetWorker
      },
      case send_super_command_to_client(State, TargetClient, Command) of
        ok ->
          send_scheduler_grants(State, Rest, Direction, MicrobatchId, StageId);
        {error, _Reason} = Error ->
          Error
      end
  end.

normalize_non_negative_int(Value, Default) ->
  ParsedValue =
    case Value of
      IntValue when is_integer(IntValue) ->
        IntValue;
      BinaryValue when is_binary(BinaryValue) ->
        safe_parse_int(binary_to_list(BinaryValue), Default);
      ListValue when is_list(ListValue) ->
        safe_parse_int(ListValue, Default);
      _ ->
        Default
    end,
  if
    ParsedValue < 0 -> Default;
    true -> ParsedValue
  end.

safe_parse_int(Value, Default) ->
  try list_to_integer(string:trim(Value)) of
    ParsedValue -> ParsedValue
  catch
    _:_ -> Default
  end.

build_scheduler_trace(PhaseName, legacy, _ParallelExecution, _WorkerParallelMap) when PhaseName =:= training; PhaseName =:= prediction -> [];
build_scheduler_trace(_PhaseName, legacy, _ParallelExecution, _WorkerParallelMap) -> [];
build_scheduler_trace(PhaseName, tensor, _ParallelExecution, _WorkerParallelMap) when PhaseName =:= training; PhaseName =:= prediction -> [];
build_scheduler_trace(_PhaseName, tensor, _ParallelExecution, _WorkerParallelMap) -> [];
build_scheduler_trace(PhaseName, Mode, ParallelExecution, WorkerParallelMap) ->
  StageWorldSize = infer_pipeline_stage_world_size(WorkerParallelMap),
  NumMicroBatches = get_execution_int(ParallelExecution, <<"numMicroBatches">>, 1),
  Scheduler = normalize_scheduler(maps:get(<<"scheduler">>, ParallelExecution, <<"gpipe">>)),
  BaseTrace =
    case {Mode, Scheduler} of
    {pipeline, gpipe} -> build_gpipe_trace(StageWorldSize, NumMicroBatches);
    {pipeline_tensor, gpipe} -> forward_only_trace(build_gpipe_trace(StageWorldSize, NumMicroBatches));
    {pipeline, '1f1b'} -> build_1f1b_trace(StageWorldSize, NumMicroBatches);
    {pipeline_tensor, '1f1b'} -> forward_only_trace(build_1f1b_trace(StageWorldSize, NumMicroBatches));
    {pipeline, interleaved} ->
      VirtualStages = get_execution_int(ParallelExecution, <<"virtualStages">>, 2),
      build_interleaved_trace(StageWorldSize, NumMicroBatches, VirtualStages);
    {pipeline_tensor, interleaved} ->
      VirtualStages = get_execution_int(ParallelExecution, <<"virtualStages">>, 2),
      forward_only_trace(build_interleaved_trace(StageWorldSize, NumMicroBatches, VirtualStages));
    _ -> build_gpipe_trace(StageWorldSize, NumMicroBatches)
  end,
  case normalize_phase_name(PhaseName) of
    prediction -> forward_only_trace(BaseTrace);
    _ -> BaseTrace
  end.

normalize_phase_name(Value) when is_atom(Value) ->
  normalize_phase_name(atom_to_list(Value));
normalize_phase_name(Value) when is_binary(Value) ->
  normalize_phase_name(binary_to_list(Value));
normalize_phase_name(Value) when is_list(Value) ->
  case string:lowercase(string:trim(Value)) of
    "prediction" -> prediction;
    _ -> training
  end;
normalize_phase_name(_) ->
  training.

forward_only_trace(Trace) ->
  [Event || Event = {Direction, _MicrobatchId, _StageId} <- Trace, Direction =:= forward].

normalize_scheduler(Scheduler) when is_atom(Scheduler) ->
  normalize_scheduler(atom_to_list(Scheduler));
normalize_scheduler(Scheduler) when is_binary(Scheduler) ->
  normalize_scheduler(binary_to_list(Scheduler));
normalize_scheduler(Scheduler) when is_list(Scheduler) ->
  case string:lowercase(string:trim(Scheduler)) of
    "1f1b" -> '1f1b';
    "interleaved" -> interleaved;
    _ -> gpipe
  end;
normalize_scheduler(_) ->
  gpipe.

get_execution_int(ParallelExecution, Field, Default) ->
  try
    case maps:get(Field, ParallelExecution, Default) of
      Value when is_integer(Value) -> Value;
      Value when is_binary(Value) -> list_to_integer(binary_to_list(Value));
      Value when is_list(Value) -> list_to_integer(Value);
      _ -> Default
    end
  catch
    _:_ -> Default
  end.

infer_pipeline_stage_world_size(WorkerParallelMap) ->
  WorkerCfgList = maps:values(WorkerParallelMap),
  StageList = [maps:get(pipeline_stage, WorkerCfg, 0) || WorkerCfg <- WorkerCfgList],
  WorldList = [maps:get(pipeline_world_size, WorkerCfg, 0) || WorkerCfg <- WorkerCfgList],
  MaxStage = case StageList of [] -> 0; _ -> lists:max(StageList) end,
  MaxWorld = case WorldList of [] -> 0; _ -> lists:max(WorldList) end,
  erlang:max(1, erlang:max(MaxStage + 1, MaxWorld)).

build_gpipe_trace(StageWorldSize, NumMicroBatches) ->
  StageForward = lists:seq(0, StageWorldSize - 1),
  StageBackward = lists:reverse(StageForward),
  ForwardEvents = lists:flatten([
    [{forward, MicrobatchId, Stage} || Stage <- StageForward]
    || MicrobatchId <- lists:seq(0, NumMicroBatches - 1)
  ]),
  BackwardEvents = lists:flatten([
    [{backward, MicrobatchId, Stage} || Stage <- StageBackward]
    || MicrobatchId <- lists:reverse(lists:seq(0, NumMicroBatches - 1))
  ]),
  ForwardEvents ++ BackwardEvents.

build_1f1b_trace(StageWorldSize, NumMicroBatches) ->
  StageForward = lists:seq(0, StageWorldSize - 1),
  StageBackward = lists:reverse(StageForward),
  Warmup = erlang:max(0, StageWorldSize - 1),
  build_1f1b_trace_loop(
    NumMicroBatches,
    StageForward,
    StageBackward,
    Warmup,
    0,
    []
  ).

build_1f1b_trace_loop(NumMicroBatches, _StageForward, StageBackward, Warmup, Index, Acc)
when Index >= NumMicroBatches ->
  DrainStart = erlang:max(0, NumMicroBatches - Warmup),
  DrainBackward =
    lists:flatten([
      [{backward, MicrobatchId, Stage} || Stage <- StageBackward]
      || MicrobatchId <- lists:seq(DrainStart, NumMicroBatches - 1)
    ]),
  Acc ++ DrainBackward;
build_1f1b_trace_loop(NumMicroBatches, StageForward, StageBackward, Warmup, Index, Acc) ->
  ForwardEvents = [{forward, Index, Stage} || Stage <- StageForward],
  BackwardEvents =
    case Index >= Warmup of
      true ->
        BackwardMicrobatch = Index - Warmup,
        [{backward, BackwardMicrobatch, Stage} || Stage <- StageBackward];
      false ->
        []
    end,
  build_1f1b_trace_loop(
    NumMicroBatches,
    StageForward,
    StageBackward,
    Warmup,
    Index + 1,
    Acc ++ ForwardEvents ++ BackwardEvents
  ).

build_interleaved_trace(StageWorldSize, NumMicroBatches, VirtualStages) ->
  TotalStages = erlang:max(1, StageWorldSize * erlang:max(1, VirtualStages)),
  build_1f1b_trace(TotalStages, NumMicroBatches).

maybe_advance_scheduler(
  {parallel_event, FromWorker, Direction, MicrobatchId, Stage, _Payload},
  State = #super_node_state{scheduler_trace = Trace, scheduler_cursor = Cursor}
) ->
  case validate_scheduler_event(FromWorker, Direction, MicrobatchId, Stage, Trace, Cursor, State) of
    {ok, UpdatedState} ->
      ?LOG_INFO(
        "Super node accepted scheduler event worker=~p direction=~p microbatch=~p stage=~p cursor=~p",
        [FromWorker, Direction, MicrobatchId, Stage, Cursor]
      ),
      maybe_send_next_scheduler_grant(UpdatedState);
    Error ->
      Error
  end;
maybe_advance_scheduler(
  {parallel_event, Direction, MicrobatchId, Stage},
  State = #super_node_state{scheduler_trace = Trace, scheduler_cursor = Cursor}
) ->
  case validate_scheduler_event(undefined, Direction, MicrobatchId, Stage, Trace, Cursor, State) of
    {ok, UpdatedState} ->
      ?LOG_INFO(
        "Super node accepted scheduler event direction=~p microbatch=~p stage=~p cursor=~p",
        [Direction, MicrobatchId, Stage, Cursor]
      ),
      maybe_send_next_scheduler_grant(UpdatedState);
    Error ->
      Error
  end;
maybe_advance_scheduler(_Data, State) ->
  {ok, State}.

validate_scheduler_event(_FromWorker, _Direction, _MicrobatchId, _Stage, [], _Cursor, State) ->
  {ok, State};
validate_scheduler_event(
  FromWorker,
  Direction,
  MicrobatchId,
  Stage,
  Trace,
  Cursor,
  State = #super_node_state{pending_grant = PendingGrant}
) ->
  TraceLen = length(Trace),
  case Cursor >= TraceLen of
    true ->
      {abort, {scheduler_cursor_out_of_bounds, Cursor, TraceLen}, State};
    false ->
      try lists:nth(Cursor + 1, Trace) of
    Expected = {Direction, MicrobatchId, Stage} ->
      validate_scheduler_grant(FromWorker, PendingGrant, Expected, State, Cursor);
    Expected ->
      {abort, {scheduler_mismatch, Expected, {Direction, MicrobatchId, Stage, FromWorker}}, State}
  catch
    error:badarg ->
      {abort, {scheduler_cursor_out_of_bounds, Cursor, TraceLen}, State}
      end
  end.

validate_scheduler_grant(
  _FromWorker,
  none,
  ExpectedEvent,
  State,
  _Cursor
) ->
  {abort, {scheduler_event_without_grant, ExpectedEvent}, State};
validate_scheduler_grant(
  FromWorker,
  PendingGrantRaw,
  ExpectedEvent = {Direction, MicrobatchId, Stage},
  State,
  Cursor
) ->
  case normalize_pending_grant(PendingGrantRaw) of
    {error, NormalizeReason} ->
      {abort, {invalid_pending_grant, PendingGrantRaw, NormalizeReason}, State};
    {ok, GrantDirection, GrantMicrobatchId, GrantStage, GrantedWorkers, AckedWorkers, EventID} ->
      case {GrantDirection, GrantMicrobatchId, GrantStage} =:= ExpectedEvent of
        false ->
          {abort, {scheduler_grant_mismatch, PendingGrantRaw, ExpectedEvent}, State};
        true ->
          case (FromWorker =:= undefined) orelse (not lists:member(FromWorker, GrantedWorkers)) of
            true ->
              {abort, {scheduler_grant_worker_mismatch, GrantedWorkers, FromWorker}, State};
            false ->
              case lists:member(FromWorker, AckedWorkers) of
                true ->
                  {abort, {scheduler_duplicate_worker_event, FromWorker, {Direction, MicrobatchId, Stage}}, State};
                false ->
                  UpdatedAckedWorkers = lists:usort([FromWorker | AckedWorkers]),
                  case lists:sort(UpdatedAckedWorkers) =:= lists:sort(GrantedWorkers) of
                    true ->
                      ?LOG_INFO(
                        "Super node grant fully acknowledged workers=~p direction=~p microbatch=~p stage=~p event_id=~p",
                        [GrantedWorkers, Direction, MicrobatchId, Stage, EventID]
                      ),
                      {ok, State#super_node_state{
                        scheduler_cursor = Cursor + 1,
                        pending_grant = none,
                        pending_grant_issued_ms = 0
                      }};
                    false ->
                      ?LOG_INFO(
                        "Super node grant partial ack worker=~p acked=~p granted=~p direction=~p microbatch=~p stage=~p event_id=~p",
                        [FromWorker, UpdatedAckedWorkers, GrantedWorkers, Direction, MicrobatchId, Stage, EventID]
                      ),
                      {ok, State#super_node_state{
                        pending_grant = #{
                          direction => Direction,
                          microbatch_id => MicrobatchId,
                          stage_id => Stage,
                          workers => GrantedWorkers,
                          acked_workers => UpdatedAckedWorkers,
                          event_id => EventID
                        }
                      }}
                  end
              end
          end
      end
  end.

normalize_pending_grant(none) ->
  {error, none};
normalize_pending_grant(PendingGrant) when is_map(PendingGrant) ->
  try
    Direction = maps:get(direction, PendingGrant),
    MicrobatchId = maps:get(microbatch_id, PendingGrant),
    StageId = maps:get(stage_id, PendingGrant),
    WorkersRaw = maps:get(workers, PendingGrant),
    AckedRaw = maps:get(acked_workers, PendingGrant, []),
    Workers = normalize_granted_workers(WorkersRaw),
    AckedWorkers = normalize_granted_workers(AckedRaw),
    EventID = maps:get(event_id, PendingGrant, grant_event_id(Direction, MicrobatchId, StageId, Workers)),
    {ok, Direction, MicrobatchId, StageId, Workers, AckedWorkers, EventID}
  catch
    _:Reason ->
      {error, Reason}
  end;
normalize_pending_grant({Direction, MicrobatchId, StageId, GrantedWorkers, AckedWorkers}) ->
  Workers = normalize_granted_workers(GrantedWorkers),
  Acked = normalize_granted_workers(AckedWorkers),
  EventID = grant_event_id(Direction, MicrobatchId, StageId, Workers),
  {ok, Direction, MicrobatchId, StageId, Workers, Acked, EventID};
normalize_pending_grant({Direction, MicrobatchId, StageId, GrantedWorker}) ->
  Workers = normalize_granted_workers([GrantedWorker]),
  EventID = grant_event_id(Direction, MicrobatchId, StageId, Workers),
  {ok, Direction, MicrobatchId, StageId, Workers, [], EventID};
normalize_pending_grant(Unexpected) ->
  {error, {unsupported_pending_grant, Unexpected}}.

normalize_granted_workers(Workers) when is_list(Workers) ->
  lists:usort(Workers);
normalize_granted_workers(Worker) ->
  [Worker].

grant_event_id(Direction, MicrobatchId, StageId, Workers) ->
  {scheduler_grant, Direction, MicrobatchId, StageId, lists:sort(Workers)}.

parallel_event_id(WorkerName, Direction, BatchID, MicrobatchID, StageID) ->
  {parallel_event, WorkerName, Direction, BatchID, MicrobatchID, StageID}.
