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
  scheduler_batch_id = undefined,
  scheduler_max_batches = undefined,
  pending_grant = none,
  pending_grant_issued_ms = 0,
  grant_timeout_ms = 5000,
  rejection_streak = 0,
  parallel_active = false,
  phase_start_ms = 0,
  phase_epoch = 0,
  phase_close_requested = [],
  phase_close_completed = false,
  last_parallel_event = none,
  last_abort = {none, 0},
  pending_parallel_deliveries = #{},
  next_parallel_delivery_id = 1,
  parallel_delivery_retry_ms = 250,
  parallel_delivery_max_retries = 20
}).

-define(HEARTBEAT_MISS_FACTOR, 3).
-define(GRANT_TIMEOUT_FACTOR, 2).
-define(MAX_GRANT_REJECTION_STREAK, 256).
-define(PARALLEL_DELIVERY_RETRY_FLOOR_MS, 100).
-define(PARALLEL_DELIVERY_MAX_RETRIES_DEFAULT, 20).

start_link(Args = {MyName, _ManagedClients, _HeartbeatMs, _MaxInflight, _NerlnetGraph}) ->
  gen_server:start_link({local, MyName}, ?MODULE, Args, []).

init({MyName, ManagedClients, HeartbeatMs, MaxInflight, NerlnetGraph}) ->
  nerl_tools:setup_logger(?MODULE),
  inets:start(),
  GrantTimeoutMs = erlang:max(5000, HeartbeatMs * ?HEARTBEAT_MISS_FACTOR * ?GRANT_TIMEOUT_FACTOR),
  ParallelDeliveryRetryMs = erlang:max(?PARALLEL_DELIVERY_RETRY_FLOOR_MS, HeartbeatMs div 4),
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
    parallel_delivery_retry_ms = ParallelDeliveryRetryMs,
    parallel_delivery_max_retries = ?PARALLEL_DELIVERY_MAX_RETRIES_DEFAULT,
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
  pending_parallel_deliveries = PendingDeliveries
}) ->
  ?LOG_INFO("Super node routing parallel worker message from ~p to ~p", [FromWorker, ToWorker]),
  FinalState =
    case maps:get(ToWorker, WorkerToClientMap, undefined) of
      undefined ->
        maybe_notify_parallel_abort(State, {unknown_target_worker, ToWorker, FromWorker});
      DestClient ->
        {DeliveryId, StateWithDeliveryId} = allocate_parallel_delivery_id(State),
        MessageBody = {parallel_deliver, DeliveryId, FromWorker, ToWorker, Data},
        case route_parallel_delivery_to_client(StateWithDeliveryId, DestClient, MessageBody) of
          ok ->
            DeliveryNowMs = erlang:system_time(millisecond),
            DeliveryInfo = #{
              client => DestClient,
              from_worker => FromWorker,
              to_worker => ToWorker,
              data => Data,
              sent_ms => DeliveryNowMs,
              retries => 0
            },
            StateWithDeliveryId#super_node_state{
              pending_parallel_deliveries = maps:put(DeliveryId, DeliveryInfo, PendingDeliveries)
            };
          {error, RouteReason} ->
            maybe_notify_parallel_abort(
              StateWithDeliveryId,
              {route_failed, DestClient, ToWorker, {parallel_delivery, RouteReason}}
            )
        end
    end,
  {noreply, FinalState};

handle_cast({parallel_deliver_ack, ClientName, DeliveryId, AckStatus}, State = #super_node_state{
  pending_parallel_deliveries = PendingDeliveries
}) ->
  case maps:get(DeliveryId, PendingDeliveries, undefined) of
    undefined ->
      ?LOG_WARNING(
        "Super node received unknown parallel delivery ack id=~p from client=~p status=~p",
        [DeliveryId, ClientName, AckStatus]
      ),
      {noreply, State};
    DeliveryInfo ->
      RemainingDeliveries = maps:remove(DeliveryId, PendingDeliveries),
      case delivery_ack_succeeded(AckStatus) of
        true ->
          ?LOG_INFO(
            "Super node acknowledged parallel delivery id=~p from ~p to ~p via client=~p",
            [DeliveryId, maps:get(from_worker, DeliveryInfo, undefined), maps:get(to_worker, DeliveryInfo, undefined), ClientName]
          ),
          {noreply, State#super_node_state{pending_parallel_deliveries = RemainingDeliveries}};
        false ->
          FailureReason = {
            parallel_delivery_failed,
            DeliveryId,
            ClientName,
            AckStatus,
            maps:get(to_worker, DeliveryInfo, undefined)
          },
          {noreply, maybe_notify_parallel_abort(State#super_node_state{pending_parallel_deliveries = RemainingDeliveries}, FailureReason)}
      end
  end;

handle_cast(
  {parallel_event, FromWorker, Direction, BatchID, MicrobatchID, StageID, Meta},
  State = #super_node_state{phase_epoch = PhaseEpoch}
) ->
  {EventEpoch, EventMeta} = extract_event_epoch_and_meta(PhaseEpoch, Meta),
  EventID = parallel_event_id(FromWorker, Direction, BatchID, MicrobatchID, StageID),
  ?LOG_INFO(
    "Super node received parallel event worker=~p direction=~p batch=~p microbatch=~p stage=~p epoch=~p event_id=~p meta=~p",
    [FromWorker, Direction, BatchID, MicrobatchID, StageID, EventEpoch, EventID, EventMeta]
  ),
  case EventEpoch =:= PhaseEpoch of
    false ->
      ?LOG_WARNING(
        "Super node ignoring stale parallel event event_id=~p worker=~p event_epoch=~p active_epoch=~p",
        [EventID, FromWorker, EventEpoch, PhaseEpoch]
      ),
      {noreply, State};
    true ->
      EventTsMs = erlang:system_time(millisecond),
      StateWithLastEvent = State#super_node_state{
        last_parallel_event = #{
          event_id => EventID,
          worker => FromWorker,
          direction => Direction,
          batch_id => BatchID,
          microbatch_id => MicrobatchID,
          stage_id => StageID,
          phase_epoch => EventEpoch,
          at_ms => EventTsMs
        },
        rejection_streak = 0
      },
      EventPayload = {parallel_event, FromWorker, Direction, BatchID, MicrobatchID, StageID, EventMeta},
      case maybe_advance_scheduler(EventPayload, StateWithLastEvent) of
        {abort, AbortReason, UpdatedState} ->
          {noreply, maybe_notify_parallel_abort(UpdatedState, AbortReason)};
        {ok, UpdatedState} ->
          {noreply, UpdatedState}
      end
  end;

handle_cast(
  {parallel_phase_update, PhaseName, ParallelMode, ParallelExecution, WorkerParallelMap},
  State = #super_node_state{}
) ->
  {_, UpdatedState} =
    apply_parallel_phase_update(PhaseName, ParallelMode, ParallelExecution, WorkerParallelMap, State),
  {noreply, UpdatedState};

handle_cast(
  {parallel_phase_close, ClientName, ClientEpoch},
  State = #super_node_state{}
) ->
  UpdatedState = handle_parallel_phase_close_request(ClientName, ClientEpoch, State),
  {noreply, UpdatedState};

handle_cast(
  {scheduler_grant_rejected, ClientName, WorkerName, Direction, BatchID, MicrobatchID, StageID, RejectReason, ClientEpoch},
  State = #super_node_state{}
) ->
  UpdatedState = handle_scheduler_grant_rejected(
                   ClientName,
                   WorkerName,
                   Direction,
                   BatchID,
                   MicrobatchID,
                   StageID,
                   RejectReason,
                   ClientEpoch,
                   State
                 ),
  {noreply, UpdatedState};
handle_cast(
  {scheduler_grant_rejected, ClientName, WorkerName, Direction, MicrobatchID, StageID, RejectReason, ClientEpoch},
  State = #super_node_state{}
) ->
  UpdatedState = handle_scheduler_grant_rejected(
                   ClientName,
                   WorkerName,
                   Direction,
                   any,
                   MicrobatchID,
                   StageID,
                   RejectReason,
                   ClientEpoch,
                   State
                 ),
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
  StateAfterDeliveryRetry = maybe_retry_pending_parallel_deliveries(StateAfterGrantTimeoutCheck, NowMs),
  erlang:send_after(HeartbeatMs, self(), check_heartbeats),
  {noreply, StateAfterDeliveryRetry};

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

notify_parallel_phase_done(#super_node_state{
  my_name = MyName,
  parallel_phase = ParallelPhase,
  parallel_mode = ParallelMode,
  phase_epoch = PhaseEpoch,
  scheduler_batch_id = SchedulerBatchID,
  my_router = {RouterHost, RouterPort}
}) ->
  MessageBody = {MyName, ParallelPhase, ParallelMode, PhaseEpoch, SchedulerBatchID},
  try
    nerl_tools:http_router_request(
      RouterHost,
      RouterPort,
      [?MAIN_SERVER_ATOM],
      atom_to_list(parallelPhaseDone),
      MessageBody
    )
  catch
    _:_ -> ok
  end.

allocate_parallel_delivery_id(State = #super_node_state{
  phase_epoch = PhaseEpoch,
  next_parallel_delivery_id = Counter
}) ->
  {{parallel_delivery, PhaseEpoch, Counter}, State#super_node_state{next_parallel_delivery_id = Counter + 1}}.

delivery_ack_succeeded(ok) ->
  true;
delivery_ack_succeeded({ok, _}) ->
  true;
delivery_ack_succeeded(_) ->
  false.

route_parallel_delivery_to_client(
  #super_node_state{my_router = {RouterHost, RouterPort}},
  DestClient,
  MessageBody
) ->
  try
    nerl_tools:http_router_request(
      RouterHost,
      RouterPort,
      [DestClient],
      atom_to_list(parallelDeliver),
      MessageBody
    ),
    ok
  catch
    Err:Reason ->
      {error, {Err, Reason}}
  end.

maybe_retry_pending_parallel_deliveries(
  State = #super_node_state{pending_parallel_deliveries = PendingDeliveries},
  _NowMs
) when map_size(PendingDeliveries) =:= 0 ->
  State;
maybe_retry_pending_parallel_deliveries(State, NowMs) ->
  lists:foldl(
    fun({DeliveryId, _DeliveryInfo}, AccState) ->
      maybe_retry_pending_parallel_delivery(DeliveryId, NowMs, AccState)
    end,
    State,
    maps:to_list(State#super_node_state.pending_parallel_deliveries)
  ).

maybe_retry_pending_parallel_delivery(
  DeliveryId,
  NowMs,
  State = #super_node_state{
    pending_parallel_deliveries = PendingDeliveries,
    parallel_delivery_retry_ms = RetryMs,
    parallel_delivery_max_retries = MaxRetries
  }
) ->
  case maps:get(DeliveryId, PendingDeliveries, undefined) of
    undefined ->
      State;
    DeliveryInfo ->
      LastSentMs = maps:get(sent_ms, DeliveryInfo, 0),
      Retries = maps:get(retries, DeliveryInfo, 0),
      case (NowMs - LastSentMs) >= RetryMs of
        false ->
          State;
        true ->
          DestClient = maps:get(client, DeliveryInfo, undefined),
          case Retries >= MaxRetries of
            true ->
              RemainingDeliveries = maps:remove(DeliveryId, PendingDeliveries),
              TimeoutReason = {
                parallel_delivery_timeout,
                DeliveryId,
                DestClient,
                maps:get(from_worker, DeliveryInfo, undefined),
                maps:get(to_worker, DeliveryInfo, undefined),
                RetryMs,
                MaxRetries
              },
              maybe_notify_parallel_abort(
                State#super_node_state{pending_parallel_deliveries = RemainingDeliveries},
                TimeoutReason
              );
            false ->
              MessageBody = {
                parallel_deliver,
                DeliveryId,
                maps:get(from_worker, DeliveryInfo, undefined),
                maps:get(to_worker, DeliveryInfo, undefined),
                maps:get(data, DeliveryInfo, undefined)
              },
              case route_parallel_delivery_to_client(State, DestClient, MessageBody) of
                ok ->
                  UpdatedInfo = DeliveryInfo#{
                    sent_ms => NowMs,
                    retries => Retries + 1
                  },
                  ?LOG_WARNING(
                    "Super node retrying parallel delivery id=~p client=~p retry=~p/~p to=~p",
                    [DeliveryId, DestClient, Retries + 1, MaxRetries, maps:get(to_worker, DeliveryInfo, undefined)]
                  ),
                  State#super_node_state{
                    pending_parallel_deliveries = maps:put(DeliveryId, UpdatedInfo, PendingDeliveries)
                  };
                {error, RouteReason} ->
                  maybe_notify_parallel_abort(
                    State,
                    {parallel_delivery_retry_route_failed, DeliveryId, DestClient, RouteReason}
                  )
              end
          end
      end
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
    {ok, Direction, BatchId, MicrobatchId, StageId, GrantedWorkers, AckedWorkers, EventID, GrantEpoch} ->
      #{
        direction => Direction,
        batch_id => BatchId,
        microbatch_id => MicrobatchId,
        stage_id => StageId,
        workers => GrantedWorkers,
        acked_workers => AckedWorkers,
        event_id => EventID,
        phase_epoch => GrantEpoch
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
    worker_to_client = WorkerToClientMap,
    phase_epoch = PreviousPhaseEpoch
  }
) ->
  PhaseEpoch = PreviousPhaseEpoch + 1,
  NormalizedMode = normalize_parallel_mode(ParallelMode),
  SchedulerTrace = build_scheduler_trace(PhaseName, NormalizedMode, ParallelExecution, WorkerParallelMap),
  SchedulerMaxBatches = resolve_scheduler_max_batches(ParallelExecution),
  ParallelActive = is_parallel_mode_active(NormalizedMode),
  PhaseStartMs =
    case ParallelActive of
      true -> erlang:system_time(millisecond);
      false -> 0
    end,
  StageWorkers = build_stage_workers(WorkerParallelMap, ManagedClients, WorkerToClientMap),
  ?LOG_INFO(
    "Super node ~p updated parallel phase ~p mode=~p epoch=~p trace_length=~p max_batches=~p active=~p",
    [MyName, PhaseName, NormalizedMode, PhaseEpoch, length(SchedulerTrace), SchedulerMaxBatches, ParallelActive]
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
    scheduler_batch_id = undefined,
    scheduler_max_batches = SchedulerMaxBatches,
    pending_grant = none,
    pending_grant_issued_ms = 0,
    rejection_streak = 0,
    parallel_active = ParallelActive,
    phase_start_ms = PhaseStartMs,
    phase_epoch = PhaseEpoch,
    phase_close_requested = [],
    phase_close_completed = false,
    last_parallel_event = none,
    pending_parallel_deliveries = #{}
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
    parallel_execution = ParallelExecution,
    phase_epoch = PhaseEpoch
  }
) ->
  lists:foldl(
    fun(ClientName, AccState) ->
      Command = {parallel_super_command, configure_parallel, ParallelMode, ParallelExecution, PhaseEpoch},
      ?LOG_INFO(
        "Super node pushing parallel config to client ~p mode=~p epoch=~p",
        [ClientName, ParallelMode, PhaseEpoch]
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
    scheduler_cursor = Cursor,
    pending_grant = PendingGrant
  }
) ->
  case {ParallelActive, Trace, PendingGrant} of
    {false, _AnyTrace, _AnyGrant} ->
      ?LOG_INFO(
        "Super node scheduler gate: parallel_active=false cursor=~p trace_len=~p pending_grant=~p",
        [Cursor, length(Trace), PendingGrant]
      ),
      {ok, State};
    {_True, [], _AnyGrant} ->
      ?LOG_INFO(
        "Super node scheduler gate: empty trace cursor=~p pending_grant=~p",
        [Cursor, PendingGrant]
      ),
      {ok, State};
    {_True, _NonEmptyTrace, none} ->
      issue_next_scheduler_grant(State);
    {_True, _NonEmptyTrace, _ExistingGrant} ->
      ?LOG_INFO(
        "Super node scheduler gate: pending grant still open cursor=~p pending_grant=~p",
        [Cursor, PendingGrant]
      ),
      {ok, State}
  end.

handle_parallel_phase_close_request(
  ClientName,
  ClientEpoch,
  State = #super_node_state{
    my_name = MyName,
    managed_clients = ManagedClients,
    phase_epoch = PhaseEpoch,
    phase_close_requested = RequestedClients,
    phase_close_completed = PhaseCloseCompleted
  }
) ->
  case lists:member(ClientName, ManagedClients) of
    false ->
      ?LOG_WARNING(
        "Super node ~p ignoring phase-close request from unmanaged client ~p",
        [MyName, ClientName]
      ),
      State;
    true ->
      case ClientEpoch =:= PhaseEpoch of
        false ->
          ?LOG_WARNING(
            "Super node ~p ignoring phase-close request from ~p due to epoch mismatch request=~p active=~p",
            [MyName, ClientName, ClientEpoch, PhaseEpoch]
          ),
          State;
        true ->
          UpdatedRequestedClients = lists:usort([ClientName | RequestedClients]),
          RequestedCount = length(UpdatedRequestedClients),
          ManagedCount = length(ManagedClients),
          ?LOG_INFO(
            "Super node ~p received phase-close request from client ~p epoch=~p progress=~p/~p",
            [MyName, ClientName, PhaseEpoch, RequestedCount, ManagedCount]
          ),
          StateWithRequest = State#super_node_state{phase_close_requested = UpdatedRequestedClients},
          case {PhaseCloseCompleted, RequestedCount =:= ManagedCount} of
            {true, _} ->
              StateWithRequest;
            {false, true} ->
              finalize_parallel_phase_close(StateWithRequest);
            _ ->
              StateWithRequest
          end
      end
  end.

finalize_parallel_phase_close(
  State = #super_node_state{
    my_name = MyName,
    phase_epoch = PhaseEpoch
  }
) ->
  ?LOG_INFO(
    "Super node ~p entering phase-close barrier epoch=~p; disabling scheduler grants and broadcasting close grant",
    [MyName, PhaseEpoch]
  ),
  ClearedState = State#super_node_state{
    parallel_active = false,
    scheduler_trace = [],
    scheduler_cursor = 0,
    scheduler_batch_id = undefined,
    scheduler_max_batches = undefined,
    pending_grant = none,
    pending_grant_issued_ms = 0,
    rejection_streak = 0,
    phase_close_completed = true,
    pending_parallel_deliveries = #{}
  },
  broadcast_phase_close_granted(ClearedState).

broadcast_phase_close_granted(
  State = #super_node_state{
    managed_clients = ManagedClients,
    phase_epoch = PhaseEpoch
  }
) ->
  lists:foldl(
    fun(ClientName, AccState) ->
      Command = {parallel_super_command, phase_close_granted, PhaseEpoch},
      case send_super_command_to_client(AccState, ClientName, Command) of
        ok ->
          ?LOG_INFO(
            "Super node sent phase-close grant to client ~p epoch=~p",
            [ClientName, PhaseEpoch]
          ),
          AccState;
        {error, RouteReason} ->
          maybe_notify_parallel_abort(
            AccState,
            {phase_close_granted_route_failed, ClientName, PhaseEpoch, RouteReason}
          )
      end
    end,
    State,
    ManagedClients
  ).

handle_scheduler_grant_rejected(
  ClientName,
  WorkerName,
  Direction,
  BatchID,
  MicrobatchID,
  StageID,
  RejectReason,
  ClientEpoch,
  State = #super_node_state{
    my_name = MyName,
    phase_epoch = PhaseEpoch,
    pending_grant = PendingGrant,
    scheduler_cursor = Cursor,
    rejection_streak = RejectionStreak
  }
) ->
  case ClientEpoch =:= PhaseEpoch of
    false ->
      ?LOG_WARNING(
        "Super node ~p ignoring scheduler grant rejection from ~p/~p due to epoch mismatch reject=~p active=~p",
        [MyName, ClientName, WorkerName, ClientEpoch, PhaseEpoch]
      ),
      State;
    true ->
      StateWithRejection = State#super_node_state{rejection_streak = RejectionStreak + 1},
      ?LOG_WARNING(
        "Super node ~p received scheduler grant rejection client=~p worker=~p direction=~p batch=~p microbatch=~p stage=~p epoch=~p reason=~p",
        [MyName, ClientName, WorkerName, Direction, BatchID, MicrobatchID, StageID, PhaseEpoch, RejectReason]
      ),
      case maybe_abort_rejection_storm(
             StateWithRejection,
             ClientName,
             WorkerName,
             RejectReason,
             PendingGrant
           ) of
        {abort, AbortedState} ->
          AbortedState;
        {continue, StableState} ->
          case is_phase_close_reject_reason(RejectReason) of
            true ->
              handle_close_related_grant_rejection(
                StableState,
                ClientName,
                ClientEpoch,
                WorkerName,
                RejectReason
              );
            false ->
              case normalize_pending_grant(PendingGrant) of
                {ok, GrantDirection, GrantBatchID, GrantMicrobatchID, GrantStageID, GrantedWorkers, AckedWorkers, EventID, GrantEpoch} ->
                  RejectedBatchID = normalize_scheduler_batch_id(BatchID),
                  ExpectedGrantTuple = {GrantDirection, GrantBatchID, GrantMicrobatchID, GrantStageID},
                  RejectedGrantTuple = {Direction, RejectedBatchID, MicrobatchID, StageID},
                  case (ExpectedGrantTuple =:= RejectedGrantTuple) andalso lists:member(WorkerName, GrantedWorkers) of
                    false ->
                      ?LOG_WARNING(
                        "Super node ~p received non-matching grant rejection worker=~p rejected=~p pending=~p",
                        [MyName, WorkerName, RejectedGrantTuple, ExpectedGrantTuple]
                      ),
                      StableState;
                    true ->
                      UpdatedAckedWorkers = lists:usort([WorkerName | AckedWorkers]),
                      case lists:sort(UpdatedAckedWorkers) =:= lists:sort(GrantedWorkers) of
                        true ->
                          ?LOG_INFO(
                            "Super node ~p treating rejected grant as terminal ack worker=~p event_id=~p",
                            [MyName, WorkerName, EventID]
                          ),
                          StateAfterAck = StableState#super_node_state{
                            scheduler_cursor = Cursor + 1,
                            scheduler_batch_id = RejectedBatchID,
                            pending_grant = none,
                            pending_grant_issued_ms = 0
                          },
                          case maybe_send_next_scheduler_grant(StateAfterAck) of
                            {ok, NextState} ->
                              NextState;
                            {abort, AbortReason, StateOnError} ->
                              maybe_notify_parallel_abort(StateOnError, AbortReason)
                          end;
                        false ->
                          StableState#super_node_state{
                            pending_grant = #{
                              direction => GrantDirection,
                              batch_id => GrantBatchID,
                              microbatch_id => GrantMicrobatchID,
                              stage_id => GrantStageID,
                              workers => GrantedWorkers,
                              acked_workers => UpdatedAckedWorkers,
                              event_id => EventID,
                              phase_epoch => GrantEpoch
                            }
                          }
                      end
                  end;
                _ ->
                  StableState
              end
          end
      end
  end.

handle_close_related_grant_rejection(
  State = #super_node_state{
    my_name = MyName,
    pending_grant = PendingGrant
  },
  ClientName,
  ClientEpoch,
  WorkerName,
  RejectReason
) ->
  PendingGrantSummary = pending_grant_summary(PendingGrant),
  ?LOG_WARNING(
    "Super node ~p converting close-related grant rejection into phase-close handling client=~p worker=~p reason=~p pending=~p",
    [MyName, ClientName, WorkerName, RejectReason, PendingGrantSummary]
  ),
  StateWithClearedGrant = clear_pending_grant_state(State),
  StateAfterCloseRequest = handle_parallel_phase_close_request(ClientName, ClientEpoch, StateWithClearedGrant),
  case StateAfterCloseRequest#super_node_state.phase_close_completed of
    true ->
      StateAfterCloseRequest;
    false ->
      maybe_notify_parallel_abort(
        clear_scheduler_for_abort(StateAfterCloseRequest),
        {phase_close_pending_before_global_barrier, ClientName, WorkerName, RejectReason, PendingGrantSummary}
      )
  end.

maybe_abort_rejection_storm(
  State = #super_node_state{
    my_name = MyName,
    rejection_streak = RejectionStreak
  },
  ClientName,
  WorkerName,
  RejectReason,
  PendingGrant
) ->
  case RejectionStreak >= ?MAX_GRANT_REJECTION_STREAK of
    false ->
      {continue, State};
    true ->
      PendingGrantSummary = pending_grant_summary(PendingGrant),
      ?LOG_ERROR(
        "Super node ~p aborting due to scheduler grant rejection storm streak=~p client=~p worker=~p reason=~p pending=~p",
        [MyName, RejectionStreak, ClientName, WorkerName, RejectReason, PendingGrantSummary]
      ),
      AbortReason = {
        scheduler_grant_rejection_storm,
        RejectionStreak,
        ClientName,
        WorkerName,
        RejectReason,
        PendingGrantSummary
      },
      {abort, maybe_notify_parallel_abort(clear_scheduler_for_abort(State), AbortReason)}
  end.

is_phase_close_reject_reason(RejectReason) ->
  case reject_reason_tag(RejectReason) of
    phase_close_pending -> true;
    phase_close_granted -> true;
    phase_close_waiting_worker_idle_ack -> true;
    client_idle_state -> true;
    _ -> false
  end.

reject_reason_tag({Tag, _}) when is_atom(Tag) ->
  Tag;
reject_reason_tag(Tag) when is_atom(Tag) ->
  Tag;
reject_reason_tag(_) ->
  unknown.

clear_pending_grant_state(State = #super_node_state{}) ->
  State#super_node_state{
    pending_grant = none,
    pending_grant_issued_ms = 0
  }.

clear_scheduler_for_abort(State = #super_node_state{}) ->
  State#super_node_state{
    parallel_active = false,
    scheduler_trace = [],
    scheduler_cursor = 0,
    scheduler_batch_id = undefined,
    scheduler_max_batches = undefined,
    pending_grant = none,
    pending_grant_issued_ms = 0,
    rejection_streak = 0,
    pending_parallel_deliveries = #{}
  }.

issue_next_scheduler_grant(
  State = #super_node_state{
    scheduler_trace = Trace,
    scheduler_cursor = Cursor,
    scheduler_batch_id = SchedulerBatchID,
    scheduler_max_batches = SchedulerMaxBatches,
    parallel_mode = ParallelMode,
    stage_workers = StageWorkers,
    stage_rr = StageRoundRobin,
    phase_epoch = PhaseEpoch
  }
) ->
  TraceLen = length(Trace),
  case TraceLen of
    0 ->
      {ok, State};
    _ ->
      case scheduler_max_batches_reached(Cursor, TraceLen, SchedulerBatchID, SchedulerMaxBatches) of
        true ->
          ?LOG_INFO(
            "Super node scheduler reached configured maxBatches=~p at batch=~p cursor=~p; initiating deterministic phase close",
            [SchedulerMaxBatches, SchedulerBatchID, Cursor]
          ),
          notify_parallel_phase_done(State),
          StateAfterClose =
            case State#super_node_state.phase_close_completed of
              true -> State;
              false -> finalize_parallel_phase_close(State)
            end,
          {ok, StateAfterClose};
        false ->
          EffectiveState =
            case Cursor >= TraceLen of
              true ->
                NextBatchID = advance_scheduler_batch_id(SchedulerBatchID),
                State#super_node_state{
                  scheduler_cursor = 0,
                  scheduler_batch_id = NextBatchID,
                  pending_grant = none,
                  pending_grant_issued_ms = 0
                };
              false ->
                State
            end,
          EffectiveCursor = EffectiveState#super_node_state.scheduler_cursor,
          EffectiveBatchID = EffectiveState#super_node_state.scheduler_batch_id,
          {Direction, MicrobatchId, StageId} = lists:nth(EffectiveCursor + 1, Trace),
          GrantBatchID = resolve_scheduler_grant_batch_id(EffectiveBatchID),
          case choose_stage_workers(ParallelMode, StageId, StageWorkers, StageRoundRobin) of
            {error, Reason} ->
              {abort, Reason, EffectiveState};
            {ok, TargetWorkers, UpdatedStageRoundRobin} ->
              EventID = grant_event_id(PhaseEpoch, Direction, GrantBatchID, MicrobatchId, StageId, TargetWorkers),
              GrantIssuedMs = erlang:system_time(millisecond),
              ?LOG_INFO(
                "Super node issuing scheduler grant direction=~p batch=~p microbatch=~p stage=~p epoch=~p targets=~p cursor=~p event_id=~p",
                [Direction, GrantBatchID, MicrobatchId, StageId, PhaseEpoch, TargetWorkers, EffectiveCursor, EventID]
              ),
              case send_scheduler_grants(
                     EffectiveState,
                     TargetWorkers,
                     Direction,
                     GrantBatchID,
                     MicrobatchId,
                     StageId,
                     PhaseEpoch
                   ) of
                ok ->
                  {ok, EffectiveState#super_node_state{
                    stage_rr = UpdatedStageRoundRobin,
                    pending_grant = #{
                      direction => Direction,
                      batch_id => GrantBatchID,
                      microbatch_id => MicrobatchId,
                      stage_id => StageId,
                      workers => TargetWorkers,
                      acked_workers => [],
                      event_id => EventID,
                      phase_epoch => PhaseEpoch
                    },
                    pending_grant_issued_ms = GrantIssuedMs
                  }};
                {error, RouteReason} ->
                  {abort,
                   {scheduler_grant_route_failed, TargetWorkers, {Direction, GrantBatchID, MicrobatchId, StageId}, RouteReason},
                   EffectiveState}
              end
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
  _BatchId,
  _MicrobatchId,
  _StageId,
  _PhaseEpoch
) ->
  ok;
send_scheduler_grants(
  State = #super_node_state{worker_to_client = WorkerToClientMap},
  [TargetWorker | Rest],
  Direction,
  BatchId,
  MicrobatchId,
  StageId,
  PhaseEpoch
) ->
  case maps:get(TargetWorker, WorkerToClientMap, undefined) of
    undefined ->
      {error, {unknown_grant_target_worker, TargetWorker}};
    TargetClient ->
      ?LOG_INFO(
        "Super node delivering grant to worker=~p via client=~p direction=~p batch=~p microbatch=~p stage=~p epoch=~p",
        [TargetWorker, TargetClient, Direction, BatchId, MicrobatchId, StageId, PhaseEpoch]
      ),
      Command = {
        parallel_super_command,
        grant_scheduler_event,
        Direction,
        BatchId,
        MicrobatchId,
        StageId,
        TargetWorker,
        PhaseEpoch
      },
      case send_super_command_to_client(State, TargetClient, Command) of
        ok ->
          send_scheduler_grants(State, Rest, Direction, BatchId, MicrobatchId, StageId, PhaseEpoch);
        {error, _Reason} = Error ->
          Error
      end
  end.

resolve_scheduler_grant_batch_id(undefined) ->
  any;
resolve_scheduler_grant_batch_id(BatchID) ->
  BatchID.

advance_scheduler_batch_id(BatchID) when is_integer(BatchID) ->
  BatchID + 1;
advance_scheduler_batch_id(_) ->
  undefined.

scheduler_max_batches_reached(_Cursor, _TraceLen, _SchedulerBatchID, undefined) ->
  false;
scheduler_max_batches_reached(_Cursor, _TraceLen, _SchedulerBatchID, MaxBatches)
when not is_integer(MaxBatches); MaxBatches < 0 ->
  false;
scheduler_max_batches_reached(_Cursor, _TraceLen, _SchedulerBatchID, 0) ->
  true;
scheduler_max_batches_reached(Cursor, TraceLen, SchedulerBatchID, MaxBatches)
when Cursor >= TraceLen, is_integer(SchedulerBatchID) ->
  (SchedulerBatchID + 1) >= MaxBatches;
scheduler_max_batches_reached(_Cursor, _TraceLen, _SchedulerBatchID, _MaxBatches) ->
  false.

resolve_scheduler_max_batches(ParallelExecution) when is_map(ParallelExecution) ->
  RawMaxBatches =
    maps:get(
      <<"maxBatches">>,
      ParallelExecution,
      maps:get(
        maxBatches,
        ParallelExecution,
        maps:get(
          <<"max_batches">>,
          ParallelExecution,
          maps:get(max_batches, ParallelExecution, undefined)
        )
      )
    ),
  case RawMaxBatches of
    undefined ->
      undefined;
    _ ->
      ParsedMaxBatches = normalize_non_negative_int(RawMaxBatches, -1),
      case ParsedMaxBatches >= 0 of
        true -> ParsedMaxBatches;
        false -> undefined
      end
  end;
resolve_scheduler_max_batches(_) ->
  undefined.

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

extract_event_epoch_and_meta(DefaultEpoch, {parallel_meta, EpochRaw, Payload}) ->
  {normalize_non_negative_int(EpochRaw, DefaultEpoch), Payload};
extract_event_epoch_and_meta(DefaultEpoch, MetaMap) when is_map(MetaMap) ->
  EpochRaw = maps:get(phase_epoch, MetaMap, DefaultEpoch),
  Payload = maps:get(payload, MetaMap, MetaMap),
  {normalize_non_negative_int(EpochRaw, DefaultEpoch), Payload};
extract_event_epoch_and_meta(DefaultEpoch, Payload) ->
  {DefaultEpoch, Payload}.

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
  {parallel_event, FromWorker, Direction, BatchID, MicrobatchId, Stage, _Payload},
  State = #super_node_state{scheduler_trace = Trace, scheduler_cursor = Cursor}
) ->
  case validate_scheduler_event(FromWorker, Direction, BatchID, MicrobatchId, Stage, Trace, Cursor, State) of
    {ok, UpdatedState} ->
      ?LOG_INFO(
        "Super node accepted scheduler event worker=~p direction=~p batch=~p microbatch=~p stage=~p cursor=~p",
        [FromWorker, Direction, BatchID, MicrobatchId, Stage, Cursor]
      ),
      maybe_send_next_scheduler_grant(UpdatedState);
    Error ->
      Error
  end;
maybe_advance_scheduler(
  {parallel_event, Direction, BatchID, MicrobatchId, Stage},
  State = #super_node_state{scheduler_trace = Trace, scheduler_cursor = Cursor}
) ->
  case validate_scheduler_event(undefined, Direction, BatchID, MicrobatchId, Stage, Trace, Cursor, State) of
    {ok, UpdatedState} ->
      ?LOG_INFO(
        "Super node accepted scheduler event direction=~p batch=~p microbatch=~p stage=~p cursor=~p",
        [Direction, BatchID, MicrobatchId, Stage, Cursor]
      ),
      maybe_send_next_scheduler_grant(UpdatedState);
    Error ->
      Error
  end;
maybe_advance_scheduler(_Data, State) ->
  {ok, State}.

validate_scheduler_event(_FromWorker, _Direction, _BatchID, _MicrobatchId, _Stage, [], _Cursor, State) ->
  {ok, State};
validate_scheduler_event(
  FromWorker,
  Direction,
  BatchID,
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
      validate_scheduler_grant(FromWorker, BatchID, PendingGrant, Expected, State, Cursor);
    Expected ->
      {abort, {scheduler_mismatch, Expected, {Direction, BatchID, MicrobatchId, Stage, FromWorker}}, State}
  catch
    error:badarg ->
      {abort, {scheduler_cursor_out_of_bounds, Cursor, TraceLen}, State}
      end
  end.

validate_scheduler_grant(
  _FromWorker,
  _BatchID,
  none,
  ExpectedEvent,
  State,
  _Cursor
) ->
  {abort, {scheduler_event_without_grant, ExpectedEvent}, State};
validate_scheduler_grant(
  FromWorker,
  BatchID,
  PendingGrantRaw,
  ExpectedEvent = {Direction, MicrobatchId, Stage},
  State = #super_node_state{phase_epoch = PhaseEpoch, scheduler_batch_id = SchedulerBatchID},
  Cursor
) ->
  case normalize_pending_grant(PendingGrantRaw) of
    {error, NormalizeReason} ->
      {abort, {invalid_pending_grant, PendingGrantRaw, NormalizeReason}, State};
    {ok, GrantDirection, GrantBatchID, GrantMicrobatchId, GrantStage, GrantedWorkers, AckedWorkers, EventID, GrantEpoch} ->
      case {GrantDirection, GrantMicrobatchId, GrantStage} =:= ExpectedEvent of
        false ->
          {abort, {scheduler_grant_mismatch, PendingGrantRaw, ExpectedEvent}, State};
        true ->
          case GrantEpoch =:= PhaseEpoch of
            true ->
              case resolve_grant_batch_id_for_event(GrantBatchID, BatchID) of
                {error, BatchReason} ->
                  {abort, {scheduler_grant_batch_mismatch, BatchReason, PendingGrantRaw, BatchID}, State};
                {ok, ResolvedBatchID} ->
                  case is_integer(SchedulerBatchID) andalso is_integer(ResolvedBatchID) andalso SchedulerBatchID =/= ResolvedBatchID of
                    true ->
                      {abort, {scheduler_state_batch_mismatch, SchedulerBatchID, ResolvedBatchID, PendingGrantRaw}, State};
                    false ->
                      case (FromWorker =:= undefined) orelse (not lists:member(FromWorker, GrantedWorkers)) of
                        true ->
                          {abort, {scheduler_grant_worker_mismatch, GrantedWorkers, FromWorker}, State};
                        false ->
                          case lists:member(FromWorker, AckedWorkers) of
                            true ->
                              {abort, {scheduler_duplicate_worker_event, FromWorker, {Direction, ResolvedBatchID, MicrobatchId, Stage}}, State};
                            false ->
                              UpdatedAckedWorkers = lists:usort([FromWorker | AckedWorkers]),
                              case lists:sort(UpdatedAckedWorkers) =:= lists:sort(GrantedWorkers) of
                                true ->
                                  ?LOG_INFO(
                                    "Super node grant fully acknowledged workers=~p direction=~p batch=~p microbatch=~p stage=~p epoch=~p event_id=~p",
                                    [GrantedWorkers, Direction, ResolvedBatchID, MicrobatchId, Stage, GrantEpoch, EventID]
                                  ),
                                  {ok, State#super_node_state{
                                    scheduler_cursor = Cursor + 1,
                                    scheduler_batch_id = ResolvedBatchID,
                                    pending_grant = none,
                                    pending_grant_issued_ms = 0
                                  }};
                                false ->
                                  ?LOG_INFO(
                                    "Super node grant partial ack worker=~p acked=~p granted=~p direction=~p batch=~p microbatch=~p stage=~p epoch=~p event_id=~p",
                                    [FromWorker, UpdatedAckedWorkers, GrantedWorkers, Direction, ResolvedBatchID, MicrobatchId, Stage, GrantEpoch, EventID]
                                  ),
                                  {ok, State#super_node_state{
                                    scheduler_batch_id = ResolvedBatchID,
                                    pending_grant = #{
                                      direction => Direction,
                                      batch_id => ResolvedBatchID,
                                      microbatch_id => MicrobatchId,
                                      stage_id => Stage,
                                      workers => GrantedWorkers,
                                      acked_workers => UpdatedAckedWorkers,
                                      event_id => EventID,
                                      phase_epoch => GrantEpoch
                                    }
                                  }}
                              end
                          end
                      end
                  end
              end;
            false ->
              {abort, {scheduler_grant_epoch_mismatch, GrantEpoch, PhaseEpoch, PendingGrantRaw}, State}
          end
      end
  end.

normalize_pending_grant(none) ->
  {error, none};
normalize_pending_grant(PendingGrant) when is_map(PendingGrant) ->
  try
    Direction = maps:get(direction, PendingGrant),
    BatchId = normalize_scheduler_batch_id(maps:get(batch_id, PendingGrant, any)),
    MicrobatchId = maps:get(microbatch_id, PendingGrant),
    StageId = maps:get(stage_id, PendingGrant),
    PhaseEpoch = maps:get(phase_epoch, PendingGrant, 0),
    WorkersRaw = maps:get(workers, PendingGrant),
    AckedRaw = maps:get(acked_workers, PendingGrant, []),
    Workers = normalize_granted_workers(WorkersRaw),
    AckedWorkers = normalize_granted_workers(AckedRaw),
    EventID = maps:get(event_id, PendingGrant, grant_event_id(PhaseEpoch, Direction, BatchId, MicrobatchId, StageId, Workers)),
    {ok, Direction, BatchId, MicrobatchId, StageId, Workers, AckedWorkers, EventID, PhaseEpoch}
  catch
    _:Reason ->
      {error, Reason}
  end;
normalize_pending_grant({Direction, MicrobatchId, StageId, GrantedWorkers, AckedWorkers}) when is_list(AckedWorkers) ->
  Workers = normalize_granted_workers(GrantedWorkers),
  Acked = normalize_granted_workers(AckedWorkers),
  EventID = grant_event_id(0, Direction, any, MicrobatchId, StageId, Workers),
  {ok, Direction, any, MicrobatchId, StageId, Workers, Acked, EventID, 0};
normalize_pending_grant({Direction, MicrobatchId, StageId, GrantedWorkers, AckedWorkers, PhaseEpoch}) ->
  Workers = normalize_granted_workers(GrantedWorkers),
  Acked = normalize_granted_workers(AckedWorkers),
  EventID = grant_event_id(PhaseEpoch, Direction, any, MicrobatchId, StageId, Workers),
  {ok, Direction, any, MicrobatchId, StageId, Workers, Acked, EventID, PhaseEpoch};
normalize_pending_grant({Direction, MicrobatchId, StageId, GrantedWorker}) ->
  Workers = normalize_granted_workers([GrantedWorker]),
  EventID = grant_event_id(0, Direction, any, MicrobatchId, StageId, Workers),
  {ok, Direction, any, MicrobatchId, StageId, Workers, [], EventID, 0};
normalize_pending_grant({Direction, MicrobatchId, StageId, GrantedWorker, PhaseEpoch}) when is_integer(PhaseEpoch) ->
  Workers = normalize_granted_workers([GrantedWorker]),
  EventID = grant_event_id(PhaseEpoch, Direction, any, MicrobatchId, StageId, Workers),
  {ok, Direction, any, MicrobatchId, StageId, Workers, [], EventID, PhaseEpoch};
normalize_pending_grant({Direction, BatchId, MicrobatchId, StageId, GrantedWorkers, AckedWorkers, PhaseEpoch}) ->
  Workers = normalize_granted_workers(GrantedWorkers),
  Acked = normalize_granted_workers(AckedWorkers),
  NormalizedBatchID = normalize_scheduler_batch_id(BatchId),
  EventID = grant_event_id(PhaseEpoch, Direction, NormalizedBatchID, MicrobatchId, StageId, Workers),
  {ok, Direction, NormalizedBatchID, MicrobatchId, StageId, Workers, Acked, EventID, PhaseEpoch};
normalize_pending_grant(Unexpected) ->
  {error, {unsupported_pending_grant, Unexpected}}.

resolve_grant_batch_id_for_event(any, BatchID) ->
  case normalize_scheduler_batch_id(BatchID) of
    any ->
      {error, unresolved_batch_id};
    Resolved ->
      {ok, Resolved}
  end;
resolve_grant_batch_id_for_event(ExpectedBatchID, BatchID) ->
  NormalizedExpected = normalize_scheduler_batch_id(ExpectedBatchID),
  NormalizedBatch = normalize_scheduler_batch_id(BatchID),
  case NormalizedExpected =:= NormalizedBatch of
    true -> {ok, NormalizedExpected};
    false -> {error, {expected, NormalizedExpected, got, NormalizedBatch}}
  end.

normalize_scheduler_batch_id(any) ->
  any;
normalize_scheduler_batch_id(BatchID) when is_integer(BatchID) ->
  BatchID;
normalize_scheduler_batch_id(BatchID) when is_binary(BatchID) ->
  normalize_scheduler_batch_id(binary_to_list(BatchID));
normalize_scheduler_batch_id(BatchID) when is_list(BatchID) ->
  try list_to_integer(string:trim(BatchID)) of
    ParsedBatchID ->
      ParsedBatchID
  catch
    _:_ ->
      any
  end;
normalize_scheduler_batch_id(_) ->
  any.

normalize_granted_workers(Workers) when is_list(Workers) ->
  lists:usort(Workers);
normalize_granted_workers(Worker) ->
  [Worker].

grant_event_id(PhaseEpoch, Direction, BatchId, MicrobatchId, StageId, Workers) ->
  {scheduler_grant, PhaseEpoch, Direction, BatchId, MicrobatchId, StageId, lists:sort(Workers)}.

parallel_event_id(WorkerName, Direction, BatchID, MicrobatchID, StageID) ->
  {parallel_event, WorkerName, Direction, BatchID, MicrobatchID, StageID}.
