%%%-------------------------------------------------------------------
%%% @author kapelnik
%%% @copyright (C) 2021, Nerlnet
%%% @doc
%%%
%%% @end
%%% Created : 14. Apr 2021 9:57 AM
%%%-------------------------------------------------------------------
-module(clientStatem).
-author("kapelnik").
-include("../nerl_tools.hrl").
-include("../Stats/stats.hrl").
-behaviour(gen_statem).

%% API
-export([start_link/1, predict/3]).

%% gen_statem callbacks
-export([init/1, format_status/2, handle_event/4, terminate/3,
  code_change/4, callback_mode/0, idle/3, training/3,waitforWorkers/3]).


-import(clientWorkersFunctions,[createWorkers/2]).
-import(nerlNIF,[validate_nerltensor_erl/1]).


-define(ETS_KV_VAL_IDX, 2). % key value pairs --> value index is 2
-define(WORKER_PID_IDX, 1).
-define(W2W_PID_IDX, 2).
-define(SERVER, ?MODULE).
-define(PHASE_CLOSE_RETRY_MS, 1000).
-define(PARALLEL_DELIVERY_SEEN_MAX, 2048).

%% client ETS table: {WorkerName, WorkerPid, WorkerArgs, TimingTuple}
%   myName - Client Name,
%   federatedServer - fed server name,
%   workersMap - this clients workers on this machine, each entry holds: WorkerName, WorkerPid, WorkerArgs, {0,0,0.0} (timing map), MissedBatches
%   NerlnetGraph, all connections needed for this client
%   msgCounter - gather messages statistics
%   timingMap - gather Timing statistics: timingMap = #{{WorkerName1=>{LastBatchReceivedTime,totalBatches,AverageTrainingime},{Worker2,..}, ...}

-record(client_statem_state, {myName, etsRef,nextState,waitforWorkers=[]}).

%%%===================================================================
%%% API
%%%===================================================================

%% @doc Creates a gen_statem process which calls Module:init/1 to
%% initialize. To ensure a synchronized start-up procedure, this
%% function does not return until Module:init/1 has returned.

%%Arguments from Cowboy Server
%%return gen_statem's Pid to Cowboy Server
%%Client_StateM_Args= {self(),RouterPort},
start_link(Args) ->
  nerl_tools:setup_logger(?MODULE),
  {ok,Pid} = gen_statem:start_link(?MODULE, Args, []),
  Pid.

%%%===================================================================
%%% gen_statem callbacks
%%%===================================================================

%% @private
%% @doc Whenever a gen_statem is started using gen_statem:start/[3,4] or
%% gen_statem:start_link/[3,4], this function is called by the new
%% process to initialize.
%% initialize and go to state - idle


%%  NerlClientsArgs=[{MyName,Workers,ConnectionsMap},...], Workers = list of maps of name and args
%%  init nerlClient with given workers and parameters, and build a map :#{workerName=>WorkerPid,...}
init({MyName,NerlnetGraph, ClientWorkers , WorkerShaMap , WorkerToClientMap , ShaToModelArgsMap, ClientSuperNode}) ->
  inets:start(),
  ?LOG_INFO("Client ~p is connected to: ~p~n",[MyName, [digraph:vertex(NerlnetGraph,Vertex) || Vertex <- digraph:out_neighbours(NerlnetGraph,MyName)]]),
  % nerl_tools:start_connection([digraph:vertex(NerlnetGraph,Vertex) || Vertex <- digraph:out_neighbours(NerlnetGraph,MyName)]),
  EtsRef = ets:new(client_data, [set, public]), %% client_data is responsible for functional attributes
  EtsStats = ets:new(ets_stats, [set]), %% ets_stats is responsible for holding all the ets stats (client + workers)
  ClientStatsEts = stats:generate_stats_ets(), %% client stats ets inside ets_stats
  % TODO add flag to control generate performance stats ets
  ClientPerformanceEts = stats:generate_performance_stats_ets(), %% client performance stats ets inside ets_stats
  ets:insert(EtsStats, {MyName, ClientStatsEts}),
  ets:insert(EtsStats, {performance_stats, ClientPerformanceEts}),
  put(ets_stats, EtsStats),
  ets:insert(EtsRef, {workerToClient, WorkerToClientMap}), % All workers in the network (map to their client)
  ets:insert(EtsRef, {workersNames, ClientWorkers}), % All THIS Client's workers
  ets:insert(EtsRef, {nerlnetGraph, NerlnetGraph}),
  ets:insert(EtsRef, {myName, MyName}),
  MyWorkersToShaMap = maps:filter(fun(Worker , _SHA) -> lists:member(Worker , ClientWorkers) end , WorkerShaMap),
  ets:insert(EtsRef, {workers_to_sha_map, MyWorkersToShaMap}),
  ets:insert(EtsRef, {sha_to_models_map , ShaToModelArgsMap}),
  ets:insert(EtsRef, {w2wcom_pids, #{}}),
  ets:insert(EtsRef, {super_node, ClientSuperNode}),
  ets:insert(EtsRef, {parallel_mode, legacy}),
  ets:insert(EtsRef, {parallel_execution, #{}}),
  ets:insert(EtsRef, {parallel_authority, main_server}),
  ets:insert(EtsRef, {parallel_phase_epoch, 0}),
  ets:insert(EtsRef, {parallel_phase_close_requested, false}),
  ets:insert(EtsRef, {parallel_phase_close_granted, false}),
  ets:insert(EtsRef, {parallel_phase_close_last_request_ms, 0}),
  ets:insert(EtsRef, {parallel_idle_requested, false}),
  ets:insert(EtsRef, {parallel_workers_drain_ready, []}),
  ets:insert(EtsRef, {parallel_workers_stream_seen, []}),
  ets:insert(EtsRef, {parallel_delivery_seen_ids, []}),
  ets:insert(EtsRef, {all_workers_done, false}),
  ets:insert(EtsRef, {active_phase_type, idle}),
  ets:insert(EtsRef, {num_of_fed_servers, 0}), % Will stay 0 if non-federated
  {MyRouterHost,MyRouterPort} = nerl_tools:getShortPath(MyName,?MAIN_SERVER_ATOM, NerlnetGraph),
  ets:insert(EtsRef, {my_router,{MyRouterHost,MyRouterPort}}),
  ets:insert(EtsRef, {monitoring_workers_done_running, false}),
  clientWorkersFunctions:create_workers(MyName , EtsRef , ShaToModelArgsMap , EtsStats),
  %% send pre_idle signal to workers
  WorkersNames = clientWorkersFunctions:get_workers_names(EtsRef),
  Pids = [clientWorkersFunctions:get_worker_pid(EtsRef , WorkerName) || WorkerName <- WorkersNames],
  [gen_statem:cast(WorkerPid, {pre_idle}) || WorkerPid <- Pids],
  NumOfFedServers = ets:lookup_element(EtsRef, num_of_fed_servers, ?DATA_IDX), % When non-federated exp this value is 0
  ets:insert(EtsRef, {num_of_training_workers, length(ClientWorkers) - NumOfFedServers}), % This number will not change 
  ets:insert(EtsRef, {training_workers, 0}), % will be updated in idle -> training & end_stream
  ets:insert(EtsRef, {active_workers_streams, []}),
  % update dictionary
  WorkersEts = ets:lookup_element(EtsRef , workers_ets , ?DATA_IDX),
  put(workers_ets, WorkersEts),
  put(nerlnetGraph, NerlnetGraph),
  put(client_data, EtsRef),
  put(ets_stats, EtsStats),
  put(client_stats_ets , ClientStatsEts),
  put(performance_stats_ets , ClientPerformanceEts),
  put(my_pid , self()),
  maybe_register_super_node(MyName, EtsRef, NerlnetGraph, ClientWorkers),
  maybe_start_super_node_heartbeat(MyName, EtsRef, NerlnetGraph),

  {ok, idle, #client_statem_state{myName= MyName, etsRef = EtsRef}}.

%% @private
%% @doc This function is called by a gen_statem when it needs to find out the callback mode of the callback module.
callback_mode() -> state_functions.

%% @private
%% @doc Called (1) whenever sys:get_status/1,2 is called by gen_statem or
%% (2) when gen_statem terminates abnormally.
%% This callback is optional.
format_status(_Opt, [_PDict, _StateName, _State]) -> Status = some_term, Status.

%% ==============STATES=================
waitforWorkers(cast, {set_parallel_mode, Mode, Source}, State = #client_statem_state{etsRef = EtsRef}) ->
  apply_parallel_mode(EtsRef, Mode, Source),
  {keep_state, State};

waitforWorkers(cast, {set_parallel_mode, Mode}, State = #client_statem_state{etsRef = EtsRef}) ->
  apply_parallel_mode(EtsRef, Mode, main_server),
  {keep_state, State};

waitforWorkers(cast, {set_parallel_execution, ParallelExecution, Source}, State = #client_statem_state{etsRef = EtsRef}) ->
  apply_parallel_execution(EtsRef, ParallelExecution, Source),
  {keep_state, State};

waitforWorkers(cast, {set_parallel_execution, ParallelExecution}, State = #client_statem_state{etsRef = EtsRef}) ->
  apply_parallel_execution(EtsRef, ParallelExecution, main_server),
  {keep_state, State};

waitforWorkers(cast, {parallel_super_command, SuperCommand}, State = #client_statem_state{etsRef = EtsRef}) ->
  apply_parallel_super_command(EtsRef, SuperCommand, waitforWorkers),
  {keep_state, State};

waitforWorkers(cast, {worker_parallel_abort, WorkerName, Reason}, State = #client_statem_state{etsRef = EtsRef}) ->
  increment_worker_messages_sent(EtsRef, WorkerName, {worker_parallel_abort, WorkerName, Reason}),
  handle_worker_parallel_abort(EtsRef, WorkerName, Reason),
  {keep_state, State};

waitforWorkers(cast, In = {stateChange,WorkerName}, State = #client_statem_state{myName = MyName,waitforWorkers = WaitforWorkers,nextState = NextState, etsRef = EtsRef}) ->
  NewWaitforWorkers = WaitforWorkers -- [WorkerName],
  ClientStatsEts = get(client_stats_ets),
  stats:increment_messages_received(ClientStatsEts),
  stats:increment_bytes_received(ClientStatsEts , nerl_tools:calculate_size(In)),
  increment_worker_messages_sent(EtsRef, WorkerName, In),
  case NewWaitforWorkers of % TODO Guy here we need to check for keep alive with workers
    [] ->   case NextState of
              idle -> reset_parallel_phase_close_state(EtsRef);
              _ -> ok
            end,
            send_client_is_ready(MyName), % when all workers done their work
            stats:increment_messages_sent(ClientStatsEts),
            ?LOG_INFO("Client ~p and its workers are ready~n",[MyName]),
            {next_state, NextState, State#client_statem_state{waitforWorkers = []}};
    _  ->   {next_state, waitforWorkers, State#client_statem_state{waitforWorkers = NewWaitforWorkers}}
  end;

waitforWorkers(cast, In = {worker_to_worker_msg, FromWorker, ToWorker, Data}, State = #client_statem_state{etsRef = EtsRef}) ->
  ClientStatsEts = get(client_stats_ets),
  stats:increment_messages_received(ClientStatsEts),
  stats:increment_bytes_received(ClientStatsEts , nerl_tools:calculate_size(In)),
  handle_w2w_msg(EtsRef, FromWorker, ToWorker, Data),
  {keep_state, State};

waitforWorkers(cast, In = {parallel_deliver, FromWorker, ToWorker, Data}, State = #client_statem_state{etsRef = EtsRef}) ->
  ClientStatsEts = get(client_stats_ets),
  stats:increment_messages_received(ClientStatsEts),
  stats:increment_bytes_received(ClientStatsEts , nerl_tools:calculate_size(In)),
  deliver_parallel_msg(EtsRef, FromWorker, ToWorker, Data),
  {keep_state, State};

waitforWorkers(cast, In = {parallel_deliver, DeliveryId, FromWorker, ToWorker, Data}, State = #client_statem_state{etsRef = EtsRef}) ->
  ClientStatsEts = get(client_stats_ets),
  stats:increment_messages_received(ClientStatsEts),
  stats:increment_bytes_received(ClientStatsEts , nerl_tools:calculate_size(In)),
  handle_parallel_delivery_with_ack(EtsRef, DeliveryId, FromWorker, ToWorker, Data),
  {keep_state, State};

waitforWorkers(cast, In = {parallel_event, FromWorker, Direction, BatchID, MicrobatchID, StageID, Meta}, State = #client_statem_state{etsRef = EtsRef}) ->
  ClientStatsEts = get(client_stats_ets),
  stats:increment_messages_received(ClientStatsEts),
  stats:increment_bytes_received(ClientStatsEts, nerl_tools:calculate_size(In)),
  forward_parallel_event(EtsRef, FromWorker, Direction, BatchID, MicrobatchID, StageID, Meta),
  {keep_state, State};

waitforWorkers(cast, In = {parallel_skip_event, FromWorker, Direction, BatchID, MicrobatchID, StageID, Meta}, State = #client_statem_state{etsRef = EtsRef}) ->
  ClientStatsEts = get(client_stats_ets),
  stats:increment_messages_received(ClientStatsEts),
  stats:increment_bytes_received(ClientStatsEts, nerl_tools:calculate_size(In)),
  forward_parallel_skip_event(EtsRef, FromWorker, Direction, BatchID, MicrobatchID, StageID, Meta),
  {keep_state, State};

waitforWorkers(cast, In = {parallel_worker_drain_ready, WorkerName, ModelPhase}, State = #client_statem_state{etsRef = EtsRef}) ->
  ClientStatsEts = get(client_stats_ets),
  stats:increment_messages_received(ClientStatsEts),
  stats:increment_bytes_received(ClientStatsEts, nerl_tools:calculate_size(In)),
  case handle_parallel_worker_drain_ready(EtsRef, WorkerName, ModelPhase) of
    {phase_complete, PhaseName} ->
      maybe_trigger_parallel_phase_close_on_stream_drain(EtsRef, PhaseName);
    _ ->
      ok
  end,
  {keep_state, State};

waitforWorkers(cast, {parallel_finalize_idle}, State = #client_statem_state{}) ->
  {keep_state, State};

waitforWorkers(cast, In = {NewState}, State = #client_statem_state{myName = _MyName, etsRef = EtsRef}) ->
  ClientStatsEts = get(client_stats_ets),
  stats:increment_messages_received(ClientStatsEts),
  stats:increment_bytes_received(ClientStatsEts , nerl_tools:calculate_size(In)),
  % ?LOG_INFO("~p in waiting going to state ~p~n",[MyName, State]),
  Workers =  clientWorkersFunctions:get_workers_names(EtsRef),
  cast_message_to_workers(EtsRef, {NewState}), %% This function increments the number of sent messages in stats ets
  {next_state, waitforWorkers, State#client_statem_state{nextState = NewState, waitforWorkers = Workers}};


waitforWorkers(cast, EventContent, State = #client_statem_state{myName = MyName}) ->
  ClientStatsEts = get(client_stats_ets),
  stats:increment_messages_received(ClientStatsEts),
  stats:increment_bytes_received(ClientStatsEts , nerl_tools:calculate_size(EventContent)),
  ?LOG_WARNING("client ~p waitforWorkers ignored!!!:  ~p ~n",[MyName, EventContent]),
  {next_state, waitforWorkers, State}.
  

%% initiating workers when they include federated workers. init stage == handshake between federated worker client and server
idle(cast, {set_parallel_mode, Mode, Source}, State = #client_statem_state{etsRef = EtsRef}) ->
  apply_parallel_mode(EtsRef, Mode, Source),
  {keep_state, State};

idle(cast, {set_parallel_mode, Mode}, State = #client_statem_state{etsRef = EtsRef}) ->
  apply_parallel_mode(EtsRef, Mode, main_server),
  {keep_state, State};

idle(cast, {set_parallel_execution, ParallelExecution, Source}, State = #client_statem_state{etsRef = EtsRef}) ->
  apply_parallel_execution(EtsRef, ParallelExecution, Source),
  {keep_state, State};

idle(cast, {set_parallel_execution, ParallelExecution}, State = #client_statem_state{etsRef = EtsRef}) ->
  apply_parallel_execution(EtsRef, ParallelExecution, main_server),
  {keep_state, State};

idle(cast, {parallel_super_command, SuperCommand}, State = #client_statem_state{etsRef = EtsRef}) ->
  apply_parallel_super_command(EtsRef, SuperCommand, idle),
  {keep_state, State};

idle(cast, {worker_parallel_abort, WorkerName, Reason}, State = #client_statem_state{etsRef = EtsRef}) ->
  increment_worker_messages_sent(EtsRef, WorkerName, {worker_parallel_abort, WorkerName, Reason}),
  handle_worker_parallel_abort(EtsRef, WorkerName, Reason),
  {keep_state, State};

idle(cast, In = {worker_to_worker_msg, FromWorker, ToWorker, Data}, State = #client_statem_state{etsRef = EtsRef}) ->
  ClientStatsEts = get(client_stats_ets),
  stats:increment_messages_received(ClientStatsEts),
  stats:increment_bytes_received(ClientStatsEts , nerl_tools:calculate_size(In)),
  handle_w2w_msg(EtsRef, FromWorker, ToWorker, Data),
  {keep_state, State};

idle(cast, In = {parallel_deliver, FromWorker, ToWorker, Data}, State = #client_statem_state{etsRef = EtsRef}) ->
  ClientStatsEts = get(client_stats_ets),
  stats:increment_messages_received(ClientStatsEts),
  stats:increment_bytes_received(ClientStatsEts , nerl_tools:calculate_size(In)),
  deliver_parallel_msg(EtsRef, FromWorker, ToWorker, Data),
  {keep_state, State};

idle(cast, In = {parallel_deliver, DeliveryId, FromWorker, ToWorker, Data}, State = #client_statem_state{etsRef = EtsRef}) ->
  ClientStatsEts = get(client_stats_ets),
  stats:increment_messages_received(ClientStatsEts),
  stats:increment_bytes_received(ClientStatsEts , nerl_tools:calculate_size(In)),
  handle_parallel_delivery_with_ack(EtsRef, DeliveryId, FromWorker, ToWorker, Data),
  {keep_state, State};

idle(cast, In = {parallel_event, FromWorker, Direction, BatchID, MicrobatchID, StageID, Meta}, State = #client_statem_state{etsRef = EtsRef}) ->
  ClientStatsEts = get(client_stats_ets),
  stats:increment_messages_received(ClientStatsEts),
  stats:increment_bytes_received(ClientStatsEts, nerl_tools:calculate_size(In)),
  forward_parallel_event(EtsRef, FromWorker, Direction, BatchID, MicrobatchID, StageID, Meta),
  {keep_state, State};

idle(cast, In = {parallel_skip_event, FromWorker, Direction, BatchID, MicrobatchID, StageID, Meta}, State = #client_statem_state{etsRef = EtsRef}) ->
  ClientStatsEts = get(client_stats_ets),
  stats:increment_messages_received(ClientStatsEts),
  stats:increment_bytes_received(ClientStatsEts, nerl_tools:calculate_size(In)),
  forward_parallel_skip_event(EtsRef, FromWorker, Direction, BatchID, MicrobatchID, StageID, Meta),
  {keep_state, State};

idle(cast, In = {parallel_worker_drain_ready, WorkerName, ModelPhase}, State = #client_statem_state{etsRef = EtsRef}) ->
  ClientStatsEts = get(client_stats_ets),
  stats:increment_messages_received(ClientStatsEts),
  stats:increment_bytes_received(ClientStatsEts, nerl_tools:calculate_size(In)),
  case handle_parallel_worker_drain_ready(EtsRef, WorkerName, ModelPhase) of
    {phase_complete, PhaseName} ->
      maybe_trigger_parallel_phase_close_on_stream_drain(EtsRef, PhaseName);
    _ ->
      ok
  end,
  {keep_state, State};

idle(cast, {parallel_finalize_idle}, State = #client_statem_state{}) ->
  {keep_state, State};

idle(cast, _In = {statistics}, State = #client_statem_state{ myName = MyName, etsRef = EtsRef}) ->
  EtsStats = get(ets_stats),
  ClientStatsEts = get(client_stats_ets),
  ClientStatsEncStr = stats:encode_ets_to_http_bin_str(ClientStatsEts),
  stats:increment_messages_received(ClientStatsEts),
  ListStatsEts = ets:tab2list(EtsStats) -- [{MyName , ClientStatsEts}], 
  PerformenceStatsEts = get(performance_stats_ets),
  ClientPerformenceStatsEncStr = ?PERF_STATS_SEPERATOR ++ stats:encode_ets_to_http_bin_str(PerformenceStatsEts) ++ ?PERF_STATS_SEPERATOR,
  WorkersStatsEncStr = create_encoded_stats_str(ListStatsEts),
  DataToSend = ClientStatsEncStr ++ ClientPerformenceStatsEncStr ++ WorkersStatsEncStr,
  StatsBody = {MyName , DataToSend},
  {RouterHost,RouterPort} = ets:lookup_element(EtsRef, my_router, ?DATA_IDX),
  nerl_tools:http_router_request(RouterHost, RouterPort, [?MAIN_SERVER_ATOM], atom_to_list(statistics), StatsBody),
  stats:increment_messages_sent(ClientStatsEts),

  erlang:garbage_collect(), % free memory when phase is changed to idle
  {next_state, idle, State};

% Main Server triggers this state
idle(cast, In = {training}, State = #client_statem_state{myName = _MyName, etsRef = EtsRef}) ->
  erlang:garbage_collect(), % free memory when phase is changed to training
  reset_parallel_phase_close_state(EtsRef),
  ets:update_element(EtsRef, parallel_delivery_seen_ids, {?DATA_IDX, []}),
  ets:update_element(EtsRef, active_phase_type, {?DATA_IDX, training}),
  ClientStatsEts = get(client_stats_ets),
  PerformanceStatsEts = get(performance_stats_ets),
  stats:increment_messages_received(ClientStatsEts),
  stats:increment_bytes_received(ClientStatsEts , nerl_tools:calculate_size(In)),
  MessageToCast = {training},
  cast_message_to_workers(EtsRef, MessageToCast),
  ets:update_element(EtsRef, all_workers_done, {?DATA_IDX, false}),
  % Training is the first phase in the flow; reset all phase metrics for a clean experiment baseline.
  stats:performance_stats_reset_phase(PerformanceStatsEts, all),
  stats:communication_stats_reset(ClientStatsEts),
  stats:tic(ClientStatsEts, time_train_total),
  stats:reset_query_cpu_util_cores(),
  {next_state, waitforWorkers, State#client_statem_state{waitforWorkers =  clientWorkersFunctions:get_workers_names(EtsRef), nextState = training}};

idle(cast, In = {predict}, State = #client_statem_state{etsRef = EtsRef}) ->
  erlang:garbage_collect(), % free memory when phase is changed to predict
  reset_parallel_phase_close_state(EtsRef),
  ets:update_element(EtsRef, parallel_delivery_seen_ids, {?DATA_IDX, []}),
  ets:update_element(EtsRef, active_phase_type, {?DATA_IDX, prediction}),
  ClientStatsEts = get(client_stats_ets),
  PerformanceStatsEts = get(performance_stats_ets),
  stats:increment_messages_received(ClientStatsEts),
  stats:increment_bytes_received(ClientStatsEts , nerl_tools:calculate_size(In)),
  MessageToCast = {predict},
  cast_message_to_workers(EtsRef, MessageToCast),
  ets:update_element(EtsRef, all_workers_done, {?DATA_IDX, false}),
  % Entering prediction should not wipe completed training metrics.
  stats:performance_stats_reset_phase(PerformanceStatsEts, predict),
  stats:communication_stats_reset(ClientStatsEts),
  stats:tic(ClientStatsEts, time_predict_total), 
  stats:reset_query_cpu_util_cores(),
  {next_state, waitforWorkers, State#client_statem_state{waitforWorkers = clientWorkersFunctions:get_workers_names(EtsRef),nextState = predict}};

idle(cast, EventContent, State = #client_statem_state{etsRef = EtsRef , myName = MyName}) ->
  ClientStatsEts = get(client_stats_ets),
  stats:increment_bad_messages(ClientStatsEts),
  ?LOG_WARNING("~p Unrecognized Message!!!:  ~p",[MyName , EventContent]),
  {next_state, idle, State#client_statem_state{etsRef = EtsRef}}.

%% passing Data from worker to worker e.g. (FedClient to FedServer)
training(cast, {set_parallel_mode, Mode, Source}, State = #client_statem_state{etsRef = EtsRef}) ->
  apply_parallel_mode(EtsRef, Mode, Source),
  {keep_state, State};

training(cast, {set_parallel_mode, Mode}, State = #client_statem_state{etsRef = EtsRef}) ->
  apply_parallel_mode(EtsRef, Mode, main_server),
  {keep_state, State};

training(cast, {set_parallel_execution, ParallelExecution, Source}, State = #client_statem_state{etsRef = EtsRef}) ->
  apply_parallel_execution(EtsRef, ParallelExecution, Source),
  {keep_state, State};

training(cast, {set_parallel_execution, ParallelExecution}, State = #client_statem_state{etsRef = EtsRef}) ->
  apply_parallel_execution(EtsRef, ParallelExecution, main_server),
  {keep_state, State};

training(cast, {parallel_super_command, SuperCommand}, State = #client_statem_state{etsRef = EtsRef}) ->
  apply_parallel_super_command(EtsRef, SuperCommand, training),
  {keep_state, State};

training(cast, {worker_parallel_abort, WorkerName, Reason}, State = #client_statem_state{etsRef = EtsRef}) ->
  increment_worker_messages_sent(EtsRef, WorkerName, {worker_parallel_abort, WorkerName, Reason}),
  handle_worker_parallel_abort(EtsRef, WorkerName, Reason),
  {keep_state, State};

training(cast, MessageIn = {update, {From, To, Data}}, State = #client_statem_state{etsRef = EtsRef}) ->
  ClientStatsEts = get(client_stats_ets),
  stats:increment_messages_received(ClientStatsEts),
  stats:increment_bytes_received(ClientStatsEts , nerl_tools:calculate_size(MessageIn)),
  WorkerOfThisClient = ets:member(EtsRef, To),
  if WorkerOfThisClient -> 
    TargetWorkerPID = ets:lookup_element(EtsRef, To, ?WORKER_PID_IDX),
    gen_statem:cast(TargetWorkerPID,{update,From,To, Data}),
    increment_worker_messages_received(EtsRef, To, {update, From, To, Data}),
    stats:increment_messages_sent(ClientStatsEts),
    stats:increment_bytes_sent(ClientStatsEts , nerl_tools:calculate_size(Data));
  true ->
    DestClient = maps:get(To, ets:lookup_element(EtsRef, workerToClient, ?ETS_KV_VAL_IDX)),
    MessageBody = term_to_binary({DestClient, update, {From, To, Data}}),
    {RouterHost,RouterPort} = ets:lookup_element(EtsRef, my_router, ?DATA_IDX),
    nerl_tools:http_router_request(RouterHost, RouterPort, [DestClient], atom_to_list(pass), MessageBody),
    stats:increment_messages_sent(ClientStatsEts),
    stats:increment_bytes_sent(ClientStatsEts , nerl_tools:calculate_size(MessageBody))
  end,
  {keep_state, State};


training(cast, In = {worker_to_worker_msg, FromWorker, ToWorker, Data}, State = #client_statem_state{etsRef = EtsRef}) ->
  ClientStatsEts = get(client_stats_ets),
  stats:increment_messages_received(ClientStatsEts),
  stats:increment_bytes_received(ClientStatsEts , nerl_tools:calculate_size(In)),
  handle_w2w_msg(EtsRef, FromWorker, ToWorker, Data),
  {keep_state, State};

training(cast, In = {parallel_deliver, FromWorker, ToWorker, Data}, State = #client_statem_state{etsRef = EtsRef}) ->
  ClientStatsEts = get(client_stats_ets),
  stats:increment_messages_received(ClientStatsEts),
  stats:increment_bytes_received(ClientStatsEts , nerl_tools:calculate_size(In)),
  deliver_parallel_msg(EtsRef, FromWorker, ToWorker, Data),
  {keep_state, State};

training(cast, In = {parallel_deliver, DeliveryId, FromWorker, ToWorker, Data}, State = #client_statem_state{etsRef = EtsRef}) ->
  ClientStatsEts = get(client_stats_ets),
  stats:increment_messages_received(ClientStatsEts),
  stats:increment_bytes_received(ClientStatsEts , nerl_tools:calculate_size(In)),
  handle_parallel_delivery_with_ack(EtsRef, DeliveryId, FromWorker, ToWorker, Data),
  {keep_state, State};

training(cast, In = {parallel_event, FromWorker, Direction, BatchID, MicrobatchID, StageID, Meta}, State = #client_statem_state{etsRef = EtsRef}) ->
  ClientStatsEts = get(client_stats_ets),
  stats:increment_messages_received(ClientStatsEts),
  stats:increment_bytes_received(ClientStatsEts, nerl_tools:calculate_size(In)),
  forward_parallel_event(EtsRef, FromWorker, Direction, BatchID, MicrobatchID, StageID, Meta),
  {keep_state, State};

training(cast, In = {parallel_skip_event, FromWorker, Direction, BatchID, MicrobatchID, StageID, Meta}, State = #client_statem_state{etsRef = EtsRef}) ->
  ClientStatsEts = get(client_stats_ets),
  stats:increment_messages_received(ClientStatsEts),
  stats:increment_bytes_received(ClientStatsEts, nerl_tools:calculate_size(In)),
  forward_parallel_skip_event(EtsRef, FromWorker, Direction, BatchID, MicrobatchID, StageID, Meta),
  {keep_state, State};

training(cast, In = {parallel_worker_drain_ready, WorkerName, ModelPhase}, State = #client_statem_state{etsRef = EtsRef}) ->
  ClientStatsEts = get(client_stats_ets),
  stats:increment_messages_received(ClientStatsEts),
  stats:increment_bytes_received(ClientStatsEts, nerl_tools:calculate_size(In)),
  case handle_parallel_worker_drain_ready(EtsRef, WorkerName, ModelPhase) of
    {phase_complete, PhaseName} ->
      maybe_trigger_parallel_phase_close_on_stream_drain(EtsRef, PhaseName);
    _ ->
      ok
  end,
  {keep_state, State};
  
% TODO Validate this state - sample and empty list 
training(cast, _In = {sample,[]}, State = #client_statem_state{etsRef = EtsRef}) ->
  ClientStatsEts = get(client_stats_ets),
  stats:increment_bad_messages(ClientStatsEts),
  ?LOG_ERROR("client got empty Vector",[]),
  {next_state, training, State#client_statem_state{etsRef = EtsRef}};

training(cast, In = {sample,Body}, State = #client_statem_state{etsRef = EtsRef}) ->
  ClientStatsEts = get(client_stats_ets),
  stats:increment_messages_received(ClientStatsEts),
  stats:increment_bytes_received(ClientStatsEts , nerl_tools:calculate_size(In)),
  {SourceName , ClientName, WorkerNameStr, BatchID, BatchOfSamples} = binary_to_term(Body),
  WorkerName = list_to_atom(WorkerNameStr),
  WorkersEts = get(workers_ets),
  WorkerOfThisClient = ets:member(WorkersEts , WorkerName),
  if WorkerOfThisClient ->
      WorkerPid = clientWorkersFunctions:get_worker_pid(EtsRef , WorkerName),
      gen_statem:cast(WorkerPid, {sample, SourceName ,BatchID ,BatchOfSamples}),
      increment_worker_messages_received(EtsRef, WorkerName, {sample, SourceName, BatchID, BatchOfSamples}),
      stats:increment_messages_sent(ClientStatsEts),
      BatchSize = nerl_tools:calculate_size(BatchOfSamples),
      stats:increment_bytes_sent(ClientStatsEts , BatchSize),
      perf_stats_memory_usage_update_train();
  true -> ?LOG_ERROR("Given worker ~p isn't found in client ~p",[WorkerName, ClientName]) end,
  {next_state, training, State#client_statem_state{etsRef = EtsRef}};

% This action is used for start_stream triggered from a clients' worker and not source
training(cast, {start_stream , {worker, WorkerName, TargetPair}}, State = #client_statem_state{etsRef = EtsRef}) ->
  increment_worker_messages_sent(EtsRef, WorkerName, {start_stream, {worker, WorkerName, TargetPair}}),
  ListOfActiveWorkersSources = ets:lookup_element(EtsRef, active_workers_streams, ?DATA_IDX),
  ets:update_element(EtsRef, active_workers_streams, {?DATA_IDX, ListOfActiveWorkersSources ++ [{WorkerName, TargetPair}]}),
  mark_worker_stream_seen(EtsRef, WorkerName),
  
  perf_stats_memory_usage_update_train(),
  {keep_state, State};

% This action is used for start_stream triggered from a source per worker
training(cast, In = {start_stream , Data}, State = #client_statem_state{etsRef = EtsRef}) ->
  {SourceName, _ClientName, WorkerName} = binary_to_term(Data),
  ListOfActiveWorkersSources = ets:lookup_element(EtsRef, active_workers_streams, ?DATA_IDX),
  ets:update_element(EtsRef, active_workers_streams, {?DATA_IDX, ListOfActiveWorkersSources ++ [{WorkerName, SourceName}]}),
  mark_worker_stream_seen(EtsRef, WorkerName),
  ClientStatsEts = get(client_stats_ets),
  stats:increment_messages_received(ClientStatsEts),
  stats:increment_bytes_received(ClientStatsEts , nerl_tools:calculate_size(In)),
  WorkerPid = clientWorkersFunctions:get_worker_pid(EtsRef , WorkerName),
  gen_statem:cast(WorkerPid, {start_stream, SourceName}),
  increment_worker_messages_received(EtsRef, WorkerName, {start_stream, SourceName}),

  perf_stats_memory_usage_update_train(),  
  {keep_state, State};


training(cast, In = {end_stream , Data}, State = #client_statem_state{etsRef = EtsRef}) ->
  {SourceName, _ClientName, WorkerName} = binary_to_term(Data),
  ClientStatsEts = get(client_stats_ets),
  stats:increment_messages_received(ClientStatsEts),
  stats:increment_bytes_received(ClientStatsEts , nerl_tools:calculate_size(In)),
  WorkerPid = clientWorkersFunctions:get_worker_pid(EtsRef , WorkerName),
  gen_statem:cast(WorkerPid, {end_stream, SourceName}),
  increment_worker_messages_received(EtsRef, WorkerName, {end_stream, SourceName}),

  perf_stats_memory_usage_update_train(),
  {keep_state, State};

training(cast, In = {stream_ended , Pair}, State = #client_statem_state{etsRef = EtsRef}) ->
  ClientStatsEts = get(client_stats_ets),
  stats:increment_messages_received(ClientStatsEts),
  stats:increment_bytes_received(ClientStatsEts , nerl_tools:calculate_size(In)),
  maybe_record_stream_ended_worker(EtsRef, Pair, In),
  ListOfActiveWorkersSources = ets:lookup_element(EtsRef, active_workers_streams, ?DATA_IDX),
  UpdatedListOfActiveWorkersSources = ListOfActiveWorkersSources -- [Pair],
  ets:update_element(EtsRef, active_workers_streams, {?DATA_IDX, UpdatedListOfActiveWorkersSources}),
  case refresh_all_workers_done_state(EtsRef) of
    true ->
      maybe_trigger_parallel_phase_close_on_stream_drain(EtsRef, training);
    false ->
      ok
  end,
  {next_state, training, State#client_statem_state{etsRef = EtsRef}};

% From MainServer
training(cast, In = {idle}, State = #client_statem_state{myName = MyName, etsRef = EtsRef}) ->
  ClientStatsEts = get(client_stats_ets),
  ClientPerformanceEts = get(performance_stats_ets),
  stats:increment_messages_received(ClientStatsEts),
  stats:increment_bytes_received(ClientStatsEts , nerl_tools:calculate_size(In)),
  ets:insert(EtsRef, {parallel_idle_requested, true}),
  WorkersDone = ets:lookup_element(EtsRef , all_workers_done , ?DATA_IDX),
  case WorkersDone of
    true ->
      case should_gate_idle_with_super_close(EtsRef) of
        true ->
          maybe_request_super_phase_close(EtsRef, training),
          case ets:lookup_element(EtsRef, parallel_phase_close_granted, ?DATA_IDX) of
            true ->
              finalize_training_idle_transition(State, MyName, EtsRef, ClientStatsEts, ClientPerformanceEts);
            false ->
              ?LOG_INFO("~p waiting for Super Node phase-close grant before idling workers", [MyName]),
              {keep_state, State#client_statem_state{etsRef = EtsRef}}
          end;
        false ->
          finalize_training_idle_transition(State, MyName, EtsRef, ClientStatsEts, ClientPerformanceEts)
      end;
    false ->  MyPid = get(my_pid),
              start_monitor_workers_done(EtsRef, MyPid),
              {keep_state, State#client_statem_state{etsRef = EtsRef}}
  end;

training(cast, {parallel_finalize_idle}, State = #client_statem_state{myName = MyName, etsRef = EtsRef}) ->
  ClientStatsEts = get(client_stats_ets),
  ClientPerformanceEts = get(performance_stats_ets),
  maybe_finalize_parallel_idle_transition(training, State, MyName, EtsRef, ClientStatsEts, ClientPerformanceEts);

training(cast, _In = {predict}, State = #client_statem_state{myName = MyName, etsRef = EtsRef}) ->
  ?LOG_ERROR("Wrong request , client ~p can't go from training to predict directly", [MyName]),
  {next_state, training, State#client_statem_state{etsRef = EtsRef}};


training(cast, In = {loss, WorkerName ,SourceName ,LossTensor ,TimeNIF , WorkerToken,BatchID ,BatchTS}, State = #client_statem_state{myName = MyName,etsRef = EtsRef}) ->
  ClientStatsEts = get(client_stats_ets),
  ClientPerformanceEts = get(performance_stats_ets),
  stats:increment_messages_received(ClientStatsEts),
  stats:increment_bytes_received(ClientStatsEts , nerl_tools:calculate_size(In)),
  increment_worker_messages_sent(EtsRef, WorkerName, In),
  {RouterHost,RouterPort} = ets:lookup_element(EtsRef, my_router, ?DATA_IDX),
  stats:increment_time_train_active(ClientPerformanceEts, trunc(TimeNIF)), % in microseconds
  MessageBody = {WorkerName , SourceName , LossTensor , TimeNIF , WorkerToken, BatchID , BatchTS},
  nerl_tools:http_router_request(RouterHost, RouterPort, [?MAIN_SERVER_ATOM], atom_to_list(lossFunction), MessageBody), %% Change lossFunction atom to lossValue
  stats:increment_messages_sent(ClientStatsEts),
  stats:increment_bytes_sent(ClientStatsEts , nerl_tools:calculate_size(MessageBody)),
  {next_state, training, State#client_statem_state{myName = MyName,etsRef = EtsRef}};

training(cast, EventContent, State = #client_statem_state{etsRef = EtsRef, myName = MyName}) ->
  ?LOG_WARNING("client ~p training ignored!!!:  ~p ~n!!!",[MyName, EventContent]),
  ClientStatsEts = get(client_stats_ets),
  stats:increment_bad_messages(ClientStatsEts),
  stats:increment_bytes_received(ClientStatsEts , nerl_tools:calculate_size(EventContent)),
  {next_state, training, State#client_statem_state{etsRef = EtsRef}}.

predict(cast, {set_parallel_mode, Mode, Source}, State = #client_statem_state{etsRef = EtsRef}) ->
  apply_parallel_mode(EtsRef, Mode, Source),
  {keep_state, State};

predict(cast, {set_parallel_mode, Mode}, State = #client_statem_state{etsRef = EtsRef}) ->
  apply_parallel_mode(EtsRef, Mode, main_server),
  {keep_state, State};

predict(cast, {set_parallel_execution, ParallelExecution, Source}, State = #client_statem_state{etsRef = EtsRef}) ->
  apply_parallel_execution(EtsRef, ParallelExecution, Source),
  {keep_state, State};

predict(cast, {set_parallel_execution, ParallelExecution}, State = #client_statem_state{etsRef = EtsRef}) ->
  apply_parallel_execution(EtsRef, ParallelExecution, main_server),
  {keep_state, State};

predict(cast, {parallel_super_command, SuperCommand}, State = #client_statem_state{etsRef = EtsRef}) ->
  apply_parallel_super_command(EtsRef, SuperCommand, predict),
  {keep_state, State};

predict(cast, {worker_parallel_abort, WorkerName, Reason}, State = #client_statem_state{etsRef = EtsRef}) ->
  increment_worker_messages_sent(EtsRef, WorkerName, {worker_parallel_abort, WorkerName, Reason}),
  handle_worker_parallel_abort(EtsRef, WorkerName, Reason),
  {keep_state, State};

predict(cast, In = {sample,Body}, State = #client_statem_state{etsRef = EtsRef}) ->
  ClientStatsEts = get(client_stats_ets),
  stats:increment_messages_received(ClientStatsEts),
  stats:increment_bytes_received(ClientStatsEts , nerl_tools:calculate_size(In)),
  {SourceName , ClientName, WorkerNameStr, BatchID, BatchOfSamples} = binary_to_term(Body),
  WorkerName = list_to_atom(WorkerNameStr),
  WorkersEts = get(workers_ets),
  WorkerOfThisClient = ets:member(WorkersEts, WorkerName),
  if 
    WorkerOfThisClient -> 
      WorkerPid = clientWorkersFunctions:get_worker_pid(EtsRef , WorkerName),
      gen_statem:cast(WorkerPid, {sample, SourceName ,BatchID ,BatchOfSamples}),
      increment_worker_messages_received(EtsRef, WorkerName, {sample, SourceName, BatchID, BatchOfSamples}),
      stats:increment_messages_sent(ClientStatsEts),
      stats:increment_bytes_sent(ClientStatsEts , nerl_tools:calculate_size(BatchOfSamples)),
      perf_stats_memory_usage_update_predict();
    true -> ?LOG_ERROR("Given worker ~p isn't found in client ~p",[WorkerName, ClientName])
  end,
  {next_state, predict, State#client_statem_state{etsRef = EtsRef}};

% This action is used for start_stream triggered from a clients' worker and not source
predict(cast, {start_stream , {worker, WorkerName, TargetName}}, State = #client_statem_state{etsRef = EtsRef}) ->
  increment_worker_messages_sent(EtsRef, WorkerName, {start_stream, {worker, WorkerName, TargetName}}),
  ListOfActiveWorkersSources = ets:lookup_element(EtsRef, active_workers_streams, ?DATA_IDX),
  ets:update_element(EtsRef, active_workers_streams, {?DATA_IDX, ListOfActiveWorkersSources ++ [{WorkerName, TargetName}]}),
  mark_worker_stream_seen(EtsRef, WorkerName),

  perf_stats_memory_usage_update_predict(),
  {keep_state, State};

% This action is used for start_stream triggered from a source per worker
predict(cast, In = {start_stream , Data}, State = #client_statem_state{etsRef = EtsRef}) ->
  {SourceName, _ClientName, WorkerName} = binary_to_term(Data),
  ListOfActiveWorkersSources = ets:lookup_element(EtsRef, active_workers_streams, ?DATA_IDX),
  ets:update_element(EtsRef, active_workers_streams, {?DATA_IDX, ListOfActiveWorkersSources ++ [{WorkerName, SourceName}]}),
  mark_worker_stream_seen(EtsRef, WorkerName),
  ClientStatsEts = get(client_stats_ets),
  stats:increment_messages_received(ClientStatsEts),
  stats:increment_bytes_received(ClientStatsEts , nerl_tools:calculate_size(In)),
  WorkerPid = clientWorkersFunctions:get_worker_pid(EtsRef , WorkerName),
  gen_statem:cast(WorkerPid, {start_stream, SourceName}),
  increment_worker_messages_received(EtsRef, WorkerName, {start_stream, SourceName}),

  perf_stats_memory_usage_update_predict(),
  {keep_state, State};

predict(cast, In = {end_stream , Data}, State = #client_statem_state{etsRef = EtsRef}) ->
  {SourceName, _ClientName, WorkerName} = binary_to_term(Data),
  ClientStatsEts = get(client_stats_ets),
  stats:increment_messages_received(ClientStatsEts),
  stats:increment_bytes_received(ClientStatsEts , nerl_tools:calculate_size(In)),
  WorkerPid = clientWorkersFunctions:get_worker_pid(EtsRef , WorkerName),
  gen_statem:cast(WorkerPid, {end_stream, SourceName}),
  increment_worker_messages_received(EtsRef, WorkerName, {end_stream, SourceName}),

  perf_stats_memory_usage_update_predict(),
  {keep_state, State};

predict(cast, In = {stream_ended , Pair}, State = #client_statem_state{etsRef = EtsRef}) ->
  ClientStatsEts = get(client_stats_ets),
  stats:increment_messages_received(ClientStatsEts),
  stats:increment_bytes_received(ClientStatsEts , nerl_tools:calculate_size(In)),
  maybe_record_stream_ended_worker(EtsRef, Pair, In),
  ListOfActiveWorkersSources = ets:lookup_element(EtsRef, active_workers_streams, ?DATA_IDX),
  UpdatedListOfActiveWorkersSources = ListOfActiveWorkersSources -- [Pair],
  ets:update_element(EtsRef, active_workers_streams, {?DATA_IDX, UpdatedListOfActiveWorkersSources}),
  case refresh_all_workers_done_state(EtsRef) of
    true ->
      maybe_trigger_parallel_phase_close_on_stream_drain(EtsRef, prediction);
    false ->
      ok
  end,
  {next_state, predict, State#client_statem_state{etsRef = EtsRef}};


% From MainServer
predict(cast, In = {idle}, State = #client_statem_state{myName = MyName, etsRef = EtsRef}) ->
  ClientStatsEts = get(client_stats_ets),
  ClientPerformanceEts = get(performance_stats_ets),
  stats:increment_messages_received(ClientStatsEts),
  stats:increment_bytes_received(ClientStatsEts , nerl_tools:calculate_size(In)),
  ets:insert(EtsRef, {parallel_idle_requested, true}),
  WorkersDone = ets:lookup_element(EtsRef , all_workers_done , ?DATA_IDX),
  case WorkersDone of
    true ->
      case should_gate_idle_with_super_close(EtsRef) of
        true ->
          maybe_request_super_phase_close(EtsRef, prediction),
          case ets:lookup_element(EtsRef, parallel_phase_close_granted, ?DATA_IDX) of
            true ->
              finalize_predict_idle_transition(State, MyName, EtsRef, ClientStatsEts, ClientPerformanceEts);
            false ->
              ?LOG_INFO("~p waiting for Super Node phase-close grant before idling workers", [MyName]),
              {keep_state, State#client_statem_state{etsRef = EtsRef}}
          end;
        false ->
          finalize_predict_idle_transition(State, MyName, EtsRef, ClientStatsEts, ClientPerformanceEts)
      end;
    false ->  
              MyPid = get(my_pid),
              start_monitor_workers_done(EtsRef, MyPid), % ← Pass EtsRef
              {keep_state, State}
  end;

predict(cast, {parallel_finalize_idle}, State = #client_statem_state{myName = MyName, etsRef = EtsRef}) ->
  ClientStatsEts = get(client_stats_ets),
  ClientPerformanceEts = get(performance_stats_ets),
  maybe_finalize_parallel_idle_transition(predict, State, MyName, EtsRef, ClientStatsEts, ClientPerformanceEts);

predict(cast, In = {predictRes,WorkerName, SourceName ,{PredictNerlTensor, NetlTensorType} , TimeTook , WorkerToken, BatchID , BatchTS}, State = #client_statem_state{myName = _MyName, etsRef = EtsRef}) ->
  ClientStatsEts = get(client_stats_ets),
  ClientPerformanceEts = get(performance_stats_ets),
  stats:increment_messages_received(ClientStatsEts),
  stats:increment_bytes_received(ClientStatsEts , nerl_tools:calculate_size(In)),
  increment_worker_messages_sent(EtsRef, WorkerName, In),

  {RouterHost,RouterPort} = ets:lookup_element(EtsRef, my_router, ?DATA_IDX),
  MessageBody = {WorkerName, SourceName, {PredictNerlTensor , NetlTensorType}, TimeTook, WorkerToken, BatchID, BatchTS}, %% SHOULD INCLUDE TYPE?
  nerl_tools:http_router_request(RouterHost, RouterPort, [?MAIN_SERVER_ATOM], atom_to_list(predictRes), MessageBody),
  
  stats:increment_messages_sent(ClientStatsEts),
  stats:increment_bytes_sent(ClientStatsEts , nerl_tools:calculate_size(MessageBody)),
  stats:increment_time_predict_active(ClientPerformanceEts, trunc(TimeTook)), % in microseconds
  {next_state, predict, State#client_statem_state{etsRef = EtsRef}};

% TODO from predict directly to training?!?!?
predict(cast,_In = {training}, State = #client_statem_state{myName = MyName}) ->
  ClientStatsEts = get(client_stats_ets),
  stats:increment_bad_messages(ClientStatsEts),
  ?LOG_ERROR("client ~p got training request in predict state",[MyName]),
  {next_state, predict, State#client_statem_state{nextState = predict}};

predict(cast, In = {worker_to_worker_msg, FromWorker, ToWorker, Data}, State = #client_statem_state{etsRef = EtsRef}) ->
  ClientStatsEts = get(client_stats_ets),
  stats:increment_messages_received(ClientStatsEts),
  stats:increment_bytes_received(ClientStatsEts , nerl_tools:calculate_size(In)),
  handle_w2w_msg(EtsRef, FromWorker, ToWorker, Data),
  {keep_state, State};

predict(cast, In = {parallel_deliver, FromWorker, ToWorker, Data}, State = #client_statem_state{etsRef = EtsRef}) ->
  ClientStatsEts = get(client_stats_ets),
  stats:increment_messages_received(ClientStatsEts),
  stats:increment_bytes_received(ClientStatsEts , nerl_tools:calculate_size(In)),
  deliver_parallel_msg(EtsRef, FromWorker, ToWorker, Data),
  {keep_state, State};

predict(cast, In = {parallel_deliver, DeliveryId, FromWorker, ToWorker, Data}, State = #client_statem_state{etsRef = EtsRef}) ->
  ClientStatsEts = get(client_stats_ets),
  stats:increment_messages_received(ClientStatsEts),
  stats:increment_bytes_received(ClientStatsEts , nerl_tools:calculate_size(In)),
  handle_parallel_delivery_with_ack(EtsRef, DeliveryId, FromWorker, ToWorker, Data),
  {keep_state, State};

predict(cast, In = {parallel_event, FromWorker, Direction, BatchID, MicrobatchID, StageID, Meta}, State = #client_statem_state{etsRef = EtsRef}) ->
  ClientStatsEts = get(client_stats_ets),
  stats:increment_messages_received(ClientStatsEts),
  stats:increment_bytes_received(ClientStatsEts, nerl_tools:calculate_size(In)),
  forward_parallel_event(EtsRef, FromWorker, Direction, BatchID, MicrobatchID, StageID, Meta),
  {keep_state, State};

predict(cast, In = {parallel_skip_event, FromWorker, Direction, BatchID, MicrobatchID, StageID, Meta}, State = #client_statem_state{etsRef = EtsRef}) ->
  ClientStatsEts = get(client_stats_ets),
  stats:increment_messages_received(ClientStatsEts),
  stats:increment_bytes_received(ClientStatsEts, nerl_tools:calculate_size(In)),
  forward_parallel_skip_event(EtsRef, FromWorker, Direction, BatchID, MicrobatchID, StageID, Meta),
  {keep_state, State};

predict(cast, In = {parallel_worker_drain_ready, WorkerName, ModelPhase}, State = #client_statem_state{etsRef = EtsRef}) ->
  ClientStatsEts = get(client_stats_ets),
  stats:increment_messages_received(ClientStatsEts),
  stats:increment_bytes_received(ClientStatsEts, nerl_tools:calculate_size(In)),
  case handle_parallel_worker_drain_ready(EtsRef, WorkerName, ModelPhase) of
    {phase_complete, PhaseName} ->
      maybe_trigger_parallel_phase_close_on_stream_drain(EtsRef, PhaseName);
    _ ->
      ok
  end,
  {keep_state, State};

%% The source sends message to main server that it has finished
%% The main server updates its' clients to move to state 'idle'

predict(cast, EventContent, State = #client_statem_state{etsRef = EtsRef}) ->
  ClientStatsEts = get(client_stats_ets),
  stats:increment_bad_messages(ClientStatsEts),
  stats:increment_bytes_received(ClientStatsEts , nerl_tools:calculate_size(EventContent)),
  ?LOG_WARNING("client predict ignored:  ~p ~n",[EventContent]),
  {next_state, predict, State#client_statem_state{etsRef = EtsRef}}.


%% @private
%% @doc If callback_mode is handle_event_function, then whenever a
%% gen_statem receives an event from call/2,  cast/2, or as a normal
%% process message, this function is called.
handle_event(_EventType, _EventContent, _StateName, State = #client_statem_state{}) ->
NextStateName = the_next_state_name,
{next_state, NextStateName, State}.

%% @private
%% @doc This function is called by a gen_statem when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_statem terminates with
%% Reason. The return value is ignored.
terminate(_Reason, _StateName, _State = #client_statem_state{}) ->
ok.

%% @private
%% @doc Convert process state when code is changed
code_change(_OldVsn, StateName, State = #client_statem_state{}, _Extra) ->
{ok, StateName, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

increment_worker_messages_received(EtsRef, WorkerNameRaw, Payload) ->
  increment_worker_transport_counter(EtsRef, WorkerNameRaw, messages_received, bytes_received, Payload).

increment_worker_messages_sent(EtsRef, WorkerNameRaw, Payload) ->
  increment_worker_transport_counter(EtsRef, WorkerNameRaw, messages_sent, bytes_sent, Payload).

increment_worker_messages_dropped(EtsRef, WorkerNameRaw) ->
  increment_worker_transport_counter(EtsRef, WorkerNameRaw, messages_dropped, none, undefined).

increment_worker_transport_counter(EtsRef, WorkerNameRaw, CounterKey, BytesCounterKey, Payload) ->
  WorkersOfThisClient = ets:lookup_element(EtsRef, workersNames, ?DATA_IDX),
  case resolve_worker_name(WorkersOfThisClient, WorkerNameRaw) of
    {error, _} ->
      ok;
    {ok, WorkerName} ->
      EtsStats = get(ets_stats),
      case ets:lookup(EtsStats, WorkerName) of
        [] ->
          ok;
        [{_WorkerName, WorkerStatsEts}] ->
          safe_increment_worker_stat(WorkerStatsEts, CounterKey, 1),
          case BytesCounterKey of
            none ->
              ok;
            _ ->
              try nerl_tools:calculate_size(Payload) of
                Bytes when is_integer(Bytes), Bytes > 0 ->
                  safe_increment_worker_stat(WorkerStatsEts, BytesCounterKey, Bytes);
                _ ->
                  ok
              catch
                _:_ ->
                  ok
              end
          end
      end
  end.

safe_increment_worker_stat(WorkerStatsEts, Key, Value) ->
  try
    stats:increment_by_value(WorkerStatsEts, Key, Value),
    ok
  catch
    _:_ ->
      ok
  end.

maybe_record_stream_ended_worker(EtsRef, Pair, Payload) ->
  case Pair of
    {WorkerName, _SourceOrTarget} ->
      increment_worker_messages_sent(EtsRef, WorkerName, Payload);
    _ ->
      ok
  end.

% Sends to main server that client is ready
send_client_is_ready(MyName) ->
  EtsRef = get(client_data),
  {RouterHost,RouterPort} = ets:lookup_element(EtsRef, my_router, ?DATA_IDX),
  %%  send an ACK to mainserver that the client is ready
  nerl_tools:http_router_request(RouterHost, RouterPort, [?MAIN_SERVER_ATOM], atom_to_list(clientReady), MyName).


%% Check if monitor is running using ETS instead of process dictionary
query_monitor_workers_done_status(EtsRef) ->
  case ets:lookup(EtsRef, monitoring_workers_done_running) of
    [] -> false;
    [{monitoring_workers_done_running, Status}] -> Status
  end.


%% This function spawns a process that monitors when all workers are done
%% and then triggers the statem to send idle message to all workers
start_monitor_workers_done(EtsRef, ClientPid) ->
  MonitorStatus = query_monitor_workers_done_status(EtsRef),
  case MonitorStatus of
    true -> ok; % already running
    false ->
      % Update ETS that monitor is running
      ets:insert(EtsRef, {monitoring_workers_done_running, true}),
      spawn(fun() -> monitor_workers_done(EtsRef, ClientPid) end)
  end.

%% Sends idle to all workers when all are done
monitor_workers_done(EtsRef, ClientPid) ->
  WorkersDone = ets:lookup_element(EtsRef, all_workers_done, ?DATA_IDX),
  case WorkersDone of
    true -> 
      gen_statem:cast(ClientPid, {idle}),
      % Update ETS that monitor is not running
      ets:insert(EtsRef, {monitoring_workers_done_running, false});
    false -> 
      timer:sleep(10), 
      monitor_workers_done(EtsRef, ClientPid)
  end.

cast_message_to_workers(EtsRef, Msg) ->
  ClientStatsEts = get(client_stats_ets),
  Workers = ets:lookup_element(EtsRef, workersNames, ?ETS_KV_VAL_IDX),
  Func = fun(WorkerName) ->
    WorkerPid = clientWorkersFunctions:get_worker_pid(EtsRef, WorkerName), 
    gen_statem:cast(WorkerPid, Msg),
    increment_worker_messages_received(EtsRef, WorkerName, Msg),
    stats:increment_messages_sent(ClientStatsEts)
  end,
  lists:foreach(Func, Workers).

normalize_model_phase(training) -> training;
normalize_model_phase(train) -> training;
normalize_model_phase(prediction) -> prediction;
normalize_model_phase(predict) -> prediction;
normalize_model_phase(idle) -> idle;
normalize_model_phase(Value) when is_binary(Value) ->
  normalize_model_phase(binary_to_list(Value));
normalize_model_phase(Value) when is_list(Value) ->
  case string:lowercase(string:trim(Value)) of
    "training" -> training;
    "train" -> training;
    "prediction" -> prediction;
    "predict" -> prediction;
    "idle" -> idle;
    _ -> unknown
  end;
normalize_model_phase(_) ->
  unknown.

all_parallel_workers_drain_ready(EtsRef) ->
  Workers = ets:lookup_element(EtsRef, parallel_workers_stream_seen, ?DATA_IDX),
  DrainReadyWorkers = ets:lookup_element(EtsRef, parallel_workers_drain_ready, ?DATA_IDX),
  lists:all(fun(WorkerName) -> lists:member(WorkerName, DrainReadyWorkers) end, Workers).

mark_worker_stream_seen(EtsRef, WorkerNameRaw) ->
  WorkersOfThisClient = ets:lookup_element(EtsRef, workersNames, ?DATA_IDX),
  case resolve_worker_name(WorkersOfThisClient, WorkerNameRaw) of
    {error, _} ->
      ok;
    {ok, WorkerName} ->
      Existing = ets:lookup_element(EtsRef, parallel_workers_stream_seen, ?DATA_IDX),
      ets:update_element(
        EtsRef,
        parallel_workers_stream_seen,
        {?DATA_IDX, lists:usort([WorkerName | Existing])}
      )
  end.

refresh_all_workers_done_state(EtsRef) ->
  ActiveWorkersStreams = ets:lookup_element(EtsRef, active_workers_streams, ?DATA_IDX),
  StreamsDrained = (ActiveWorkersStreams =:= []),
  RequiresWorkerDrainAcks = should_gate_idle_with_super_close(EtsRef),
  WorkersDrained = all_parallel_workers_drain_ready(EtsRef),
  Done = StreamsDrained andalso ((not RequiresWorkerDrainAcks) orelse WorkersDrained),
  ets:update_element(EtsRef, all_workers_done, {?DATA_IDX, Done}),
  Done.

handle_parallel_worker_drain_ready(EtsRef, WorkerNameRaw, ModelPhaseRaw) ->
  increment_worker_messages_sent(
    EtsRef,
    WorkerNameRaw,
    {parallel_worker_drain_ready, WorkerNameRaw, ModelPhaseRaw}
  ),
  WorkersOfThisClient = ets:lookup_element(EtsRef, workersNames, ?DATA_IDX),
  case resolve_worker_name(WorkersOfThisClient, WorkerNameRaw) of
    {error, _} ->
      ignored;
    {ok, WorkerName} ->
      ActivePhase = normalize_model_phase(ets:lookup_element(EtsRef, active_phase_type, ?DATA_IDX)),
      ReportedPhase = normalize_model_phase(ModelPhaseRaw),
      case {ActivePhase, ReportedPhase} of
        {idle, _AnyPhase} ->
          ignored;
        {unknown, _AnyPhase} ->
          ignored;
        {Phase, Phase} ->
          Existing = ets:lookup_element(EtsRef, parallel_workers_drain_ready, ?DATA_IDX),
          ets:update_element(
            EtsRef,
            parallel_workers_drain_ready,
            {?DATA_IDX, lists:usort([WorkerName | Existing])}
          ),
          case refresh_all_workers_done_state(EtsRef) of
            true -> {phase_complete, Phase};
            false -> pending
          end;
        _ ->
          ignored
      end
  end.

maybe_trigger_parallel_phase_close_on_stream_drain(EtsRef, PhaseName) ->
  case should_gate_idle_with_super_close(EtsRef) of
    true ->
      maybe_request_super_phase_close(EtsRef, PhaseName),
      case ets:lookup_element(EtsRef, parallel_phase_close_granted, ?DATA_IDX) of
        true ->
          gen_statem:cast(get(my_pid), {parallel_finalize_idle});
        false ->
          ok
      end;
    false ->
      ok
  end.

reset_parallel_phase_close_state(EtsRef) ->
  ets:insert(EtsRef, {parallel_phase_close_requested, false}),
  ets:insert(EtsRef, {parallel_phase_close_granted, false}),
  ets:insert(EtsRef, {parallel_phase_close_last_request_ms, 0}),
  ets:insert(EtsRef, {parallel_idle_requested, false}),
  ets:insert(EtsRef, {parallel_workers_drain_ready, []}),
  ets:insert(EtsRef, {parallel_workers_stream_seen, []}),
  ok.

resolve_parallel_phase_epoch(EtsRef, current) ->
  ets:lookup_element(EtsRef, parallel_phase_epoch, ?DATA_IDX);
resolve_parallel_phase_epoch(EtsRef, EpochRaw) ->
  CurrentEpoch = ets:lookup_element(EtsRef, parallel_phase_epoch, ?DATA_IDX),
  ParsedEpoch =
    case EpochRaw of
      EpochInt when is_integer(EpochInt) -> EpochInt;
      EpochBin when is_binary(EpochBin) ->
        try list_to_integer(binary_to_list(EpochBin)) of
          Value -> Value
        catch
          _:_ -> CurrentEpoch
        end;
      EpochList when is_list(EpochList) ->
        try list_to_integer(string:trim(EpochList)) of
          Value -> Value
        catch
          _:_ -> CurrentEpoch
        end;
      _ ->
        CurrentEpoch
    end,
  case ParsedEpoch < 0 of
    true -> CurrentEpoch;
    false -> ParsedEpoch
  end.

should_gate_idle_with_super_close(EtsRef) ->
  ParallelMode = ets:lookup_element(EtsRef, parallel_mode, ?DATA_IDX),
  SuperNode = ets:lookup_element(EtsRef, super_node, ?DATA_IDX),
  ParallelAuthority = get_parallel_authority(EtsRef),
  (ParallelMode =/= legacy) andalso (SuperNode =/= none) andalso (ParallelAuthority =:= super_node).

maybe_request_super_phase_close(EtsRef, PhaseName) ->
  Requested = ets:lookup_element(EtsRef, parallel_phase_close_requested, ?DATA_IDX),
  CloseGranted = ets:lookup_element(EtsRef, parallel_phase_close_granted, ?DATA_IDX),
  LastRequestMs = ets:lookup_element(EtsRef, parallel_phase_close_last_request_ms, ?DATA_IDX),
  NowMs = erlang:system_time(millisecond),
  ShouldSend =
    case {Requested, CloseGranted} of
      {_AnyRequested, true} -> false;
      {false, false} -> true;
      {true, false} -> (NowMs - LastRequestMs) >= ?PHASE_CLOSE_RETRY_MS
    end,
  case ShouldSend of
    false ->
      ok;
    true ->
      ClientName = ets:lookup_element(EtsRef, myName, ?DATA_IDX),
      SuperNode = ets:lookup_element(EtsRef, super_node, ?DATA_IDX),
      PhaseEpoch = ets:lookup_element(EtsRef, parallel_phase_epoch, ?DATA_IDX),
      case SuperNode of
        none ->
          ?LOG_WARNING(
            "Client ~p cannot request phase-close (phase=~p epoch=~p): missing super node",
            [ClientName, PhaseName, PhaseEpoch]
          ),
          ets:insert(EtsRef, {parallel_phase_close_requested, true}),
          ets:insert(EtsRef, {parallel_phase_close_granted, true}),
          ets:insert(EtsRef, {parallel_phase_close_last_request_ms, NowMs});
        _ ->
          MessageBody = {parallel_phase_close, ClientName, PhaseEpoch},
          {RouterHost,RouterPort} = ets:lookup_element(EtsRef, my_router, ?DATA_IDX),
          ?LOG_INFO(
            "Client ~p sending phase-close request to super node ~p phase=~p epoch=~p requested_before=~p",
            [ClientName, SuperNode, PhaseName, PhaseEpoch, Requested]
          ),
          try
            nerl_tools:http_router_request(
              RouterHost,
              RouterPort,
              [SuperNode],
              atom_to_list(parallelPhaseClose),
              MessageBody
            ),
            ets:insert(EtsRef, {parallel_phase_close_requested, true}),
            ets:insert(EtsRef, {parallel_phase_close_last_request_ms, NowMs})
          catch
            Err:Reason ->
              ?LOG_ERROR(
                "Client ~p failed to request phase-close from super node ~p: ~p",
                [ClientName, SuperNode, {Err, Reason}]
              ),
              notify_parallel_abort(EtsRef, {parallel_phase_close_route_failed, SuperNode, PhaseName, {Err, Reason}})
          end
      end
  end.

maybe_finalize_parallel_idle_transition(training, State, MyName, EtsRef, ClientStatsEts, ClientPerformanceEts) ->
  IdleRequested = ets:lookup_element(EtsRef, parallel_idle_requested, ?DATA_IDX),
  WorkersDone = ets:lookup_element(EtsRef, all_workers_done, ?DATA_IDX),
  CloseGranted = ets:lookup_element(EtsRef, parallel_phase_close_granted, ?DATA_IDX),
  case {IdleRequested, WorkersDone, should_gate_idle_with_super_close(EtsRef), CloseGranted} of
    {true, true, false, _} ->
      finalize_training_idle_transition(State, MyName, EtsRef, ClientStatsEts, ClientPerformanceEts);
    {true, _, true, true} ->
      finalize_training_idle_transition(State, MyName, EtsRef, ClientStatsEts, ClientPerformanceEts);
    _ ->
      {keep_state, State#client_statem_state{etsRef = EtsRef}}
  end;
maybe_finalize_parallel_idle_transition(predict, State, MyName, EtsRef, ClientStatsEts, ClientPerformanceEts) ->
  IdleRequested = ets:lookup_element(EtsRef, parallel_idle_requested, ?DATA_IDX),
  WorkersDone = ets:lookup_element(EtsRef, all_workers_done, ?DATA_IDX),
  CloseGranted = ets:lookup_element(EtsRef, parallel_phase_close_granted, ?DATA_IDX),
  case {IdleRequested, WorkersDone, should_gate_idle_with_super_close(EtsRef), CloseGranted} of
    {true, true, false, _} ->
      finalize_predict_idle_transition(State, MyName, EtsRef, ClientStatsEts, ClientPerformanceEts);
    {true, _, true, true} ->
      finalize_predict_idle_transition(State, MyName, EtsRef, ClientStatsEts, ClientPerformanceEts);
    _ ->
      {keep_state, State#client_statem_state{etsRef = EtsRef}}
  end.

finalize_training_idle_transition(
  State = #client_statem_state{},
  MyName,
  EtsRef,
  ClientStatsEts,
  ClientPerformanceEts
) ->
  ets:update_element(EtsRef, active_phase_type, {?DATA_IDX, idle}),
  cast_message_to_workers(EtsRef, {idle}),
  Workers = clientWorkersFunctions:get_workers_names(EtsRef),
  ?LOG_INFO("~p sent idle to workers: ~p , waiting for confirmation...~n",[MyName, ets:lookup_element(EtsRef, workersNames, ?DATA_IDX)]),
  Elapsed = stats:toc(ClientStatsEts, time_train_total),
  stats:increment_time_train_total(ClientPerformanceEts, Elapsed),
  stats:update_cpu_util_per_core(ClientPerformanceEts, train),
  {next_state, waitforWorkers, State#client_statem_state{etsRef = EtsRef, waitforWorkers = Workers , nextState = idle}}.

finalize_predict_idle_transition(
  State = #client_statem_state{},
  MyName,
  EtsRef,
  ClientStatsEts,
  ClientPerformanceEts
) ->
  ets:update_element(EtsRef, active_phase_type, {?DATA_IDX, idle}),
  cast_message_to_workers(EtsRef, {idle}),
  Workers = clientWorkersFunctions:get_workers_names(EtsRef),
  ?LOG_INFO("~p sent idle to workers: ~p , waiting for confirmation...~n",[MyName, ets:lookup_element(EtsRef, workersNames, ?DATA_IDX)]),
  Elapsed = stats:toc(ClientStatsEts, time_predict_total),
  stats:increment_time_predict_total(ClientPerformanceEts, Elapsed),
  stats:update_cpu_util_per_core(ClientPerformanceEts, predict),
  {next_state, waitforWorkers, State#client_statem_state{etsRef = EtsRef, waitforWorkers = Workers , nextState = idle}}.

scheduler_grant_reject_reason(EtsRef, GrantEpoch, StateName) ->
  ActiveEpoch = ets:lookup_element(EtsRef, parallel_phase_epoch, ?DATA_IDX),
  IdleRequested = ets:lookup_element(EtsRef, parallel_idle_requested, ?DATA_IDX),
  WorkersDone = ets:lookup_element(EtsRef, all_workers_done, ?DATA_IDX),
  CloseRequested = ets:lookup_element(EtsRef, parallel_phase_close_requested, ?DATA_IDX),
  CloseGranted = ets:lookup_element(EtsRef, parallel_phase_close_granted, ?DATA_IDX),
  case GrantEpoch =:= ActiveEpoch of
    false ->
      {stale_phase_epoch, GrantEpoch, ActiveEpoch};
    true ->
      case {StateName, IdleRequested, WorkersDone, CloseRequested, CloseGranted} of
        {idle, _AnyIdleRequested, _AnyWorkersDone, _AnyCloseRequested, _AnyCloseGranted} ->
          {client_idle_state, StateName};
        {waitforWorkers, true, _AnyWorkersDone, _AnyCloseRequested, _AnyCloseGranted} ->
          {phase_close_waiting_worker_idle_ack, StateName};
        {_AnyState, _AnyIdleRequested, _AnyWorkersDone, true, true} ->
          {phase_close_granted, StateName};
        {_AnyState, _AnyIdleRequested, _AnyWorkersDone, true, false} ->
          {phase_close_pending, StateName};
        _ -> none
      end
  end.

notify_scheduler_grant_rejected(
  EtsRef,
  WorkerName,
  Direction,
  BatchID,
  MicrobatchID,
  StageID,
  GrantEpoch,
  RejectReason
) ->
  SuperNode = ets:lookup_element(EtsRef, super_node, ?DATA_IDX),
  ClientName = ets:lookup_element(EtsRef, myName, ?DATA_IDX),
  case SuperNode of
    none ->
      ?LOG_WARNING(
        "Client ~p rejected scheduler grant but cannot notify super node (missing super): worker=~p direction=~p batch=~p microbatch=~p stage=~p epoch=~p reason=~p",
        [ClientName, WorkerName, Direction, BatchID, MicrobatchID, StageID, GrantEpoch, RejectReason]
      );
    _ ->
      ?LOG_INFO(
        "Client ~p rejecting scheduler grant worker=~p direction=~p batch=~p microbatch=~p stage=~p epoch=~p reason=~p",
        [ClientName, WorkerName, Direction, BatchID, MicrobatchID, StageID, GrantEpoch, RejectReason]
      ),
      MessageBody = {scheduler_grant_rejected, ClientName, WorkerName, Direction, BatchID, MicrobatchID, StageID, RejectReason, GrantEpoch},
      {RouterHost,RouterPort} = ets:lookup_element(EtsRef, my_router, ?DATA_IDX),
      try
        nerl_tools:http_router_request(
          RouterHost,
          RouterPort,
          [SuperNode],
          atom_to_list(schedulerGrantRejected),
          MessageBody
        )
      catch
        Err:Reason ->
          ?LOG_ERROR(
            "Client ~p failed notifying scheduler grant rejection to super node ~p: ~p",
            [ClientName, SuperNode, {Err, Reason}]
          ),
          notify_parallel_abort(EtsRef, {scheduler_grant_rejection_route_failed, SuperNode, {Err, Reason}})
      end
  end.

notify_scheduler_grant_accepted(
  EtsRef,
  WorkerName,
  Direction,
  BatchID,
  MicrobatchID,
  StageID,
  GrantEpoch
) ->
  SuperNode = ets:lookup_element(EtsRef, super_node, ?DATA_IDX),
  ClientName = ets:lookup_element(EtsRef, myName, ?DATA_IDX),
  case SuperNode of
    none ->
      ok;
    _ ->
      MessageBody = {scheduler_grant_accepted, ClientName, WorkerName, Direction, BatchID, MicrobatchID, StageID, GrantEpoch},
      {RouterHost,RouterPort} = ets:lookup_element(EtsRef, my_router, ?DATA_IDX),
      try
        nerl_tools:http_router_request(
          RouterHost,
          RouterPort,
          [SuperNode],
          atom_to_list(schedulerGrantAccepted),
          MessageBody
        )
      catch
        _:_ ->
          ok
      end
  end.

notify_parallel_skip_ack(
  EtsRef,
  WorkerName,
  Direction,
  BatchID,
  MicrobatchID,
  StageID,
  Reason,
  EventID,
  PhaseEpoch
) ->
  SuperNode = ets:lookup_element(EtsRef, super_node, ?DATA_IDX),
  ClientName = ets:lookup_element(EtsRef, myName, ?DATA_IDX),
  case SuperNode of
    none ->
      ok;
    _ ->
      MessageBody = {parallel_skip_ack, ClientName, WorkerName, Direction, BatchID, MicrobatchID, StageID, Reason, EventID, PhaseEpoch},
      {RouterHost,RouterPort} = ets:lookup_element(EtsRef, my_router, ?DATA_IDX),
      try
        nerl_tools:http_router_request(
          RouterHost,
          RouterPort,
          [SuperNode],
          atom_to_list(parallelSkipAck),
          MessageBody
        )
      catch
        _:_ ->
          ok
      end
  end.

notify_parallel_skip_relay_failed(
  EtsRef,
  WorkerName,
  Direction,
  BatchID,
  MicrobatchID,
  StageID,
  Reason,
  EventID,
  PhaseEpoch
) ->
  SuperNode = ets:lookup_element(EtsRef, super_node, ?DATA_IDX),
  ClientName = ets:lookup_element(EtsRef, myName, ?DATA_IDX),
  case SuperNode of
    none ->
      ok;
    _ ->
      MessageBody = {parallel_skip_relay_failed, ClientName, WorkerName, Direction, BatchID, MicrobatchID, StageID, Reason, EventID, PhaseEpoch},
      {RouterHost,RouterPort} = ets:lookup_element(EtsRef, my_router, ?DATA_IDX),
      try
        nerl_tools:http_router_request(
          RouterHost,
          RouterPort,
          [SuperNode],
          atom_to_list(parallelSkipRelayFailed),
          MessageBody
        )
      catch
        _:_ ->
          ok
      end
  end.

apply_parallel_mode(EtsRef, Mode, SourceRaw) ->
  Source = normalize_parallel_source(SourceRaw),
  NormalizedMode = normalize_parallel_mode(Mode),
  CurrentAuthority = get_parallel_authority(EtsRef),
  case should_accept_parallel_update(CurrentAuthority, Source, NormalizedMode) of
    false ->
      ok;
    true ->
      ets:update_element(EtsRef, parallel_mode, {?DATA_IDX, NormalizedMode}),
      UpdatedAuthority = resolve_parallel_authority(Source, NormalizedMode),
      ets:update_element(EtsRef, parallel_authority, {?DATA_IDX, UpdatedAuthority}),
      ClientName = ets:lookup_element(EtsRef, myName, ?DATA_IDX),
      ?LOG_INFO(
        "Client ~p parallel mode update source=~p mode=~p authority=~p",
        [ClientName, Source, NormalizedMode, UpdatedAuthority]
      ),
      case NormalizedMode of
        legacy ->
          reset_parallel_phase_close_state(EtsRef),
          ets:insert(EtsRef, {parallel_phase_epoch, 0}),
          ets:update_element(EtsRef, parallel_execution, {?DATA_IDX, #{}}),
          cast_message_to_workers(EtsRef, {set_parallel_mode, legacy}),
          cast_message_to_workers(EtsRef, {set_parallel_execution, #{}}),
          cast_message_to_workers(EtsRef, {set_parallel_authority, false});
        _ ->
          cast_message_to_workers(EtsRef, {set_parallel_mode, NormalizedMode}),
          cast_message_to_workers(EtsRef, {set_parallel_authority, Source =:= super_node})
      end
  end.

apply_parallel_execution(EtsRef, ParallelExecution, SourceRaw) ->
  Source = normalize_parallel_source(SourceRaw),
  Mode = ets:lookup_element(EtsRef, parallel_mode, ?DATA_IDX),
  CurrentAuthority = get_parallel_authority(EtsRef),
  NormalizedExecution = case is_map(ParallelExecution) of
                          true -> ParallelExecution;
                          false -> #{}
                        end,
  case should_accept_parallel_update(CurrentAuthority, Source, Mode) of
    false ->
      ok;
    true ->
      case Mode of
        legacy ->
          ets:update_element(EtsRef, parallel_execution, {?DATA_IDX, #{}}),
          ClientName = ets:lookup_element(EtsRef, myName, ?DATA_IDX),
          ?LOG_INFO("Client ~p cleared parallel execution (legacy mode)", [ClientName]),
          cast_message_to_workers(EtsRef, {set_parallel_execution, #{}});
        _ ->
          ets:update_element(EtsRef, parallel_execution, {?DATA_IDX, NormalizedExecution}),
          ClientName = ets:lookup_element(EtsRef, myName, ?DATA_IDX),
          ?LOG_INFO(
            "Client ~p parallel execution update source=~p mode=~p payload=~p",
            [ClientName, Source, Mode, NormalizedExecution]
          ),
          cast_message_to_workers(EtsRef, {set_parallel_execution, NormalizedExecution})
      end
  end.

apply_parallel_super_command(EtsRef, {parallel_super_command, configure_parallel, Mode, ParallelExecution}, StateName) ->
  apply_parallel_super_command(EtsRef, {parallel_super_command, configure_parallel, Mode, ParallelExecution, current}, StateName);
apply_parallel_super_command(EtsRef, {configure_parallel, Mode, ParallelExecution}, StateName) ->
  apply_parallel_super_command(EtsRef, {parallel_super_command, configure_parallel, Mode, ParallelExecution, current}, StateName);
apply_parallel_super_command(EtsRef, {parallel_super_command, configure_parallel, Mode, ParallelExecution, PhaseEpochRaw}, _StateName) ->
  ClientName = ets:lookup_element(EtsRef, myName, ?DATA_IDX),
  PhaseEpoch = resolve_parallel_phase_epoch(EtsRef, PhaseEpochRaw),
  ?LOG_INFO("Client ~p received super command configure_parallel mode=~p epoch=~p", [ClientName, Mode, PhaseEpoch]),
  apply_parallel_mode(EtsRef, Mode, super_node),
  apply_parallel_execution(EtsRef, ParallelExecution, super_node),
  reset_parallel_phase_close_state(EtsRef),
  ets:insert(EtsRef, {parallel_phase_epoch, PhaseEpoch});
apply_parallel_super_command(
  EtsRef,
  {parallel_super_command, grant_scheduler_event, Direction, BatchID, MicrobatchID, StageID, WorkerName},
  StateName
) ->
  apply_parallel_super_command(
    EtsRef,
    {parallel_super_command, grant_scheduler_event, Direction, BatchID, MicrobatchID, StageID, WorkerName, current},
    StateName
  );
apply_parallel_super_command(
  EtsRef,
  {parallel_super_command, grant_scheduler_event, Direction, MicrobatchID, StageID, WorkerName},
  StateName
) ->
  apply_parallel_super_command(
    EtsRef,
    {parallel_super_command, grant_scheduler_event, Direction, any, MicrobatchID, StageID, WorkerName, current},
    StateName
  );
apply_parallel_super_command(
  EtsRef,
  {grant_scheduler_event, Direction, BatchID, MicrobatchID, StageID, WorkerName},
  StateName
) ->
  apply_parallel_super_command(
    EtsRef,
    {parallel_super_command, grant_scheduler_event, Direction, BatchID, MicrobatchID, StageID, WorkerName, current},
    StateName
  );
apply_parallel_super_command(
  EtsRef,
  {grant_scheduler_event, Direction, MicrobatchID, StageID, WorkerName},
  StateName
) ->
  apply_parallel_super_command(
    EtsRef,
    {parallel_super_command, grant_scheduler_event, Direction, any, MicrobatchID, StageID, WorkerName, current},
    StateName
  );
apply_parallel_super_command(
  EtsRef,
  {parallel_super_command, grant_scheduler_event, Direction, BatchID, MicrobatchID, StageID, WorkerName, GrantEpochRaw},
  StateName
) ->
  GrantEpoch = resolve_parallel_phase_epoch(EtsRef, GrantEpochRaw),
  ClientName = ets:lookup_element(EtsRef, myName, ?DATA_IDX),
  ?LOG_INFO(
    "Client ~p received scheduler grant direction=~p batch=~p microbatch=~p stage=~p target=~p epoch=~p",
    [ClientName, Direction, BatchID, MicrobatchID, StageID, WorkerName, GrantEpoch]
  ),
  deliver_scheduler_grant(EtsRef, Direction, BatchID, MicrobatchID, StageID, WorkerName, GrantEpoch, StateName);
apply_parallel_super_command(
  EtsRef,
  {parallel_super_command, skip_work_item, Direction, BatchID, MicrobatchID, StageID, WorkerName, Reason, EventID, PhaseEpochRaw},
  _StateName
) ->
  PhaseEpoch = resolve_parallel_phase_epoch(EtsRef, PhaseEpochRaw),
  deliver_skip_work_item(EtsRef, Direction, BatchID, MicrobatchID, StageID, WorkerName, Reason, EventID, PhaseEpoch);
apply_parallel_super_command(
  EtsRef,
  {parallel_super_command, skip_work_item, Direction, BatchID, MicrobatchID, StageID, WorkerName, Reason, EventID},
  StateName
) ->
  apply_parallel_super_command(
    EtsRef,
    {parallel_super_command, skip_work_item, Direction, BatchID, MicrobatchID, StageID, WorkerName, Reason, EventID, current},
    StateName
  );
apply_parallel_super_command(EtsRef, {parallel_super_command, phase_close_granted, PhaseEpochRaw}, _StateName) ->
  PhaseEpoch = resolve_parallel_phase_epoch(EtsRef, PhaseEpochRaw),
  ActiveEpoch = ets:lookup_element(EtsRef, parallel_phase_epoch, ?DATA_IDX),
  ClientName = ets:lookup_element(EtsRef, myName, ?DATA_IDX),
  case PhaseEpoch =:= ActiveEpoch of
    false ->
      ?LOG_WARNING(
        "Client ~p ignored stale phase-close grant epoch=~p active_epoch=~p",
        [ClientName, PhaseEpoch, ActiveEpoch]
      );
    true ->
      ?LOG_INFO("Client ~p received phase-close grant epoch=~p", [ClientName, PhaseEpoch]),
      ets:insert(EtsRef, {parallel_phase_close_granted, true}),
      ets:insert(EtsRef, {parallel_idle_requested, true}),
      ets:update_element(EtsRef, active_workers_streams, {?DATA_IDX, []}),
      refresh_all_workers_done_state(EtsRef),
      gen_statem:cast(get(my_pid), {parallel_finalize_idle})
  end;
apply_parallel_super_command(EtsRef, {phase_close_granted, PhaseEpochRaw}, StateName) ->
  apply_parallel_super_command(EtsRef, {parallel_super_command, phase_close_granted, PhaseEpochRaw}, StateName);
apply_parallel_super_command(_EtsRef, UnknownCommand, _StateName) ->
  ?LOG_WARNING("Ignoring unknown parallel super command: ~p", [UnknownCommand]),
  ok.

deliver_scheduler_grant(EtsRef, Direction, BatchID, MicrobatchID, StageID, WorkerNameRaw, GrantEpoch, StateName) ->
  case scheduler_grant_reject_reason(EtsRef, GrantEpoch, StateName) of
    none ->
      deliver_scheduler_grant_to_worker(EtsRef, Direction, BatchID, MicrobatchID, StageID, WorkerNameRaw, GrantEpoch);
    RejectReason ->
      notify_scheduler_grant_rejected(
        EtsRef,
        WorkerNameRaw,
        Direction,
        BatchID,
        MicrobatchID,
        StageID,
        GrantEpoch,
        RejectReason
      )
  end.

deliver_scheduler_grant_to_worker(EtsRef, Direction, BatchID, MicrobatchID, StageID, WorkerNameRaw, GrantEpoch) ->
  WorkersOfThisClient = ets:lookup_element(EtsRef, workersNames, ?DATA_IDX),
  case resolve_worker_name(WorkersOfThisClient, WorkerNameRaw) of
    {error, _} ->
      notify_parallel_abort(EtsRef, {scheduler_grant_non_local_worker, WorkerNameRaw, Direction, BatchID, MicrobatchID, StageID});
    {ok, WorkerName} ->
      ClientName = ets:lookup_element(EtsRef, myName, ?DATA_IDX),
      ?LOG_INFO(
        "Client ~p forwarding scheduler grant to worker ~p direction=~p batch=~p microbatch=~p stage=~p epoch=~p",
        [ClientName, WorkerName, Direction, BatchID, MicrobatchID, StageID, GrantEpoch]
      ),
      WorkerPid = clientWorkersFunctions:get_worker_pid(EtsRef, WorkerName),
      gen_statem:cast(
        WorkerPid,
        {parallel_scheduler_grant, normalize_parallel_direction(Direction), BatchID, MicrobatchID, StageID}
      ),
      increment_worker_messages_received(
        EtsRef,
        WorkerName,
        {parallel_scheduler_grant, normalize_parallel_direction(Direction), BatchID, MicrobatchID, StageID}
      ),
      notify_scheduler_grant_accepted(
        EtsRef,
        WorkerName,
        Direction,
        BatchID,
        MicrobatchID,
        StageID,
        GrantEpoch
      )
  end.

deliver_skip_work_item(EtsRef, Direction, BatchID, MicrobatchID, StageID, WorkerNameRaw, Reason, EventID, PhaseEpoch) ->
  WorkersOfThisClient = ets:lookup_element(EtsRef, workersNames, ?DATA_IDX),
  case resolve_worker_name(WorkersOfThisClient, WorkerNameRaw) of
    {error, _} ->
      notify_parallel_skip_relay_failed(
        EtsRef,
        WorkerNameRaw,
        Direction,
        BatchID,
        MicrobatchID,
        StageID,
        {non_local_worker, Reason},
        EventID,
        PhaseEpoch
      );
    {ok, WorkerName} ->
      WorkerPid = clientWorkersFunctions:get_worker_pid(EtsRef, WorkerName),
      gen_statem:cast(
        WorkerPid,
        {parallel_skip_work_item, normalize_parallel_direction(Direction), BatchID, MicrobatchID, StageID, Reason, EventID, PhaseEpoch}
      ),
      increment_worker_messages_received(
        EtsRef,
        WorkerName,
        {parallel_skip_work_item, normalize_parallel_direction(Direction), BatchID, MicrobatchID, StageID, Reason, EventID, PhaseEpoch}
      ),
      notify_parallel_skip_ack(
        EtsRef,
        WorkerName,
        Direction,
        BatchID,
        MicrobatchID,
        StageID,
        Reason,
        EventID,
        PhaseEpoch
      )
  end.

normalize_parallel_direction(Direction) when is_atom(Direction) ->
  Direction;
normalize_parallel_direction(Direction) when is_binary(Direction) ->
  normalize_parallel_direction(binary_to_list(Direction));
normalize_parallel_direction(Direction) when is_list(Direction) ->
  case string:lowercase(string:trim(Direction)) of
    "backward" -> backward;
    _ -> forward
  end;
normalize_parallel_direction(_) ->
  forward.

resolve_worker_name(Workers, WorkerNameRaw) when is_atom(WorkerNameRaw) ->
  case lists:member(WorkerNameRaw, Workers) of
    true -> {ok, WorkerNameRaw};
    false -> {error, unknown_worker}
  end;
resolve_worker_name(Workers, WorkerNameRaw) ->
  CandidateText = worker_name_to_text(WorkerNameRaw),
  resolve_worker_name_by_text(Workers, CandidateText).

resolve_worker_name_by_text([], _CandidateText) ->
  {error, unknown_worker};
resolve_worker_name_by_text([WorkerName | Rest], CandidateText) ->
  case worker_name_to_text(WorkerName) =:= CandidateText of
    true -> {ok, WorkerName};
    false -> resolve_worker_name_by_text(Rest, CandidateText)
  end.

worker_name_to_text(WorkerName) when is_atom(WorkerName) ->
  atom_to_list(WorkerName);
worker_name_to_text(WorkerName) when is_binary(WorkerName) ->
  binary_to_list(WorkerName);
worker_name_to_text(WorkerName) when is_list(WorkerName) ->
  WorkerName;
worker_name_to_text(WorkerName) ->
  lists:flatten(io_lib:format("~p", [WorkerName])).

normalize_parallel_source(super_node) -> super_node;
normalize_parallel_source(main_server) -> main_server;
normalize_parallel_source(_) -> main_server.

should_accept_parallel_update(_CurrentAuthority, _Source, legacy) ->
  true;
should_accept_parallel_update(super_node, main_server, _Mode) ->
  false;
should_accept_parallel_update(_CurrentAuthority, _Source, _Mode) ->
  true.

resolve_parallel_authority(_Source, legacy) ->
  main_server;
resolve_parallel_authority(super_node, _Mode) ->
  super_node;
resolve_parallel_authority(_Source, _Mode) ->
  main_server.

get_parallel_authority(EtsRef) ->
  case ets:lookup(EtsRef, parallel_authority) of
    [{parallel_authority, Authority}] -> Authority;
    _ -> main_server
  end.

handle_worker_parallel_abort(EtsRef, WorkerName, Reason) ->
  notify_parallel_abort(EtsRef, {worker_parallel_abort, WorkerName, Reason}).

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

create_encoded_stats_str(ListStatsEts) ->
  Func = fun({WorkerName , StatsEts}) ->
    WorkerEncStatsStr = stats:encode_workers_ets_to_http_bin_str(StatsEts),
    %% |w1&bytes_sent:6.0:float#bad_messages:0:int....|
    ?API_SERVER_ENTITY_SEPERATOR ++ atom_to_list(WorkerName) ++ ?WORKER_SEPERATOR ++ WorkerEncStatsStr
    end,
  lists:flatten(lists:map(Func , ListStatsEts)).

handle_w2w_msg(EtsRef, FromWorker, ToWorker, Data) ->
  increment_worker_messages_sent(EtsRef, FromWorker, {worker_to_worker_msg, FromWorker, ToWorker, Data}),
  ParallelMode = case ets:lookup(EtsRef, parallel_mode) of
                   [] -> legacy;
                   [{parallel_mode, Mode}] -> Mode
                 end,
  case ParallelMode of
    legacy -> handle_w2w_msg_legacy(EtsRef, FromWorker, ToWorker, Data);
    _Else -> handle_w2w_msg_super(EtsRef, FromWorker, ToWorker, Data)
  end.

handle_w2w_msg_legacy(EtsRef, FromWorker, ToWorker, Data) ->
  ClientStatsEts = get(client_stats_ets),
  WorkersOfThisClient = ets:lookup_element(EtsRef, workersNames, ?DATA_IDX),
  WorkerOfThisClient = lists:member(ToWorker, WorkersOfThisClient),
  case WorkerOfThisClient of
    true -> 
      % Extract W2WPID from Ets
      W2WPidsMap = ets:lookup_element(EtsRef, w2wcom_pids, ?DATA_IDX),
      TargetWorkerW2WPID = maps:get(ToWorker, W2WPidsMap),
      {ok, _Reply} = gen_server:call(TargetWorkerW2WPID, {worker_to_worker_msg, FromWorker, ToWorker, Data}),
      increment_worker_messages_received(EtsRef, ToWorker, {worker_to_worker_msg, FromWorker, ToWorker, Data}),
      stats:increment_messages_sent(ClientStatsEts);
    _ ->
      %% Send to the correct client
      DestClient = maps:get(ToWorker, ets:lookup_element(EtsRef, workerToClient, ?ETS_KV_VAL_IDX)),
      % ClientName = ets:lookup_element(EtsRef, myName , ?DATA_IDX),
      MessageBody = {worker_to_worker_msg, FromWorker, ToWorker, Data},
      {RouterHost,RouterPort} = ets:lookup_element(EtsRef, my_router, ?DATA_IDX),
      nerl_tools:http_router_request(RouterHost, RouterPort, [DestClient], atom_to_list(worker_to_worker_msg), MessageBody),
      stats:increment_messages_sent(ClientStatsEts),
      stats:increment_bytes_sent(ClientStatsEts , nerl_tools:calculate_size(MessageBody))
  end.

handle_w2w_msg_super(EtsRef, FromWorker, ToWorker, Data) ->
  ClientStatsEts = get(client_stats_ets),
  SuperNode = ets:lookup_element(EtsRef, super_node, ?DATA_IDX),
  case SuperNode of
    none ->
      notify_parallel_abort(EtsRef, {missing_super_node, FromWorker, ToWorker}),
      stats:increment_bad_messages(ClientStatsEts);
    _ ->
      ClientName = ets:lookup_element(EtsRef, myName, ?DATA_IDX),
      ?LOG_INFO(
        "Client ~p routing worker message via super node ~p from ~p to ~p",
        [ClientName, SuperNode, FromWorker, ToWorker]
      ),
      MessageBody = {parallel_worker_message, FromWorker, ToWorker, Data},
      {RouterHost,RouterPort} = ets:lookup_element(EtsRef, my_router, ?DATA_IDX),
      try
        nerl_tools:http_router_request(RouterHost, RouterPort, [SuperNode], atom_to_list(parallelWorkerMessage), MessageBody),
        stats:increment_messages_sent(ClientStatsEts),
        stats:increment_bytes_sent(ClientStatsEts , nerl_tools:calculate_size(MessageBody))
      catch
        Err:Reason ->
          ?LOG_ERROR("Failed sending parallel worker message through super node ~p: ~p", [SuperNode, {Err, Reason}]),
          notify_parallel_abort(EtsRef, {super_route_failed, SuperNode, FromWorker, ToWorker, {Err, Reason}}),
          stats:increment_bad_messages(ClientStatsEts)
      end
  end.

deliver_parallel_msg(EtsRef, FromWorker, ToWorker, Data) ->
  ClientStatsEts = get(client_stats_ets),
  WorkersOfThisClient = ets:lookup_element(EtsRef, workersNames, ?DATA_IDX),
  case lists:member(ToWorker, WorkersOfThisClient) of
    true ->
      W2WPidsMap = ets:lookup_element(EtsRef, w2wcom_pids, ?DATA_IDX),
      case maps:get(ToWorker, W2WPidsMap, undefined) of
        undefined ->
          notify_parallel_abort(EtsRef, {parallel_target_w2w_missing, ToWorker, FromWorker}),
          stats:increment_bad_messages(ClientStatsEts),
          increment_worker_messages_dropped(EtsRef, ToWorker),
          {error, {parallel_target_w2w_missing, ToWorker, FromWorker}};
        TargetWorkerW2WPID ->
          try
            {ok, _Reply} = gen_server:call(TargetWorkerW2WPID, {worker_to_worker_msg, FromWorker, ToWorker, Data}),
            increment_worker_messages_received(EtsRef, ToWorker, {parallel_deliver, FromWorker, ToWorker, Data}),
            stats:increment_messages_sent(ClientStatsEts),
            stats:increment_bytes_sent(ClientStatsEts, nerl_tools:calculate_size({FromWorker, ToWorker, Data})),
            ok
          catch
            Err:Reason ->
              notify_parallel_abort(EtsRef, {parallel_local_delivery_failed, ToWorker, FromWorker, {Err, Reason}}),
              stats:increment_bad_messages(ClientStatsEts),
              increment_worker_messages_dropped(EtsRef, ToWorker),
              {error, {parallel_local_delivery_failed, ToWorker, FromWorker, {Err, Reason}}}
          end
      end;
    false ->
      notify_parallel_abort(EtsRef, {parallel_deliver_non_local_target, ToWorker, FromWorker}),
      stats:increment_bad_messages(ClientStatsEts),
      {error, {parallel_deliver_non_local_target, ToWorker, FromWorker}}
  end.

handle_parallel_delivery_with_ack(EtsRef, DeliveryId, FromWorker, ToWorker, Data) ->
  case is_parallel_delivery_duplicate(EtsRef, DeliveryId) of
    true ->
      ?LOG_WARNING(
        "Client ~p dropping duplicate parallel delivery id=~p to=~p",
        [ets:lookup_element(EtsRef, myName, ?DATA_IDX), DeliveryId, ToWorker]
      ),
      maybe_ack_parallel_delivery(EtsRef, DeliveryId, ok);
    false ->
      mark_parallel_delivery_seen(EtsRef, DeliveryId),
      DeliveryStatus = deliver_parallel_msg(EtsRef, FromWorker, ToWorker, Data),
      maybe_ack_parallel_delivery(EtsRef, DeliveryId, DeliveryStatus)
  end.

is_parallel_delivery_duplicate(EtsRef, DeliveryId) ->
  SeenIds = ets:lookup_element(EtsRef, parallel_delivery_seen_ids, ?DATA_IDX),
  lists:member(DeliveryId, SeenIds).

mark_parallel_delivery_seen(EtsRef, DeliveryId) ->
  SeenIds = ets:lookup_element(EtsRef, parallel_delivery_seen_ids, ?DATA_IDX),
  UpdatedSeenIds = trim_parallel_delivery_seen_ids(SeenIds ++ [DeliveryId]),
  ets:update_element(EtsRef, parallel_delivery_seen_ids, {?DATA_IDX, UpdatedSeenIds}).

trim_parallel_delivery_seen_ids(SeenIds) ->
  Overflow = length(SeenIds) - ?PARALLEL_DELIVERY_SEEN_MAX,
  case Overflow > 0 of
    true -> lists:nthtail(Overflow, SeenIds);
    false -> SeenIds
  end.

maybe_ack_parallel_delivery(EtsRef, DeliveryId, AckStatus) ->
  SuperNode = ets:lookup_element(EtsRef, super_node, ?DATA_IDX),
  case SuperNode of
    none ->
      ok;
    _ ->
      ClientName = ets:lookup_element(EtsRef, myName, ?DATA_IDX),
      AckPayload = {parallel_deliver_ack, ClientName, DeliveryId, AckStatus},
      {RouterHost,RouterPort} = ets:lookup_element(EtsRef, my_router, ?DATA_IDX),
      try
        nerl_tools:http_router_request(
          RouterHost,
          RouterPort,
          [SuperNode],
          atom_to_list(parallelDeliverAck),
          AckPayload
        ),
        ok
      catch
        _:_ ->
          ok
      end
  end.

forward_parallel_event(EtsRef, FromWorker, Direction, BatchID, MicrobatchID, StageID, Meta) ->
  ClientStatsEts = get(client_stats_ets),
  increment_worker_messages_sent(
    EtsRef,
    FromWorker,
    {parallel_event, FromWorker, Direction, BatchID, MicrobatchID, StageID, Meta}
  ),
  SuperNode = ets:lookup_element(EtsRef, super_node, ?DATA_IDX),
  PhaseEpoch = ets:lookup_element(EtsRef, parallel_phase_epoch, ?DATA_IDX),
  EventID = parallel_event_id(FromWorker, Direction, BatchID, MicrobatchID, StageID),
  case SuperNode of
    none ->
      notify_parallel_abort(EtsRef, {missing_super_node_parallel_event, EventID, FromWorker, Direction}),
      stats:increment_bad_messages(ClientStatsEts);
    _ ->
      ClientName = ets:lookup_element(EtsRef, myName, ?DATA_IDX),
      ?LOG_INFO(
        "Client ~p forwarding parallel event to super node ~p worker=~p direction=~p batch=~p microbatch=~p stage=~p epoch=~p event_id=~p",
        [ClientName, SuperNode, FromWorker, Direction, BatchID, MicrobatchID, StageID, PhaseEpoch, EventID]
      ),
      TaggedMeta = {parallel_meta, PhaseEpoch, Meta},
      MessageBody = {parallel_event, FromWorker, Direction, BatchID, MicrobatchID, StageID, TaggedMeta},
      {RouterHost,RouterPort} = ets:lookup_element(EtsRef, my_router, ?DATA_IDX),
      try
        {LatencyUs, RouterReply} =
          timer:tc(
            nerl_tools,
            http_router_request,
            [RouterHost, RouterPort, [SuperNode], atom_to_list(parallelEvent), MessageBody]
          ),
        ?LOG_INFO(
          "Client ~p routed parallel event_id=~p epoch=~p to super node ~p latency_us=~p reply=~p",
          [ClientName, EventID, PhaseEpoch, SuperNode, LatencyUs, RouterReply]
        ),
        stats:increment_messages_sent(ClientStatsEts),
        stats:increment_bytes_sent(ClientStatsEts, nerl_tools:calculate_size(MessageBody))
      catch
        Err:Reason ->
          ?LOG_ERROR(
            "Client ~p failed routing parallel event_id=~p to super node ~p reason=~p",
            [ClientName, EventID, SuperNode, {Err, Reason}]
          ),
          notify_parallel_abort(EtsRef, {parallel_event_route_failed, EventID, SuperNode, FromWorker, {Err, Reason}}),
          stats:increment_bad_messages(ClientStatsEts)
      end
  end.

forward_parallel_skip_event(EtsRef, FromWorker, Direction, BatchID, MicrobatchID, StageID, Meta) ->
  ClientStatsEts = get(client_stats_ets),
  increment_worker_messages_sent(
    EtsRef,
    FromWorker,
    {parallel_skip_event, FromWorker, Direction, BatchID, MicrobatchID, StageID, Meta}
  ),
  SuperNode = ets:lookup_element(EtsRef, super_node, ?DATA_IDX),
  PhaseEpoch = ets:lookup_element(EtsRef, parallel_phase_epoch, ?DATA_IDX),
  case SuperNode of
    none ->
      notify_parallel_abort(EtsRef, {missing_super_node_parallel_skip_event, FromWorker, Direction}),
      stats:increment_bad_messages(ClientStatsEts);
    _ ->
      TaggedMeta = {parallel_meta, PhaseEpoch, Meta},
      MessageBody = {parallel_skip_event, FromWorker, Direction, BatchID, MicrobatchID, StageID, TaggedMeta},
      {RouterHost,RouterPort} = ets:lookup_element(EtsRef, my_router, ?DATA_IDX),
      try
        nerl_tools:http_router_request(
          RouterHost,
          RouterPort,
          [SuperNode],
          atom_to_list(parallelSkipEvent),
          MessageBody
        ),
        stats:increment_messages_sent(ClientStatsEts),
        stats:increment_bytes_sent(ClientStatsEts, nerl_tools:calculate_size(MessageBody))
      catch
        _:_ ->
          stats:increment_bad_messages(ClientStatsEts)
      end
  end.

parallel_event_id(WorkerName, Direction, BatchID, MicrobatchID, StageID) ->
  {parallel_event, WorkerName, Direction, BatchID, MicrobatchID, StageID}.

notify_parallel_abort(EtsRef, Reason) ->
  {RouterHost,RouterPort} = ets:lookup_element(EtsRef, my_router, ?DATA_IDX),
  ClientName = ets:lookup_element(EtsRef, myName, ?DATA_IDX),
  ?LOG_ERROR("Client ~p sending parallelAbort reason=~p", [ClientName, Reason]),
  MessageBody = {ClientName, Reason},
  try
    nerl_tools:http_router_request(RouterHost, RouterPort, [?MAIN_SERVER_ATOM], atom_to_list(parallelAbort), MessageBody)
  catch
    _:_ -> ok
  end.

maybe_register_super_node(MyName, EtsRef, _NerlnetGraph, ClientWorkers) ->
  SuperNode = ets:lookup_element(EtsRef, super_node, ?DATA_IDX),
  case SuperNode of
    none -> ok;
    _ ->
      ?LOG_INFO("Client ~p registering to super node ~p with workers ~p", [MyName, SuperNode, ClientWorkers]),
      {RouterHost,RouterPort} = ets:lookup_element(EtsRef, my_router, ?DATA_IDX),
      MessageBody = {register_client, MyName, ClientWorkers},
      try nerl_tools:http_router_request(RouterHost, RouterPort, [SuperNode], atom_to_list(registerClient), MessageBody) of
        _ -> ok
      catch
        _:_ -> ok
      end
  end.

maybe_start_super_node_heartbeat(MyName, EtsRef, _NerlnetGraph) ->
  SuperNode = ets:lookup_element(EtsRef, super_node, ?DATA_IDX),
  case SuperNode of
    none -> ok;
    _ ->
      ?LOG_INFO("Client ~p starting super node heartbeat loop for ~p", [MyName, SuperNode]),
      spawn(fun() -> super_node_heartbeat_loop(MyName, EtsRef, SuperNode) end),
      ok
  end.

super_node_heartbeat_loop(MyName, EtsRef, SuperNode) ->
  timer:sleep(1000),
  {RouterHost,RouterPort} = ets:lookup_element(EtsRef, my_router, ?DATA_IDX),
  HeartbeatPayload = {heartbeat, MyName, erlang:system_time(millisecond)},
  try
    nerl_tools:http_router_request(RouterHost, RouterPort, [SuperNode], atom_to_list(superHeartbeat), HeartbeatPayload)
  catch
    _:_ -> ok
  end,
  super_node_heartbeat_loop(MyName, EtsRef, SuperNode).

perf_stats_memory_usage_update_train() ->
  % memory usage update
  PerformanceStatsEts = get(performance_stats_ets),
  MemoryUsageValue = stats:query_memory_usage(),
  stats:update_memory_peak_usage_train(PerformanceStatsEts, MemoryUsageValue),
  stats:update_memory_train_ema_usage(PerformanceStatsEts, MemoryUsageValue).

perf_stats_memory_usage_update_predict() ->
  % memory usage update
  PerformanceStatsEts = get(performance_stats_ets),
  MemoryUsageValue = stats:query_memory_usage(),
  stats:update_memory_peak_usage_predict(PerformanceStatsEts, MemoryUsageValue),
  stats:update_memory_predict_ema_usage(PerformanceStatsEts, MemoryUsageValue).
