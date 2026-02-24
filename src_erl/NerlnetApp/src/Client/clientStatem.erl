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
  ets:insert(EtsRef, {all_workers_done, false}),
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
  apply_parallel_super_command(EtsRef, SuperCommand),
  {keep_state, State};

waitforWorkers(cast, {worker_parallel_abort, WorkerName, Reason}, State = #client_statem_state{etsRef = EtsRef}) ->
  handle_worker_parallel_abort(EtsRef, WorkerName, Reason),
  {keep_state, State};

waitforWorkers(cast, In = {stateChange,WorkerName}, State = #client_statem_state{myName = MyName,waitforWorkers = WaitforWorkers,nextState = NextState, etsRef = _EtsRef}) ->
  NewWaitforWorkers = WaitforWorkers -- [WorkerName],
  ClientStatsEts = get(client_stats_ets),
  stats:increment_messages_received(ClientStatsEts),
  stats:increment_bytes_received(ClientStatsEts , nerl_tools:calculate_size(In)),
  case NewWaitforWorkers of % TODO Guy here we need to check for keep alive with workers
    [] ->   send_client_is_ready(MyName), % when all workers done their work
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

waitforWorkers(cast, In = {parallel_event, FromWorker, Direction, BatchID, MicrobatchID, StageID, Meta}, State = #client_statem_state{etsRef = EtsRef}) ->
  ClientStatsEts = get(client_stats_ets),
  stats:increment_messages_received(ClientStatsEts),
  stats:increment_bytes_received(ClientStatsEts, nerl_tools:calculate_size(In)),
  forward_parallel_event(EtsRef, FromWorker, Direction, BatchID, MicrobatchID, StageID, Meta),
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
  apply_parallel_super_command(EtsRef, SuperCommand),
  {keep_state, State};

idle(cast, {worker_parallel_abort, WorkerName, Reason}, State = #client_statem_state{etsRef = EtsRef}) ->
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

idle(cast, In = {parallel_event, FromWorker, Direction, BatchID, MicrobatchID, StageID, Meta}, State = #client_statem_state{etsRef = EtsRef}) ->
  ClientStatsEts = get(client_stats_ets),
  stats:increment_messages_received(ClientStatsEts),
  stats:increment_bytes_received(ClientStatsEts, nerl_tools:calculate_size(In)),
  forward_parallel_event(EtsRef, FromWorker, Direction, BatchID, MicrobatchID, StageID, Meta),
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
  ClientStatsEts = get(client_stats_ets),
  PerformanceStatsEts = get(performance_stats_ets),
  stats:increment_messages_received(ClientStatsEts),
  stats:increment_bytes_received(ClientStatsEts , nerl_tools:calculate_size(In)),
  MessageToCast = {training},
  cast_message_to_workers(EtsRef, MessageToCast),
  ets:update_element(EtsRef, all_workers_done, {?DATA_IDX, false}),
  stats:performance_stats_reset(PerformanceStatsEts),
  stats:communication_stats_reset(ClientStatsEts),
  stats:tic(ClientStatsEts, time_train_total),
  stats:reset_query_cpu_util_cores(),
  {next_state, waitforWorkers, State#client_statem_state{waitforWorkers =  clientWorkersFunctions:get_workers_names(EtsRef), nextState = training}};

idle(cast, In = {predict}, State = #client_statem_state{etsRef = EtsRef}) ->
  erlang:garbage_collect(), % free memory when phase is changed to predict
  ClientStatsEts = get(client_stats_ets),
  PerformanceStatsEts = get(performance_stats_ets),
  stats:increment_messages_received(ClientStatsEts),
  stats:increment_bytes_received(ClientStatsEts , nerl_tools:calculate_size(In)),
  MessageToCast = {predict},
  cast_message_to_workers(EtsRef, MessageToCast),
  stats:performance_stats_reset(PerformanceStatsEts),
  stats:communication_stats_reset(ClientStatsEts),
  stats:tic(ClientStatsEts, time_predict_total), 
  stats:reset_query_cpu_util_cores(),
  {next_state, waitforWorkers, State#client_statem_state{waitforWorkers = clientWorkersFunctions:get_workers_names(EtsRef),nextState = predict}};

idle(cast, EventContent, State = #client_statem_state{etsRef = EtsRef , myName = MyName}) ->
  ClientStatsEts = get(client_stats_ets),
  stats:increment_bad_messages(ClientStatsEts),
  ?LOG_WARNING("~p Unrecognized Message!!!:  ~p",[MyName , EventContent]),
  {next_state, training, State#client_statem_state{etsRef = EtsRef}}.

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
  apply_parallel_super_command(EtsRef, SuperCommand),
  {keep_state, State};

training(cast, {worker_parallel_abort, WorkerName, Reason}, State = #client_statem_state{etsRef = EtsRef}) ->
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

training(cast, In = {parallel_event, FromWorker, Direction, BatchID, MicrobatchID, StageID, Meta}, State = #client_statem_state{etsRef = EtsRef}) ->
  ClientStatsEts = get(client_stats_ets),
  stats:increment_messages_received(ClientStatsEts),
  stats:increment_bytes_received(ClientStatsEts, nerl_tools:calculate_size(In)),
  forward_parallel_event(EtsRef, FromWorker, Direction, BatchID, MicrobatchID, StageID, Meta),
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
      stats:increment_messages_sent(ClientStatsEts),
      BatchSize = nerl_tools:calculate_size(BatchOfSamples),
      stats:increment_bytes_sent(ClientStatsEts , BatchSize),
      perf_stats_memory_usage_update_train();
  true -> ?LOG_ERROR("Given worker ~p isn't found in client ~p",[WorkerName, ClientName]) end,
  {next_state, training, State#client_statem_state{etsRef = EtsRef}};

% This action is used for start_stream triggered from a clients' worker and not source
training(cast, {start_stream , {worker, WorkerName, TargetPair}}, State = #client_statem_state{etsRef = EtsRef}) ->
  ListOfActiveWorkersSources = ets:lookup_element(EtsRef, active_workers_streams, ?DATA_IDX),
  ets:update_element(EtsRef, active_workers_streams, {?DATA_IDX, ListOfActiveWorkersSources ++ [{WorkerName, TargetPair}]}),
  
  perf_stats_memory_usage_update_train(),
  {keep_state, State};

% This action is used for start_stream triggered from a source per worker
training(cast, In = {start_stream , Data}, State = #client_statem_state{etsRef = EtsRef}) ->
  {SourceName, _ClientName, WorkerName} = binary_to_term(Data),
  ListOfActiveWorkersSources = ets:lookup_element(EtsRef, active_workers_streams, ?DATA_IDX),
  ets:update_element(EtsRef, active_workers_streams, {?DATA_IDX, ListOfActiveWorkersSources ++ [{WorkerName, SourceName}]}),
  ClientStatsEts = get(client_stats_ets),
  stats:increment_messages_received(ClientStatsEts),
  stats:increment_bytes_received(ClientStatsEts , nerl_tools:calculate_size(In)),
  WorkerPid = clientWorkersFunctions:get_worker_pid(EtsRef , WorkerName),
  gen_statem:cast(WorkerPid, {start_stream, SourceName}),

  perf_stats_memory_usage_update_train(),  
  {keep_state, State};


training(cast, In = {end_stream , Data}, State = #client_statem_state{etsRef = EtsRef}) ->
  {SourceName, _ClientName, WorkerName} = binary_to_term(Data),
  ClientStatsEts = get(client_stats_ets),
  stats:increment_messages_received(ClientStatsEts),
  stats:increment_bytes_received(ClientStatsEts , nerl_tools:calculate_size(In)),
  WorkerPid = clientWorkersFunctions:get_worker_pid(EtsRef , WorkerName),
  gen_statem:cast(WorkerPid, {end_stream, SourceName}),

  perf_stats_memory_usage_update_train(),
  {keep_state, State};

training(cast, In = {stream_ended , Pair}, State = #client_statem_state{etsRef = EtsRef}) ->
  ClientStatsEts = get(client_stats_ets),
  stats:increment_messages_received(ClientStatsEts),
  stats:increment_bytes_received(ClientStatsEts , nerl_tools:calculate_size(In)),
  ListOfActiveWorkersSources = ets:lookup_element(EtsRef, active_workers_streams, ?DATA_IDX),
  UpdatedListOfActiveWorkersSources = ListOfActiveWorkersSources -- [Pair],
  ets:update_element(EtsRef, active_workers_streams, {?DATA_IDX, UpdatedListOfActiveWorkersSources}),
  case length(UpdatedListOfActiveWorkersSources) of 
    0 ->  ets:update_element(EtsRef, all_workers_done, {?DATA_IDX, true});
    _ ->  ok
  end,
  {next_state, training, State#client_statem_state{etsRef = EtsRef}};

% From MainServer
training(cast, In = {idle}, State = #client_statem_state{myName = MyName, etsRef = EtsRef}) ->
  ClientStatsEts = get(client_stats_ets),
  ClientPerformanceEts = get(performance_stats_ets),
  stats:increment_messages_received(ClientStatsEts),
  stats:increment_bytes_received(ClientStatsEts , nerl_tools:calculate_size(In)),
  MessageToCast = {idle},
  WorkersDone = ets:lookup_element(EtsRef , all_workers_done , ?DATA_IDX),
  case WorkersDone of
    true ->   cast_message_to_workers(EtsRef, MessageToCast),
              Workers =  clientWorkersFunctions:get_workers_names(EtsRef),
              ?LOG_INFO("~p sent idle to workers: ~p , waiting for confirmation...~n",[MyName, ets:lookup_element(EtsRef, workersNames, ?DATA_IDX)]),
              Elapsed = stats:toc(ClientStatsEts, time_train_total),
              stats:increment_time_train_total(ClientPerformanceEts, Elapsed),
              stats:update_cpu_util_per_core(ClientPerformanceEts, train), % Update CPU utilization for training phase
              {next_state, waitforWorkers, State#client_statem_state{etsRef = EtsRef, waitforWorkers = Workers , nextState = idle}};
    false ->  MyPid = get(my_pid),
              start_monitor_workers_done(EtsRef, MyPid),
              {keep_state, State#client_statem_state{etsRef = EtsRef}}
  end;

training(cast, _In = {predict}, State = #client_statem_state{myName = MyName, etsRef = EtsRef}) ->
  ?LOG_ERROR("Wrong request , client ~p can't go from training to predict directly", [MyName]),
  {next_state, training, State#client_statem_state{etsRef = EtsRef}};


training(cast, In = {loss, WorkerName ,SourceName ,LossTensor ,TimeNIF , WorkerToken,BatchID ,BatchTS}, State = #client_statem_state{myName = MyName,etsRef = EtsRef}) ->
  ClientStatsEts = get(client_stats_ets),
  ClientPerformanceEts = get(performance_stats_ets),
  stats:increment_messages_received(ClientStatsEts),
  stats:increment_bytes_received(ClientStatsEts , nerl_tools:calculate_size(In)),
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
  apply_parallel_super_command(EtsRef, SuperCommand),
  {keep_state, State};

predict(cast, {worker_parallel_abort, WorkerName, Reason}, State = #client_statem_state{etsRef = EtsRef}) ->
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
      stats:increment_messages_sent(ClientStatsEts),
      stats:increment_bytes_sent(ClientStatsEts , nerl_tools:calculate_size(BatchOfSamples)),
      perf_stats_memory_usage_update_predict();
    true -> ?LOG_ERROR("Given worker ~p isn't found in client ~p",[WorkerName, ClientName])
  end,
  {next_state, predict, State#client_statem_state{etsRef = EtsRef}};

% This action is used for start_stream triggered from a clients' worker and not source
predict(cast, {start_stream , {worker, WorkerName, TargetName}}, State = #client_statem_state{etsRef = EtsRef}) ->
  ListOfActiveWorkersSources = ets:lookup_element(EtsRef, active_workers_streams, ?DATA_IDX),
  ets:update_element(EtsRef, active_workers_streams, {?DATA_IDX, ListOfActiveWorkersSources ++ [{WorkerName, TargetName}]}),

  perf_stats_memory_usage_update_predict(),
  {keep_state, State};

% This action is used for start_stream triggered from a source per worker
predict(cast, In = {start_stream , Data}, State = #client_statem_state{etsRef = EtsRef}) ->
  {SourceName, _ClientName, WorkerName} = binary_to_term(Data),
  ListOfActiveWorkersSources = ets:lookup_element(EtsRef, active_workers_streams, ?DATA_IDX),
  ets:update_element(EtsRef, active_workers_streams, {?DATA_IDX, ListOfActiveWorkersSources ++ [{WorkerName, SourceName}]}),
  ClientStatsEts = get(client_stats_ets),
  stats:increment_messages_received(ClientStatsEts),
  stats:increment_bytes_received(ClientStatsEts , nerl_tools:calculate_size(In)),
  WorkerPid = clientWorkersFunctions:get_worker_pid(EtsRef , WorkerName),
  gen_statem:cast(WorkerPid, {start_stream, SourceName}),

  perf_stats_memory_usage_update_predict(),
  {keep_state, State};

predict(cast, In = {end_stream , Data}, State = #client_statem_state{etsRef = EtsRef}) ->
  {SourceName, _ClientName, WorkerName} = binary_to_term(Data),
  ClientStatsEts = get(client_stats_ets),
  stats:increment_messages_received(ClientStatsEts),
  stats:increment_bytes_received(ClientStatsEts , nerl_tools:calculate_size(In)),
  WorkerPid = clientWorkersFunctions:get_worker_pid(EtsRef , WorkerName),
  gen_statem:cast(WorkerPid, {end_stream, SourceName}),

  perf_stats_memory_usage_update_predict(),
  {keep_state, State};

predict(cast, In = {stream_ended , Pair}, State = #client_statem_state{etsRef = EtsRef}) ->
  ClientStatsEts = get(client_stats_ets),
  stats:increment_messages_received(ClientStatsEts),
  stats:increment_bytes_received(ClientStatsEts , nerl_tools:calculate_size(In)),
  ListOfActiveWorkersSources = ets:lookup_element(EtsRef, active_workers_streams, ?DATA_IDX),
  UpdatedListOfActiveWorkersSources = ListOfActiveWorkersSources -- [Pair],
  ets:update_element(EtsRef, active_workers_streams, {?DATA_IDX, UpdatedListOfActiveWorkersSources}),
  case length(UpdatedListOfActiveWorkersSources) of 
    0 ->  ets:update_element(EtsRef, all_workers_done, {?DATA_IDX, true});
    _ ->  ok
  end,
  {next_state, predict, State#client_statem_state{etsRef = EtsRef}};


% From MainServer
predict(cast, In = {idle}, State = #client_statem_state{myName = MyName, etsRef = EtsRef}) ->
  ClientStatsEts = get(client_stats_ets),
  ClientPerformanceEts = get(performance_stats_ets),
  stats:increment_messages_received(ClientStatsEts),
  stats:increment_bytes_received(ClientStatsEts , nerl_tools:calculate_size(In)),
  MessageToCast = {idle},
  WorkersDone = ets:lookup_element(EtsRef , all_workers_done , ?DATA_IDX),
  case WorkersDone of
    true ->   cast_message_to_workers(EtsRef, MessageToCast),
              Workers =  clientWorkersFunctions:get_workers_names(EtsRef),
              ?LOG_INFO("~p sent idle to workers: ~p , waiting for confirmation...~n",[MyName, ets:lookup_element(EtsRef, workersNames, ?DATA_IDX)]),
              Elapsed = stats:toc(ClientStatsEts, time_predict_total),
              stats:increment_time_predict_total(ClientPerformanceEts, Elapsed),
              stats:update_cpu_util_per_core(ClientPerformanceEts, predict), % Update CPU utilization for predict phase
              {next_state, waitforWorkers, State#client_statem_state{etsRef = EtsRef, waitforWorkers = Workers , nextState = idle}};
    false ->  
              MyPid = get(my_pid),
              start_monitor_workers_done(EtsRef, MyPid), % ← Pass EtsRef
              {keep_state, State}
  end;

predict(cast, In = {predictRes,WorkerName, SourceName ,{PredictNerlTensor, NetlTensorType} , TimeTook , WorkerToken, BatchID , BatchTS}, State = #client_statem_state{myName = _MyName, etsRef = EtsRef}) ->
  ClientStatsEts = get(client_stats_ets),
  ClientPerformanceEts = get(performance_stats_ets),
  stats:increment_messages_received(ClientStatsEts),
  stats:increment_bytes_received(ClientStatsEts , nerl_tools:calculate_size(In)),

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

predict(cast, In = {parallel_event, FromWorker, Direction, BatchID, MicrobatchID, StageID, Meta}, State = #client_statem_state{etsRef = EtsRef}) ->
  ClientStatsEts = get(client_stats_ets),
  stats:increment_messages_received(ClientStatsEts),
  stats:increment_bytes_received(ClientStatsEts, nerl_tools:calculate_size(In)),
  forward_parallel_event(EtsRef, FromWorker, Direction, BatchID, MicrobatchID, StageID, Meta),
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
    stats:increment_messages_sent(ClientStatsEts)
  end,
  lists:foreach(Func, Workers).

apply_parallel_mode(EtsRef, Mode) ->
  apply_parallel_mode(EtsRef, Mode, main_server).

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
      case NormalizedMode of
        legacy ->
          ets:update_element(EtsRef, parallel_execution, {?DATA_IDX, #{}}),
          cast_message_to_workers(EtsRef, {set_parallel_mode, legacy}),
          cast_message_to_workers(EtsRef, {set_parallel_execution, #{}}),
          cast_message_to_workers(EtsRef, {set_parallel_authority, false});
        _ ->
          cast_message_to_workers(EtsRef, {set_parallel_mode, NormalizedMode}),
          cast_message_to_workers(EtsRef, {set_parallel_authority, Source =:= super_node})
      end
  end.

apply_parallel_execution(EtsRef, ParallelExecution) ->
  apply_parallel_execution(EtsRef, ParallelExecution, main_server).

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
          cast_message_to_workers(EtsRef, {set_parallel_execution, #{}});
        _ ->
          ets:update_element(EtsRef, parallel_execution, {?DATA_IDX, NormalizedExecution}),
          cast_message_to_workers(EtsRef, {set_parallel_execution, NormalizedExecution})
      end
  end.

apply_parallel_super_command(EtsRef, {parallel_super_command, configure_parallel, Mode, ParallelExecution}) ->
  apply_parallel_mode(EtsRef, Mode, super_node),
  apply_parallel_execution(EtsRef, ParallelExecution, super_node);
apply_parallel_super_command(EtsRef, {configure_parallel, Mode, ParallelExecution}) ->
  apply_parallel_mode(EtsRef, Mode, super_node),
  apply_parallel_execution(EtsRef, ParallelExecution, super_node);
apply_parallel_super_command(
  EtsRef,
  {parallel_super_command, grant_scheduler_event, Direction, MicrobatchID, StageID, WorkerName}
) ->
  deliver_scheduler_grant(EtsRef, Direction, MicrobatchID, StageID, WorkerName);
apply_parallel_super_command(EtsRef, {grant_scheduler_event, Direction, MicrobatchID, StageID, WorkerName}) ->
  deliver_scheduler_grant(EtsRef, Direction, MicrobatchID, StageID, WorkerName);
apply_parallel_super_command(_EtsRef, UnknownCommand) ->
  ?LOG_WARNING("Ignoring unknown parallel super command: ~p", [UnknownCommand]),
  ok.

deliver_scheduler_grant(EtsRef, Direction, MicrobatchID, StageID, WorkerNameRaw) ->
  WorkersOfThisClient = ets:lookup_element(EtsRef, workersNames, ?DATA_IDX),
  case resolve_worker_name(WorkersOfThisClient, WorkerNameRaw) of
    {error, _} ->
      notify_parallel_abort(EtsRef, {scheduler_grant_non_local_worker, WorkerNameRaw, Direction, MicrobatchID, StageID});
    {ok, WorkerName} ->
      WorkerPid = clientWorkersFunctions:get_worker_pid(EtsRef, WorkerName),
      gen_statem:cast(
        WorkerPid,
        {parallel_scheduler_grant, normalize_parallel_direction(Direction), MicrobatchID, StageID}
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
          stats:increment_bad_messages(ClientStatsEts);
        TargetWorkerW2WPID ->
          {ok, _Reply} = gen_server:call(TargetWorkerW2WPID, {worker_to_worker_msg, FromWorker, ToWorker, Data}),
          stats:increment_messages_sent(ClientStatsEts),
          stats:increment_bytes_sent(ClientStatsEts, nerl_tools:calculate_size({FromWorker, ToWorker, Data}))
      end;
    false ->
      notify_parallel_abort(EtsRef, {parallel_deliver_non_local_target, ToWorker, FromWorker}),
      stats:increment_bad_messages(ClientStatsEts)
  end.

forward_parallel_event(EtsRef, FromWorker, Direction, BatchID, MicrobatchID, StageID, Meta) ->
  ClientStatsEts = get(client_stats_ets),
  SuperNode = ets:lookup_element(EtsRef, super_node, ?DATA_IDX),
  case SuperNode of
    none ->
      notify_parallel_abort(EtsRef, {missing_super_node_parallel_event, FromWorker, Direction}),
      stats:increment_bad_messages(ClientStatsEts);
    _ ->
      MessageBody = {parallel_event, FromWorker, Direction, BatchID, MicrobatchID, StageID, Meta},
      {RouterHost,RouterPort} = ets:lookup_element(EtsRef, my_router, ?DATA_IDX),
      try
        nerl_tools:http_router_request(RouterHost, RouterPort, [SuperNode], atom_to_list(parallelEvent), MessageBody),
        stats:increment_messages_sent(ClientStatsEts),
        stats:increment_bytes_sent(ClientStatsEts, nerl_tools:calculate_size(MessageBody))
      catch
        Err:Reason ->
          notify_parallel_abort(EtsRef, {parallel_event_route_failed, SuperNode, FromWorker, {Err, Reason}}),
          stats:increment_bad_messages(ClientStatsEts)
      end
  end.

notify_parallel_abort(EtsRef, Reason) ->
  {RouterHost,RouterPort} = ets:lookup_element(EtsRef, my_router, ?DATA_IDX),
  ClientName = ets:lookup_element(EtsRef, myName, ?DATA_IDX),
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
