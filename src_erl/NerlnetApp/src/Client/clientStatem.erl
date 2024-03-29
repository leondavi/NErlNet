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
init({MyName,NerlnetGraph, ClientWorkers , WorkerShaMap , WorkerToClientMap , ShaToModelArgsMap}) ->
  inets:start(),
  ?LOG_INFO("Client ~p is connected to: ~p~n",[MyName, [digraph:vertex(NerlnetGraph,Vertex) || Vertex <- digraph:out_neighbours(NerlnetGraph,MyName)]]),
  % nerl_tools:start_connection([digraph:vertex(NerlnetGraph,Vertex) || Vertex <- digraph:out_neighbours(NerlnetGraph,MyName)]),
  EtsRef = ets:new(client_data, [set]), %% client_data is responsible for functional attributes
  EtsStats = ets:new(ets_stats, [set]), %% ets_stats is responsible for holding all the ets stats (client + workers)
  ClientStatsEts = stats:generate_stats_ets(), %% client stats ets inside ets_stats
  ets:insert(EtsStats, {MyName, ClientStatsEts}),
  put(ets_stats, EtsStats),
  ets:insert(EtsRef, {workerToClient, WorkerToClientMap}),
  ets:insert(EtsRef, {workersNames, ClientWorkers}),
  ets:insert(EtsRef, {nerlnetGraph, NerlnetGraph}),
  ets:insert(EtsRef, {myName, MyName}),
  MyWorkersToShaMap = maps:filter(fun(Worker , _SHA) -> lists:member(Worker , ClientWorkers) end , WorkerShaMap),
  ets:insert(EtsRef, {workers_to_sha_map, MyWorkersToShaMap}),
  ets:insert(EtsRef, {sha_to_models_map , ShaToModelArgsMap}),
  {MyRouterHost,MyRouterPort} = nerl_tools:getShortPath(MyName,?MAIN_SERVER_ATOM, NerlnetGraph),
  ets:insert(EtsRef, {my_router,{MyRouterHost,MyRouterPort}}),

  clientWorkersFunctions:create_workers(MyName , EtsRef , ShaToModelArgsMap , EtsStats),
  %% send pre_idle signal to workers
  WorkersNames = clientWorkersFunctions:get_workers_names(EtsRef),
  [gen_statem:cast(clientWorkersFunctions:get_worker_pid(EtsRef , WorkerName), {pre_idle}) || WorkerName <- WorkersNames],

  % update dictionary
  WorkersEts = ets:lookup_element(EtsRef , workers_ets , ?DATA_IDX),
  put(workers_ets, WorkersEts),
  put(nerlnetGraph, NerlnetGraph),
  put(client_data, EtsRef),
  put(ets_stats, EtsStats),
  put(client_stats_ets , ClientStatsEts),

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
waitforWorkers(cast, In = {stateChange,WorkerName}, State = #client_statem_state{myName = MyName,waitforWorkers = WaitforWorkers,nextState = NextState, etsRef = _EtsRef}) ->
  NewWaitforWorkers = WaitforWorkers--[WorkerName],
  ClientStatsEts = get(client_stats_ets),
  stats:increment_messages_received(ClientStatsEts),
  stats:increment_bytes_received(ClientStatsEts , nerl_tools:calculate_size(In)),
  case NewWaitforWorkers of % TODO Guy here we need to check for keep alive with workers
    [] ->   send_client_is_ready(MyName), % when all workers done their work
            stats:increment_messages_sent(ClientStatsEts),
            {next_state, NextState, State#client_statem_state{waitforWorkers = []}};
    _->  {next_state, waitforWorkers, State#client_statem_state{waitforWorkers = NewWaitforWorkers}}
  end;

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
%% TODO: make custom_worker_message in all states to send messages from workers to entities (not just client)
idle(cast, In = {custom_worker_message, {From, To}}, State = #client_statem_state{etsRef = EtsRef}) ->
  ClientStatsEts = get(client_stats_ets),
  stats:increment_messages_received(ClientStatsEts),
  stats:increment_bytes_received(ClientStatsEts , nerl_tools:calculate_size(In)),
  WorkerOfThisClient = ets:member(EtsRef, To),
  if WorkerOfThisClient -> 
    TargetWorkerPID = ets:lookup_element(EtsRef, To, ?WORKER_PID_IDX),
    gen_statem:cast(TargetWorkerPID,{post_idle,From}),
    stats:increment_messages_sent(ClientStatsEts);
  true ->
    %% send to FedServer that worker From is connecting to it
    DestClient = maps:get(To, ets:lookup_element(EtsRef, workerToClient, ?ETS_KV_VAL_IDX)),
    MessageBody = {DestClient, custom_worker_message, {From, To}},
    {RouterHost,RouterPort} = ets:lookup_element(EtsRef, my_router, ?DATA_IDX),
    nerl_tools:http_router_request(RouterHost, RouterPort, [DestClient], atom_to_list(custom_worker_message), term_to_binary(MessageBody)),
    stats:increment_messages_sent(ClientStatsEts),
    stats:increment_bytes_sent(ClientStatsEts , nerl_tools:calculate_size(MessageBody))
  end,
  {keep_state, State};

idle(cast, _In = {statistics}, State = #client_statem_state{ myName = MyName, etsRef = EtsRef}) ->
  EtsStats = get(ets_stats),
  ClientStatsEts = get(client_stats_ets),
  ClientStatsEncStr = stats:encode_ets_to_http_bin_str(ClientStatsEts),
  %ClientStatsToSend = atom_to_list(MyName) ++ ?API_SERVER_WITHIN_ENTITY_SEPERATOR ++ ClientStatsEncStr ++ ?API_SERVER_ENTITY_SEPERATOR,
  stats:increment_messages_received(ClientStatsEts),
  ListStatsEts = ets:tab2list(EtsStats) -- [{MyName , ClientStatsEts}], 
  WorkersStatsEncStr = create_encoded_stats_str(ListStatsEts),
  DataToSend = ClientStatsEncStr ++ WorkersStatsEncStr,
  %% io:format("DataToSend: ~p~n",[DataToSend]),
  StatsBody = {MyName , DataToSend},
  {RouterHost,RouterPort} = ets:lookup_element(EtsRef, my_router, ?DATA_IDX),
  nerl_tools:http_router_request(RouterHost, RouterPort, [?MAIN_SERVER_ATOM], atom_to_list(statistics), StatsBody),
  stats:increment_messages_sent(ClientStatsEts),
  {next_state, idle, State};

idle(cast, In = {training}, State = #client_statem_state{myName = _MyName, etsRef = EtsRef}) ->
  ClientStatsEts = get(client_stats_ets),
  stats:increment_messages_received(ClientStatsEts),
  stats:increment_bytes_received(ClientStatsEts , nerl_tools:calculate_size(In)),  MessageToCast = {training},
  cast_message_to_workers(EtsRef, MessageToCast),
  {next_state, waitforWorkers, State#client_statem_state{waitforWorkers =  clientWorkersFunctions:get_workers_names(EtsRef), nextState = training}};

idle(cast, In = {predict}, State = #client_statem_state{etsRef = EtsRef}) ->
  ClientStatsEts = get(client_stats_ets),
  stats:increment_messages_received(ClientStatsEts),
  stats:increment_bytes_received(ClientStatsEts , nerl_tools:calculate_size(In)),
  MessageToCast = {predict},
  cast_message_to_workers(EtsRef, MessageToCast),
  {next_state, waitforWorkers, State#client_statem_state{waitforWorkers = clientWorkersFunctions:get_workers_names(EtsRef),nextState = predict}};

idle(cast, EventContent, State = #client_statem_state{etsRef = EtsRef , myName = MyName}) ->
  ClientStatsEts = get(client_stats_ets),
  stats:increment_bad_messages(ClientStatsEts),
  ?LOG_WARNING("~p Unrecognized Message!!!:  ~p",[MyName , EventContent]),
  {next_state, training, State#client_statem_state{etsRef = EtsRef}}.

%% passing Data from worker to worker e.g. (FedClient to FedServer)
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


%% This is a generic way to move data from worker to worker
%% TODO fix variables names to make it more generic
%% federated server sends AvgWeights to workers
training(cast, InMessage = {custom_worker_message, WorkersList, WeightsTensor}, State = #client_statem_state{etsRef = EtsRef}) ->
  ClientStatsEts = get(client_stats_ets),
  stats:increment_messages_received(ClientStatsEts),
  stats:increment_bytes_received(ClientStatsEts , nerl_tools:calculate_size(InMessage)),
  Func = fun(WorkerName) ->
    DestClient = maps:get(WorkerName, ets:lookup_element(EtsRef, workerToClient, ?ETS_KV_VAL_IDX)),
    MessageBody = term_to_binary({DestClient, update, {_FedServer = "server", WorkerName, WeightsTensor}}), % TODO - fix client should not be aware of the data of custom worker message

    {RouterHost,RouterPort} = ets:lookup_element(EtsRef, my_router, ?DATA_IDX),
    nerl_tools:http_router_request(RouterHost, RouterPort, [DestClient], atom_to_list(custom_worker_message), MessageBody),
    stats:increment_messages_sent(ClientStatsEts),
    stats:increment_bytes_sent(ClientStatsEts , nerl_tools:calculate_size(MessageBody))
  end,
  lists:foreach(Func, WorkersList), % can be optimized with broadcast instead of unicast
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
      stats:increment_bytes_sent(ClientStatsEts , BatchSize);
  true -> ?LOG_ERROR("Given worker ~p isn't found in client ~p",[WorkerName, ClientName]) end,
  {next_state, training, State#client_statem_state{etsRef = EtsRef}};

training(cast, In = {idle}, State = #client_statem_state{myName = MyName, etsRef = EtsRef}) ->
  ClientStatsEts = get(client_stats_ets),
  stats:increment_messages_received(ClientStatsEts),
  stats:increment_bytes_received(ClientStatsEts , nerl_tools:calculate_size(In)),
  MessageToCast = {idle},
  cast_message_to_workers(EtsRef, MessageToCast),
  Workers =  clientWorkersFunctions:get_workers_names(EtsRef),
  ?LOG_INFO("~p setting workers at idle: ~p~n",[MyName, ets:lookup_element(EtsRef, workersNames, ?DATA_IDX)]),
  {next_state, waitforWorkers, State#client_statem_state{etsRef = EtsRef, waitforWorkers = Workers , nextState = idle}};

training(cast, _In = {predict}, State = #client_statem_state{myName = MyName, etsRef = EtsRef}) ->
  ?LOG_ERROR("Wrong request , client ~p can't go from training to predict directly", [MyName]),
  {next_state, training, State#client_statem_state{etsRef = EtsRef}};

training(cast, In = {loss, WorkerName ,SourceName ,LossTensor ,TimeNIF ,BatchID ,BatchTS}, State = #client_statem_state{myName = MyName,etsRef = EtsRef}) ->
  ClientStatsEts = get(client_stats_ets),
  stats:increment_messages_received(ClientStatsEts),
  stats:increment_bytes_received(ClientStatsEts , nerl_tools:calculate_size(In)),
  {RouterHost,RouterPort} = ets:lookup_element(EtsRef, my_router, ?DATA_IDX),
  MessageBody = {WorkerName , SourceName , LossTensor , TimeNIF , BatchID , BatchTS},
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
      stats:increment_bytes_sent(ClientStatsEts , nerl_tools:calculate_size(BatchOfSamples));
    true -> ?LOG_ERROR("Given worker ~p isn't found in client ~p",[WorkerName, ClientName])
  end,
  {next_state, predict, State#client_statem_state{etsRef = EtsRef}};

predict(cast, In = {predictRes,WorkerName, SourceName ,{PredictNerlTensor, NetlTensorType} , TimeTook , BatchID , BatchTS}, State = #client_statem_state{myName = _MyName, etsRef = EtsRef}) ->
  ClientStatsEts = get(client_stats_ets),
  stats:increment_messages_received(ClientStatsEts),
  stats:increment_bytes_received(ClientStatsEts , nerl_tools:calculate_size(In)),

  {RouterHost,RouterPort} = ets:lookup_element(EtsRef, my_router, ?DATA_IDX),
  MessageBody = {WorkerName, SourceName, {PredictNerlTensor , NetlTensorType}, TimeTook, BatchID, BatchTS}, %% SHOULD INCLUDE TYPE?
  nerl_tools:http_router_request(RouterHost, RouterPort, [?MAIN_SERVER_ATOM], atom_to_list(predictRes), MessageBody),
  
  stats:increment_messages_sent(ClientStatsEts),
  stats:increment_bytes_sent(ClientStatsEts , nerl_tools:calculate_size(MessageBody)),
  {next_state, predict, State#client_statem_state{etsRef = EtsRef}};

% TODO from predict directly to training?!?!?
predict(cast,_In = {training}, State = #client_statem_state{myName = MyName}) ->
  ClientStatsEts = get(client_stats_ets),
  stats:increment_bad_messages(ClientStatsEts),
  ?LOG_ERROR("client ~p got training request in predict state",[MyName]),
  {next_state, predict, State#client_statem_state{nextState = predict}};

%% The source sends message to main server that it has finished
%% The main server updates its' clients to move to state 'idle'
predict(cast, In = {idle}, State = #client_statem_state{etsRef = EtsRef , myName = _MyName}) ->

  MsgToCast = {idle},
  ClientStatsEts = get(client_stats_ets),
  stats:increment_messages_received(ClientStatsEts),
  stats:increment_bytes_received(ClientStatsEts , nerl_tools:calculate_size(In)),
  cast_message_to_workers(EtsRef, MsgToCast),
  Workers = clientWorkersFunctions:get_workers_names(EtsRef),
  {next_state, waitforWorkers, State#client_statem_state{nextState = idle, waitforWorkers = Workers, etsRef = EtsRef}};

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

cast_message_to_workers(EtsRef, Msg) ->
  ClientStatsEts = get(client_stats_ets),
  Workers = ets:lookup_element(EtsRef, workersNames, ?ETS_KV_VAL_IDX),
  Func = fun(WorkerKey) ->
    WorkerPid = clientWorkersFunctions:get_worker_pid(EtsRef, WorkerKey), % WorkerKey is the worker name
    gen_statem:cast(WorkerPid, Msg),
    stats:increment_messages_sent(ClientStatsEts)
  end,
  lists:foreach(Func, Workers).

create_encoded_stats_str(ListStatsEts) ->
  Func = fun({WorkerName , StatsEts}) ->
    WorkerEncStatsStr = stats:encode_workers_ets_to_http_bin_str(StatsEts),
    %% |w1&bytes_sent:6.0:float#bad_messages:0:int....|
    ?API_SERVER_ENTITY_SEPERATOR ++ atom_to_list(WorkerName) ++ ?WORKER_SEPERATOR ++ WorkerEncStatsStr
    end,
  lists:flatten(lists:map(Func , ListStatsEts)).