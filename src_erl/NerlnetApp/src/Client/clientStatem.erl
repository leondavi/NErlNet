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

-behaviour(gen_statem).

%% API
-export([start_link/1, predict/3]).

%% gen_statem callbacks
-export([init/1, format_status/2, handle_event/4, terminate/3,
  code_change/4, callback_mode/0, idle/3, training/3,waitforWorkers/3]).


-import(clientWorkersFunctions,[createWorkers/2]).
-import(nerlNIF,[validate_nerltensor_erl/1]).

-define(ETS_KV_VAL_IDX, 2). % key value pairs --> value index is 2
-define(WORKER_PID_IDX, 2).
-define(WORKER_TIMING_IDX, 4).
-define(WORKER_TRAIN_MISSED_IDX, 5).
-define(WORKER_PRED_MISSED_IDX, 6).
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
  io:format("Client ~p is connected to: ~p~n",[MyName, [digraph:vertex(NerlnetGraph,Vertex) || Vertex <- digraph:out_neighbours(NerlnetGraph,MyName)]]),
  % nerl_tools:start_connection([digraph:vertex(NerlnetGraph,Vertex) || Vertex <- digraph:out_neighbours(NerlnetGraph,MyName)]),
  EtsRef = ets:new(client_data, [set]),
  ets:insert(EtsRef, {workerToClient, WorkerToClientMap}),
  ets:insert(EtsRef, {nerlnetGraph, NerlnetGraph}),
  ets:insert(EtsRef, {msgCounter, 1}),
  ets:insert(EtsRef, {infoIn, 0}),
  ets:insert(EtsRef, {myName, MyName}),
  MyWorkersToShaMap = maps:filter(fun(Worker , _SHA) -> lists:member(Worker , ClientWorkers) end , WorkerShaMap),
  io:format("client ~p workers to sha map: ~p~n",[MyName, MyWorkersToShaMap]),
  ets:insert(EtsRef, {workers_to_sha_map, MyWorkersToShaMap}),
  ets:insert(EtsRef, {sha_to_models_map , ShaToModelArgsMap}),

  clientWorkersFunctions:create_workers(MyName , EtsRef , ShaToModelArgsMap),

  %% send pre_idle signal to workers
  [gen_statem:cast(ets:lookup_element(EtsRef, WorkerPID, ?WORKER_PID_IDX), {pre_idle}) || WorkerPID <- ets:lookup_element(EtsRef, workersNames, ?ETS_KV_VAL_IDX)],
  io:format("client ~p sent pre_idle signal to ~p",[MyName, ets:lookup_element(EtsRef, workersNames, ?WORKER_PID_IDX)]),

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
waitforWorkers(cast, In = {stateChange,WorkerName}, State = #client_statem_state{myName = MyName,waitforWorkers = WaitforWorkers,nextState = NextState, etsRef = EtsRef}) ->
  NewWaitforWorkers = WaitforWorkers--[WorkerName],
  % io:format("remaining workers = ~p~n",[NewWaitforWorkers]),
  ets:update_counter(EtsRef, msgCounter, 1), % last is increment value
  ets:update_counter(EtsRef, infoIn, nerl_tools:calculate_size(In)),
  case NewWaitforWorkers of
    [] ->   ack(MyName,ets:lookup_element(EtsRef, nerlnetGraph, 2)),
            % ?LOG_INFO("~p going to state ~p~n",[MyName, NextState]),
            {next_state, NextState, State#client_statem_state{waitforWorkers = []}};
    _->  {next_state, waitforWorkers, State#client_statem_state{waitforWorkers = NewWaitforWorkers}}
  end;

waitforWorkers(cast, In = {NewState}, State = #client_statem_state{myName = _MyName, etsRef = EtsRef}) ->
  ets:update_counter(EtsRef, msgCounter, 1),
  ets:update_counter(EtsRef, infoIn, nerl_tools:calculate_size(In)),
  % ?LOG_INFO("~p in waiting going to state ~p~n",[MyName, State]),
  Workers = ets:lookup_element(EtsRef, workersNames, ?ETS_KV_VAL_IDX),
  cast_message_to_workers(EtsRef, {NewState}),
  {next_state, waitforWorkers, State#client_statem_state{nextState = NewState, waitforWorkers = Workers}};

waitforWorkers(cast, EventContent, State = #client_statem_state{etsRef = EtsRef}) ->
  ets:update_counter(EtsRef, msgCounter, 1),
  ets:update_counter(EtsRef, infoIn, nerl_tools:calculate_size(EventContent)),
  ?LOG_WARNING("client waitforWorkers ignored!!!:  ~p ~n",[EventContent]),
  {next_state, waitforWorkers, State}.
  

%% initiating workers when they include federated workers. init stage == handshake between federated worker client and server
%% TODO: make custom_worker_message in all states to send messages from workers to entities (not just client)
idle(cast, In = {custom_worker_message, {From, To}}, State = #client_statem_state{etsRef = EtsRef, myName = MyName}) ->
  % io:format("client ~p got ~p~n",[MyName, {From, To}]),
  ets:update_counter(EtsRef, msgCounter, 1),
  ets:update_counter(EtsRef, infoIn, nerl_tools:calculate_size(In)),
  WorkerOfThisClient = ets:member(EtsRef, To),
  if WorkerOfThisClient -> 
    TargetWorkerPID = ets:lookup_element(EtsRef, To, ?WORKER_PID_IDX),
    gen_statem:cast(TargetWorkerPID,{post_idle,From});
  true ->
    %% send to FedServer that worker From is connecting to it
    NerlnetGraph = ets:lookup_element(EtsRef, nerlnetGraph, ?ETS_KV_VAL_IDX),
    DestClient = maps:get(To, ets:lookup_element(EtsRef, workerToClient, ?ETS_KV_VAL_IDX)),
    {Host,Port} = nerl_tools:getShortPath(MyName,DestClient,NerlnetGraph),
    Body = {DestClient, custom_worker_message, {From, To}},
    % io:format("client ~p passing ~p~n",[MyName, Body]),
    nerl_tools:http_request(Host,Port, "custom_worker_message", term_to_binary(Body))
  end,
  % io:format("initiating, CONFIG received:~p ~n",[CONFIG]),
  {keep_state, State};

idle(cast, In = {statistics}, State = #client_statem_state{ myName = _MyName, etsRef = EtsRef}) ->
  sendStatistics(EtsRef),
  ets:update_counter(EtsRef, msgCounter, 1), % last param is increment value
  ets:update_counter(EtsRef, infoIn, nerl_tools:calculate_size(In)),
  {next_state, idle, State};

idle(cast, In = {training}, State = #client_statem_state{etsRef = EtsRef}) ->
  ets:update_counter(EtsRef, msgCounter, 1),
  ets:update_counter(EtsRef, infoIn, nerl_tools:calculate_size(In)),
  MessageToCast = {training},
  cast_message_to_workers(EtsRef, MessageToCast),
  {next_state, waitforWorkers, State#client_statem_state{waitforWorkers= ets:lookup_element(EtsRef, workersNames, ?ETS_KV_VAL_IDX), nextState = training}};

idle(cast, In = {predict}, State = #client_statem_state{etsRef = EtsRef}) ->
  io:format("client going to state predict~n",[]),
  ets:update_counter(EtsRef, msgCounter, 1),
  ets:update_counter(EtsRef, infoIn, nerl_tools:calculate_size(In)),
  MessageToCast = {predict},
  cast_message_to_workers(EtsRef, MessageToCast),
  {next_state, waitforWorkers, State#client_statem_state{waitforWorkers= ets:lookup_element(EtsRef, workersNames, ?ETS_KV_VAL_IDX),nextState = predict}};

idle(cast, EventContent, State = #client_statem_state{etsRef = EtsRef}) ->
  ets:update_counter(EtsRef, msgCounter, 1),
  ets:update_counter(EtsRef, infoIn, nerl_tools:calculate_size(EventContent)),
  io:format("client idle ignored!!!:  ~p ~n",[EventContent]),
  {next_state, training, State#client_statem_state{etsRef = EtsRef}}.

%% passing vector from FedClient to FedServer
training(cast, In = {update, {From, To, Data}}, State = #client_statem_state{etsRef = EtsRef, myName = MyName}) ->
  % io:format("client ~p got ~p~n",[MyName, {update, From, To, nerlTensor}]),
  ets:update_counter(EtsRef, msgCounter, 1),
  ets:update_counter(EtsRef, infoIn, nerl_tools:calculate_size(In)),
  WorkerOfThisClient = ets:member(EtsRef, To),
  if WorkerOfThisClient -> 
    TargetWorkerPID = ets:lookup_element(EtsRef, To, ?WORKER_PID_IDX),
    gen_statem:cast(TargetWorkerPID,{update,From,To, Data});
  true ->
    %% send to FedServer that worker From is connecting to it
    NerlnetGraph = ets:lookup_element(EtsRef, nerlnetGraph, ?ETS_KV_VAL_IDX),
    DestClient = maps:get(To, ets:lookup_element(EtsRef, workerToClient, ?ETS_KV_VAL_IDX)),
    {Host,Port} = nerl_tools:getShortPath(MyName,DestClient,NerlnetGraph),
    Body = {DestClient, update, {From, To, Data}},
    % io:format("client ~p passing ~p~n",[MyName, {update, From, To, nerlTensor}]),
    nerl_tools:http_request(Host,Port, "pass", term_to_binary(Body))
  end,
  % io:format("initiating, CONFIG received:~p ~n",[CONFIG]),
  {keep_state, State};

%% federated server sends AvgWeights to workers
training(cast, In = {custom_worker_message, WorkersList, WeightsTensor}, State = #client_statem_state{etsRef = EtsRef, myName = MyName}) ->
  % io:format("client ~p got ~p~n",[MyName, {custom_worker_message, WorkersList, nerlTensor}]),
  ets:update_counter(EtsRef, msgCounter, 1),
  ets:update_counter(EtsRef, infoIn, nerl_tools:calculate_size(In)),
  NerlnetGraph = ets:lookup_element(EtsRef, nerlnetGraph, ?ETS_KV_VAL_IDX),
  Func = fun(WorkerName) ->
    DestClient = maps:get(WorkerName, ets:lookup_element(EtsRef, workerToClient, ?ETS_KV_VAL_IDX)),
    {Host,Port} = nerl_tools:getShortPath(MyName,DestClient,NerlnetGraph),
    Body = {DestClient, update, {_FedServer = "server", WorkerName, WeightsTensor}},
    % io:format("client ~p passing ~p~n",[MyName, Body]),
    % io:format("client ~p passing new weights~n",[MyName]),
    nerl_tools:http_request(Host,Port, "custom_worker_message", term_to_binary(Body))
  end,
  lists:foreach(Func, WorkersList),
  {keep_state, State};
  
  
% TODO Validate this state - sample and empty list 
training(cast, In = {sample,[]}, State = #client_statem_state{etsRef = EtsRef}) ->
  ets:update_counter(EtsRef, msgCounter, 1),
  ets:update_counter(EtsRef, infoIn, nerl_tools:calculate_size(In)),
  io:format("client got empty Vector~n",[]),
  {next_state, training, State#client_statem_state{etsRef = EtsRef}};

training(cast, In = {sample,Body}, State = #client_statem_state{etsRef = EtsRef}) ->
  ets:update_counter(EtsRef, msgCounter, 1),
  ets:update_counter(EtsRef, infoIn, nerl_tools:calculate_size(In)),
  % io:format("client got sample ~p~n",[binary_to_term(Body)]),
  %%    Body:   {ClientName,WorkerName,CSVName,BatchNumber,BatchOfSamples}
  {ClientName, WorkerNameStr, _CSVName, _BatchNumber, BatchOfSamples} = binary_to_term(Body),
  WorkerName = list_to_atom(WorkerNameStr),
  WorkerOfThisClient = ets:member(EtsRef, WorkerName),
  if WorkerOfThisClient ->
      WorkerPid = ets:lookup_element(EtsRef, WorkerName, ?WORKER_PID_IDX),
      TimingTuple = ets:lookup_element(EtsRef, WorkerName, ?WORKER_TIMING_IDX), %todo refactor timing map
      {_LastBatchReceivedTime,TotalBatches,TotalTime} = TimingTuple,
      Start = os:timestamp(),
      NewTimingTuple = {Start,TotalBatches+1,TotalTime},
      ets:update_element(EtsRef, WorkerName,[{?WORKER_PID_IDX, WorkerPid},{?WORKER_TIMING_IDX,NewTimingTuple}]),
      gen_statem:cast(WorkerPid, {sample, BatchOfSamples});
  true -> ?LOG_ERROR("Given worker ~p isn't found in client ~p",[WorkerName, ClientName]) end,
  {next_state, training, State#client_statem_state{etsRef = EtsRef}};

training(cast, In = {idle}, State = #client_statem_state{myName = _MyName, etsRef = EtsRef}) ->
  ets:update_counter(EtsRef, msgCounter, 1),
  ets:update_counter(EtsRef, infoIn, nerl_tools:calculate_size(In)),
  % ?LOG_INFO("~p going to state idle~n",[MyName]),
  MessageToCast = {idle},
  cast_message_to_workers(EtsRef, MessageToCast),
  Workers = ets:lookup_element(EtsRef, workersNames, ?ETS_KV_VAL_IDX),
  ?LOG_INFO("setting workers at idle: ~p~n",[ets:lookup_element(EtsRef, workersNames, ?DATA_IDX)]),
  {next_state, waitforWorkers, State#client_statem_state{etsRef = EtsRef, waitforWorkers = Workers}};

training(cast, In = {predict}, State = #client_statem_state{myName = MyName, etsRef = EtsRef}) ->
  ets:update_counter(EtsRef, msgCounter, 1),
  ets:update_counter(EtsRef, infoIn, nerl_tools:calculate_size(In)),
  io:format("~p going to state predict~n",[MyName]),
  MessageToCast = {predict},
  cast_message_to_workers(EtsRef,MessageToCast),
  Workers = ets:lookup_element(EtsRef, workersNames, ?ETS_KV_VAL_IDX),
  {next_state, waitforWorkers, State#client_statem_state{nextState = predict,  waitforWorkers = Workers, etsRef = EtsRef}};


% training get path to main server
training(cast, In = {loss,WorkerName,nan,_Time_NIF}, State = #client_statem_state{myName = MyName, etsRef = EtsRef}) ->
  ets:update_counter(EtsRef, msgCounter, 1),
  ets:update_counter(EtsRef, infoIn, nerl_tools:calculate_size(In)),
  NerlnetGraph = ets:lookup_element(EtsRef, nerlnetGraph, ?ETS_KV_VAL_IDX),
  {RouterHost,RouterPort} = nerl_tools:getShortPath(MyName,?MAIN_SERVER_ATOM,NerlnetGraph),
  nerl_tools:http_request(RouterHost,RouterPort,"lossFunction", term_to_binary({WorkerName,"nan"})),
  {next_state, training, State#client_statem_state{etsRef = EtsRef}};

training(cast, In = {loss,WorkerName,LossFunction,_Time_NIF}, State = #client_statem_state{myName = MyName,etsRef = EtsRef}) ->
  ets:update_counter(EtsRef, msgCounter, 1),
  ets:update_counter(EtsRef, infoIn, nerl_tools:calculate_size(In)),
  NerlnetGraph = ets:lookup_element(EtsRef, nerlnetGraph, ?ETS_KV_VAL_IDX),
  updateTimingMap(EtsRef, WorkerName),
  {RouterHost,RouterPort} = nerl_tools:getShortPath(MyName,?MAIN_SERVER_ATOM,NerlnetGraph),
  nerl_tools:http_request(RouterHost,RouterPort,"lossFunction", term_to_binary({WorkerName,LossFunction})),
  {next_state, training, State#client_statem_state{myName = MyName,etsRef = EtsRef}};

training(cast, EventContent, State = #client_statem_state{etsRef = EtsRef, myName = MyName}) ->
  ?LOG_WARNING("client ~p training ignored!!!:  ~p ~n!!!",[MyName, EventContent]),
  ets:update_counter(EtsRef, infoIn, nerl_tools:calculate_size(EventContent)),
  {next_state, training, State#client_statem_state{etsRef = EtsRef}}.

predict(cast, In = {sample,Body}, State = #client_statem_state{etsRef = EtsRef}) ->
  %%    Body:   ClientName#WorkerName#CSVName#BatchNumber#BatchOfSamples
  ets:update_counter(EtsRef, msgCounter, 1),
  ets:update_counter(EtsRef, infoIn, nerl_tools:calculate_size(In)),
  {ClientName, WorkerNameStr, CSVName, BatchNumber, BatchOfSamples} = binary_to_term(Body),
  WorkerName = list_to_atom(WorkerNameStr),

  Start = os:timestamp(),
  WorkerOfThisClient = ets:member(EtsRef, WorkerName),
  if 
    WorkerOfThisClient ->
    TimingTuple = ets:lookup_element(EtsRef, WorkerName, ?WORKER_TIMING_IDX), %todo refactor timing map
    {_LastBatchReceivedTime,TotalBatches,TotalTime} = TimingTuple,
    NewTimingTuple = {Start,TotalBatches+1,TotalTime},
    ets:update_element(EtsRef, WorkerName,[{?WORKER_TIMING_IDX,NewTimingTuple}]);
    true -> ?LOG_ERROR("Given worker ~p isn't found in client ~p",[WorkerName, ClientName])
  end,

  WorkerPid = ets:lookup_element(EtsRef, WorkerName, ?WORKER_PID_IDX),
  gen_statem:cast(WorkerPid, {sample, CSVName, BatchNumber, BatchOfSamples}),
  {next_state, predict, State#client_statem_state{etsRef = EtsRef}};

%% TODO: add nif timing statistics
predict(cast, In = {predictRes,WorkerName,InputName,ResultID,PredictNerlTensor, Type, _TimeTook}, State = #client_statem_state{myName = MyName, etsRef = EtsRef}) ->
  ets:update_counter(EtsRef, msgCounter, 1),
  ets:update_counter(EtsRef, infoIn, nerl_tools:calculate_size(In)),
  NerlnetGraph = ets:lookup_element(EtsRef, nerlnetGraph, ?ETS_KV_VAL_IDX),
  updateTimingMap(EtsRef, WorkerName),    
  {RouterHost,RouterPort} = nerl_tools:getShortPath(MyName,?MAIN_SERVER_ATOM,NerlnetGraph),

  nerl_tools:http_request(RouterHost,RouterPort,"predictRes", term_to_binary({atom_to_list(WorkerName), InputName, ResultID, {PredictNerlTensor, Type}})),
  {next_state, predict, State#client_statem_state{etsRef = EtsRef}};

% TODO from predict directly to training?!?!?
predict(cast, In = {training}, State = #client_statem_state{etsRef = EtsRef}) ->
  ets:update_counter(EtsRef, msgCounter, 1),
  ets:update_counter(EtsRef, infoIn, nerl_tools:calculate_size(In)),
  MsgToCast =  {training},
  cast_message_to_workers(EtsRef, MsgToCast),
  Workers = ets:lookup_element(EtsRef, workersNames, ?ETS_KV_VAL_IDX),
  {next_state, waitforWorkers, State#client_statem_state{nextState = training, etsRef = EtsRef,  waitforWorkers = Workers}};


predict(cast, In = {idle}, State = #client_statem_state{etsRef = EtsRef}) ->
  MsgToCast = {idle},
  ets:update_counter(EtsRef, msgCounter, 1),
  ets:update_counter(EtsRef, infoIn, nerl_tools:calculate_size(In)),
  cast_message_to_workers(EtsRef, MsgToCast),
  ?LOG_INFO("client going to state idle"),
  Workers = ets:lookup_element(EtsRef, workersNames, ?ETS_KV_VAL_IDX),
  {next_state, waitforWorkers, State#client_statem_state{nextState = idle, waitforWorkers = Workers, etsRef = EtsRef}};

predict(cast, EventContent, State = #client_statem_state{etsRef = EtsRef}) ->
  ets:update_counter(EtsRef, msgCounter, 1),
  ets:update_counter(EtsRef, infoIn, erts_debug:flat_size(EventContent)),
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

ack(MyName, NerlnetGraph) ->
  % ?LOG_INFO("~p sending ACK   ~n",[MyName]),
  {RouterHost,RouterPort} = nerl_tools:getShortPath(MyName,?MAIN_SERVER_ATOM,NerlnetGraph),
  %%  send an ACK to mainserver that the client is ready
  nerl_tools:http_request(RouterHost,RouterPort,"unicast",term_to_binary({?MAIN_SERVER_ATOM,{"clientReady",MyName}})).

% calculates the avarage training time
updateTimingMap(EtsRef, WorkerName) when is_atom(WorkerName) ->
  {Start,TotalBatches,TotalTime} = ets:lookup_element(EtsRef, WorkerName, ?WORKER_TIMING_IDX), % retrieving old value
  Finish = os:timestamp(),
  TotalTrainingTime = (timer:now_diff(Finish, Start) / 1000),
  NewTimingTuple = {Start,TotalBatches,TotalTrainingTime+TotalTime},
  ets:update_element(EtsRef, WorkerName,[{?WORKER_TIMING_IDX,NewTimingTuple}]). %% TODO update WorkerStatsETS

%% statistics format: clientName:workerName=avgTime,...
%% adding client c1=MsgNum,w1=...
sendStatistics(EtsRef)->
  NerlnetGraph = ets:lookup_element(EtsRef, nerlnetGraph, ?ETS_KV_VAL_IDX),
  Workers = ets:lookup_element(EtsRef, workersNames, ?ETS_KV_VAL_IDX),
  TimingMap = [{WorkerKey,ets:lookup_element(EtsRef, WorkerKey, ?WORKER_TIMING_IDX)} || WorkerKey <- Workers],
  MissedCounts = [{WorkerKey,ets:lookup_element(EtsRef, WorkerKey, ?WORKER_TRAIN_MISSED_IDX)} || WorkerKey <- Workers],
  Counter = ets:lookup_element(EtsRef, msgCounter, ?ETS_KV_VAL_IDX),
  InfoSize = ets:lookup_element(EtsRef, infoIn, ?ETS_KV_VAL_IDX),
  MyName = ets:lookup_element(EtsRef, myName, ?ETS_KV_VAL_IDX),

  TimingStats = lists:flatten([atom_to_list(WorkerName)++"_Train_Avg_Time="++float_to_list(TotalTime/TotalBatches,[{decimals, 3}])++","||{WorkerName,{_LastTime,TotalBatches,TotalTime}}<-TimingMap]),
  MissingStats = lists:flatten([atom_to_list(WorkerName)++"_Train_Miss="++integer_to_list(MissCount)++","||{WorkerName,MissCount}<-MissedCounts]),
  MyStats = atom_to_list(MyName)++"_Msg_Count="++integer_to_list(Counter)++","++atom_to_list(MyName)++"_info_Size="++integer_to_list(InfoSize)++",",

  {RouterHost,RouterPort} = nerl_tools:getShortPath(MyName, ?MAIN_SERVER_ATOM, NerlnetGraph),
  nerl_tools:http_request(RouterHost,RouterPort,"statistics", list_to_binary(atom_to_list(MyName)++":"++MyStats++MissingStats++lists:droplast(TimingStats))).

cast_message_to_workers(EtsRef, Msg) ->
  Workers = ets:lookup_element(EtsRef, workersNames, ?ETS_KV_VAL_IDX),
  Func = fun(WorkerKey) -> 
    WorkerPid = ets:lookup_element(EtsRef, WorkerKey, ?WORKER_PID_IDX),
    gen_statem:cast(WorkerPid, Msg)
  end,
  lists:foreach(Func, Workers).