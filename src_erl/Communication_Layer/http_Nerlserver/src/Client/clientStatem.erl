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
-export([init/1, format_status/2, state_name/3, handle_event/4, terminate/3,
  code_change/4, callback_mode/0, idle/3, training/3,waitforWorkers/3]).

-define(ETF_KV_VAL_IDX, 2). % key value pairs --> value index is 2
-define(WORKER_PID_IDX, 2).
-define(WORKER_TIMING_IDX, 4).
-define(SERVER, ?MODULE).

%   myName - Client Name,
%   federatedServer - fed.server name,
%   workersMap - this clients workers on this machine,
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


%%NerlClientsArgs=[{MyName,Workers,ConnectionsMap},...], Workers = list of maps of name and args
%%  init nerlClient with given workers and parameters, and build a map :#{workerName=>WorkerPid,...}
init({MyName,NerlnetGraph}) ->
  inets:start(),
  io:format("Client ~p is connected to: ~p~n",[MyName, [digraph:vertex(NerlnetGraph,Vertex) || Vertex <- digraph:out_neighbours(NerlnetGraph,MyName)]]),
  % nerl_tools:start_connection([digraph:vertex(NerlnetGraph,Vertex) || Vertex <- digraph:out_neighbours(NerlnetGraph,MyName)]),
  EtsRef = ets:new(client_data, [set]),
  ets:insert(EtsRef, {nerlnetGraph, NerlnetGraph}),
  ets:insert(EtsRef, {msgCounter, 1}),

  createWorkers(MyName,EtsRef),

  {ok, idle, #client_statem_state{myName= MyName, etsRef = EtsRef}}.

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
state_name(_EventType, _EventContent, State = #client_statem_state{}) ->
  NextStateName = next_state,
  {next_state, NextStateName, State}.


waitforWorkers(cast, {stateChange,WorkerName}, State = #client_statem_state{myName = MyName,waitforWorkers = WaitforWorkers,nextState = NextState, etsRef = EtsRef}) ->
  NewWaitforWorkers = WaitforWorkers--[WorkerName],

  ets:update_counter(EtsRef, msgCounter, 1), % last is increment value
  case NewWaitforWorkers of
    [] ->   ack(MyName,ets:lookup_element(EtsRef, nerlnetGraph, 2)),
            {next_state, NextState, State#client_statem_state{waitforWorkers = []}};
    _->  {next_state, waitforWorkers, State#client_statem_state{waitforWorkers = NewWaitforWorkers}}
  end;

waitforWorkers(cast, {NewState}, State = #client_statem_state{etsRef = EtsRef}) ->
  ets:update_counter(EtsRef, msgCounter, 1),
  ?LOG_INFO("client going to state ~p~n",[State]),

  Workers = ets:lookup_element(EtsRef, workersNames, ?ETF_KV_VAL_IDX),    %% TODO: macro this 2
  cast_message_to_workers(EtsRef, {NewState}),
  {next_state, waitforWorkers, State#client_statem_state{nextState = NewState, waitforWorkers = Workers}};

  
waitforWorkers(cast, EventContent, State = #client_statem_state{etsRef = EtsRef}) ->
  ets:update_counter(EtsRef, msgCounter, 1),
  ?LOG_WARNING("client waitforWorkers ignored!!!:  ~p ~n",[EventContent]),
  {next_state, waitforWorkers, State}.
  

%%initiating workers when they include federated workers. init stage == handshake between federated worker client and server
idle(cast, {custom_worker_message, {From, To}}, State = #client_statem_state{etsRef = EtsRef, myName = MyName}) ->
  ets:update_counter(EtsRef, msgCounter, 1),
  WorkerHere = ets:member(EtsRef, To),
  if WorkerHere -> 
    TargetWorkerPID = ets:lookup_element(EtsRef, To, ?WORKER_PID_IDX),
    gen_statem:cast(TargetWorkerPID,{init,From});
  true ->
    %% send to FedServer that worker From is connecting to it
    NerlnetGraph = ets:lookup_element(EtsRef, nerlnetGraph, ?ETF_KV_VAL_IDX),
    {Host,Port} = nerl_tools:getShortPath(MyName,To,NerlnetGraph),
    nerl_tools:http_request(Host,Port, "init", From ++"#"++ To)
  end,
  % io:format("initiating, CONFIG received:~p ~n",[CONFIG]),
  {next_state, idle, State};

idle(cast, {statistics}, State = #client_statem_state{ myName = MyName, etsRef = EtsRef}) ->
  NerlnetGraph = ets:lookup_element(EtsRef, nerlnetGraph, ?ETF_KV_VAL_IDX),
  {RouterHost,RouterPort} = nerl_tools:getShortPath(MyName,?MAIN_SERVER_ATOM,NerlnetGraph),
  Workers = ets:lookup_element(EtsRef, workersNames, ?ETF_KV_VAL_IDX),    %% TODO: macro this 2
  TimingMap = [{WorkerKey,ets:lookup_element(EtsRef, WorkerKey, ?WORKER_TIMING_IDX)} || WorkerKey <- Workers],
  Counter = ets:lookup_element(EtsRef, msgCounter, ?ETF_KV_VAL_IDX),
  sendStatistics(RouterHost,RouterPort,MyName,Counter,TimingMap),
  ets:update_counter(EtsRef, msgCounter, 1), % last param is increment value
  {next_state, idle, State};

idle(cast, {training}, State = #client_statem_state{etsRef = EtsRef}) ->
  ets:update_counter(EtsRef, msgCounter, 1),
  MessageToCast = {training},
  cast_message_to_workers(EtsRef, MessageToCast),
  {next_state, waitforWorkers, State#client_statem_state{nextState = training}};

idle(cast, {predict}, State = #client_statem_state{etsRef = EtsRef}) ->
  io:format("client going to state predict",[]),
  ets:update_counter(EtsRef, msgCounter, 1),
  MessageToCast = {predict},
  cast_message_to_workers(EtsRef, MessageToCast),
  {next_state, waitforWorkers, State#client_statem_state{nextState = predict}};

idle(cast, EventContent, State = #client_statem_state{etsRef = EtsRef}) ->
  ets:update_counter(EtsRef, msgCounter, 1),
  io:format("client idle ignored!!!:  ~p ~n",[EventContent]),
  {next_state, training, State#client_statem_state{etsRef = EtsRef}}.


% TODO Validate this state - sample and empty list 
training(cast, {sample,[]}, State = #client_statem_state{etsRef = EtsRef}) ->
  ets:update_counter(EtsRef, msgCounter, 1),
  io:format("client got empty Vector~n",[]),
  {next_state, training, State#client_statem_state{etsRef = EtsRef}};

training(cast, {sample,Body}, State = #client_statem_state{etsRef = EtsRef}) ->
  ets:update_counter(EtsRef, msgCounter, 1),
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

training(cast, {idle}, State = #client_statem_state{etsRef = EtsRef}) ->
  ets:update_counter(EtsRef, msgCounter, 1),
  io:format("client going to state idle~n",[]),
  MessageToCast = {idle},
  cast_message_to_workers(EtsRef, MessageToCast),
  Workers = ets:lookup_element(EtsRef, workersNames, ?ETF_KV_VAL_IDX),
  io:format("setting workers at idle: ~p~n",[ets:lookup_element(EtsRef, workersNames, ?DATA_IDX)]),
  {next_state, waitforWorkers, State#client_statem_state{etsRef = EtsRef, waitforWorkers = Workers}};

training(cast, {predict}, State = #client_statem_state{etsRef = EtsRef}) ->
  ets:update_counter(EtsRef, msgCounter, 1),
  io:format("client going to state predict",[]),
  MessageToCast = {predict},
  cast_message_to_workers(EtsRef,MessageToCast),
  Workers = ets:lookup_element(EtsRef, workersNames, ?ETF_KV_VAL_IDX),
  {next_state, waitforWorkers, State#client_statem_state{nextState = predict,  waitforWorkers = Workers, etsRef = EtsRef}};


% training get path to main server
training(cast, {loss,WorkerName,nan,_Time_NIF}, State = #client_statem_state{myName = MyName, etsRef = EtsRef}) ->
  ets:update_counter(EtsRef, msgCounter, 1),
  NerlnetGraph = ets:lookup_element(EtsRef, nerlnetGraph, ?ETF_KV_VAL_IDX),
  {RouterHost,RouterPort} = nerl_tools:getShortPath(MyName,?MAIN_SERVER_ATOM,NerlnetGraph),
  nerl_tools:http_request(RouterHost,RouterPort,"lossFunction", term_to_binary({WorkerName,"nan"})),
  {next_state, training, State#client_statem_state{etsRef = EtsRef}};

training(cast, {loss,WorkerName,LossFunction,_Time_NIF}, State = #client_statem_state{myName = MyName,etsRef = EtsRef}) ->
  ets:update_counter(EtsRef, msgCounter, 1),
  NerlnetGraph = ets:lookup_element(EtsRef, nerlnetGraph, ?ETF_KV_VAL_IDX),
  updateTimingMap(EtsRef, WorkerName),
  {RouterHost,RouterPort} = nerl_tools:getShortPath(MyName,?MAIN_SERVER_ATOM,NerlnetGraph),
  nerl_tools:http_request(RouterHost,RouterPort,"lossFunction", term_to_binary({WorkerName,LossFunction})),
  {next_state, training, State#client_statem_state{myName = MyName,etsRef = EtsRef}};

training(cast, EventContent, State = #client_statem_state{etsRef = EtsRef}) ->
  ?LOG_WARNING("client training ignored!!!:  ~p ~n!!!",[EventContent]),
  {next_state, training, State#client_statem_state{etsRef = EtsRef}}.

predict(cast, {sample,Body}, State = #client_statem_state{etsRef = EtsRef}) ->
  %%    Body:   ClientName#WorkerName#CSVName#BatchNumber#BatchOfSamples
  ets:update_counter(EtsRef, msgCounter, 1),
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
predict(cast, {predictRes,WorkerName,InputName,ResultID,PredictNerlTensor, Type, _TimeTook}, State = #client_statem_state{myName = MyName, etsRef = EtsRef}) ->
  ets:update_counter(EtsRef, msgCounter, 1),
  NerlnetGraph = ets:lookup_element(EtsRef, nerlnetGraph, ?ETF_KV_VAL_IDX),
  updateTimingMap(EtsRef, WorkerName),    
  {RouterHost,RouterPort} = nerl_tools:getShortPath(MyName,?MAIN_SERVER_ATOM,NerlnetGraph),

  nerl_tools:http_request(RouterHost,RouterPort,"predictRes", term_to_binary({atom_to_list(WorkerName), InputName, ResultID, {PredictNerlTensor, Type}})),
  {next_state, predict, State#client_statem_state{etsRef = EtsRef}};

% TODO from predict directly to training?!?!?
predict(cast, {training}, State = #client_statem_state{etsRef = EtsRef}) ->
  ets:update_counter(EtsRef, msgCounter, 1),
  MsgToCast =  {training},
  cast_message_to_workers(EtsRef, MsgToCast),
  Workers = ets:lookup_element(EtsRef, workersNames, ?ETF_KV_VAL_IDX),    %% TODO: macro this 2
  {next_state, waitforWorkers, State#client_statem_state{nextState = training, etsRef = EtsRef,  waitforWorkers = Workers}};


predict(cast, {idle}, State = #client_statem_state{etsRef = EtsRef}) ->
  MsgToCast = {idle},
  ets:update_counter(EtsRef, msgCounter, 1),
  cast_message_to_workers(EtsRef, MsgToCast),
  ?LOG_INFO("client going to state idle"),
  Workers = ets:lookup_element(EtsRef, workersNames, ?ETF_KV_VAL_IDX),    %% TODO: macro this 2
  {next_state, waitforWorkers, State#client_statem_state{nextState = idle, waitforWorkers = Workers, etsRef = EtsRef}};

predict(cast, EventContent, State = #client_statem_state{etsRef = EtsRef}) ->
  ets:update_counter(EtsRef, msgCounter, 1),
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
  ?LOG_INFO("~p sending ACK   ~n",[MyName]),
  {RouterHost,RouterPort} = nerl_tools:getShortPath(MyName,?MAIN_SERVER_ATOM,NerlnetGraph),
  %%  send an ACK to mainserver that the client is ready
  nerl_tools:http_request(RouterHost,RouterPort,"clientReady",MyName).

createWorkers(ClientName, EtsRef) ->
  CLIENT_WORKES_MAPS_TUPLE_IDX = 3,
  MapOfHostClients = maps:from_list(ets:lookup_element(nerlnet_data, hostClients, ?DATA_IDX)), % This is the format of hostClients {Name,{Port,ClientWorkers,ClientWorkersMaps}}
  ClientWorkersMaps = element(CLIENT_WORKES_MAPS_TUPLE_IDX,maps:get(ClientName, MapOfHostClients)),
  Func = fun(WorkerMap) -> 
    ModelId = erlang:unique_integer([positive]),
    %%  TODO: move to json parser
    WorkerName = list_to_atom(binary_to_list(maps:get(<<"name">>,WorkerMap))),
    ModelType = list_to_integer(binary_to_list(maps:get(<<"modelType">>,WorkerMap))),
    ScalingMethod = list_to_integer(binary_to_list(maps:get(<<"scalingMethod">>,WorkerMap))),

    LayerTypesList = nerl_tools:string_to_list_int(maps:get(<<"layerTypesList">>,WorkerMap)),
    LayersSizes = nerl_tools:string_to_list_int(maps:get(<<"layersSizes">>,WorkerMap)),
    LayersActivationFunctions = nerl_tools:string_to_list_int(maps:get(<<"layersActivationFunctions">>,WorkerMap)),

    Optimizer = list_to_integer(binary_to_list(maps:get(<<"optimizer">>,WorkerMap))),
    Features = list_to_integer(binary_to_list(maps:get(<<"features">>,WorkerMap))),
    Labels = list_to_integer(binary_to_list(maps:get(<<"labels">>,WorkerMap))),
    LossMethod = list_to_integer(binary_to_list(maps:get(<<"lossMethod">>,WorkerMap))),
    LearningRate = list_to_float(binary_to_list(maps:get(<<"learningRate">>,WorkerMap))),

    % TODO add documentation about this case of 
    case ModelType of
      ?E_CUSTOMNN ->
        CustomFunc = fun workerNN:controller/2,
        WorkerData = none;
      ?E_FEDERATED_CLIENT ->
        CustomFunc = fun workerFederatedClient:controller/2,
        FedServer = list_to_atom(binary_to_list(maps:get(<<"federatedServer">>,WorkerMap))),
        SyncCount = list_to_integer(binary_to_list(maps:get(<<"syncCount">>,WorkerMap))),
        WorkerData = #workerFederatedClient{syncMaxCount = SyncCount, syncCount = 0, myName = WorkerName, clientPID = self(), serverName = FedServer};
      ?E_FEDERATED_SERVER ->
        CustomFunc = fun workerFederatedServer:controller/2,
        SyncCount = list_to_integer(binary_to_list(maps:get(<<"syncCount">>,WorkerMap))),
        WorkerData = #workerFederatedServer{syncMaxCount = SyncCount, syncCount = 0, myName = WorkerName, clientPID = self(), workersNamesList = []}
      end,

    WorkerArgs = {WorkerName,ModelId,ModelType,ScalingMethod, LayerTypesList, LayersSizes,
      LayersActivationFunctions, Optimizer, Features, Labels, LossMethod, LearningRate, self(), CustomFunc, WorkerData},

    WorkerPid = workerGeneric:start_link(WorkerArgs),

    ets:insert(EtsRef, {WorkerName, WorkerPid, WorkerArgs, {0,0,0.0}}),
    WorkerName
  end,

  WorkersNames = lists:map(Func, ClientWorkersMaps),
  ets:insert(EtsRef, {workersNames, WorkersNames}).   %% TODO: collect forbidden names (keys of ets:insert)

% calculates the avarage training time
updateTimingMap(EtsRef, WorkerName) when is_atom(WorkerName) ->
  {Start,TotalBatches,TotalTime} = ets:lookup_element(EtsRef, WorkerName, ?WORKER_TIMING_IDX), % retrieving old value
  Finish = os:timestamp(),
  TotalTrainingTime = (timer:now_diff(Finish, Start) / 1000),
  NewTimingTuple = {Start,TotalBatches,TotalTrainingTime+TotalTime},
  ets:update_element(EtsRef, WorkerName,[{?WORKER_TIMING_IDX,NewTimingTuple}]).


sendStatistics(RouterHost,RouterPort,MyName,_MsgCount,TimingTuples)->
  Statistics = lists:flatten([atom_to_list(WorkerName)++"="++float_to_list(TotalTime/TotalBatches,[{decimals, 3}])++","||{WorkerName,{_LastTime,TotalBatches,TotalTime}}<-TimingTuples]),
  nerl_tools:http_request(RouterHost,RouterPort,"statistics", list_to_binary(atom_to_list(MyName)++":"++lists:droplast(Statistics))).

cast_message_to_workers(EtsRef, Msg) ->
  Workers = ets:lookup_element(EtsRef, workersNames, 2),    %% TODO: macro this 2
  Func = fun(WorkerKey) -> 
    WorkerPid = ets:lookup_element(EtsRef, WorkerKey, ?WORKER_PID_IDX),
    gen_statem:cast(WorkerPid, Msg)
  end,
  lists:foreach(Func, Workers).