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
    {ok,Pid} = gen_statem:start_link(?MODULE, Args, []),
    Pid.

%%%===================================================================
%%% gen_statem callbacks
%%%===================================================================

%% @private
%% @doc Whenever a gen_statem is started using gen_statem:start/[3,4] or
%% gen_statem:start_link/[3,4], this function is called by the new
%% process to initialize.
%%initialize and go to state - idle


%%NerlClientsArgs=[{MyName,Workers,ConnectionsMap},...], Workers = list of maps of name and args
%%  init nerlClient with given workers and parameters, and build a map :#{workerName=>WorkerPid,...}
init({MyName,Workers,NerlnetGraph}) ->
  inets:start(),
  io:format("Client ~p Connecting to: ~p~n",[MyName, [digraph:vertex(NerlnetGraph,Vertex) || Vertex <- digraph:out_neighbours(NerlnetGraph,MyName)]]),
  % nerl_tools:start_connection([digraph:vertex(NerlnetGraph,Vertex) || Vertex <- digraph:out_neighbours(NerlnetGraph,MyName)]),
  EtsRef = ets:new(client_data, [set]),
  ets:insert(client_data, {nerlnetGraph, NerlnetGraph}),
  ets:insert(client_data, {msgCounter, 1}),

  createWorkers(),

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

  ets:update_counter(EtsRef, msgCounter, 1),
  case NewWaitforWorkers of
    [] ->   ack(MyName,ets:lookup_element(EtsRef, nerlnetGraph, 2)),
            {next_state, NextState, State#client_statem_state{waitforWorkers = []}};
    _->  {next_state, waitforWorkers, State#client_statem_state{waitforWorkers = NewWaitforWorkers}}
  end;

waitforWorkers(cast, {NewState}, State = #client_statem_state{etsRef = EtsRef}) ->
  ets:update_counter(EtsRef, msgCounter, 1),
  io:format("client going to state ~p~n",[State]),

  Workers = ets:lookup_element(EtsRef, workersNames, 2),    %% TODO: macro this 2
  Func = fun(WorkerKey) -> 
    WorkerPid = ets:lookup_element(EtsRef, WorkerKey, ?WORKER_PID_IDX),
    gen_statem:cast(WorkerPid,{NewState})
  end,
  lists:foreach(Func, Workers),
  {next_state, waitforWorkers, State#client_statem_state{nextState = NewState, waitforWorkers = Workers}};

  
waitforWorkers(cast, EventContent, State = #client_statem_state{etsRef = EtsRef}) ->
  ets:update_counter(EtsRef, msgCounter, 1),
  io:format("client waitforWorkers ignored!!!:  ~p ~n",[EventContent]),
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
    NerlnetGraph = ets:lookup_element(EtsRef, nerlnetGraph, 2),
    {Host,Port} = nerl_tools:getShortPath(MyName,To,NerlnetGraph),
    nerl_tools:http_request(Host,Port, "init", From ++"#"++ To)
  end,
  % io:format("initiating, CONFIG received:~p ~n",[CONFIG]),
  {next_state, idle, State};

idle(cast, {statistics}, State = #client_statem_state{ myName = MyName, etsRef = EtsRef}) ->
  NerlnetGraph = ets:lookup_element(EtsRef, nerlnetGraph, 2),
  {RouterHost,RouterPort} = nerl_tools:getShortPath(MyName,"mainServer",NerlnetGraph),
  Workers = ets:lookup_element(EtsRef, workersNames, 2),    %% TODO: macro this 2
  TimingMap = [ets:lookup_element(EtsRef, WorkerKey, ?WORKER_TIMING_IDX) || WorkerKey <- Workers],
  Counter = ets:lookup_element(EtsRef, msgCounter, 2),
  sendStatistics(RouterHost,RouterPort,MyName,Counter,TimingMap),
  ets:update_counter(EtsRef, msgCounter, 1),
  {next_state, idle, State};


idle(cast, {training}, State = #client_statem_state{workersMap = WorkersMap, msgCounter = Counter}) ->
  Workers = maps:to_list(WorkersMap),
  [gen_statem:cast(WorkerPid,{training})|| {_WorkerName,WorkerPid}<-Workers],
MyWorkers =  [WorkerName|| {WorkerName,_WorkerPid}<-Workers],
%%  send an ACK to mainserver that the CSV file is ready
  % ack(MyName,NerlnetGraph),
  {next_state, waitforWorkers, State#client_statem_state{nextState = training, msgCounter = Counter+1,waitforWorkers = MyWorkers}};

idle(cast, {predict}, State = #client_statem_state{workersMap = WorkersMap,msgCounter = Counter}) ->
  io:format("client going to state predict",[]),
  Workers = maps:to_list(WorkersMap),
  [gen_statem:cast(WorkerPid,{predict})|| {_WorkerName,WorkerPid} <- Workers],
  MyWorkers =  [WorkerName|| {WorkerName,_WorkerPid} <- Workers],
  {next_state, waitforWorkers, State#client_statem_state{nextState = predict, waitforWorkers = MyWorkers, msgCounter = Counter+1}};

idle(cast, EventContent, State = #client_statem_state{msgCounter = Counter}) ->
  io:format("client idle ignored!!!:  ~p ~n",[EventContent]),
  {next_state, training, State#client_statem_state{msgCounter = Counter+1}}.


training(cast, {sample,[]}, State = #client_statem_state{msgCounter = Counter}) ->
  io:format("client got empty Vector~n",[]),
  {next_state, training, State#client_statem_state{msgCounter = Counter+1}};

training(cast, {sample,Body}, State = #client_statem_state{msgCounter = Counter,workersMap = WorkersMap,timingMap = TimingMap}) ->
  %%    Body:   {ClientName,WorkerName,CSVName,BatchNumber,BatchOfSamples}
  {_ClientName, WorkerName, _CSVName, _BatchNumber, BatchOfSamples} = binary_to_term(Body),
  % ToSend =  decodeList(BatchOfSamples),
  Start = os:timestamp(),
  {_LastBatchReceivedTime,TotalBatches,TotalTime} = maps:get(list_to_atom(WorkerName),TimingMap),
  NewTimingMap = maps:put(list_to_atom(WorkerName),{Start,TotalBatches+1,TotalTime},TimingMap),
  WorkerPid = maps:get(list_to_atom(WorkerName),WorkersMap),
  gen_statem:cast(WorkerPid, {sample,BatchOfSamples}),
  {next_state, training, State#client_statem_state{msgCounter = Counter+1,timingMap = NewTimingMap}};

training(cast, {idle}, State = #client_statem_state{workersMap = WorkersMap,msgCounter = Counter}) ->
  io:format("client going to state idle~n",[]),
  Workers = maps:to_list(WorkersMap),
  [gen_statem:cast(WorkerPid,{idle})|| {_WorkerName,WorkerPid}<-Workers],
  MyWorkers =  [WorkerName|| {WorkerName,_WorkerPid}<-Workers],
  io:format("setting workers at idle: ~p~n",[MyWorkers]),
  {next_state, waitforWorkers, State#client_statem_state{waitforWorkers = MyWorkers, nextState = idle, msgCounter = Counter+1}};

training(cast, {predict}, State = #client_statem_state{workersMap = WorkersMap, msgCounter = Counter}) ->
  io:format("client going to state predict",[]),
  Workers = maps:to_list(WorkersMap),
  [gen_statem:cast(WorkerPid,{predict})|| {_WorkerName,WorkerPid}<-Workers],
  MyWorkers =  [WorkerName|| {WorkerName,_WorkerPid}<-Workers],
  {next_state, waitforWorkers, State#client_statem_state{nextState = predict, waitforWorkers = MyWorkers, msgCounter = Counter+1}};

training(cast, {loss,WorkerName,nan,_Time_NIF}, State = #client_statem_state{myName = MyName,nerlnetGraph = NerlnetGraph,  msgCounter = Counter}) ->
%%   io:format("LossFunction1: ~p   ~n",[LossFunction]),
  {RouterHost,RouterPort} = nerl_tools:getShortPath(MyName,"mainServer",NerlnetGraph),
  nerl_tools:http_request(RouterHost,RouterPort,"lossFunction", term_to_binary({WorkerName,"nan"})),
%%  http_request(RouterHost,RouterPort,"lossFunction", list_to_binary([list_to_binary(atom_to_list(WorkerName)),<<"#">>,<<"nan">>])),
  {next_state, training, State#client_statem_state{msgCounter = Counter+1}};

training(cast, {loss,WorkerName,LossFunction,_Time_NIF}, State = #client_statem_state{myName = MyName,nerlnetGraph = NerlnetGraph,  msgCounter = Counter,timingMap = TimingMap}) ->
  % Start = maps:get(WorkerName,TimingMap),
  
      % io:format("AverageTrainingTime: ~p~n",[NewAverage])
  NewTimingMap = updateTimingMap(WorkerName,TimingMap),
  
  {RouterHost,RouterPort} = nerl_tools:getShortPath(MyName,"mainServer",NerlnetGraph),
  nerl_tools:http_request(RouterHost,RouterPort,"lossFunction", term_to_binary({WorkerName,LossFunction})),
  {next_state, training, State#client_statem_state{msgCounter = Counter+1,timingMap = NewTimingMap}};


%%Federated Mode:
training(cast, {loss,federated_weights, _Worker, _LOSS_FUNC, Ret_weights}, State = #client_statem_state{federatedServer = Federated,myName = MyName,nerlnetGraph = NerlnetGraph,  msgCounter = Counter}) ->
%%  io:format("Worker: ~p~n, LossFunction: ~p~n,  Ret_weights_tuple: ~p~n",[Worker, LOSS_FUNC, Ret_weights_tuple]),
  % {RouterHost,RouterPort} = maps:get(Federated,NerlnetGraph),
  {RouterHost,RouterPort} = nerl_tools:getShortPath(MyName,Federated,NerlnetGraph),
  % ToSend = term_to_binary({Federated,decodeListOfLists(Ret_weights)}),
  nerl_tools:http_request(RouterHost,RouterPort,"federatedWeightsVector", term_to_binary({Federated,Ret_weights})),
  {next_state, training, State#client_statem_state{msgCounter = Counter+1}};

training(cast, {loss, federated_weights, MyName, _LOSS_FUNC}, State = #client_statem_state{myName = MyName, msgCounter = Counter}) ->
  {next_state, training, State#client_statem_state{msgCounter = Counter+1}};

training(cast, {federatedAverageWeights,Body}, State = #client_statem_state{workersMap = WorkersMap, msgCounter = Counter}) ->
%% io:format("federatedAverageWeights Body!!!!: ~p~n",[Body]),
  {_ClientName,WorkerName,Weights} = binary_to_term(Body),

%%  [_ClientName,WorkerName,Weights] = re:split(binary_to_list(Body),"#",[{return,list}]),
  WorkerPid = maps:get(WorkerName,WorkersMap),
  % io:format("client decoding weights!!!:   ~n!!!",[]),

  % DecodedWeights = decodeListOfLists(BinaryWeights),
  % io:format("client finished decoding weights!!!:   ~n!!!",[]),
  gen_statem:cast(WorkerPid, {set_weights,  Weights}),
  {next_state, training, State#client_statem_state{msgCounter = Counter+1}};

training(cast, EventContent, State = #client_statem_state{msgCounter = Counter}) ->
  io:format("client training ignored!!!:  ~p ~n!!!",[EventContent]),
  {next_state, training, State#client_statem_state{msgCounter = Counter+1}}.


predict(cast, {sample,Body}, State = #client_statem_state{msgCounter = Counter,workersMap = WorkersMap, timingMap = TimingMap}) ->
  %%    Body:   ClientName#WorkerName#CSVName#BatchNumber#BatchOfSamples
  {_ClientName, WorkerName, CSVName, BatchNumber, BatchOfSamples} = binary_to_term(Body),
  % ToSend =  decodeList(BatchOfSamples),
  Start = os:timestamp(),
  {_LastBatchReceivedTime,TotalBatches,TotalTime} = maps:get(list_to_atom(WorkerName),TimingMap),
  NewTimingMap = maps:put(list_to_atom(WorkerName),{Start,TotalBatches+1,TotalTime},TimingMap),
%%  io:format("CSVName: ~p, BatchNumber: ~p~n",[CSVName,BatchNumber]),
%%  io:format("Vector: ~p~n",[ToSend]),
  WorkerPid = maps:get(list_to_atom(WorkerName),WorkersMap),
  gen_statem:cast(WorkerPid, {sample,CSVName, BatchNumber,BatchOfSamples}),
%%  gen_statem:cast(WorkerPid, {sample,ToSend}),
  {next_state, predict, State#client_statem_state{msgCounter = Counter+1,timingMap = NewTimingMap}};

% predict(cast, {predictRes,WorkerName,InputName,ResultID,[]}, State = #client_statem_state{myName = MyName, msgCounter = Counter,nerlnetGraph = NerlnetGraph}) ->
%   {RouterHost,RouterPort} = nerl_tools:getShortPath(MyName,"mainServer",NerlnetGraph),
%   nerl_tools:http_request(RouterHost,RouterPort,"predictRes", term_to_binary({atom_to_list(WorkerName),InputName,ResultID,""})),
%   {next_state, predict, State#client_statem_state{msgCounter = Counter+1}};


%% TODO: add nif timing statistics
predict(cast, {predictRes,WorkerName,InputName,ResultID,PredictNerlTensor, Type, _TimeTook}, State = #client_statem_state{myName = MyName, msgCounter = Counter,nerlnetGraph = NerlnetGraph,timingMap = TimingMap}) ->
    NewTimingMap = updateTimingMap(WorkerName,TimingMap),

    {RouterHost,RouterPort} = nerl_tools:getShortPath(MyName,"mainServer",NerlnetGraph),

    %io:format("Client got result from predict-~nInputName: ~p,ResultID: ~p, ~nResult:~p~n",[InputName,ResultID,Result]),
    nerl_tools:http_request(RouterHost,RouterPort,"predictRes", term_to_binary({atom_to_list(WorkerName),InputName,ResultID,{PredictNerlTensor, Type}})),
    {next_state, predict, State#client_statem_state{timingMap =NewTimingMap, msgCounter = Counter+1}};

predict(cast, {training}, State = #client_statem_state{workersMap = WorkersMap,msgCounter = Counter}) ->
    Workers = maps:to_list(WorkersMap),
    [gen_statem:cast(WorkerPid,{training})|| {_WorkerName,WorkerPid}<-Workers],
    MyWorkers =  [WorkerName|| {WorkerName,_WorkerPid}<-Workers],
    {next_state, waitforWorkers, State#client_statem_state{nextState = training,  waitforWorkers = MyWorkers, msgCounter = Counter+1}};


predict(cast, {idle}, State = #client_statem_state{workersMap = WorkersMap,msgCounter = Counter}) ->
    Workers = maps:to_list(WorkersMap),
    [gen_statem:cast(WorkerPid,{idle})|| {_WorkerName,WorkerPid}<-Workers],
    io:format("client going to state idle~n",[]),
    MyWorkers =  [WorkerName|| {WorkerName,_WorkerPid}<-Workers],

    {next_state, waitforWorkers, State#client_statem_state{nextState = idle,  waitforWorkers = MyWorkers,msgCounter = Counter+1}};

predict(cast, EventContent, State = #client_statem_state{msgCounter = Counter}) ->
    io:format("client predict ignored:  ~p ~n",[EventContent]),
    {next_state, predict, State#client_statem_state{msgCounter = Counter+1}}.


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
  io:format("~p sending ACK   ~n",[MyName]),
  {RouterHost,RouterPort} = nerl_tools:getShortPath(MyName,"mainServer",NerlnetGraph),
  %%  send an ACK to mainserver that the CSV file is ready
  nerl_tools:http_request(RouterHost,RouterPort,"clientReady",MyName).

createWorkers(EtsRef) ->
  DATA_IDX = 3,

  ClientWorkersMaps = ets:lookup(nerlnet_data, hostClients, DATA_IDX),

  Func = fun(ModelId, WorkerMap) -> 
    %%  TODO: move to json parser
    WorkerName = list_to_atom(binary_to_list(maps:get(<<"name">>,Worker))),
    ModelType = list_to_integer(binary_to_list(maps:get(<<"modelType">>,Worker))),
    ScalingMethod = list_to_integer(binary_to_list(maps:get(<<"scalingMethod">>,Worker))),

    LayerTypesList = nerl_tools:string_to_list_int(maps:get(<<"layerTypesList">>,Worker)),
    LayersSizes = nerl_tools:string_to_list_int(maps:get(<<"layersSizes">>,Worker)),
    LayersActivationFunctions = nerl_tools:string_to_list_int(maps:get(<<"layersActivationFunctions">>,Worker)),

    Optimizer = list_to_integer(binary_to_list(maps:get(<<"optimizer">>,Worker))),
    Features = list_to_integer(binary_to_list(maps:get(<<"features">>,Worker))),
    Labels = list_to_integer(binary_to_list(maps:get(<<"labels">>,Worker))),
    LossMethod = list_to_integer(binary_to_list(maps:get(<<"lossMethod">>,Worker))),
    LearningRate = list_to_float(binary_to_list(maps:get(<<"learningRate">>,Worker))),


    case ModelType of
      ?E_CUSTOMNN ->
        CustomFunc = fun workerNN:controller/2,
        WorkerData = none;
      ?E_FEDERATED_CLIENT ->
        CustomFunc = fun workerFederatedClient:controller/2,
        FedServer = list_to_atom(binary_to_list(maps:get(<<"federatedServer">>,Worker))),
        SyncCount = list_to_integer(binary_to_list(maps:get(<<"syncCount">>,Worker))),
        WorkerData = #workerFederatedClient{syncMaxCount = SyncCount, syncCount = 0, myName = WorkerName, clientPID = self(), serverName = FedServer};
      ?E_FEDERATED_SERVER ->
        CustomFunc = fun workerFederatedServer:controller/2,
        SyncCount = list_to_integer(binary_to_list(maps:get(<<"syncCount">>,Worker))),
        WorkerData = #workerFederatedServer{syncMaxCount = SyncCount, syncCount = 0, myName = WorkerName, clientPID = self(), workersNamesList = []}
      end,

    WorkerArgs = {WorkerName,ModelId,ModelType,ScalingMethod, LayerTypesList, LayersSizes,
      LayersActivationFunctions, Optimizer, Features, Labels, LossMethod, LearningRate, self(), CustomFunc, WorkerData},

    WorkerPid = workerGeneric:start_link(WorkerArgs),

    ets:insert(EtsRef, {WorkerName, WorkerPid, WorkerArgs, #{WorkerName => {0,0,0.0}}}),
    WorkerName
  end,

  WorkersNames = lists:zipwith(Func, lists:seq(1, length(ClientWorkersMaps), ClientWorkersMaps)),
  ets:insert(EtsRef, {workersNames, WorkersNames}).   %% TODO: collect forbidden names (keys of ets:insert)

% calculates the avarage training time
updateTimingMap(WorkerName,TimingMap)->

  {Start,TotalBatches,TotalTime} = maps:get(WorkerName,TimingMap),
  Finish = os:timestamp(),
  TotalTrainingTime = (timer:now_diff(Finish, Start) / 1000),
  % NewAverage = ((AverageTrainingTime*(TotalBatches))+TotalTrainingTime)/TotalBatches,
  % io:format("{Start,TotalBatches,AverageTrainingTime}:  ~p  ~n",[{Start,TotalBatches,AverageTrainingTime}]),
  % io:format("WorkerName: ~p ,  total time: ~p  ~n",[WorkerName, TotalTrainingTime]),

  maps:put(WorkerName,{Start,TotalBatches,TotalTrainingTime+TotalTime},TimingMap).


sendStatistics(RouterHost,RouterPort,MyName,_MsgCount,TimingMap)->
  S = lists:flatten([atom_to_list(WorkerName)++"="++float_to_list(TotalTime/TotalBatches,[{decimals, 3}])++","||{WorkerName,{_LastTime,TotalBatches,TotalTime}}<-maps:to_list(TimingMap)]),
  % io:format("client Timing map: ~p~n ",[S]),
  nerl_tools:http_request(RouterHost,RouterPort,"statistics", list_to_binary(MyName++":"++lists:droplast(S))).
