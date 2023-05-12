%%%-------------------------------------------------------------------
%%% @author ziv
%%% @copyright (C) 2021, Nerlnet
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(cppSANNFedServStateM).
-author("ziv").

-behaviour(gen_statem).

%% API
-export([start_link/1]).

%% gen_statem callbacks
-export([init/1, format_status/2, state_name/3, handle_event/4, terminate/3,code_change/4, callback_mode/0]).
%% Extra functions  
-export([averageFun/3]).
%% States functions
-export([average/3, receives/3]).

-define(SERVER, ?MODULE).

%% federatedMode = 0 - Not federated, 1 - Federated get and send weights, 2 - Federated set weights
%% countLimit - Number of samples to count before sending the weights for averaging. Predifined in the json file.
%% count - Number of samples recieved for training after the last weights sended.
-record(fedServ_state, {counter, counterLimit, buffer, myName, myWorkers, workersMap, portMap, nerlnetGraph, last2, cells, first = 1, msgCounter = 0}).

%%%===================================================================
%%% API
%%%===================================================================

%% @doc Creates a gen_statem process which calls Module:init/1 to
%% initialize. To ensure a synchronized start-up procedure, this
%% function does not return until Module:init/1 has returned.
start_link(ARGS) ->
  {ok,Pid} = gen_statem:start_link(?MODULE, ARGS, []),
  Pid.

%%%===================================================================
%%% gen_statem callbacks
%%%===================================================================

%% @private
%% @doc Whenever a gen_statem is started using gen_statem:start/[3,4] or
%% gen_statem:start_link/[3,4], this function is called by the new
%% process to initialize.
init({MyName,CounterLimit,WorkersMap,NerlnetGraph}) ->

  io:fwrite("start federated server stateM ~n"),
  Workers = getWorkersNames(WorkersMap),
  inets:start(),
  io:format("Client ~p Connecting to: ~p~n",[MyName, [digraph:vertex(NerlnetGraph,Vertex) || Vertex <- digraph:out_neighbours(NerlnetGraph,MyName)]]),
  start_connection([digraph:vertex(NerlnetGraph,Vertex) || Vertex <- digraph:out_neighbours(NerlnetGraph,MyName)]),

  {ok, receives, #fedServ_state{msgCounter = 1, counter = 0, counterLimit =  CounterLimit, buffer = [], myName = MyName,myWorkers = Workers, workersMap=WorkersMap, nerlnetGraph = NerlnetGraph}}.

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
state_name(_EventType, _EventContent, State = #fedServ_state{}) ->
  NextStateName = next_state,
  {next_state, NextStateName, State}.

%% @private
%% @doc If callback_mode is handle_event_function, then whenever a
%% gen_statem receives an event from call/2, cast/2, or as a normal
%% process message, this function is called.
handle_event(_EventType, _EventContent, _StateName, State = #fedServ_state{}) ->
  NextStateName = the_next_state_name,
  {next_state, NextStateName, State}.

%% @private
%% @doc This function is called by a gen_statem when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_statem terminates with
%% Reason. The return value is ignored.
terminate(_Reason, _StateName, _State) ->
  ok.

%% @private
%% @doc Convert process state when code is changed
code_change(_OldVsn, StateName, State = #fedServ_state{}, _Extra) ->
  {ok, StateName, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================


%% Define states


%% State receives
%% Receives the weights from the client
% receives(cast, {federatedWeightsVector,Ret_weights}, State = #fedServ_state{first = 1, msgCounter = MsgCounter, buffer = Buffer, counter = Counter, counterLimit =  CounterLimit,myWorkers = _Workers,workersMap = _WorkersMap}) ->
%   NewCount = Counter + 1,
%   {_MyName,Weights} = binary_to_term(Ret_weights),
%   % io:format("Federated Weights: ~p~n",[Weights]),
%   % DecodedWeights= decodeListOfLists(Weights),
%   % DecodedWeightsWithout2Last = remove2Lasts(DecodedWeights),
%   % Cells = [length(X)||X<-DecodedWeightsWithout2Last],

%   % TwoLasts = lists:sublist(DecodedWeights,length(DecodedWeights)-1,length(DecodedWeights)),

%   if
%       NewCount == 1 ->
%       NewBuffer =  Weights,
%       % {next_state, receives, State#fedServ_state{first = 0,cells = Cells,last2 = TwoLasts, msgCounter = MsgCounter+1, counter = NewCount, buffer = NewBuffer}};
%       {next_state, receives, State#fedServ_state{first = 0, msgCounter = MsgCounter+1, counter = NewCount, buffer = NewBuffer}};

%     NewCount < CounterLimit ->
%       %% Add to the buffer, increment the counter and continue to recieve weights
%       % NewBuffer = Buffer ++ Weights,
%       NewBuffer =  lists:zipwith(fun(X, Y) -> X+Y end,Buffer , Weights),
%       % {next_state, receives, State#fedServ_state{first = 0,cells = Cells,last2 = TwoLasts, msgCounter = MsgCounter+1, counter = NewCount, buffer = NewBuffer}};
%       {next_state, receives, State#fedServ_state{first = 0, msgCounter = MsgCounter+1, counter = NewCount, buffer = NewBuffer}};

%     NewCount >= CounterLimit ->
%       %% Reset the buffer, decrease the counter by CounterLimit, start averaging in a different process and go to average state
%       SelfPid = self(),
%       _Pid = spawn(fun()-> averageFun(Buffer ++ Weights,SelfPid,NewCount) end),
%       {next_state, average, State#fedServ_state{first = 0,  msgCounter = MsgCounter+1, counter = 0, buffer = []}};

%     true ->
%       io:fwrite("Error: cppSANNEdServStateM not supposed to be here.\n"),
%       {next_state, receives, State#fedServ_state{msgCounter = MsgCounter+1, counter = NewCount}}
%   end;

%% State receives
%% Receives the weights from the client
receives(cast, {federatedWeightsVector,Ret_weights}, State = #fedServ_state{msgCounter = MsgCounter, buffer = Buffer, counter = Counter, counterLimit =  CounterLimit}) ->
  NewCount = Counter + 1,
  {_MyName,Weights} = binary_to_term(Ret_weights),

  % DecodedWeights = decodeListOfLists(Weights),
  % DecodedWeightsWithout2Last = remove2Lasts(DecodedWeights),

  if
    NewCount == 1 ->
      NewBuffer =  Weights,
      % {next_state, receives, State#fedServ_state{first = 0,cells = Cells,last2 = TwoLasts, msgCounter = MsgCounter+1, counter = NewCount, buffer = NewBuffer}};
      {next_state, receives, State#fedServ_state{first = 0, msgCounter = MsgCounter+1, counter = NewCount, buffer = NewBuffer}};

    NewCount < CounterLimit ->

      %% Add to the buffer, increment the counter and continue to recieve weights    
      % NewBuffer = Buffer ++ Weights,
      NewBuffer =  lists:zipwith(fun(X, Y) -> X+Y end,Buffer , Weights),
      {next_state, receives, State#fedServ_state{msgCounter = MsgCounter+1, counter = NewCount, buffer = NewBuffer}};

    NewCount >= CounterLimit ->

      %% Reset the buffer, decrease the counter by CounterLimit, start averaging in a different process and go to average state
      SelfPid = self(),
      % NewBuffer = Buffer ++ Weights,
      NewBuffer =  lists:zipwith(fun(X, Y) -> X+Y end,Buffer , Weights),
      _Pid = spawn(fun()-> averageFun(NewBuffer,SelfPid,NewCount) end),

      {next_state, average, State#fedServ_state{msgCounter = MsgCounter+1, counter = 0, buffer = []}};
    
    true ->
      io:fwrite("Error: cppSANNEdServStateM not supposed to be here.\n"),
      {next_state, receives, State#fedServ_state{msgCounter = MsgCounter+1, counter = NewCount}}
  end;

%%sending statistics to main server
receives(cast, {statistics}, State = #fedServ_state{myName = MyName, msgCounter = MsgCounter, nerlnetGraph =  NerlnetGraph}) ->
  % {RouterHost,RouterPort} = maps:get(mainServer,ConnectionMap),
    {RouterHost,RouterPort} = getShortPath(MyName,"mainServer",NerlnetGraph),

  http_request(RouterHost,RouterPort,"statistics", list_to_binary(MyName++"#"++integer_to_list(MsgCounter))),
  {next_state, receives, State#fedServ_state{msgCounter = MsgCounter+1}};

receives(cast, Else, State) ->
  io:fwrite("Error: State receives in cppSANNFedServStateM.erl. Got: ~p\n",[Else]),
  {next_state, receives, State}.


%% State average

%% Got weights results
average(cast, {federatedWeightsVector,Ret_weights}, State = #fedServ_state{msgCounter = MsgCounter, buffer = [], counter = Counter}) ->
  {_MyName,Weights} = binary_to_term(Ret_weights),
  NewBuffer =  Weights,

  {next_state, average, State#fedServ_state{msgCounter = MsgCounter+1, counter =  1, buffer = NewBuffer}};

%% Got weights results
average(cast, {federatedWeightsVector,Ret_weights}, State = #fedServ_state{msgCounter = MsgCounter, buffer = Buffer, counter = Counter}) ->
  {_MyName,Weights} = binary_to_term(Ret_weights),
  NewBuffer =  lists:zipwith(fun(X, Y) -> X+Y end,Buffer , Weights),

  {next_state, average, State#fedServ_state{msgCounter = MsgCounter+1, counter = Counter + 1, buffer = NewBuffer}};

%%sending statistics to main server
average(cast, {statistics}, State = #fedServ_state{myName = MyName, msgCounter = MsgCounter, nerlnetGraph =  NerlnetGraph}) ->
  % {RouterHost,RouterPort} = maps:get(mainServer,ConnectionMap),
  {RouterHost,RouterPort} = getShortPath(MyName,"mainServer",NerlnetGraph),

  http_request(RouterHost,RouterPort,"statistics", list_to_binary(MyName++"#"++integer_to_list(MsgCounter))),
  {next_state, average, State#fedServ_state{msgCounter = MsgCounter+1}};


%% Got average results
average(cast, {average, WeightsList}, State= #fedServ_state{msgCounter = MsgCounter,myName = MyName, last2 = Last2,cells = Cells,  myWorkers = Workers,workersMap = WorkersMap,nerlnetGraph =  NerlnetGraph}) ->

  Triplets =getHostPort(Workers,WorkersMap, MyName, NerlnetGraph,[]),
  % ListsofWeights = getCells(Cells,WeightsList),

  % ToSend = decodeListOfLists(ListsofWeights++Last2),
  _Pid = spawn(fun()-> broadcastWeights(WeightsList,Triplets) end),  %% Send the results to the clients through the main server
%%  gen_statem:cast(FedServPID,{averageResult, MyName, WeightsTuple}),TODO add broadcast
  {next_state, receives, State#fedServ_state{msgCounter = MsgCounter+1}};

%% Not supposed to be here
average(cast, _Else, State) ->
  io:fwrite("Error: State average in cppSANNFedServStateM.erl. Got: \n",[]),
  {next_state, average, State}.


% Functions

%% Do the averaging
averageFun(WeightsBuffer,CallerPid,Count) ->
  % io:fwrite("FedServer, averageFun: WeightsAndBiasNumber: ~p\n", [WeightsAndBiasNumber]),
  % io:fwrite("FedServer, averageFun: WeightsBuffer: ~p\n", [WeightsBuffer]),

  % AveragedWeights = erlModule:average_weights(WeightsBuffer, WeightsAndBiasNumber),
  AveragedWeights = [X/Count || X<-WeightsBuffer],

  % io:fwrite("AveragedWeights at averageFun: ~p.\n",[AveragedWeights]),
  gen_statem:cast(CallerPid,{average, AveragedWeights}).


getWorkersNames(Map) ->getWorkersNames(maps:to_list(Map),[]).
getWorkersNames([],L) ->L;
getWorkersNames([{Worker,_Client}|Tail],L) ->getWorkersNames(Tail,L++[Worker]).

broadcastWeights(BinaryWeights,Triplets) ->

  [http_request(RouterHost,RouterPort,"federatedWeights", term_to_binary({ClientName,WorkerName,BinaryWeights}))
    || {ClientName,WorkerName,RouterHost,RouterPort}<-Triplets].


getHostPort([],_WorkersMap,_MyName,_NerlnetGraph,Ret)-> Ret;
getHostPort([WorkerName|WorkersNames],WorkersMap,MyName,NerlnetGraph,Ret)->
  ClientName = maps:get(WorkerName,WorkersMap),
  % {RouterHost,RouterPort} = maps:get(ClientName,PortMap),
  {RouterHost,RouterPort} = getShortPath(MyName,ClientName,NerlnetGraph),

  getHostPort(WorkersNames,WorkersMap,MyName, NerlnetGraph,Ret++[{ClientName,WorkerName,RouterHost,RouterPort}]).

remove2Lasts(L)-> lists:sublist(L,length(L)-2).

getCells(Cells,WeightsList) ->
  getCells(Cells,WeightsList,[]).
getCells([_Cell],WeightsList,Ret) ->Ret++[WeightsList];
getCells([Cell|Cells],WeightsList,Ret) ->
getCells(Cells,lists:sublist(WeightsList,Cell+1,length(WeightsList)),Ret++[lists:sublist(WeightsList,Cell)]).


%%This encoder receives a lists of lists: [[1.0,1.1,11.2],[2.0,2.1,22.2]] and returns a binary
decodeListOfLists(L)->decodeListOfLists(L,[]).
decodeListOfLists([],Ret)->term_to_binary(Ret);
decodeListOfLists([H|T],Ret)->decodeListOfLists(T,Ret++[decodeFloatsList(H)]).
decodeFloatsList(ListOfFloats)->
  ListOfBinaries = [<<X:64/float>>||X<-ListOfFloats],
  list_to_binary(ListOfBinaries).

%%This decoder receives a binary <<131,108,0,0,0,2,106...>> and returns a lists of lists: [[1.0,1.1,11.2],[2.0,2.1,22.2]]
decodeListOfLists(L)->decodeListOfLists(binary_to_term(L),[]).
decodeListOfLists([],Ret)->Ret;
decodeListOfLists([H|T],Ret)->decodeListOfLists(T,Ret++[decodeList(H)]).
decodeList(Binary)->  decodeList(Binary,[]).
decodeList(<<>>,L) -> L;
decodeList(<<A:64/float,Rest/binary>>,L) -> decodeList(Rest,L++[A]).

%%NETWORK FUNCTIONS
start_connection([])->ok;
start_connection([{_ServerName,{Host, Port}}|Tail]) ->
  httpc:set_options([{proxy, {{Host, Port},[Host]}}]),
  start_connection(Tail).

http_request(Host, Port,Path, Body)->
%%  io:format("sending body ~p to path ~p to hostport:~p~n",[Body,Path,{Host,Port}]),
URL = "http://" ++ Host ++ ":"++integer_to_list(Port) ++ "/" ++ Path,
httpc:set_options([{proxy, {{Host, Port},[Host]}}]),
httpc:request(post,{URL, [],"application/x-www-form-urlencoded",Body}, [], []).



getShortPath(From,To,NerlnetGraph) when is_atom(To)-> 

  First = lists:nth(2,digraph:get_short_path(NerlnetGraph,From,atom_to_list(To))),

{_First,{Host,Port}} = digraph:vertex(NerlnetGraph,First),
{Host,Port};

getShortPath(From,To,NerlnetGraph) -> 
First = lists:nth(2,digraph:get_short_path(NerlnetGraph,From,To)),
{_First,{Host,Port}} = digraph:vertex(NerlnetGraph,First),
{Host,Port}.