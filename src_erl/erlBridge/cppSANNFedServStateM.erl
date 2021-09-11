%%%-------------------------------------------------------------------
%%% @author ziv
%%% @copyright (C) 2021, <COMPANY>
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
-export([averageFun/2]).
%% States functions
-export([average/3, receives/3]).

-define(SERVER, ?MODULE).

%% federatedMode = 0 - Not federated, 1 - Federated get and send weights, 2 - Federated set weights
%% countLimit - Number of samples to count before sending the weights for averaging. Predifined in the json file.
%% count - Number of samples recieved for training after the last weights sended.
-record(fedServ_state, {counter, counterLimit, buffer, myName, myWorkers, workersMap, portMap, connectionMap, last2, cells, first=1}).

%%%===================================================================
%%% API
%%%===================================================================

%% @doc Creates a gen_statem process which calls Module:init/1 to
%% initialize. To ensure a synchronized start-up procedure, this
%% function does not return until Module:init/1 has returned.
start_link(ARGS) ->
  %{ok,Pid} = gen_statem:start_link({local, ?SERVER}, ?MODULE, [], []),
  {ok,Pid} = gen_statem:start_link(?MODULE, ARGS, []),
  Pid.

%%%===================================================================
%%% gen_statem callbacks
%%%===================================================================

%% @private
%% @doc Whenever a gen_statem is started using gen_statem:start/[3,4] or
%% gen_statem:start_link/[3,4], this function is called by the new
%% process to initialize.
init({MyName,CounterLimit,WorkersMap,ConnectionsMap}) ->

  io:fwrite("start federated server stateM ~n"),
  Workers = getWorkersNames(WorkersMap),
  inets:start(),
  start_connection(maps:to_list(ConnectionsMap)),
  {ok, receives, #fedServ_state{counter = 0, counterLimit =  CounterLimit, buffer = [], myName = MyName,myWorkers = Workers, workersMap=WorkersMap, connectionMap = ConnectionsMap}}.

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
receives(cast, {weights,Ret_weights}, State = #fedServ_state{first = 1, buffer = Buffer, counter = Counter, counterLimit =  CounterLimit,myWorkers = Workers,workersMap = WorkersMap,connectionMap =  ConnectionMap}) ->
  io:fwrite("receives state\n"),
  NewCount = Counter + 1,

  [_MyName,Weights] = re:split(binary_to_list(Ret_weights),"#",[{return,list}]),
  io:format("got Weights: ~p~n",[Weights]),
  io:format("got NewCount: ~p~n",[NewCount]),
  io:format("got CounterLimit: ~p~n",[CounterLimit]),
  Cells = getCells(Weights),
  Splitted=re:split(Weights,"%",[{return,list}]),
  [Nth,Nth2]=lists:sublist(Splitted,length(Splitted)-1,length(Splitted)),
  Last2=Nth++"%"++Nth2,
  io:format("got Cells!!!!!!!!!!!!!!@@@@@@@@@@@@@@@: ~p~n",[Cells]),

  if
    NewCount < CounterLimit ->
%%      %%      TODO REMOVE!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
%%      io:format("@@@@@@@@@@@@@@@@@@@@@@@@@@sending average weights to worker:~p~n",[WorkersMap]),
%%      Triplets =getHostPort(Workers,WorkersMap,ConnectionMap,[]),
%%
%%      _Pid = spawn(fun()-> broadcastWeights(<<"AVARAGEWEIGHTS">>,Triplets) end),
%%%%      TODO REMOVE!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
%%
      io:format("NewCount < CounterLimit ~n"),

      %% Add to the buffer, increment the counter and continue to recieve weights
      io:format("Weights: ~p ~n",[Weights]),
      io:format("remove2Lasts(Weights): ~p ~n",[remove2Lasts(Weights)]),
      {next_state, receives, State#fedServ_state{last2 = Last2, cells = Cells,first = 0, counter = NewCount, buffer = Buffer ++ [remove2Lasts(Weights)]}};

    NewCount >= CounterLimit ->
      %% Reset the buffer, decrease the counter by CounterLimit, start averaging in a different process and go to average state
      io:fwrite("sending to average.\n"),
      SelfPid = self(),
      io:format("self(): ~p~n",[SelfPid]),

      _Pid = spawn(fun()-> averageFun(Buffer ++ [remove2Lasts(Weights)],SelfPid) end),



      {next_state, average, State#fedServ_state{last2 = Last2, cells = Cells,first = 0, counter = NewCount - CounterLimit, buffer = []}};

    true ->
      io:fwrite("Error: cppSANNEdServStateM not supposed to be here.\n"),
      {next_state, receives, State#fedServ_state{last2 = Last2, cells = Cells,first = 0, counter = NewCount}}
  end;

%% State receives
%% Receives the weights from the client
receives(cast, {weights,Ret_weights}, State = #fedServ_state{buffer = Buffer, counter = Counter, counterLimit =  CounterLimit,myWorkers = Workers,workersMap = WorkersMap,connectionMap =  ConnectionMap}) ->
  io:fwrite("receives state\n"),
  NewCount = Counter + 1,

  [_MyName,Weights] = re:split(binary_to_list(Ret_weights),"#",[{return,list}]),
  io:format("got Weights: ~p~n",[Weights]),
  io:format("got NewCount: ~p~n",[NewCount]),
  io:format("got CounterLimit: ~p~n",[CounterLimit]),

  if
    NewCount < CounterLimit ->
%%      %%      TODO REMOVE!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
%%      io:format("@@@@@@@@@@@@@@@@@@@@@@@@@@sending average weights to worker:~p~n",[WorkersMap]),
%%      Triplets =getHostPort(Workers,WorkersMap,ConnectionMap,[]),
%%
%%      _Pid = spawn(fun()-> broadcastWeights(<<"AVARAGEWEIGHTS">>,Triplets) end),
%%%%      TODO REMOVE!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
%%
      io:format("NewCount < CounterLimit ~n"),

      %% Add to the buffer, increment the counter and continue to recieve weights
      io:format("Weights: ~p ~n",[Weights]),
      io:format("remove2Lasts(Weights): ~p ~n",[remove2Lasts(Weights)]),
      {next_state, receives, State#fedServ_state{counter = NewCount, buffer = Buffer ++ [remove2Lasts(Weights)]}};

    NewCount >= CounterLimit ->
      %% Reset the buffer, decrease the counter by CounterLimit, start averaging in a different process and go to average state
      io:fwrite("sending to average.\n"),
      SelfPid = self(),
      io:format("self(): ~p~n",[SelfPid]),

      _Pid = spawn(fun()-> averageFun(Buffer ++ [remove2Lasts(Weights)],SelfPid) end),



      {next_state, average, State#fedServ_state{counter = NewCount - CounterLimit, buffer = []}};
    
    true ->
      io:fwrite("Error: cppSANNEdServStateM not supposed to be here.\n"),
      {next_state, receives, State#fedServ_state{counter = NewCount}}
  end;


%% Not supposed to be here
receives(cast, Else, State) ->
  io:fwrite("Error: State receives in cppSANNFedServStateM.erl. Got: ~p\n",[Else]),
  {next_state, receives, State}.



%% State average

%% Got weights results
average(cast, {weights,WeightsTuple}, State = #fedServ_state{buffer = Buffer, counter = Counter}) ->
  io:fwrite("Got weights at average state!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!.\n"),
  {next_state, average, State#fedServ_state{counter = Counter + 1, buffer = Buffer ++ [remove2Lasts(WeightsTuple)]}};

%% Got average results
average(cast, {average, {WeightsList,Ziv}}, State= #fedServ_state{myName = MyName,last2 = Last2,cells = Cells, myWorkers = Workers,workersMap = WorkersMap,connectionMap = ConnectionMap}) ->
  io:fwrite("The average results are: ~p\n",[WeightsList]),
  Triplets =getHostPort(Workers,WorkersMap,ConnectionMap,[]),
  ToSend = encodeBeforeSend(WeightsList,lists:sublist(Cells,length(Cells)-2))++Last2,
  io:fwrite("ToSend: ~p\n",[ToSend]),

  _Pid = spawn(fun()-> broadcastWeights(list_to_binary(ToSend),Triplets) end),  %% Send the results to the clients through the main server
%%  gen_statem:cast(FedServPID,{averageResult, MyName, WeightsTuple}),TODO add broadcast
  {next_state, receives, State};


%% Not supposed to be here
average(cast, Else, State) ->
  io:fwrite("Error: State average in cppSANNFedServStateM.erl. Got: ~p\n",[Else]),
  {next_state, average, State}.


% Functions

%% Do the averaging
averageFun(WeightsBuffer,CallerPid) ->
  % TODO: call average in the nif
  io:fwrite("WeightsBuffer: ~p\n", [WeightsBuffer]),
  WeightsBufferList = flatEncodedList(WeightsBuffer),
  io:fwrite("WeightsBufferList: ~p\n", [WeightsBufferList]),

  AveragedWeights = erlModule:average_weights(WeightsBufferList, length(WeightsBuffer)),
  io:fwrite("GaverageFun**********************************************************.\n"),
  %[Weights|_T] = WeightsBuffer,
  io:fwrite("Weights at averageFun: ~p.\n",[AveragedWeights]),

  gen_statem:cast(CallerPid,{average, AveragedWeights}).


getWorkersNames(Map) ->getWorkersNames(maps:to_list(Map),[]).
getWorkersNames([],L) ->L;
getWorkersNames([{Worker,_Client}|Tail],L) ->getWorkersNames(Tail,L++[Worker]).

%%NETWORK FUNCTIONS
start_connection([])->ok;
start_connection([{_ServerName,{Host, Port}}|Tail]) ->
  httpc:set_options([{proxy, {{Host, Port},[Host]}}]),
  start_connection(Tail).

http_request(Host, Port,Path, Body)->
  URL = "http://" ++ Host ++ ":"++integer_to_list(Port) ++ "/" ++ Path,
  httpc:set_options([{proxy, {{Host, Port},[Host]}}]),
  httpc:request(post,{URL, [],"application/x-www-form-urlencoded",Body}, [], []).


%%broadcastWeights(WeightsTuple,[Worker|Workers],ConnectionMap) ->
%%  {RouterHost,RouterPort} = maps:get(Worker,ConnectionMap),
%%  io:format("@@@@@@@@@@@@@@@@@@@@@@@@@@sending average weights to worker:~p~n",[Worker]),
%%  http_request(RouterHost,RouterPort,"federatedWeights", list_to_binary([list_to_binary(atom_to_list(Worker)),<<"#">>,<<"asd">>])),
%%  broadcastWeights(WeightsTuple,Workers,ConnectionMap).

broadcastWeights(BinaryWeights,Triplets) ->
  [http_request(RouterHost,RouterPort,"federatedWeights", list_to_binary([list_to_binary(atom_to_list(ClientName)),<<"#">>,list_to_binary(atom_to_list(WorkerName)),<<"#">>,BinaryWeights]))
    || {ClientName,WorkerName,RouterHost,RouterPort}<-Triplets].


getHostPort([],_WorkersMap,_PortMap,Ret)-> Ret;
getHostPort([WorkerName|WorkersNames],WorkersMap,PortMap,Ret)->
  ClientName = maps:get(WorkerName,WorkersMap),
  {RouterHost,RouterPort} = maps:get(ClientName,PortMap),
  getHostPort(WorkersNames,WorkersMap, PortMap,Ret++[{ClientName,WorkerName,RouterHost,RouterPort}]).

remove2Lasts(L)->
  Splitted = re:split(L,"%",[{return,list}]),
  Cutted = lists:sublist(Splitted,length(Splitted)-2),
  encodeList(Cutted).

encodeList(L)->[_H|T] = encodeList(L,[]), T.
encodeList([],L) ->L;
encodeList([H|T],L)->encodeList(T,L++"%"++H).

flatEncodedList(L)->
  lists:flatten([[list_to_float(Y)||Y<-re:split(X,"@",[{return,list}])]||X<-re:split(encodeList(L),"%",[{return,list}])]).

getCells(Weights) ->
%%  io:format("~p~n",[Weights]),
  [length(re:split(X,"@",[{return,list}]))||X<-re:split(Weights,"%",[{return,list}])].


encodeBeforeSend(L,Cells)->io:format("L  ~p,Cells  ~p~n",[[float_to_list(X)||X<-L],Cells]),encodeBeforeSend([float_to_list(X)||X<-L],Cells,[]).
encodeBeforeSend([],_Cells,Ret)->Ret;
encodeBeforeSend(L,[Cells],Ret)->encodeBeforeSend([],Cells,Ret++encodeHelper(lists:sublist(L,Cells,length(L)))++"%");
encodeBeforeSend(L,[Head|Cells],Ret)->encodeBeforeSend(lists:sublist(L,Head,length(L)),Cells,Ret++encodeHelper(lists:sublist(L,Head))++"%").

encodeHelper(L)->[_H|T] = encodeHelper(L,[]), T.
encodeHelper([],L) ->L;
encodeHelper([H|T],L)->encodeHelper(T,L++"@"++H).
