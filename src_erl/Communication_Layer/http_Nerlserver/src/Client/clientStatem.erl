%%%-------------------------------------------------------------------
%%% @author kapelnik
%%% @copyright (C) 2021, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 14. Apr 2021 9:57 AM
%%%-------------------------------------------------------------------
-module(clientStatem).
-author("kapelnik").

-behaviour(gen_statem).

%%-import('nerlNetStatem', []).
%%-import('../../erlBridge/nerlNetStatem', []).

%% API
-export([start_link/1, predict/3]).

%% gen_statem callbacks
-export([init/1, format_status/2, state_name/3, handle_event/4, terminate/3,
  code_change/4, callback_mode/0, idle/3, training/3]).

-define(SERVER, ?MODULE).

-record(client_statem_state, {myName, workersMap, portMap, msgCounter}).


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


%%NerlClientsArgs=[{ClientName,cppSANN Args},...]
init({MyName,Workers,ConnectionsMap}) ->
  inets:start(),
  start_connection(maps:to_list(ConnectionsMap)),

%%  init nerlClient with given parameters, and build a map :#{workerName=>WorkerPid,...}
  WorkersPids = [{WorkerName,nerlNetStatem:start_link({self(), WorkerName, CppSANNArgs})}||{WorkerName,CppSANNArgs}<-maps:to_list(Workers)],
  WorkersMap = maps:from_list(WorkersPids),

%%TODO workers = WorkersMap <-TODO ADD
  {ok, idle, #client_statem_state{myName= MyName, workersMap = WorkersMap, portMap = ConnectionsMap, msgCounter = 0}}.


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



%%initiating nerlnet, given parameters in Body received by Cowboy init_handler
idle(cast, {init,CONFIG}, State = #client_statem_state{msgCounter = Counter}) ->
  io:format("initiating, CONFIG received:~p ~n",[CONFIG]),
  {next_state, idle, State#client_statem_state{msgCounter = Counter+1}};

idle(cast, {training}, State = #client_statem_state{workersMap = WorkersMap, myName = MyName,msgCounter = Counter,portMap = PortMap}) ->
  Workers = maps:to_list(WorkersMap),
  [gen_statem:cast(WorkerPid,{training})|| {_WorkerName,WorkerPid}<-Workers],
%%  io:format("sending ACK   ~n",[]),
%%  {RouterHost,RouterPort} = maps:get(mainServer,PortMap),
%%  send an ACK to mainserver that the CSV file is ready
  ack(MyName,PortMap),
  {next_state, training, State#client_statem_state{msgCounter = Counter+1}};

idle(cast, {predict}, State = #client_statem_state{workersMap = WorkersMap,myName = MyName,msgCounter = Counter,portMap = PortMap}) ->
  Workers = maps:to_list(WorkersMap),
  [gen_statem:cast(WorkerPid,{predict})|| {_WorkerName,WorkerPid}<-Workers],
  %%  send an ACK to mainserver that the CSV file is ready
  ack(MyName,PortMap),
  {next_state, predict, State#client_statem_state{msgCounter = Counter+1}};

idle(cast, EventContent, State = #client_statem_state{myName = MyName,msgCounter = Counter,portMap = PortMap}) ->
  io:format("client training ignored:  ~p ~n",[EventContent]),
  {next_state, training, State#client_statem_state{msgCounter = Counter+1}}.

%%[WorkerName|[Sample]] = re:split(binary_to_list(Vector), "#", [{return, list}]),
%%Input= lists:sublist(Sample,1,length(Sample)-1),
%%Inp = re:split(Input, ",", [{return, list}]),
%%Inps = lists:reverse(getNumbers(Inp,[])).
training(cast, {sample,Vector}, State = #client_statem_state{msgCounter = Counter,workersMap = WorkersMap}) ->
  io:format("sending samples to train ~p ~n",[Vector]),
  [WorkerName|[Sample1]] = re:split(binary_to_list(Vector), "#", [{return, list}]),
  Sample= lists:sublist(Sample1,1,length(Sample1)-1),
  Splitted = re:split(Sample, ",", [{return, list}]),
  ToSend =  lists:reverse(getNumbers(Splitted,[])),
  WorkerPid = maps:get(list_to_atom(WorkerName),WorkersMap),
  gen_statem:cast(WorkerPid, {sample,ToSend}),
  {next_state, training, State#client_statem_state{msgCounter = Counter+1}};

training(cast, {idle}, State = #client_statem_state{workersMap = WorkersMap,msgCounter = Counter}) ->
  io:format("client going to state idle",[]),
  Workers = maps:to_list(WorkersMap),
  [gen_statem:cast(WorkerPid,{idle})|| {_WorkerName,WorkerPid}<-Workers],
  {next_state, idle, State#client_statem_state{msgCounter = Counter+1}};

training(cast, {predict}, State = #client_statem_state{workersMap = WorkersMap,myName = MyName, portMap = PortMap, msgCounter = Counter}) ->
  Workers = maps:to_list(WorkersMap),
  [gen_statem:cast(WorkerPid,{predict})|| {_WorkerName,WorkerPid}<-Workers],
  ack(MyName,PortMap),
  {next_state, predict, State#client_statem_state{msgCounter = Counter+1}};

training(cast, EventContent, State = #client_statem_state{msgCounter = Counter}) ->
  io:format("client training ignored:  ~p ~n",[EventContent]),
  {next_state, training, State#client_statem_state{msgCounter = Counter+1}}.


predict(cast, {sample,Vector}, State = #client_statem_state{msgCounter = Counter,workersMap = WorkersMap}) ->
  io:format("sending samples to predict ~p ~n",[Vector]),
  [WorkerName|[Sample1]] = re:split(binary_to_list(Vector), "#", [{return, list}]),
  Sample= lists:sublist(Sample1,1,length(Sample1)-1),
  Splitted = re:split(Sample, ",", [{return, list}]),
  ToSend =  lists:reverse(getNumbers(Splitted,[])),
  WorkerPid = maps:get(list_to_atom(WorkerName),WorkersMap),
  gen_statem:cast(WorkerPid, {sample,ToSend}),
  {next_state, predict, State#client_statem_state{msgCounter = Counter+1}};

predict(cast, {training}, State = #client_statem_state{workersMap = WorkersMap,myName = MyName,portMap = PortMap,msgCounter = Counter}) ->
  Workers = maps:to_list(WorkersMap),
  [gen_statem:cast(WorkerPid,{training})|| {_WorkerName,WorkerPid}<-Workers],
  ack(MyName,PortMap),
  {next_state, training, State#client_statem_state{msgCounter = Counter+1}};

predict(cast, {idle}, State = #client_statem_state{workersMap = WorkersMap,msgCounter = Counter}) ->
  Workers = maps:to_list(WorkersMap),
  [gen_statem:cast(WorkerPid,{idle})|| {_WorkerName,WorkerPid}<-Workers],
  io:format("client going to state idle",[]),
  {next_state, idle, State#client_statem_state{msgCounter = Counter+1}};

predict(cast, EventContent, State = #client_statem_state{msgCounter = Counter}) ->
  io:format("client predict ignored:  ~p ~n",[EventContent]),
  {next_state, predict, State#client_statem_state{msgCounter = Counter+1}}.


%%io:format("vector _handler got Body~n",[]),


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
start_connection([])->ok;
start_connection([{_ServerName,{Host, Port}}|Tail]) ->
  httpc:set_options([{proxy, {{Host, Port},[Host]}}]),
  start_connection(Tail).

http_request(Host, Port,Path, Body)->
  httpc:request(post,{"http://" ++ Host ++ ":"++integer_to_list(Port) ++ "/" ++ Path, [],"application/x-www-form-urlencoded",Body}, [], []).



ack(MyName, PortMap) ->
  io:format("sending ACK   ~n",[]),
  {RouterHost,RouterPort} = maps:get(mainServer,PortMap),
%%  send an ACK to mainserver that the CSV file is ready
  http_request(RouterHost,RouterPort,"clientReady",atom_to_list(MyName)).

getNumbers([],List)->List;
getNumbers([Head|Tail], List) ->
%%  io:format("Head:~p~n",[Head]),
  try list_to_float(Head) of
    Float->    %io:format("~p~n",[Float]),
      getNumbers(Tail,[(Float)]++List)
  catch
    error:Error->
      %io:format("~p~n",[Error]),
      getNumbers(Tail,[list_to_integer(Head)]++List)

  end.
