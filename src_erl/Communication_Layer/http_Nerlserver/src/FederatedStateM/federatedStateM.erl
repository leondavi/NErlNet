%%%-------------------------------------------------------------------
%%% @author kapelnik
%%% @copyright (C) 2021, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 14. Apr 2021 9:57 AM
%%%-------------------------------------------------------------------
-module(federatedStateM).
-author("kapelnik").

-behaviour(gen_statem).

%%-import('nerlNetStatem', []).
%%-import('../../erlBridge/nerlNetStatem', []).

%% API
-export([start_link/1]).

%% gen_statem callbacks
-export([init/1, format_status/2, state_name/3, handle_event/4, terminate/3,
  code_change/4, callback_mode/0, averaging/3, receiveWeights/3]).

-define(SERVER, ?MODULE).

-record(federated_statem_state, {myName, workersMap, portMap, msgCounter,countLimit, weightsCount=0}).


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
init({MyName,ConnectionsMap,CountLimit}) ->
  inets:start(),
  start_connection(maps:to_list(ConnectionsMap)),

%%TODO workers = WorkersMap <-TODO ADD
  {ok, idle, #federated_statem_state{myName= MyName, portMap = ConnectionsMap, msgCounter = 0,countLimit= CountLimit}}.



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
state_name(_EventType, _EventContent, State = #federated_statem_state{}) ->
  NextStateName = next_state,
  {next_state, NextStateName, State}.



receiveWeights(cast, {weights,Weights}, State = #federated_statem_state{msgCounter = Counter,countLimit = CountLimit, weightsCount = CountLimit-1}) ->
  io:format("got count limit, going to averaging:~p ~n",[]),
  {next_state, averaging, State#federated_statem_state{msgCounter = Counter+1,weightsCount  = 0}};

receiveWeights(cast, {weights,Weights}, State = #federated_statem_state{msgCounter = Counter,weightsCount = WeightsCount}) ->
  {next_state, receiveWeights, State#federated_statem_state{msgCounter = Counter+1,weightsCount  = WeightsCount+1}}.

averaging(cast, {averageWeights,Weights}, State = #federated_statem_state{msgCounter = Counter}) ->
  io:format("finished averaging. new weights:~p ~n",[Weights]),
  {next_state, receiveWeights, State#federated_statem_state{msgCounter = Counter+1}}.



%% @private
%% @doc If callback_mode is handle_event_function, then whenever a
%% gen_statem receives an event from call/2,  cast/2, or as a normal
%% process message, this function is called.
handle_event(_EventType, _EventContent, _StateName, State = #federated_statem_state{}) ->
  NextStateName = the_next_state_name,
  {next_state, NextStateName, State}.

%% @private
%% @doc This function is called by a gen_statem when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_statem terminates with
%% Reason. The return value is ignored.
terminate(_Reason, _StateName, _State = #federated_statem_state{}) ->
  ok.

%% @private
%% @doc Convert process state when code is changed
code_change(_OldVsn, StateName, State = #federated_statem_state{}, _Extra) ->
  {ok, StateName, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
start_connection([])->ok;
start_connection([{_ServerName,{Host, Port}}|Tail]) ->
  httpc:set_options([{proxy, {{Host, Port},[Host]}}]),
  start_connection(Tail).

http_request(Host, Port,Path, Body)->
  URL = "http://" ++ Host ++ ":"++integer_to_list(Port) ++ "/" ++ Path,
  httpc:set_options([{proxy, {{Host, Port},[Host]}}]),
  httpc:request(post,{URL, [],"application/x-www-form-urlencoded",Body}, [], []).



