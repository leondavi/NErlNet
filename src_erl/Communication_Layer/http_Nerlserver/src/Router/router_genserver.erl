%%%-------------------------------------------------------------------
%%% @author kapelnik
%%% @copyright (C) 2021, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 27. Apr 2021 11:05 AM
%%%-------------------------------------------------------------------
-module(router_genserver).
-author("kapelnik").

-behaviour(gen_server).

%% API
-export([start_link/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
  code_change/3]).

-define(SERVER, ?MODULE).


-record(router_genserver_state, {connectionsMap}).

%%%===================================================================
%%% API
%%%===================================================================

%% @doc Spawns the server and registers the local name (unique)
-spec(start_link(args) ->
  {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).

start_link(ConnectionsMap) ->
  {ok,Pid} = gen_server:start_link({local, ?SERVER}, ?MODULE, ConnectionsMap, []),
  Pid.

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%% @private
%% @doc Initializes the server
-spec(init(Args :: term()) ->
  {ok, State :: #router_genserver_state{}} | {ok, State :: #router_genserver_state{}, timeout() | hibernate} |
  {stop, Reason :: term()} | ignore).
%%TODO  Args = [MainServerHostandPort,ClientsHostsandPorts,SourcesHostsandPorts]

init({_SupPid,ConnectionsMap}) ->
  inets:start(),
  start_connection(maps:to_list(ConnectionsMap)),
  {ok, #router_genserver_state{connectionsMap = ConnectionsMap}}.


%% @private
%% @doc Handling cast messages
-spec(handle_cast(Request :: term(), State :: #router_genserver_state{}) ->
  {noreply, NewState :: #router_genserver_state{}} |
  {noreply, NewState :: #router_genserver_state{}, timeout() | hibernate} |
  {stop, Reason :: term(), NewState :: #router_genserver_state{}}).

handle_cast({updateCSV,ListOfSources}, State = #router_genserver_state{connectionsMap = ConnectionMap}) ->
%%  Body contrains list of sources to send the request, and input name
  io:format("~p~n",[ListOfSources]),
  findroutAndsend(splitbycouple(ListOfSources,[]),ConnectionMap),
  {noreply, State};

handle_cast({start_training,Body}, State = #router_genserver_state{}) ->
%%  Body contrains list of sources to send the request, and input name
  case Body of
    _ ->  httpc:request(post,{"http://localhost:8082/start_training", [],"application/x-www-form-urlencoded",Body}, [], [])
  end,
  {noreply, State};

handle_cast({stop_training,Body}, State = #router_genserver_state{}) ->
%%  Body contrains list of sources to send the request, and input name
  case Body of
    _ ->  httpc:request(post,{"http://localhost:8082/stop_training", [],"application/x-www-form-urlencoded",Body}, [], [])
  end,
  {noreply, State};


handle_cast(_Request, State = #router_genserver_state{}) ->
  {noreply, State}.




%% @private
%% @doc Handling call messages
-spec(handle_call(Request :: term(), From :: {pid(), Tag :: term()},
    State :: #router_genserver_state{}) ->
  {reply, Reply :: term(), NewState :: #router_genserver_state{}} |
  {reply, Reply :: term(), NewState :: #router_genserver_state{}, timeout() | hibernate} |
  {noreply, NewState :: #router_genserver_state{}} |
  {noreply, NewState :: #router_genserver_state{}, timeout() | hibernate} |
  {stop, Reason :: term(), Reply :: term(), NewState :: #router_genserver_state{}} |
  {stop, Reason :: term(), NewState :: #router_genserver_state{}}).
handle_call(_Request, _From, State = #router_genserver_state{}) ->
  {reply, ok, State}.


%% @private
%% @doc Handling all non call/cast messages
-spec(handle_info(Info :: timeout() | term(), State :: #router_genserver_state{}) ->
  {noreply, NewState :: #router_genserver_state{}} |
  {noreply, NewState :: #router_genserver_state{}, timeout() | hibernate} |
  {stop, Reason :: term(), NewState :: #router_genserver_state{}}).
handle_info(_Info, State = #router_genserver_state{}) ->
  {noreply, State}.

%% @private
%% @doc This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
-spec(terminate(Reason :: (normal | shutdown | {shutdown, term()} | term()),
    State :: #router_genserver_state{}) -> term()).
terminate(_Reason, _State = #router_genserver_state{}) ->
  ok.


%% @private
%% @doc Convert process state when code is changed
-spec(code_change(OldVsn :: term() | {down, term()}, State :: #router_genserver_state{},
    Extra :: term()) ->
  {ok, NewState :: #router_genserver_state{}} | {error, Reason :: term()}).
code_change(_OldVsn, State = #router_genserver_state{}, _Extra) ->
  {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
start_connection([])->ok;
start_connection([{_ServerName,{Host, Port}}|Tail]) ->
  httpc:set_options([{proxy, {{Host, Port},[Host]}}]),
  start_connection(Tail).

findroutAndsend([],_)->ok;
findroutAndsend([[SourceName,InputFile]|ListOfSources], ConnectionsMap) ->
%%  io:format("sourceName = ~p~n, Map = ~p~n",[SourceName,ConnectionsMap]),
  {SourceHost,SourcePort} =maps:get(list_to_atom(SourceName),ConnectionsMap),
  io:format("~p~n",[SourcePort]),
  httpc:request(post,{"http://"++ SourceHost ++ ":"++integer_to_list(SourcePort) ++ "/updateCSV", [],"application/x-www-form-urlencoded",InputFile}, [], []),
  findroutAndsend(ListOfSources,ConnectionsMap).


splitbycouple([],Ret) ->Ret;
splitbycouple(ListofCouples,Ret) ->
  L1 = lists:sublist(ListofCouples,1,2),
  L2 = lists:sublist(ListofCouples,3,length(ListofCouples)-1),
  splitbycouple(L2,Ret++[L1]).