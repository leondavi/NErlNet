%%%-------------------------------------------------------------------
%%% @author kapelnik
%%% @copyright (C) 2021, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 27. Apr 2021 3:35 AM
%%%-------------------------------------------------------------------
-module(main_genserver).
-author("kapelnik").

-behaviour(gen_server).

%% API
-export([start_link/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
  code_change/3]).

-define(SERVER, ?MODULE).

-record(main_genserver_state, {connectionsMap}).

%%%===============================================================

%%% API
%%%===================================================================

%% @doc Spawns the server and registers the local name (unique)
-spec(start_link(args) ->
  {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link(ConnectionsMap) ->
  {ok,Gen_Server_Pid} = gen_server:start_link({local, ?SERVER}, ?MODULE, ConnectionsMap, []),
  Gen_Server_Pid.


%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%% @private
%% @doc Initializes the server
-spec(init(Args :: term()) ->
{ok, State :: #main_genserver_state{}} | {ok, State :: #main_genserver_state{}, timeout() | hibernate} |
{stop, Reason :: term()} | ignore).
init({_SupPid,ConnectionsMap}) ->
  inets:start(),
%%  io:format("~p~n",[ConnectionsMap]),
  start_connection(maps:to_list(ConnectionsMap)),
  {ok, #main_genserver_state{connectionsMap = ConnectionsMap}}.

%% @private
%% @doc Handling call messages
-spec(handle_call(Request :: term(), From :: {pid(), Tag :: term()},
    State :: #main_genserver_state{}) ->
{reply, Reply :: term(), NewState :: #main_genserver_state{}} |
{reply, Reply :: term(), NewState :: #main_genserver_state{}, timeout() | hibernate} |
{noreply, NewState :: #main_genserver_state{}} |
{noreply, NewState :: #main_genserver_state{}, timeout() | hibernate} |
{stop, Reason :: term(), Reply :: term(), NewState :: #main_genserver_state{}} |
{stop, Reason :: term(), NewState :: #main_genserver_state{}}).
handle_call(_Request, _From, State = #main_genserver_state{}) ->
{reply, ok, State}.

%% @private
%% @doc Handling cast messages
-spec(handle_cast(Request :: term(), State :: #main_genserver_state{}) ->
{noreply, NewState :: #main_genserver_state{}} |
{noreply, NewState :: #main_genserver_state{}, timeout() | hibernate} |
{stop, Reason :: term(), NewState :: #main_genserver_state{}}).

%%TODO change atom sources to list of sources
handle_cast({initCSV,ListOfSources}, State = #main_genserver_state{connectionsMap = ConnectionMap}) ->
%%  send router http request, to rout this message to all sensors
  io:format("main server: requesting router to update csv at sources: Body: ~p~n",[ListOfSources]),
%%  TODO find the router that can send this request to Sources**
  findroutAndsend(ListOfSources,ConnectionMap),
  {noreply, State};

handle_cast({start_learning,Body}, State = #main_genserver_state{}) ->
  httpc:request(post,{"http://localhost:8082/start_training", [],"application/x-www-form-urlencoded",Body}, [], []),
  {noreply, State};

handle_cast({stop_learning,Body}, State = #main_genserver_state{}) ->
  httpc:request(post,{"http://localhost:8082/stop_training", [],"application/x-www-form-urlencoded",Body}, [], []),
  {noreply, State};





handle_cast(_Request, State = #main_genserver_state{}) ->
  httpc:request(post,{"http://localhost:8082/updateCSV", [],"application/x-www-form-urlencoded","./input/input.csv"}, [], []),
  {noreply, State}.

%% @private
%% @doc Handling all non call/cast messages
-spec(handle_info(Info :: timeout() | term(), State :: #main_genserver_state{}) ->
{noreply, NewState :: #main_genserver_state{}} |
{noreply, NewState :: #main_genserver_state{}, timeout() | hibernate} |
{stop, Reason :: term(), NewState :: #main_genserver_state{}}).
handle_info(_Info, State = #main_genserver_state{}) ->
{noreply, State}.


%% @private
%% @doc This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
-spec(terminate(Reason :: (normal | shutdown | {shutdown, term()} | term()),
    State :: #main_genserver_state{}) -> term()).
terminate(_Reason, _State = #main_genserver_state{}) ->
ok.

%% @private
%% @doc Convert process state when code is changed
-spec(code_change(OldVsn :: term() | {down, term()}, State :: #main_genserver_state{},
Extra :: term()) ->
{ok, NewState :: #main_genserver_state{}} | {error, Reason :: term()}).
code_change(_OldVsn, State = #main_genserver_state{}, _Extra) ->
{ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%%Receives a list of routers and connects to them
start_connection([])->ok;
start_connection([{_ServerName,{Host, Port}}|Tail]) ->
  httpc:set_options([{proxy, {{Host, Port},[Host]}}]),
  start_connection(Tail).


%%find Router and send message: finds the path for the named machine from connection map and send to the right router to forword.
findroutAndsend([],_)->ok;
findroutAndsend([[SourceName,InputFile]|ListOfSources], ConnectionsMap) ->
%%  io:format("sourceName = ~p~n, Map = ~p~n",[SourceName,ConnectionsMap]),
  {RouterHost,RouterPort} =maps:get(list_to_atom(SourceName),ConnectionsMap),
  io:format("~p~n",[RouterPort]),
  httpc:request(post,{"http://"++ RouterHost ++ ":"++integer_to_list(RouterPort) ++ "/updateCSV", [],"application/x-www-form-urlencoded",SourceName++","++InputFile}, [], []),
  findroutAndsend(ListOfSources,ConnectionsMap).