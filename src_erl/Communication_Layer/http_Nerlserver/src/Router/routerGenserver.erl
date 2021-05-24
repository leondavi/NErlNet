%%%-------------------------------------------------------------------
%%% @author kapelnik
%%% @copyright (C) 2021, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 27. Apr 2021 11:05 AM
%%%-------------------------------------------------------------------
-module(routerGenserver).
-author("kapelnik").

-behaviour(gen_server).

%% API
-export([start_link/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
  code_change/3]).

-define(SERVER, ?MODULE).


-record(router_genserver_state, {myName, connectionsMap}).

%%%===================================================================
%%% API
%%%===================================================================

%% @doc Spawns the server and registers the local name (unique)
-spec(start_link(args) ->
  {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).

start_link({MyName,ConnectionsMap}) ->

  {ok,Pid} = gen_server:start_link({local, list_to_atom(binary_to_list(MyName))}, ?MODULE, {MyName,ConnectionsMap}, []),
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

init({MyName,ConnectionsMap}) ->
  inets:start(),
  start_connection(maps:to_list(ConnectionsMap)),
  {ok, #router_genserver_state{myName = MyName, connectionsMap = ConnectionsMap}}.


%% @private
%% @doc Handling cast messages
-spec(handle_cast(Request :: term(), State :: #router_genserver_state{}) ->
  {noreply, NewState :: #router_genserver_state{}} |
  {noreply, NewState :: #router_genserver_state{}, timeout() | hibernate} |
  {stop, Reason :: term(), NewState :: #router_genserver_state{}}).

handle_cast({rout,Body}, State = #router_genserver_state{connectionsMap = ConnectionsMap}) ->
%%  Body contrains list of sources to send the request, and input name list of clients should be before  '@'
  [To|_Vector] = binary:split(Body,<<"#">>),
  {Host,Port} =maps:get(list_to_atom(binary_to_list(To)),ConnectionsMap),
  http_request(Host,Port,"weightsVector",Body),
  {noreply, State};


handle_cast({clientIdle, Body}, State = #router_genserver_state{connectionsMap = ConnectionsMap}) ->
%%  sending client "training" request
%%  io:format("sending client to idle~p~n",[ClientName]),
  {Host,Port} =maps:get(list_to_atom(binary_to_list(Body)),ConnectionsMap),
  http_request(Host,Port,"clientIdle",Body),
  {noreply, State};

handle_cast({clientTraining, Body}, State = #router_genserver_state{connectionsMap = ConnectionsMap}) ->
%%  sending client "training" request
  {Host,Port} =maps:get(list_to_atom(binary_to_list(Body)),ConnectionsMap),
  http_request(Host,Port,"clientTraining",Body),
  {noreply, State};

handle_cast({clientPredict, Body}, State = #router_genserver_state{connectionsMap = ConnectionsMap}) ->
%%  sending client "training" request
%%  io:format("sending to client predict: ~p~n",[Body]),
  {Host,Port} =maps:get(list_to_atom(binary_to_list(Body)),ConnectionsMap),
  http_request(Host,Port,"clientPredict",Body),
  {noreply, State};

handle_cast({updateCSV,Source,Body}, State = #router_genserver_state{connectionsMap = ConnectionsMap}) ->
%%  Body contrains list of sources to send the request, and input name
io:format("router to Source - ~p  sending Body - ~p~n",[Source,Body]),
  {Host,Port} = maps:get(list_to_atom(Source),ConnectionsMap),
  http_request(Host,Port,"updateCSV",Body),

%%findroutAndsend(splitbyTriplets(SourcesClientsPaths,[]),ConnectionsMap),TODO RETURN THIS!!!!
{noreply, State};

handle_cast({csvReady,Body}, State = #router_genserver_state{connectionsMap = ConnectionsMap}) ->
%%  Body contrains list of sources to send the request, and input name
  {MainHost,MainPort} =maps:get(mainServer,ConnectionsMap),
  http_request(MainHost,MainPort,"csvReady",Body),
  {noreply, State};

handle_cast({sourceDone,Body}, State = #router_genserver_state{connectionsMap = ConnectionsMap}) ->
%%  Body contrains list of sources to send the request, and input name
  {MainHost,MainPort} =maps:get(mainServer,ConnectionsMap),
  http_request(MainHost,MainPort,"sourceDone",Body),
  {noreply, State};

handle_cast({clientReady,Body}, State = #router_genserver_state{connectionsMap = ConnectionsMap}) ->
%%  Body contrains list of sources to send the request, and input name
  {MainHost,MainPort} =maps:get(mainServer,ConnectionsMap),
  http_request(MainHost,MainPort,"clientReady",Body),
  {noreply, State};

handle_cast({startCasting,Body}, State = #router_genserver_state{connectionsMap = ConnectionsMap}) ->
%%  Body contrains list of sources to send the request, and input name
    {SourceHost,SourcePort} =maps:get(list_to_atom(binary_to_list(Body)),ConnectionsMap),
    http_request(SourceHost,SourcePort,"startCasting",Body),
  {noreply, State};

handle_cast({stopCasting,Body}, State = #router_genserver_state{connectionsMap = ConnectionsMap}) ->
%%  Body contrains list of sources to send the request, and input name
    {SourceHost,SourcePort} =maps:get(list_to_atom(binary_to_list(Body)),ConnectionsMap),
    http_request(SourceHost,SourcePort,"stopCasting",Body),
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
start_connection([{ServerName,{Host, Port}}|Tail]) ->
  Result = httpc:set_options([{proxy, {{Host, Port},[Host]}}]),
  io:format("Router is now connected to ~p, Result: ~p~n",[{ServerName,Host, Port},Result]),
  start_connection(Tail).

%%list_to_binary([list_to_binary([Name,<<"#">>]),BinaryHead]))

%%findroutAndsend([],_)->ok;
%%findroutAndsend([[SourceName,ClientName,InputFile]|ListOfSources], ConnectionsMap) ->
%%%%  io:format("sourceName = ~p~n, Map = ~p~n",[SourceName,ConnectionsMap]),
%%  {SourceHost,SourcePort} =maps:get(list_to_atom(SourceName),ConnectionsMap),
%%%%  io:format("~p~n",[SourcePort]),
%%  http_request(SourceHost,SourcePort,"updateCSV",ClientName++","++InputFile),
%%  findroutAndsend(ListOfSources,ConnectionsMap).
%%
%%
%%splitbyTriplets([],Ret) ->Ret;
%%splitbyTriplets(ListofTriplets,Ret) ->
%%  L1 = lists:sublist(ListofTriplets,1,3),
%%  L2 = lists:sublist(ListofTriplets,4,length(ListofTriplets)-1),
%%  splitbyTriplets(L2,Ret++[L1]).


http_request(Host, Port,Path, Body)->
%%  io:format("sending body ~p to path ~p to hostport:~p~n",[Body,Path,{Host,Port}]),
  URL = "http://" ++ Host ++ ":"++integer_to_list(Port) ++ "/" ++ Path,
  httpc:set_options([{proxy, {{Host, Port},[Host]}}]),
  httpc:request(post,{URL, [],"application/x-www-form-urlencoded",Body}, [], []).
