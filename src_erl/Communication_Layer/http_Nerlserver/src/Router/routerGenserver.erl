%%%-------------------------------------------------------------------
%%% @author kapelnik
%%% @copyright (C) 2021, Nerlnet
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


-record(router_genserver_state, {myName, nerlnetGraph,msgCounter = 0}).

%%%===================================================================
%%% API
%%%===================================================================

%% @doc Spawns the server and registers the local name (unique)
-spec(start_link(args) ->
  {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).

start_link({MyName,NerlnetGraph}) ->

  {ok,Pid} = gen_server:start_link({local, list_to_atom(MyName)}, ?MODULE, {MyName,NerlnetGraph}, []),
  Pid.

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%% @private
%% @doc Initializes the server
-spec(init(Args :: term()) ->
  {ok, State :: #router_genserver_state{}} | {ok, State :: #router_genserver_state{}, timeout() | hibernate} |
  {stop, Reason :: term()} | ignore).

init({MyName,NerlnetGraph}) ->
  inets:start(),
    io:format("Router ~p Connecting to: ~p~n",[MyName, [digraph:vertex(NerlnetGraph,Vertex) || Vertex <- digraph:out_neighbours(NerlnetGraph,MyName)]]),
    % nerl_tools:start_connection([digraph:vertex(NerlnetGraph,Vertex) || Vertex <- digraph:out_neighbours(NerlnetGraph,MyName)]),

  {ok, #router_genserver_state{msgCounter = 1, myName = MyName, nerlnetGraph = NerlnetGraph}}.


%% @private
%% @doc Handling cast messages
-spec(handle_cast(Request :: term(), State :: #router_genserver_state{}) ->
  {noreply, NewState :: #router_genserver_state{}} |
  {noreply, NewState :: #router_genserver_state{}, timeout() | hibernate} |
  {stop, Reason :: term(), NewState :: #router_genserver_state{}}).

handle_cast({rout,Body}, State = #router_genserver_state{myName = MyName, msgCounter = MsgCounter, nerlnetGraph = NerlnetGraph}) ->
%%  Body contains list of sources to send the request, and input name list of clients should be before  '@'
%%  ToSend = term_to_binary({ClientName, WorkerName, CSVPath, Counter, Head}),
  {To, _WorkerName, _CSVPath, _Counter, _Head} = binary_to_term(Body),
  {Host,Port} = nerl_tools:getShortPath(MyName,To,NerlnetGraph),
  %{Host,Port} =maps:get(To,NerlnetGraph),
  nerl_tools:http_request(Host,Port,"weightsVector",Body),
  {noreply, State#router_genserver_state{msgCounter = MsgCounter+1}};


handle_cast({statistics,Body}, State = #router_genserver_state{myName = MyName,msgCounter = MsgCounter, nerlnetGraph = NerlnetGraph}) ->
%%  Body contrains list of sources to send the request, and input name list of clients should be before  '@'.
%%  if Body = my name, its for me, if the body contains #, its for main server, else its for someone else
  BodyString = binary_to_list(Body),
  if BodyString == MyName ->
          {Host,Port} = nerl_tools:getShortPath(MyName,"mainServer",NerlnetGraph),
          nerl_tools:http_request(Host,Port,"statistics",list_to_binary(MyName++":"++integer_to_list(MsgCounter)));
    true ->
        Splitted =  re:split(Body, ":", [{return, binary}]),
        if length(Splitted) ==1 ->
            {Host,Port} = nerl_tools:getShortPath(MyName,list_to_atom(binary_to_list(Body)),NerlnetGraph),

            %{Host,Port} =maps:get(list_to_atom(binary_to_list(Body)),NerlnetGraph),
            nerl_tools:http_request(Host,Port,"statistics",Body);
          true ->
              {Host,Port} = nerl_tools:getShortPath(MyName,"mainServer",NerlnetGraph),
              nerl_tools:http_request(Host,Port,"statistics",Body)
            end
    end,
  {noreply, State#router_genserver_state{msgCounter = MsgCounter+1}};

handle_cast({lossFunction,Body}, State = #router_genserver_state{myName = MyName, msgCounter = MsgCounter, nerlnetGraph = NerlnetGraph}) ->
%%  Body contrains list of sources to send the request, and input name list of clients should be before  '@'
    {Host,Port} = nerl_tools:getShortPath(MyName,"mainServer",NerlnetGraph),
    % io:format("at router: lossFucntion: ~p~n",[Body]),
    nerl_tools:http_request(Host,Port,"lossFunction",Body),
  {noreply, State#router_genserver_state{msgCounter = MsgCounter+1}};

handle_cast({predictRes,Body}, State = #router_genserver_state{myName = MyName, msgCounter = MsgCounter, nerlnetGraph = NerlnetGraph}) ->
%%  Body contrains list of sources to send the request, and input name list of clients should be before  '@'
    {Host,Port} = nerl_tools:getShortPath(MyName,"mainServer",NerlnetGraph),
    nerl_tools:http_request(Host,Port,"predictRes",Body),
  {noreply, State#router_genserver_state{msgCounter = MsgCounter+1}};

handle_cast({federatedWeightsVector,Body}, State = #router_genserver_state{myName = MyName, msgCounter = MsgCounter, nerlnetGraph = NerlnetGraph}) ->
%%  Body contrains list of sources to send the request, and input name list of clients should be before  '@'
  {To,_Vector} = binary_to_term(Body),
  {Host,Port} = nerl_tools:getShortPath(MyName,To,NerlnetGraph),
  %{Host,Port} =maps:get(To,NerlnetGraph),
  nerl_tools:http_request(Host,Port,"federatedWeightsVector",Body),
  {noreply, State#router_genserver_state{msgCounter = MsgCounter+1}};

handle_cast({federatedWeights,Body}, State = #router_genserver_state{myName = MyName, msgCounter = MsgCounter, nerlnetGraph = NerlnetGraph}) ->
%%  Body contrains list of sources to send the request, and input name list of clients should be before  '@'
    {ClientName,_WorkerName,_BinaryWeights} = binary_to_term(Body),
    {Host,Port} = nerl_tools:getShortPath(MyName,ClientName,NerlnetGraph),
    %{Host,Port} =maps:get(ClientName,NerlnetGraph),
    nerl_tools:http_request(Host,Port,"federatedWeights",Body),
    {noreply, State#router_genserver_state{msgCounter = MsgCounter+1}};


handle_cast({clientIdle, Body}, State = #router_genserver_state{myName = MyName, msgCounter = MsgCounter, nerlnetGraph = NerlnetGraph}) ->
    %%  sending client "training" request
    %%  io:format("sending client to idle~p~n",[ClientName]),
    {Host,Port} = nerl_tools:getShortPath(MyName,list_to_atom(binary_to_list(Body)),NerlnetGraph),
    %{Host,Port} =maps:get(list_to_atom(binary_to_list(Body)),NerlnetGraph),
    nerl_tools:http_request(Host,Port,"clientIdle",Body),
    {noreply, State#router_genserver_state{msgCounter = MsgCounter+1}};

handle_cast({clientTraining, Body}, State = #router_genserver_state{myName = MyName, msgCounter = MsgCounter, nerlnetGraph = NerlnetGraph}) ->
%%  sending client "training" request
    {Host,Port} = nerl_tools:getShortPath(MyName,list_to_atom(binary_to_list(Body)),NerlnetGraph),
    %{Host,Port} =maps:get(list_to_atom(binary_to_list(Body)),NerlnetGraph),
    nerl_tools:http_request(Host,Port,"clientTraining",Body),
    {noreply, State#router_genserver_state{msgCounter = MsgCounter+1}};

handle_cast({clientPredict, Body}, State = #router_genserver_state{myName = MyName, msgCounter = MsgCounter, nerlnetGraph = NerlnetGraph}) ->
%%  sending client "training" request
%%  io:format("sending to client predict: ~p~n",[Body]),
    {Host,Port} = nerl_tools:getShortPath(MyName,list_to_atom(binary_to_list(Body)),NerlnetGraph),
    %{Host,Port} =maps:get(list_to_atom(binary_to_list(Body)),NerlnetGraph),
    nerl_tools:http_request(Host,Port,"clientPredict",Body),
    {noreply, State#router_genserver_state{msgCounter = MsgCounter+1}};

handle_cast({updateCSV,Source,Body}, State = #router_genserver_state{myName = MyName, nerlnetGraph = NerlnetGraph}) ->
%%  Body contrains list of sources to send the request, and input name
    %io:format("router to Source - ~p  sending Body - ~p~n",[Source,Body]),
    {Host,Port} = nerl_tools:getShortPath(MyName,list_to_atom(Source),NerlnetGraph),
    %{Host,Port} = maps:get(list_to_atom(Source),NerlnetGraph),
    nerl_tools:http_request(Host,Port,"updateCSV",Body),

%%findroutAndsend(splitbyTriplets(SourcesClientsPaths,[]),NerlnetGraph),TODO RETURN THIS!!!!
{noreply, State};

handle_cast({csvReady,Body}, State = #router_genserver_state{myName = MyName, msgCounter = MsgCounter, nerlnetGraph = NerlnetGraph}) ->
    %%  Body contrains list of sources to send the request, and input name
    {Host,Port} = nerl_tools:getShortPath(MyName,"mainServer",NerlnetGraph),
    %{MainHost,MainPort} =maps:get(mainServer,NerlnetGraph),
    nerl_tools:http_request(Host,Port,"csvReady",Body),
    {noreply, State#router_genserver_state{msgCounter = MsgCounter+1}};

handle_cast({sourceDone,Body}, State = #router_genserver_state{myName = MyName, msgCounter = MsgCounter, nerlnetGraph = NerlnetGraph}) ->
    %%  Body contrains list of sources to send the request, and input name
    {Host,Port} = nerl_tools:getShortPath(MyName,"mainServer",NerlnetGraph),
     %io:format("source finished casting- ~n sending to mainserver via: ~p~n",[{Host,Port}]),
    % {MainHost,MainPort} =maps:get(mainServer,NerlnetGraph),
    nerl_tools:http_request(Host,Port,"sourceDone",Body),
    {noreply, State#router_genserver_state{msgCounter = MsgCounter+1}};

handle_cast({clientReady,Body}, State = #router_genserver_state{myName = MyName, msgCounter = MsgCounter, nerlnetGraph = NerlnetGraph}) ->
    %%  Body contrains list of sources to send the request, and input name
    {Host,Port} = nerl_tools:getShortPath(MyName,"mainServer",NerlnetGraph),
    %{MainHost,MainPort} =maps:get(mainServer,NerlnetGraph),
    nerl_tools:http_request(Host,Port,"clientReady",Body),
    {noreply, State#router_genserver_state{msgCounter = MsgCounter+1}};

handle_cast({startCasting,Body}, State = #router_genserver_state{myName = MyName, msgCounter = MsgCounter, nerlnetGraph = NerlnetGraph}) ->
    %%  Body contrains list of sources to send the request, and input name
    [Source|_] = re:split(binary_to_list(Body), ",", [{return, list}]),
    {Host,Port} = nerl_tools:getShortPath(MyName,list_to_atom(Source),NerlnetGraph),
    %{SourceHost,SourcePort} =maps:get(list_to_atom(Source),NerlnetGraph),
    nerl_tools:http_request(Host,Port,"startCasting",Body),
    {noreply, State#router_genserver_state{msgCounter = MsgCounter+1}};

handle_cast({stopCasting,Body}, State = #router_genserver_state{myName = MyName, msgCounter = MsgCounter, nerlnetGraph = NerlnetGraph}) ->
%%  Body contrains list of sources to send the request, and input name
    {Host,Port} = nerl_tools:getShortPath(MyName,list_to_atom(binary_to_list(Body)),NerlnetGraph),
    %{SourceHost,SourcePort} =maps:get(list_to_atom(binary_to_list(Body)),NerlnetGraph),
    nerl_tools:http_request(Host,Port,"stopCasting",Body),
    {noreply, State#router_genserver_state{msgCounter = MsgCounter+1}};


%%%%%%%GUI ACTIONS
handle_cast({getStats,_Body}, State  = #router_genserver_state{myName = MyName, msgCounter = MsgCounter, nerlnetGraph = NerlnetGraph}) ->
    {_Name, {Host,Port}} = digraph:vertex(NerlnetGraph, "nerlGUI"),
    Connected = [ V++", " || V <- digraph:out_neighbours(NerlnetGraph, MyName)],
    %io:format("Conn list is: ~p~n",[Connected]),
    Mes = "routerScreen@"++MyName++",messStats="++integer_to_list(MsgCounter)++";connList="++lists:concat(Connected),
    nerl_tools:http_request(Host,Port,"routerStats",Mes),
    {noreply, State#router_genserver_state{msgCounter = MsgCounter+1}};


handle_cast(_Request, State = #router_genserver_state{msgCounter = MsgCounter }) ->
  {noreply, State#router_genserver_state{msgCounter = MsgCounter+1}}.



%% @private
%% @doc Handling call messages
-spec(handle_call(Request :: term(), From :: {pid(), Tag :: term()},
    State :: #router_genserver_state{ }) ->
  {reply, Reply :: term(), NewState :: #router_genserver_state{ }} |
  {reply, Reply :: term(), NewState :: #router_genserver_state{ }, timeout() | hibernate} |
  {noreply, NewState :: #router_genserver_state{ }} |
  {noreply, NewState :: #router_genserver_state{ }, timeout() | hibernate} |
  {stop, Reason :: term(), Reply :: term(), NewState :: #router_genserver_state{ }} |
  {stop, Reason :: term(), NewState :: #router_genserver_state{ }}).
handle_call(_Request, _From, State = #router_genserver_state{ }) ->
  {reply, ok, State}.


%% @private
%% @doc Handling all non call/cast messages
-spec(handle_info(Info :: timeout() | term(), State :: #router_genserver_state{ }) ->
  {noreply, NewState :: #router_genserver_state{ }} |
  {noreply, NewState :: #router_genserver_state{ }, timeout() | hibernate} |
  {stop, Reason :: term(), NewState :: #router_genserver_state{ }}).
handle_info(_Info, State = #router_genserver_state{ }) ->
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
-spec(code_change(OldVsn :: term() | {down, term()}, State :: #router_genserver_state{ },
    Extra :: term()) ->
  {ok, NewState :: #router_genserver_state{}} | {error, Reason :: term()}).
code_change(_OldVsn, State = #router_genserver_state{}, _Extra) ->
  {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
