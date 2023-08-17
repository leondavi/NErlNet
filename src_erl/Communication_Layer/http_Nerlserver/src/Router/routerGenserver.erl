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

-include("../nerl_tools.hrl").

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
  {ok,Pid} = gen_server:start_link({local, MyName}, ?MODULE, {MyName,NerlnetGraph}, []),
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
  nerl_tools:setup_logger(?MODULE),
  inets:start(),
  ?LOG_NOTICE("Router ~p is connected to: ~p~n",[MyName, [digraph:vertex(NerlnetGraph,Vertex) || Vertex <- digraph:out_neighbours(NerlnetGraph,MyName)]]),
    % nerl_tools:start_connection([digraph:vertex(NerlnetGraph,Vertex) || Vertex <- digraph:out_neighbours(NerlnetGraph,MyName)]),
  put(nerlnetGraph, NerlnetGraph),
  {ok, #router_genserver_state{msgCounter = 1, myName = MyName }}.


%% @private
%% @doc Handling cast messages
-spec(handle_cast(Request :: term(), State :: #router_genserver_state{}) ->
  {noreply, NewState :: #router_genserver_state{}} |
  {noreply, NewState :: #router_genserver_state{}, timeout() | hibernate} |
  {stop, Reason :: term(), NewState :: #router_genserver_state{}}).

%% data passing format: {To, Action, Data}
handle_cast({pass,Body}, State = #router_genserver_state{myName = MyName, msgCounter = MsgCounter }) ->
  {To, Action, Data} = binary_to_term(Body),
  nerl_tools:sendHTTP(MyName, To, Action, Body),
{noreply, State#router_genserver_state{msgCounter = MsgCounter+1}};

handle_cast({rout,Body}, State = #router_genserver_state{myName = MyName, msgCounter = MsgCounter }) ->
%%  Body contains list of sources to send the request, and input name list of clients should be before  '@'
%%  ToSend = term_to_binary({ClientName, WorkerName, CSVPath, Counter, Head}),
  {To, _WorkerName, _CSVPath, _Counter, _Head} = binary_to_term(Body),
  nerl_tools:sendHTTP(MyName, To, "weightsVector", Body),
  {noreply, State#router_genserver_state{msgCounter = MsgCounter+1}};


handle_cast({statistics,Body}, State = #router_genserver_state{myName = MyName,msgCounter = MsgCounter }) ->
%%  Body contrains list of sources to send the request, and input name list of clients should be before  '@'.
%%  if Body = my name, its for me, if the body contains #, its for main server, else its for someone else
  BodyString = binary_to_list(Body),
  MyNameStr = atom_to_list(MyName),
  if BodyString == MyNameStr ->
          nerl_tools:sendHTTP(MyName, ?MAIN_SERVER_ATOM, "statistics", list_to_binary(MyNameStr++":"++integer_to_list(MsgCounter)));
    true ->
        Splitted =  re:split(Body, ":", [{return, binary}]),
        if length(Splitted) ==1 ->  %% one entity to query
            nerl_tools:sendHTTP(MyName, list_to_atom(binary_to_list(Body)), "statistics", Body);
          true -> 
            nerl_tools:sendHTTP(MyName, ?MAIN_SERVER_ATOM, "statistics", Body)
        end
    end,
  {noreply, State#router_genserver_state{msgCounter = MsgCounter+1}};

handle_cast({lossFunction,Body}, State = #router_genserver_state{myName = MyName, msgCounter = MsgCounter }) ->
%%  Body contrains list of sources to send the request, and input name list of clients should be before  '@'
  nerl_tools:sendHTTP(MyName, ?MAIN_SERVER_ATOM, "lossFunction", Body),
  {noreply, State#router_genserver_state{msgCounter = MsgCounter+1}};

handle_cast({predictRes,Body}, State = #router_genserver_state{myName = MyName, msgCounter = MsgCounter }) ->
%%  Body contrains list of sources to send the request, and input name list of clients should be before  '@'
  nerl_tools:sendHTTP(MyName, ?MAIN_SERVER_ATOM, "predictRes", Body),
  {noreply, State#router_genserver_state{msgCounter = MsgCounter+1}};

% handle_cast({federatedWeightsVector,Body}, State = #router_genserver_state{myName = MyName, msgCounter = MsgCounter }) ->
% %%  Body contrains list of sources to send the request, and input name list of clients should be before  '@'
%   {To,_Vector} = binary_to_term(Body),
%   nerl_tools:sendHTTP(MyName, To, "federatedWeightsVector", Body),  
%   {noreply, State#router_genserver_state{msgCounter = MsgCounter+1}};

% handle_cast({federatedWeights,Body}, State = #router_genserver_state{myName = MyName, msgCounter = MsgCounter }) ->
% %%  Body contrains list of sources to send the request, and input name list of clients should be before  '@'
%     {ClientName,_WorkerName,_BinaryWeights} = binary_to_term(Body),
%     nerl_tools:sendHTTP(MyName, ClientName, "federatedWeights", Body), 
%     {noreply, State#router_genserver_state{msgCounter = MsgCounter+1}};


handle_cast({clientIdle, Body}, State = #router_genserver_state{myName = MyName, msgCounter = MsgCounter }) ->
    %%  sending client "idle" request
    nerl_tools:sendHTTP(MyName, list_to_atom(binary_to_list(Body)), "clientIdle", Body), 
    {noreply, State#router_genserver_state{msgCounter = MsgCounter+1}};

handle_cast({clientTraining, Body}, State = #router_genserver_state{myName = MyName, msgCounter = MsgCounter }) ->
%%  sending client "training" request
    nerl_tools:sendHTTP(MyName, list_to_atom(binary_to_list(Body)), "clientTraining", Body), 
    {noreply, State#router_genserver_state{msgCounter = MsgCounter+1}};

handle_cast({clientPredict, Body}, State = #router_genserver_state{myName = MyName, msgCounter = MsgCounter }) ->
%%  sending client "predict" request
%%  io:format("sending to client predict: ~p~n",[Body]),
    nerl_tools:sendHTTP(MyName, list_to_atom(binary_to_list(Body)), "clientPredict", Body), 
    {noreply, State#router_genserver_state{msgCounter = MsgCounter+1}};

handle_cast({updateCSV,Source,Body}, State = #router_genserver_state{myName = MyName }) ->
%%  Body contrains list of sources to send the request, and input name
    %io:format("router to Source - ~p  sending Body - ~p~n",[Source,Body]),
    nerl_tools:sendHTTP(MyName, list_to_atom(Source), "updateCSV", Body), 
%%findroutAndsend(splitbyTriplets(SourcesClientsPaths,[]),NerlnetGraph),TODO RETURN THIS!!!!
{noreply, State};

handle_cast({csvReady,Body}, State = #router_genserver_state{myName = MyName, msgCounter = MsgCounter }) ->
    %%  Body contrains list of sources to send the request, and input name
    nerl_tools:sendHTTP(MyName, ?MAIN_SERVER_ATOM, "csvReady", Body), 
    {noreply, State#router_genserver_state{msgCounter = MsgCounter+1}};

handle_cast({sourceDone,Body}, State = #router_genserver_state{myName = MyName, msgCounter = MsgCounter }) ->
    %%  Body contrains list of sources to send the request, and input name
    nerl_tools:sendHTTP(MyName, ?MAIN_SERVER_ATOM, "sourceDone", Body), 
    {noreply, State#router_genserver_state{msgCounter = MsgCounter+1}};

handle_cast({clientReady,Body}, State = #router_genserver_state{myName = MyName, msgCounter = MsgCounter }) ->
    %%  Body contrains list of sources to send the request, and input name
    nerl_tools:sendHTTP(MyName, ?MAIN_SERVER_ATOM, "clientReady", Body), 
    {noreply, State#router_genserver_state{msgCounter = MsgCounter+1}};

handle_cast({startCasting,Body}, State = #router_genserver_state{myName = MyName, msgCounter = MsgCounter }) ->
    %%  Body contrains list of sources to send the request, and input name
    [Source|_] = re:split(binary_to_list(Body), ",", [{return, list}]),
    nerl_tools:sendHTTP(MyName, list_to_atom(Source), "startCasting", Body), 
    {noreply, State#router_genserver_state{msgCounter = MsgCounter+1}};

handle_cast({stopCasting,Body}, State = #router_genserver_state{myName = MyName, msgCounter = MsgCounter }) ->
%%  Body contrains list of sources to send the request, and input name
    nerl_tools:sendHTTP(MyName, list_to_atom(binary_to_list(Body)), "stopCasting", Body), 
    {noreply, State#router_genserver_state{msgCounter = MsgCounter+1}};


%%%%%%%GUI ACTIONS
handle_cast({getStats,_Body}, State  = #router_genserver_state{myName = MyName, msgCounter = MsgCounter }) ->
    {_Name, {Host,Port}} = digraph:vertex(get(nerlnetGraph), "nerlGUI"),
    Connected = [ V++", " || V <- digraph:out_neighbours(get(nerlnetGraph), MyName)],
    %io:format("Conn list is: ~p~n",[Connected]),
    Mes = "routerScreen@"++MyName++",messStats="++integer_to_list(MsgCounter)++";connList="++lists:concat(Connected),
    nerl_tools:http_request(Host,Port,"routerStats",Mes),
    {noreply, State#router_genserver_state{msgCounter = MsgCounter+1}};

handle_cast({worker_kill , Body} , State = #router_genserver_state{msgCounter = MsgCounter, myName = MyName}) ->
    io:format("Body is ~p~n",[binary_to_term(Body)]),
    {ClientName , _} = binary_to_term(Body),
    nerl_tools:sendHTTP(MyName, ClientName, "worker_kill", Body),
    {noreply, State#router_genserver_state{msgCounter = MsgCounter+1}};


%monitor
handle_cast({worker_down,Body}, State = #router_genserver_state{myName = MyName, msgCounter = MsgCounter, nerlnetGraph = NerlnetGraph}) ->
    nerl_tools:sendHTTP(MyName, ?MAIN_SERVER_ATOM, "worker_down", Body),
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
