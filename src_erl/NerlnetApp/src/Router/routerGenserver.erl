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
-define(UNICAST_ACTION_ATOM, unicast).

-record(router_genserver_state, {myName, nerlnetGraph,msgCounter = 0,etsRef}).
%%%===================================================================
%%% API
%%%===================================================================

%% @doc Spawns the server and registers the local name (unique)
-spec(start_link(args) ->
  {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).

start_link({MyName, Policy ,NerlnetGraph}) ->
  {ok,Pid} = gen_server:start_link({local, MyName}, ?MODULE, {MyName, Policy ,NerlnetGraph}, []),
  Pid.

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init({MyName , _Policy , NerlnetGraph}) -> %% TODO : Add policy to router
  nerl_tools:setup_logger(?MODULE),
  inets:start(),
  ?LOG_NOTICE("Router ~p is connected to: ~p~n",[MyName, [digraph:vertex(NerlnetGraph,Vertex) || Vertex <- digraph:out_neighbours(NerlnetGraph,MyName)]]),
  put(nerlnetGraph, NerlnetGraph),
  put(myName, MyName),
  RoutingTableEtsRef = ets:new(routing_table, [set]),
  RouterStatsEts = stats:generate_stats_ets(),
  put(router_stats_ets, RouterStatsEts),
  EntitiesList=digraph:vertices(NerlnetGraph),
  nerl_tools:make_routing_table(RoutingTableEtsRef,EntitiesList--[MyName],MyName,NerlnetGraph),
  {ok, #router_genserver_state{msgCounter = 1, myName = MyName, etsRef=RoutingTableEtsRef}}.

handle_cast({statistics , _Body} , State=#router_genserver_state{etsRef = Routing_table}) ->

  RouterStatsEts = get(router_stats_ets),
  stats:increment_messages_received(RouterStatsEts),

  MyName = get(myName),
  StatsEtsStr = stats:encode_ets_to_http_bin_str(RouterStatsEts),
  StatisticsBody = {term_to_binary(MyName) , list_to_binary(StatsEtsStr)}, % old data
  [{_Dest,{_Name , RouterHost , RouterPort}}] = ets:lookup(Routing_table , ?MAIN_SERVER_ATOM),
  nerl_tools:http_router_request(RouterHost, RouterPort, [?MAIN_SERVER_ATOM], atom_to_list(statistics), StatisticsBody),
  stats:increment_messages_sent(RouterStatsEts),
  {noreply , State};

handle_cast({unicast,{Dest,Body}}, State = #router_genserver_state{msgCounter = MsgCounter,etsRef=Routing_table }) ->
  RouterStatsEts = get(router_stats_ets),
  stats:increment_messages_received(RouterStatsEts),
  DestAtom = if is_list(Dest)-> list_to_atom(Dest);
                is_binary(Dest)-> binary_to_atom(Dest);
                true -> Dest
            end,
  [{DestAtom,{Name,Host,Port}}] =  ets:lookup(Routing_table, DestAtom),
  case DestAtom of
    Name->
      %the destination is the next hop, send as regular message
      {Action,Data}=Body;
    _->
      %next hop isnt the destination, continue as geneal router message
      Action=atom_to_list(?UNICAST_ACTION_ATOM),
      Data={Dest,Body}
    end,
  DataToSend = 
    if ?API_SERVER_ATOM =:= DestAtom -> Data;
    true -> term_to_binary(Data)
  end,
  nerl_tools:http_request(Host, Port, Action, term_to_binary(Data)),
  stats:increment_messages_sent(RouterStatsEts),
  {noreply, State#router_genserver_state{msgCounter = MsgCounter+1,etsRef=Routing_table }};

handle_cast({broadcast,{DestList,Body}}, State = #router_genserver_state{etsRef=Routing_table }) ->
  RouterStatsEts = get(router_stats_ets),
  stats:increment_messages_received(RouterStatsEts),
  MapFunc=fun(Dest,Acc)->
    %make a map when keys are addreses to send a message to, and values are lists of destination of the message that go throu key addres
    [{Dest,{Name,Host,Port}}]=ets:lookup(Routing_table,Dest),
    case maps:is_key({Name,Host,Port},Acc) of
      true->
        %addres alread in, append Dest to exsisting value 
        NewVal=maps:get({Name,Host,Port},Acc)++[Dest];
      false->
        %addres not in yet, create new value for it
        NewVal=[Dest]
    end,
    maps:put({Name,Host,Port},NewVal,Acc)
  end,

  NextHopMap=lists:foldl(MapFunc,#{},DestList),

  SendFunc=fun({Name,Host,Port},DestEntityList)->
    %iterate on the maps keys (addreses) and forword message according to 1 of 3 cases
    case length(DestEntityList) of
      1->
        [Entity]=DestEntityList,
        case Entity of
          Name->
            {Action,Data}=Body;
          _->
            Action=atom_to_list(?UNICAST_ACTION_ATOM),
            Data={Entity,Body}
        end;
      _->
        %multipul destinations continue as broadcast message
        Action="broadcast",
        Data={DestEntityList,Body}
    end,
    nerl_tools:http_request(Host, Port,Action, term_to_binary(Data)),
    stats:increment_messages_sent(RouterStatsEts)
  end,
  maps:foreach(SendFunc,NextHopMap),
  {noreply, State#router_genserver_state{etsRef=Routing_table }};


handle_cast(_Request, State) ->
  ?LOG_ERROR("Unrecognized handle cast message! only unicast/broadcast types are allowed"),
  stats:increment_bad_messages(get(router_stats_ets)),
  {noreply, State}.



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
  ?LOG_ERROR("Wrong handle call message was received!"),
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
