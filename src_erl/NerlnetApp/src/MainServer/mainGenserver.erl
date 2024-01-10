%%%-------------------------------------------------------------------
%%% @author Tal Kapelnik, Haran Cohen, Guy Perets, David Leon
%%% @copyright (C) 2024, Nerlnet
%%% @doc
%%%
%%% @end
%%% Created : 27. Apr 2021 3:35 AM
%%%-------------------------------------------------------------------
-module(mainGenserver).
-author("kapelnik").

-behaviour(gen_server).
-include("../nerl_tools.hrl").
-include("../Stats/stats.hrl").
-include("mainServerDefs.hrl").


%% API
-export([start_link/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(SERVER, ?MODULE).


-record(main_genserver_state, {statisticsCounter = 0, myName, state, workersMap, clients, nerlnetGraph, sourcesCastingList = [], sourcesWaitingList = [], clientsWaitingList = [], statisticsMap, msgCounter = 0, batchSize}).

%%%===============================================================
%%% API
%%%===================================================================

%% @doc Spawns the server and registers the local name (unique)
-spec(start_link(args) ->
  {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link(Args) ->
  {ok,Gen_Server_Pid} = gen_server:start_link({local, ?SERVER}, ?MODULE, Args, []),
  Gen_Server_Pid.

handle_call(_Call, _From, State) ->
    {noreply, State}.


%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%% @private
%% @doc Initializes the server
-spec(init(Args :: term()) ->
{ok, State :: #main_genserver_state{}} | {ok, State :: #main_genserver_state{}, timeout() | hibernate} |
{stop, Reason :: term()} | ignore).

init({MyName,ClientsNames,BatchSize,WorkersMap,NerlnetGraph}) ->
  nerl_tools:setup_logger(?MODULE),
  inets:start(),
  MyNameStr = atom_to_list(MyName),
  ?MAIN_SERVER_ATOM = MyName, % must be identical
  ?LOG_NOTICE("Main Server starts"),
  ConnectedEntities = [digraph:vertex(NerlnetGraph,Vertex) || Vertex <- digraph:out_neighbours(NerlnetGraph,?MAIN_SERVER_ATOM)],
  ?LOG_NOTICE("Main Server is connected to: ~p~n",[ConnectedEntities]),
  MainServerEts = ets:new(main_server_ets , [set]),
  put(main_server_ets, MainServerEts),
  put(nerlnet_graph, NerlnetGraph),
  EtsStats = ets:new(stats , [set]),
  put(etsStats, EtsStats), %% All entities including mainServer ets tables statistics
  Entities = [digraph:vertex(NerlnetGraph,Vertex) || Vertex <- digraph:vertices(NerlnetGraph)--[?API_SERVER_ATOM]],
  generate_stats_ets_tables(Entities),
  ets:insert(MainServerEts , {entities_names_list , Entities -- [?MAIN_SERVER_ATOM]}),
  ets:insert(MainServerEts , {batch_size , BatchSize}),
  ets:insert(MainServerEts , {workers_map , WorkersMap}),
  ets:insert(MainServerEts , {clients_names_list , ClientsNames}),
  ets:insert(MainServerEts , {counter_received_stats, 0}),
  % Getting the router that main server is connected with
  {MyRouterHost,MyRouterPort} = nerl_tools:getShortPath(MyName,hd(ClientsNames),get(nerlnet_graph)),
  ets:insert(MainServerEts, {my_router,{MyRouterHost,MyRouterPort}}),
  {ok, #main_genserver_state{myName = MyNameStr , state=idle}}.


handle_cast({initCSV, SourceName ,SourceData}, State = #main_genserver_state{state = idle, sourcesWaitingList = SourcesWaitingList}) ->
  {RouterHost,RouterPort} = ets:lookup_element(get(main_server_ets), my_router, ?DATA_IDX),
  ActionStr = atom_to_list(updateCSV),
  nerltools:http_router_request(RouterHost,RouterPort, [SourceName], ActionStr, SourceData), % update the source with its data
  UpdatedSourceWaitingList = SourcesWaitingList++[list_to_atom(SourceName)],
  {noreply, State#main_genserver_state{sourcesWaitingList = UpdatedSourceWaitingList}};

handle_cast({initCSV, _SourceName ,_SourceData}, State) ->
  ?LOG_ERROR("initCSV is only applicalble when main server is in idle state!"),
  {noreply, State#main_genserver_state{}};


handle_cast({clientsTraining, Body}, State = #main_genserver_state{state = casting}) ->
  ?LOG_WARNING("Received training request during casting phase",[]),
  ?LOG_INFO("Body content ~p",[Body]),
  StatsEts = get_entity_stats_ets(?MAIN_SERVER_ATOM),
  stats:increment_messages_received(StatsEts),
  stats:increment_bad_messages(StatsEts),
  {noreply, State#main_genserver_state{}};

handle_cast({clientsTraining, _Body}, State = #main_genserver_state{myName = MyName}) ->
%%  send router http request, to route this message to all sensors
  ?LOG_INFO("Casts the training phase message to all clients"),
  StatsEts = get_entity_stats_ets(?MAIN_SERVER_ATOM),
  stats:increment_messages_received(StatsEts),
  update_clients_phase(clientsTraining, MyName), % update all clients with clientsTraining phase
  stats:increment_messages_sent(StatsEts),
  {noreply, State#main_genserver_state{}};

handle_cast({clientsPredict, Body}, State = #main_genserver_state{state = casting}) ->
  ?LOG_WARNING("Received prediction request during casting phase",[]),
  ?LOG_INFO("Body content ~p",[Body]),
  StatsEts = get_entity_stats_ets(?MAIN_SERVER_ATOM),
  stats:increment_messages_received(StatsEts),
  stats:increment_bad_messages(StatsEts),
  {noreply, State#main_genserver_state{}};

handle_cast({clientsPredict,_Body}, State = #main_genserver_state{myName = MyName}) ->
  %  send router http request, to rout this message to all sensors
  %  updating all clients on clientsPredict state
  ?LOG_INFO("Casts the prediction phase message to all clients"),
  StatsEts = get_entity_stats_ets(?MAIN_SERVER_ATOM),
  stats:increment_messages_received(StatsEts),
  PhaseAtom = clientPredict,
  update_clients_phase(PhaseAtom, MyName),
  stats:increment_messages_sent(StatsEts),
  {noreply, State#main_genserver_state{}};


handle_cast({clientsIdle}, State = #main_genserver_state{state = idle, myName = MyName}) ->
  %%  send router http request, to route this message to all sensors
  ?LOG_INFO("Casts the idle phase message to all clients"),
  StatsEts = get_entity_stats_ets(?MAIN_SERVER_ATOM),
  stats:increment_messages_received(StatsEts),
  PhaseAtom = clientsIdle,
  update_clients_phase(PhaseAtom, MyName),
  stats:increment_messages_sent(StatsEts),
  {noreply, State#main_genserver_state{}};

% TODO
%%% get Statistics from all Entities in the network
handle_cast({statistics,Body}, State = #main_genserver_state{myName = MyName}) ->
    StatsEts = get_entity_stats_ets(?MAIN_SERVER_ATOM),
    stats:increment_messages_received(StatsEts),
    if Body == <<"getStatistics">> ->   %% initial message from APIServer, get stats from entities
        statistics_requests_to_entities(), % Broadcast sends - worth 1 message
        stats:increment_messages_sent(StatsEts);
      Body == <<>> ->  ?LOG_ERROR("~p: Wrong statistics message",[MyName]);

      true ->
          %% TODO - Guy here you should get the the encoded statistics from entities and decode it use it the function you should implement
          %%      statistics arrived from Entity
          {From, StatsEtsEncStr} = Body,
          EntityName = binary_to_atom(From),
          EntityStatsEts = get_entity_stats_ets(EntityName),
          stats:decode_http_bin_str_to_ets(StatsEtsEncStr, EntityStatsEts, overwrite), %TODO Guy
          % TODO increase counter_received_stats ets by 1

          % [From|[NewCounter]] = re:split(binary_to_list(Body), ":", [{return, list}]),

          % NewStatisticsMap = maps:put(From,NewCounter,StatisticsMap),
          % NewState = State#main_genserver_state{msgCounter = MsgCounter+1,statisticsMap = NewStatisticsMap,statisticsCounter = StatisticsCounter-1},

          ReceivedCounterStatsValue = ets:lookup_element(get(main_server_ets), counter_received_stats, ?DATA_IDX),
          TotalNumOfEntities = length(ets:lookup_element(get(main_server_ets), entities_names_list, ?DATA_IDX)), % without MainServer!

          if ReceivedCounterStatsValue == TotalNumOfEntities ->  %% got stats from all entities
              % TODO Guy Here we send all stats to Api Server - We need to define the new scheme and Noa and Ohad should implement it
              % Statistics = maps:to_list(NewStatisticsMap),
              % S = mapToString(Statistics,[]) ,
              % ?LOG_NOTICE("Sending stats: ~p~n",[S]),
              % {RouterHost,RouterPort} = nerl_tools:getShortPath(MyName,?API_SERVER_ATOM,NerlnetGraph),
              % nerl_tools:http_request(RouterHost,RouterPort,"statistics", S ++ "|mainServer:" ++integer_to_list(MsgCounter));
            todo;
          true -> wait_for_more_stats end
      end,
    {noreply, State#main_genserver_state{}};

%% Trigerred when Source finished to cast all of its batches.
%% sourcesCastingList - List of sources that are still casting - this function  removes the source from this list
handle_cast({sourceDone,Body}, State = #main_genserver_state{myName = MyName, sourcesCastingList = SourcesCastingList}) ->
  StatsEts = get_entity_stats_ets(?MAIN_SERVER_ATOM),
  stats:increment_messages_received(StatsEts),
  SourceName = list_to_atom(binary_to_list(Body)),
  UpdatedSourcesCastingList = SourcesCastingList--[SourceName],
  case UpdatedSourcesCastingList of
    [] -> % the list is empty - all sources were done casting their batches
      PhaseAtom = clientIdle,
      update_clients_phase(PhaseAtom, MyName),
      stats:increment_messages_sent(StatsEts),
      NextState = State#main_genserver_state{state = idle, sourcesCastingList = UpdatedSourcesCastingList};
    _ -> NextState = State#main_genserver_state{state = casting, sourcesCastingList = UpdatedSourcesCastingList}
  end,
  {noreply, NextState};

handle_cast({sourceAck,Body}, State = #main_genserver_state{sourcesWaitingList = WaitingList,msgCounter = MsgCounter}) ->
    NewWaitingList = WaitingList--[list_to_atom(binary_to_list(Body))],
    % io:format("waiting for source:~p~n",[NewWaitingList]),
    if length(NewWaitingList) == 0 -> ack();
    true-> ok end,
  {noreply, State#main_genserver_state{sourcesWaitingList = NewWaitingList,msgCounter = MsgCounter+1}};


handle_cast({clientAck,Body}, State = #main_genserver_state{clientsWaitingList = WaitingList,msgCounter = MsgCounter}) ->
  NewWaitingList = WaitingList--[binary_to_term(Body)],
  %io:format("new Waiting List: ~p ~n",[NewWaitingList]),
  if length(NewWaitingList) == 0 -> ack();
  true-> ok end,
  {noreply, State#main_genserver_state{clientsWaitingList = NewWaitingList, msgCounter = MsgCounter+1}};

%%TODO change Client_Names to list of clients
handle_cast({startCasting,SourcesNames}, State = #main_genserver_state{state = idle, sourcesCastingList=CastingList, sourcesWaitingList = [], clientsWaitingList = []}) ->
  StatsEts = get_entity_stats_ets(?MAIN_SERVER_ATOM),
  stats:increment_messages_received(StatsEts),
  
  Splitted = re:split(binary_to_list(SourcesNames), ",", [{return, list}]), % SourceNames holds also num of samples to send at the end of it!
  NumOfSampleToSend = lists:last(Splitted),
  Sources = lists:sublist(Splitted,length(Splitted)-1),
  SourcesAtoms = [list_to_atom(Source_Name) || Source_Name <- Sources],
  
  sources_start_casting(Sources,NumOfSampleToSend), % each source gets a unicast message of start casting action
  stats:increment_messages_sent(StatsEts, length(Sources)),
  {noreply, State#main_genserver_state{ state = casting, sourcesCastingList = CastingList++SourcesAtoms}};


handle_cast({startCasting,_SourceNames}, State = #main_genserver_state{sourcesWaitingList = SourcesWaiting, clientsWaitingList = ClientsWaiting}) ->
  ?LOG_WARNING("There are Sources and Clients that are still waiting! ~p",[{SourcesWaiting, ClientsWaiting}]),
  {noreply, State};


handle_cast({stopCasting, SourceName}, State = #main_genserver_state{state = casting}) ->
    StatsEts = get_entity_stats_ets(?MAIN_SERVER_ATOM),
    stats:increment_messages_received(StatsEts),
  
    {RouterHost,RouterPort} = ets:lookup_element(get(main_server_ets), my_router, ?DATA_IDX),
    % TODO - implement in source this function when SourceStatem is in cast mode
    ?LOG_ERROR("Unsupported yet"),
    %{RouterHost,RouterPort} = maps:get(list_to_atom(binary_to_list(Source_Names)),ConnectionMap),
    nerl_tools:http_router_request(RouterHost, RouterPort, [binary_to_atom(SourceName)], atom_to_list(stopCasting), ""),
    {noreply, State#main_genserver_state{state = idle}};

handle_cast({lossFunction,<<>>}, State = #main_genserver_state{}) ->
  StatsEts = get_entity_stats_ets(?MAIN_SERVER_ATOM),
  stats:increment_bad_messages(StatsEts),
  ?LOG_ERROR("Empty loss function body received"),
  {noreply, State#main_genserver_state{}};

% sending loss to ApiServer when data of loss is received from worker (through client)
handle_cast({lossFunction,Body}, State = #main_genserver_state{myName = MyName}) ->
  StatsEts = get_entity_stats_ets(?MAIN_SERVER_ATOM),
  stats:increment_messages_received(StatsEts),
  try
    {RouterHost,RouterPort} = ets:lookup_element(get(main_server_ets), my_router, ?DATA_IDX), % get main_server's router,
    case   binary_to_term(Body) of
      {WorkerName,{LossFunction,_Time}} -> % average time should be gathered
        nerl_tools:http_router_request(RouterHost, RouterPort, [?API_SERVER_ATOM], atom_to_list(stopCasting), atom_to_list(WorkerName)++"#"++float_to_list(LossFunction)),
        stats:increment_messages_sent(StatsEts);
      {WorkerName,LossFunction} ->
        nerl_tools:http_router_request(RouterHost, RouterPort, [?API_SERVER_ATOM], atom_to_list(stopCasting), atom_to_list(WorkerName)++"#"++float_to_list(LossFunction)),
        stats:increment_messages_sent(StatsEts);
      _ELSE ->
        ?LOG_ERROR("~p Wrong loss function pattern received from client and its worker ~p", [MyName, Body])
    end
  catch Err:E ->
        ?LOG_ERROR("~p Error receiving loss function ~p",[MyName, {Err,E}])
  end,
  {noreply, State#main_genserver_state{}};


handle_cast({predictRes,Body}, State = #main_genserver_state{batchSize = BatchSize}) ->
  StatsEts = get_entity_stats_ets(?MAIN_SERVER_ATOM),
  stats:increment_messages_received(StatsEts),
  try 
      {RouterHost,RouterPort} = ets:lookup_element(get(main_server_ets), my_router, ?DATA_IDX), % get main_server's router,
      {WorkerName, InputName, BatchID, {NerlTensor, Type}} = binary_to_term(Body),   %% TODO: add convention with client
      {DecodedNerlTensor, _Type} =
      if 
        (NerlTensor==<<>>) -> ?LOG_ERROR(?LOG_HEADER++"Got empty tensor"), empty_nerltensor_err;
        true ->  nerlNIF:nerltensor_conversion({NerlTensor, Type}, nerlNIF:erl_type_conversion(Type)) % converting nerltensor from binary to erlang type using NerlNIF
      end,
      ListToSend = [WorkerName, integer_to_list(BatchID), integer_to_list(BatchSize), InputName, nerl_tools:string_format("~p",[DecodedNerlTensor])],
      BodyToSend = tl(lists:flatten(["#"++Item || Item <- ListToSend])),
      nerl_tools:http_router_request(RouterHost, RouterPort, [?API_SERVER_ATOM], atom_to_list(predRes), BodyToSend),
      stats:increment_messages_sent(StatsEts)
  catch Err:E ->  
    ?LOG_ERROR(?LOG_HEADER++"Error receiving predict result ~p~n",[{Err,E}])
  end,
  {noreply, State#main_genserver_state{}};


handle_cast(Request, State = #main_genserver_state{}) ->
  ?LOG_WARNING("main server cast ignored request ~p",[Request]),
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

update_clients_phase(PhaseAtom, MessageBody) when is_atom(PhaseAtom) ->
  ListOfClients = ets:lookup_element(get(main_server_ets), clients_names_list, ?DATA_IDX),
  {RouterHost,RouterPort} = ets:lookup_element(get(main_server_ets), my_router, ?DATA_IDX),
  ActionStr = atom_to_list(PhaseAtom),
  DestinationsList = ListOfClients,
  nerltools:http_router_request(RouterHost, RouterPort, DestinationsList, ActionStr, MessageBody).

% Sends requests for statisics from all entities excludes main server
statistics_requests_to_entities() ->
  ListOfEntities = ets:lookup_element(get(main_server_ets), entities_names_list, ?DATA_IDX),
  {RouterHost,RouterPort} = ets:lookup_element(get(main_server_ets), my_router, ?DATA_IDX),
  ActionStr = atom_to_list(statistics),
  DestinationsList = ListOfEntities,
  MessageBody = "", % there is no need for body in statistics request
  nerltools:http_router_request(RouterHost, RouterPort, DestinationsList, ActionStr, MessageBody).

generate_stats_ets_tables(VerticesList) ->
  MainServerEtsStats = get(etsStats),
  Func = 
    fun({Name, {_Host, _Port, _DeviceName}}) ->
        EntityStatsEts = stats:generate_stats_ets(),
        ets:insert(MainServerEtsStats, {Name, EntityStatsEts})
    end,
  lists:foreach(Func, VerticesList).

get_entity_stats_ets(EntityName) ->
  MainServerEtsStats = get(etsStats),
  ets:lookup_element(MainServerEtsStats, EntityName , ?DATA_IDX).


sources_start_casting([],_NumOfSampleToSend)->done;
sources_start_casting([SourceName|SourceNames],NumOfSamplesToSend) ->
  ?LOG_NOTICE("Sending start casting command to: ~p",[SourceName]),
  ActionStr = atom_to_list(startCasting),
  {RouterHost,RouterPort} = ets:lookup_element(get(main_server_ets), my_router, ?DATA_IDX),
  MessageBody = SourceName++[","]++NumOfSamplesToSend,
  nerltools:http_router_request(RouterHost, RouterPort, [SourceName], ActionStr, MessageBody),
  sources_start_casting(SourceNames, NumOfSamplesToSend).


ack() ->
  {RouterHost,RouterPort} = ets:lookup_element(get(main_server_ets), my_router, ?DATA_IDX),
  % io:format("mainserver sending ACK~n"),
  Body = "ack",
  {ok, Response} = nerl_tools:http_router_request(RouterHost, RouterPort, [?API_SERVER_ATOM], atom_to_list(ackP), Body),

  case Response of 
    {{_Protocol, _Code = 404, _Meaning}, _Headers, _Body} -> 
          ?LOG_WARNING("Main Server retries again ack send"), ack();    %% Ack not received, retry again
    {{_Protocol, _Code = 200, _Meaning}, _Headers, _Body} -> done;
    _Other -> ?LOG_ERROR("Bad response from ApiServer"), bad_response
  end.
  

%% TODO remove - will be replaced by stats.erl
%%
%% encodes stats to string:
%% "Entity1:Stats,...|Entity2:Stats,...|....."
% mapToString([],Ret) -> Ret;
% mapToString([{Name,Data}|StatisticsList],[]) -> mapToString(StatisticsList,Name++":"++Data);
% mapToString([{Name,Data}|StatisticsList],Ret) -> mapToString(StatisticsList,Ret++"|"++Name++":"++Data).