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


-record(main_genserver_state, {myName, state, workersMap, clients, nerlnetGraph, sourcesCastingList = [], sourcesWaitingList = [], clientsWaitingList = [], statisticsMap, total_sources=0, sources_data_ready_ctr = 0}).

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

init({MyName,ClientsNames,BatchSize,WorkersMap,NerlnetGraph , DeviceName}) ->
  nerl_tools:setup_logger(?MODULE),
  inets:start(),
  MyNameStr = atom_to_list(MyName),
  ?MAIN_SERVER_ATOM = MyName, % must be identical
  ?LOG_NOTICE("Main Server starts"),
  ConnectedEntities = [digraph:vertex(NerlnetGraph,Vertex) || Vertex <- digraph:out_neighbours(NerlnetGraph,?MAIN_SERVER_ATOM)],
  ?LOG_NOTICE("Main Server is connected to: ~p~n",[ConnectedEntities]),
  MainServerEts = ets:new(main_server_ets , [set]),
  PhaseResultDataEts = ets:new(phase_res_data_ets, [set]),
  put(main_server_ets, MainServerEts),
  put(phase_res_data_ets, PhaseResultDataEts),
  put(nerlnet_graph, NerlnetGraph),
  put(device_name, DeviceName),
  put(active_phase, none),
  EtsStats = ets:new(stats , [set]),
  put(etsStats, EtsStats), %% All entities including mainServer ets tables statistics
  Entities = [digraph:vertex(NerlnetGraph,Vertex) || Vertex <- digraph:vertices(NerlnetGraph)--[?API_SERVER_ATOM]],
  EntitiesNames = [Name || {Name, _CommTuple} <- Entities],
  generate_stats_ets_tables(EntitiesNames),
  ets:insert(MainServerEts , {entities_names_list , EntitiesNames -- [?MAIN_SERVER_ATOM]}),
  ets:insert(MainServerEts , {batch_size , BatchSize}),
  ets:insert(MainServerEts , {workers_map , WorkersMap}),
  ets:insert(MainServerEts , {clients_names_list , ClientsNames}),
  ets:insert(MainServerEts , {counter_received_stats, 0}),
  ets:insert(MainServerEts , {json_received_counter, 0}),
  % Getting the router that main server is connected with
  {MyRouterHost,MyRouterPort} = nerl_tools:getShortPath(MyName,hd(ClientsNames),get(nerlnet_graph)),
  ets:insert(MainServerEts, {my_router,{MyRouterHost,MyRouterPort}}),
  {ok, #main_genserver_state{myName = MyNameStr , state=idle, total_sources=0, sources_data_ready_ctr = 0}}.


handle_cast({initCSV, _Index, TotalSources, SourceName, WorkersList, Phase, NumOfBatches, NerlTensorType, Data}, State = #main_genserver_state{state = idle, sourcesWaitingList = SourcesWaitingList, total_sources = TotalSourcesOld, sources_data_ready_ctr = SourcesDataReadyCtrOld}) ->
  {RouterHost,RouterPort} = ets:lookup_element(get(main_server_ets), my_router, ?DATA_IDX),
  ActionStr = atom_to_list(updateCSV),
  {TotalSourcesInt, _Rest} = string:to_integer(TotalSources),
  % MessageBody = WorkersList ++ "#" ++ NumOfBatches ++ "#" ++ NerlTensorType ++ "#" ++ Data,
  WorkersListSeperated = string:split(WorkersList, ",", all),
  MessageBody = {WorkersListSeperated, Phase, NumOfBatches, NerlTensorType, zlib:compress(list_to_binary(Data))},
  nerl_tools:http_router_request(RouterHost,RouterPort, [SourceName], ActionStr, MessageBody), % update the source with its data
  UpdatedSourceWaitingList = SourcesWaitingList++[list_to_atom(SourceName)],
  {SourcesDataReadyCtr, NewTotalSources} = 
  if 
    TotalSourcesOld =/= TotalSourcesInt -> {0, TotalSourcesInt};
    true -> {SourcesDataReadyCtrOld, TotalSourcesOld}
  end,
  {noreply, State#main_genserver_state{sourcesWaitingList = UpdatedSourceWaitingList, total_sources = NewTotalSources, sources_data_ready_ctr = SourcesDataReadyCtr}};

% TODO Guy - I think this pattern is redundant - it is not relevant to the main server state
handle_cast({initCSV, _Index, _TotalSources, _SourceName ,_SourceData}, State) ->
  ?LOG_ERROR("initCSV is only applicalble when main server is in idle state!"),
  {noreply, State#main_genserver_state{}};

handle_cast({restart, _Body} , State = #main_genserver_state{}) ->
  ?LOG_NOTICE("*************NERLNET RESTARTING*************"),
  URL = "http://" ++ nerl_tools:getdeviceIP() ++ ":8484/restart",
  {ok , _} = httpc:request(post, {URL, [],"application/x-www-form-urlencoded",[]}, [], []),
  {noreply, State#main_genserver_state{}};

handle_cast({jsonReceived,Body}, State = #main_genserver_state{}) ->
  StatsEts = get_entity_stats_ets(?MAIN_SERVER_ATOM),
  stats:increment_messages_received(StatsEts),
  {DeviceName , TotalNumberOfDevices} = binary_to_term(Body),
  case TotalNumberOfDevices of 
    0 -> ack(atom_to_list(received_jsons_done)); %% if the exeperiment runs on a single device
    _ -> ok
  end,
  MainServerDeviceName = get(device_name),
  case DeviceName of
    MainServerDeviceName -> ?LOG_NOTICE("Device ~p received the json files and is ready to start", [DeviceName]);
    _OtherDevice ->
      ?LOG_NOTICE("Device ~p received the json files and is ready to start", [DeviceName]),
      ets:update_counter(get(main_server_ets), json_received_counter, 1),
      NumOfDevicesReady = ets:lookup_element(get(main_server_ets), json_received_counter, ?DATA_IDX),
      if NumOfDevicesReady == TotalNumberOfDevices -> 
        ack(atom_to_list(received_jsons_done));
        true -> ok
      end
  end,
  stats:increment_messages_sent(StatsEts),
  {noreply, State#main_genserver_state{}};

% Updating mainserver process dict with active phase!
handle_cast({clientsPhaseUpdate , Phase}, State = #main_genserver_state{myName = MyName}) ->
  put(curr_phase_ack , update_phase_done),
  ?LOG_INFO("Received clientsPhaseUpdate message with phase ~p",[binary_to_list(Phase)]),
  StatsEts = get_entity_stats_ets(?MAIN_SERVER_ATOM),
  case binary_to_atom(Phase) of
    training ->   stats:increment_messages_received(StatsEts),put(active_phase, training), 
                  update_clients_phase(clientTraining, MyName);
    prediction -> stats:increment_messages_received(StatsEts),put(active_phase, prediction),
                  update_clients_phase(clientPredict, MyName);
    _Else -> ?LOG_ERROR("Wrong phase was received: ~p",[Phase]), stats:increment_bad_messages(StatsEts)
  end,
  ListOfClients = ets:lookup_element(get(main_server_ets), clients_names_list, ?DATA_IDX),
  {noreply, State#main_genserver_state{clientsWaitingList = ListOfClients}};

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
  ClientTrainingAction = clientTraining, % client! not clients!
  update_clients_phase(ClientTrainingAction, MyName), % update all clients with clientsTraining phase
  stats:increment_messages_sent(StatsEts),
  ListOfClients = ets:lookup_element(get(main_server_ets), clients_names_list, ?DATA_IDX),
  {noreply, State#main_genserver_state{clientsWaitingList = ListOfClients}};

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
  ListOfClients = ets:lookup_element(get(main_server_ets), clients_names_list, ?DATA_IDX),

  {noreply, State#main_genserver_state{clientsWaitingList = ListOfClients}};


handle_cast({statistics,Body}, State = #main_genserver_state{myName = MyName}) ->
    StatsEts = get_entity_stats_ets(?MAIN_SERVER_ATOM),
    stats:increment_messages_received(StatsEts),
    if Body == <<"getStatistics">> ->   %% initial message from APIServer, get stats from entities
        statistics_requests_to_entities(), % Broadcast sends - worth 1 message
        stats:increment_messages_sent(StatsEts);
      Body == <<>> ->  ?LOG_ERROR("~p: Wrong statistics message",[MyName]);

      true ->
          %% statistics arrived from Entity
          {From, StatsEtsEncStr} = binary_to_term(Body),
          set_entity_stats_ets_str(From, StatsEtsEncStr),

          % increase counter_received_stats ets by 1
          ets:update_counter(get(main_server_ets), counter_received_stats, 1),
          stats:increment_messages_received(StatsEts),

          ReceivedCounterStatsValue = ets:lookup_element(get(main_server_ets), counter_received_stats, ?DATA_IDX),
          EntitiesNamesList = ets:lookup_element(get(main_server_ets), entities_names_list, ?DATA_IDX),
          TotalNumOfEntities = length(EntitiesNamesList), % without MainServer!
          if ReceivedCounterStatsValue == TotalNumOfEntities ->  %% got stats from all entities  
            ets:update_element(get(main_server_ets), counter_received_stats, {?STATS_KEYVAL_VAL_IDX, 0}),
            Func = fun(Entity) ->
              EntityStatsEncStr = get_entity_stats_ets_str(Entity),
              atom_to_list(Entity) ++ ?API_SERVER_WITHIN_ENTITY_SEPERATOR ++ EntityStatsEncStr ++ ?API_SERVER_ENTITY_SEPERATOR
            end,
            MainServerEncStatsEts = stats:encode_ets_to_http_bin_str(get_entity_stats_ets_str(?MAIN_SERVER_ATOM)),
            MainServerStr = atom_to_list(?MAIN_SERVER_ATOM) ++ ?API_SERVER_WITHIN_ENTITY_SEPERATOR ++ MainServerEncStatsEts ++ ?API_SERVER_ENTITY_SEPERATOR,
            StatsToSend = lists:flatten([Func(Entity) || Entity <- EntitiesNamesList] ++ MainServerStr), % add main server to the list
            {RouterHost,RouterPort} = ets:lookup_element(get(main_server_ets), my_router, ?DATA_IDX),
            ActionStr = atom_to_list(statistics),
            nerl_tools:http_router_request(RouterHost,RouterPort, [?API_SERVER_ATOM], ActionStr, list_to_binary(StatsToSend)); % update the source with its data
            
          true -> wait_for_more_stats 
        end
      end,
    {noreply, State#main_genserver_state{}};

%% Trigerred when Source finished to cast all of its batches.
%% sourcesCastingList - List of sources that are still casting - this function  removes the source from this list
handle_cast({sourceDone,Body}, State = #main_genserver_state{myName = MyName, sourcesCastingList = SourcesCastingList}) ->
  StatsEts = get_entity_stats_ets(?MAIN_SERVER_ATOM),
  stats:increment_messages_received(StatsEts),
  SourceName = binary_to_term(Body),
  UpdatedSourcesCastingList = SourcesCastingList--[SourceName],

  case UpdatedSourcesCastingList of
    [] -> % the list is empty - all sources were done casting their batches
      ?LOG_NOTICE("[Main-Server] All sources finished casting"),
      PhaseAtom = clientIdle,
      update_clients_phase(PhaseAtom, MyName),
      ListOfClients = ets:lookup_element(get(main_server_ets), clients_names_list, ?DATA_IDX),
      stats:increment_messages_sent(StatsEts),
      NextState = State#main_genserver_state{state = idle, sourcesCastingList = UpdatedSourcesCastingList, clientsWaitingList = ListOfClients, total_sources = 0};
    _ -> NextState = State#main_genserver_state{state = casting, sourcesCastingList = UpdatedSourcesCastingList}
  end,
  {noreply, NextState};

% Each update CSV process generates sourceAck that is sent to main server when all sources are ready
handle_cast({sourceAckDataReady,Body}, State = #main_genserver_state{sourcesWaitingList = WaitingList, total_sources = TotalSources, sources_data_ready_ctr = SourcesDataReadyCtr}) ->
    StatsEts = get_entity_stats_ets(?MAIN_SERVER_ATOM),
    stats:increment_messages_received(StatsEts),
    SourceName = binary_to_term(Body),
    NewWaitingList = WaitingList--[SourceName],
    SourcesDataReadyCtrNew = SourcesDataReadyCtr + 1, % The total sources comes from the APIServer - The ApiServer sends each source data in different streams, therefore, 
                                                      % we can't count on the waiting list since it can be empty during the process
    if
    SourcesDataReadyCtrNew =:= TotalSources -> ack(atom_to_list(update_csv_done)), {noreply, State#main_genserver_state{sourcesWaitingList = NewWaitingList, sources_data_ready_ctr = 0}};
    true-> {noreply, State#main_genserver_state{sourcesWaitingList = NewWaitingList, sources_data_ready_ctr = SourcesDataReadyCtrNew}}
    end;


handle_cast({clientAck,Body}, State = #main_genserver_state{clientsWaitingList = WaitingList}) ->
  StatsEts = get_entity_stats_ets(?MAIN_SERVER_ATOM),
  stats:increment_messages_received(StatsEts),
  ClientName = binary_to_term(Body),
  NewWaitingList = WaitingList--[ClientName], % waitingList is initialized in clientsTraining or clientsPredict handl cast calls
  if length(NewWaitingList) == 0 ->
            PhaseResultsDataMap = generate_phase_result_data_map(),
            NothingToSend = string:is_empty(PhaseResultsDataMap),
            if 
              NothingToSend -> pass;
              true ->  Action = case get(active_phase) of
                                        training -> trainRes;
                                        prediction -> predRes
                                    end,
                            {RouterHost,RouterPort} = ets:lookup_element(get(main_server_ets), my_router, ?DATA_IDX), % get main_server's router
                            nerl_tools:http_router_request(RouterHost, RouterPort, [?API_SERVER_ATOM], atom_to_list(Action), {json, PhaseResultsDataMap}),
                            stats:increment_messages_sent(StatsEts),
                            clean_phase_result_data_to_send_ets() % getting ready for next phase after data was sent to APIServer
            end,
            ack(atom_to_list(get(curr_phase_ack)));
    true-> ok 
  end,
  {noreply, State#main_genserver_state{clientsWaitingList = NewWaitingList}};

%%TODO change Client_Names to list of clients
handle_cast({startCasting,SourcesNames}, State = #main_genserver_state{state = idle, sourcesCastingList=CastingList, sourcesWaitingList = [], clientsWaitingList = []}) ->
  put(curr_phase_ack , start_casting_done),
  StatsEts = get_entity_stats_ets(?MAIN_SERVER_ATOM),
  stats:increment_messages_received(StatsEts),
  SourcesList = re:split(binary_to_list(SourcesNames), "," , [{return, list}]), 
  %% NumOfSampleToSend = lists:last(Splitted),
  %% Sources = lists:sublist(Splitted,length(Splitted)-1),
  SourcesAtoms = [list_to_atom(Source_Name) || Source_Name <- SourcesList],
  
  sources_start_casting(SourcesList), % each source gets a unicast message of start casting action
  stats:increment_messages_sent(StatsEts, length(SourcesList)),
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
% trainRes
handle_cast({lossFunction,Body}, State = #main_genserver_state{myName = MyName}) ->
  StatsEts = get_entity_stats_ets(?MAIN_SERVER_ATOM),
  stats:increment_messages_received(StatsEts),
  try
    case binary_to_term(Body) of
        {WorkerName , SourceName , {LossNerlTensor , LossNerlTensorType} , TimeNIF , WorkerToken, BatchID , BatchTS} ->
        Key = atom_to_list(WorkerName) ++ ?PHASE_RES_VALUES_IN_KEY_SEPARATOR ++ atom_to_list(SourceName) ++ 
              ?PHASE_RES_VALUES_IN_KEY_SEPARATOR ++ integer_to_list(BatchID) ++ ?PHASE_RES_VALUES_IN_KEY_SEPARATOR ++ 
              integer_to_list(BatchTS) ++ ?PHASE_RES_VALUES_IN_KEY_SEPARATOR ++ float_to_list(TimeNIF) ++
              ?PHASE_RES_VALUES_IN_KEY_SEPARATOR ++ WorkerToken ++ ?PHASE_RES_VALUES_IN_KEY_SEPARATOR ++
              atom_to_list(LossNerlTensorType),
              % data is encoded in key with separators as follows:
              % WorkerName + SourceName + BatchID + BatchTS + TimeNIF + WorkerToken + LossNerlTensorType
        store_phase_result_data_to_send_ets(Key, binary_to_list(LossNerlTensor));
      _ELSE ->
        ?LOG_ERROR("~p Wrong loss function pattern received from client and its worker ~p", [MyName, Body])
    end
  catch Err:E ->
        ?LOG_ERROR("~p Error receiving loss function ~p",[MyName, {Err,E}])
  end,
  {noreply, State#main_genserver_state{}};


% predictRes
handle_cast({predictRes,Body}, State) ->
  StatsEts = get_entity_stats_ets(?MAIN_SERVER_ATOM),
  _BatchSize = ets:lookup_element(get(main_server_ets), batch_size, ?DATA_IDX),
  stats:increment_messages_received(StatsEts),
  try 
      {WorkerName, SourceName, {NerlTensor, NerlTensorType}, TimeNIF , WorkerToken, BatchID, BatchTS} = binary_to_term(Body),
      Key = atom_to_list(WorkerName) ++ ?PHASE_RES_VALUES_IN_KEY_SEPARATOR ++ atom_to_list(SourceName) ++ 
            ?PHASE_RES_VALUES_IN_KEY_SEPARATOR ++ integer_to_list(BatchID) ++ ?PHASE_RES_VALUES_IN_KEY_SEPARATOR ++ 
            integer_to_list(BatchTS) ++ ?PHASE_RES_VALUES_IN_KEY_SEPARATOR ++ float_to_list(TimeNIF) ++ 
            ?PHASE_RES_VALUES_IN_KEY_SEPARATOR ++ WorkerToken ++ ?PHASE_RES_VALUES_IN_KEY_SEPARATOR ++
            atom_to_list(NerlTensorType),
            % data is encoded in key with separators as follows:
            % WorkerName + SourceName + BatchID + BatchTS + TimeNIF + WorkerToken + NerlTensorType
      store_phase_result_data_to_send_ets(Key, binary_to_list(NerlTensor))
  catch Err:E ->  
    ?LOG_ERROR(?LOG_HEADER++"Error receiving predict result ~p",[{Err,E}])
  end,
  {noreply, State};


handle_cast(Request, State = #main_genserver_state{}) ->
  ?LOG_ERROR("main server cast ignored request ~p",[Request]),
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
  nerl_tools:http_router_request(RouterHost, RouterPort, DestinationsList, ActionStr, MessageBody).

% Sends requests for statisics from all entities excludes main server
statistics_requests_to_entities() ->
  ListOfEntities = ets:lookup_element(get(main_server_ets), entities_names_list, ?DATA_IDX),
  % remove mainServer from the list
  %DestinationsList = lists:map(fun({Entity , CommTuple}) when Entity =/= ?MAIN_SERVER_ATOM -> {Entity, CommTuple} end, ListOfEntities),
  {RouterHost,RouterPort} = ets:lookup_element(get(main_server_ets), my_router, ?DATA_IDX),
  ActionStr = atom_to_list(statistics),
  MessageBody = "", % there is no need for body in statistics request
  nerl_tools:http_router_request(RouterHost, RouterPort, ListOfEntities, ActionStr, MessageBody).

generate_stats_ets_tables(EntitiesNamesList) ->
  MainServerEtsStats = get(etsStats),
  Func = 
    fun(Name) ->
        EntityStatsEts = stats:generate_stats_ets(),
        ets:insert(MainServerEtsStats, {Name, EntityStatsEts})
    end,
  lists:foreach(Func, EntitiesNamesList).

get_entity_stats_ets(EntityName) ->
  MainServerEtsStats = get(etsStats),
  ets:lookup_element(MainServerEtsStats, EntityName , ?DATA_IDX).

get_entity_stats_ets_str(EntityName) ->
  MainServerEtsStats = get(etsStats),
  ets:lookup_element(MainServerEtsStats, EntityName , ?DATA_IDX).

set_entity_stats_ets_str(EntityName , StatsEncStr) ->
  MainServerEtsStats = get(etsStats),
  ets:insert(MainServerEtsStats, {EntityName, StatsEncStr}).


sources_start_casting([])->done;
sources_start_casting([CurrSource|RemainingSources]) ->
  ?LOG_NOTICE("Sending start casting command to: ~p",[CurrSource]),
  ActionStr = atom_to_list(startCasting),
  {RouterHost,RouterPort} = ets:lookup_element(get(main_server_ets), my_router, ?DATA_IDX),
  nerl_tools:http_router_request(RouterHost, RouterPort, [CurrSource], ActionStr, []),
  sources_start_casting(RemainingSources).


ack(MsgStr) ->
  {RouterHost,RouterPort} = ets:lookup_element(get(main_server_ets), my_router, ?DATA_IDX),
  Body = MsgStr,
  HttpRouterRequestFunc = fun() -> 
        nerl_tools:http_router_request(RouterHost, RouterPort, [?API_SERVER_ATOM], ?API_SERVER_ACTION_ACK, Body)
      end,
  {ok, Response} = retransmission_to_apiserver(HttpRouterRequestFunc, ?VALIDATION_OF_TRANSMISSION_WITH_API_SERVER_NUMOF_TRIALS),
  StatsEts = get_entity_stats_ets(?MAIN_SERVER_ATOM),
  stats:increment_messages_sent(StatsEts),
  case Response of 
    {{_Protocol, _Code = 404, _Meaning}, _Headers, _Body} -> 
          ?LOG_WARNING("Main Server retries again ack send"), ack(MsgStr);    %% Ack not received, retry again
    {{_Protocol, _Code = 200, _Meaning}, _Headers, _Body} -> done;
    _Other -> ?LOG_ERROR("Bad response from ApiServer"), bad_response
  end.

% ack validation
retransmission_to_apiserver(HttpRouterRequestFunc, 0) -> HttpRouterRequestFunc();
retransmission_to_apiserver(HttpRouterRequestFunc, Trials) ->
  receive
    {apiserver_ack_validation, _Body} -> ok
  after ?VALIDATION_OF_TRANSMISSION_WITH_API_SERVER_INTERVAL_MS ->  HttpRouterRequestFunc(), retransmission_to_apiserver(HttpRouterRequestFunc, Trials - 1) % TODO fix magic number
  end.


store_phase_result_data_to_send_ets(Key, NerlTensorData) ->
  KeyBin = list_to_binary(Key),
  ets:insert(get(phase_res_data_ets),{KeyBin, NerlTensorData}).


generate_phase_result_data_map() ->
  ListOfData = ets:tab2list(get(phase_res_data_ets)),
  ListOfData.

clean_phase_result_data_to_send_ets() ->
  ets:delete_all_objects(get(phase_res_data_ets)).