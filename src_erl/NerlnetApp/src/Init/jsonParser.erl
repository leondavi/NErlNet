%%%-------------------------------------------------------------------
%%% @authors Haran Cohen, David Leon, Tal Kapelnik
%%% @copyright (C) 2021, Nerlnet
%%% @doc
%%%
%%% @end
%%% Created : 14. May 2021 4:48 AM
%%%-------------------------------------------------------------------
-module(jsonParser).
-include("../nerl_tools.hrl").
-include("../dc_definitions_ag.hrl").
-export([parseJsons/3, json_to_ets/2]).

-define(SINGLE_STRONG_COMPONENT,2). %% apiServer alone + all network 
-define(ETS_DATA_IDX, 2).
-define(PORT_IDX, 1). % port is always at the first index of any entity that has a port!
-define(NERLNET_DATA_ETS_LOG_DST, "/usr/local/lib/nerlnet-lib/log/nerlnet_data_ets.log").

is_special_entity(EntityName) ->  lists:member(EntityName, ?LIST_OF_SPECIAL_SERVERS).

get_special_entities(DCMap, HostEntities)->
  SpecialEntities = [ Entity || Entity <- HostEntities, is_special_entity(Entity)],
  Func = fun(SpecialEntityName) ->
    EntityMap = maps:get(atom_to_binary(SpecialEntityName), DCMap),
    Port = list_to_integer(binary_to_list(maps:get(?DC_PORT_FIELD_STR_BIN, EntityMap))),
    Args = binary_to_list(maps:get(?DC_ARGS_FIELD_STR_BIN, EntityMap)),
    {SpecialEntityName, {Port, Args}}
  end,
  [Func(E) || E <- SpecialEntities].

get_clients_map([],ClientsMap) -> ClientsMap;
get_clients_map([Client|Clients],ClientsMap)->
  ClientName = binary_to_atom(maps:get(?DC_NAME_FIELD_STR_BIN,Client)),
  Workers = [ list_to_atom(WorkerStr) || WorkerStr <- re:split(binary_to_list(maps:get(?DC_WORKERS_FIELD_STR_BIN,Client)),",",[{return,list}])],    %% TODO: implement as function
  Port =  list_to_integer(binary_to_list(maps:get(?DC_PORT_FIELD_STR_BIN,Client))),
  NewMap = maps:put(ClientName, {Workers, Port}, ClientsMap),
  get_clients_map(Clients,NewMap).

get_devices(DCMap) -> 
  Devices = maps:get(?DC_KEY_DEVICES_STR_BIN,DCMap),
  DeviceNameToIPv4EntitiesFunc = fun(DeviceMap) ->
    IPv4 = maps:get(?DC_IPV4_FIELD_STR_BIN,DeviceMap),
    DeviceName = maps:get(?DC_NAME_FIELD_STR_BIN,DeviceMap), 
    Entities = [ list_to_atom(EntityStr) || EntityStr <- re:split(binary_to_list(maps:get(?DC_ENTITIES_FIELD_STR_BIN,DeviceMap)),",",[{return,list}])],
    {DeviceName , {IPv4, Entities}}
  end,
  DeviceNameToIPv4EntitiesList = [DeviceNameToIPv4EntitiesFunc(D) || D <- Devices],
  IPv4ToDeviceNameMap = maps:from_list(lists:map(fun({DeviceName, {IPv4, _Entities}}) -> {IPv4, DeviceName} end, DeviceNameToIPv4EntitiesList)),
  DeviceNameToIPv4EntitiesMap = maps:from_list(DeviceNameToIPv4EntitiesList),
  {IPv4ToDeviceNameMap, DeviceNameToIPv4EntitiesMap}.
  



get_host_clients(DCMap, HostEntities) -> get_host_clients(DCMap, HostEntities, false).
get_host_clients(DCMap, HostEntities, PrintLog) ->
  AllClients = [ ClientMap || ClientMap <- maps:get(<<"clients">>,DCMap) ],
  HostClients = [ ClientMap || ClientMap <- AllClients, lists:member(binary_to_atom(maps:get(<<"name">>, ClientMap)), HostEntities) ],
  Func = fun(ClientMap) -> 
    Name = binary_to_atom(maps:get(<<"name">>,ClientMap)),
    Port = list_to_integer(binary_to_list(maps:get(<<"port">>, ClientMap))),
    ClientWorkers = [ list_to_atom(WorkerStr) || WorkerStr <- re:split(binary_to_list(maps:get(<<"workers">>,ClientMap)),",",[{return,list}])],
    WorkersMaps = maps:get(<<"workers">>, DCMap),    %% workers arguments from level 1 of arch json
    ClientWorkersMaps = [ WorkerMap || WorkerMap <- WorkersMaps, lists:member(binary_to_atom(maps:get(<<"name">>, WorkerMap)), ClientWorkers) ],
    if PrintLog ->
      ?LOG_NOTICE("Host Client Name: ~p Port: ~p Client Workers ~p",[Name,Port,ClientWorkers]);
      true -> skip
    end,
    {Name,{Port,ClientWorkers,ClientWorkersMaps,get_workers_map(AllClients, #{})}}
  end,
  [Func(S) || S <- HostClients]. % list of tuples: [Name,{Port,WorkersMap}]

generate_workers_map([],WorkersMap,_ClientName)->WorkersMap;
generate_workers_map([Worker|Workers],WorkersMap,ClientName)->
  generate_workers_map(Workers,maps:put(Worker, ClientName,WorkersMap),ClientName). 

% TODO Map of workerName -> SHA
generate_workername_to_sha_map() -> [].

%%returns a map for all workers  - key workerName, Value ClientName
get_workers_map([],WorkersMap)->WorkersMap;
get_workers_map([ClientMap|Clients],WorkersMap)->
  ClientName = list_to_atom(binary_to_list(maps:get(<<"name">>,ClientMap))),
  Workers = [ list_to_atom(WorkerStr) || WorkerStr <- re:split(binary_to_list(maps:get(<<"workers">>,ClientMap)),",",[{return,list}])],    %% TODO: implement as function
  NewMap = generate_workers_map(Workers,WorkersMap,ClientName),
  get_workers_map(Clients,NewMap).

get_host_sources(DCMap, HostEntities) ->
  HostSources = [ SourceMap || SourceMap <- maps:get(<<"sources">>,DCMap), lists:member(binary_to_atom(maps:get(<<"name">>, SourceMap)), HostEntities) ],
  Func = fun(SourceMap) -> 
    SourceName = binary_to_atom(maps:get(<<"name">>,SourceMap)),
    SourcePort = list_to_integer(binary_to_list(maps:get(<<"port">>, SourceMap))),
    SourceMethod = list_to_integer(binary_to_list(maps:get(<<"method">>, SourceMap))),
    IsCustomFreq = maps:is_key(<<"frequency">>, SourceMap),
    CustomFreq =
    if IsCustomFreq -> list_to_integer(binary_to_list(maps:get(<<"frequency">>, SourceMap)));
       true -> none
    end,
    {SourceName,{SourcePort,SourceMethod, CustomFreq}}
  end,
  [Func(S) || S <- HostSources]. % list of tuples: [{SourceName,SourcePort,SourceMethod}]

get_host_routers(DCMap, HostEntities) ->
  HostRouters = [ RouterMap || RouterMap <- maps:get(<<"routers">>,DCMap), lists:member(binary_to_atom(maps:get(<<"name">>, RouterMap)), HostEntities) ],
  Func = fun(RouterMap) -> 
    RouterName = binary_to_atom(maps:get(<<"name">>,RouterMap)),
    RouterPort = list_to_integer(binary_to_list(maps:get(<<"port">>, RouterMap))),
    RouterRouting = "", % TODO
    RouterFiltering = "", % TODO
    %RouterRouting = list_to_integer(binary_to_list(maps:get(<<"routing">>, RouterMap))),
    %RouterFiltering = list_to_integer(binary_to_list(maps:get(<<"filtering">>, RouterMap))),
    {RouterName,{RouterPort,RouterRouting,RouterFiltering}}
  end,
  [Func(R) || R <- HostRouters]. % list of tuples: [{RouterName,{RouterPort,RouterRouting,RouterFiltering}}]

%% ------------------- nerlnet data ets creation -----------------
%% Stores json files data into ets table called nerlnet_data
%% return the ets name
%% --------------------------------------------------------------
json_to_ets(HostName, JsonDCMap) ->

  % update hostname
  ets:insert(nerlnet_data, {hostname, HostName}),
  ets:insert(nerlnet_data, {hostname_bin, list_to_binary(HostName)}),
  ?LOG_NOTICE("Host IP=~p~n",[HostName]),

  % Get NerlNetSettings, batch size, frequency etc..
  NerlNetSettings = maps:get(?DC_KEY_NERLNET_SETTINGS_STR_BIN,JsonDCMap),
  BatchSize = list_to_integer(binary_to_list(maps:get(?DC_KEY_BATCH_SIZE_STR_BIN,NerlNetSettings))),
  Frequency = list_to_integer(binary_to_list(maps:get(?DC_KEY_FREQUENCY_STR_BIN,NerlNetSettings))),
  
  ets:insert(nerlnet_data, {?DC_KEY_FREQUENCY_ATOM,Frequency}),
  ets:insert(nerlnet_data, {?DC_KEY_BATCH_SIZE_ATOM,BatchSize}),

  JsonClients = maps:get(?DC_KEY_CLIENTS_STR_BIN,JsonDCMap),
  MapOfClients = get_clients_map(JsonClients, #{}), % each client has {WorkersList, Port}
  ets:insert(nerlnet_data, {?DC_KEY_CLIENTS_ATOM, MapOfClients}),

  %%  get workers to clients map
  MapOfWorkers = get_workers_map(JsonClients, #{}),
  ets:insert(nerlnet_data, {?DC_KEY_WORKERS_ATOM, MapOfWorkers}),

  {IPv4ToDeviceNameMap, DeviceNameToIPv4EntitiesMap} = get_devices(JsonDCMap), % get all hosts 
  ets:insert(nerlnet_data, {ipv4_to_devices,IPv4ToDeviceNameMap}),
  ets:insert(nerlnet_data, {devices,DeviceNameToIPv4EntitiesMap}),
  %%  retrive this device entities
  HostEntities = maps:get(list_to_binary(HostName), ets:lookup_element(nerlnet_data, hosts, ?ETS_DATA_IDX)), % List of host entities
  ets:insert(nerlnet_data, {hostEntities, HostEntities}),

  HostSpecialEntities = get_special_entities(JsonDCMap, HostEntities),
  SpecialEntityAttributeFunc = fun(SpecialEntity) -> ets:insert(nerlnet_data,SpecialEntity) end,
  lists:foreach(SpecialEntityAttributeFunc, HostSpecialEntities),
  ?LOG_NOTICE("Host special entities: ~p", [HostSpecialEntities]),
  %%  retrive THIS device Clients And Workers
  ?LOG_NOTICE("Adding Host Entities:"),
  ets:insert(nerlnet_data, {hostClients, get_host_clients(JsonDCMap, HostEntities, true)}),

  %%  retrive this device of sources, [{SourceName, {Port, Method}}]
  Sources = get_host_sources(JsonDCMap, HostEntities),
  ets:insert(nerlnet_data, {sources, maps:from_list(Sources)}), % Stores list of sources in ets as {SourceName, {Port, Method}}

  %%  retrive THIS device Routers, returns a list of tuples:[{RoutersArgumentsMap, ConnectionMap},..]
  Routers = get_host_routers(JsonDCMap, HostEntities),
  ets:insert(nerlnet_data, {routers, maps:from_list(Routers)}). % Stores list of Routers in ets as {RouterName, {Port,Routing,Filtering}}


%% all data of host is stored to an ets named table: nerlnet_data
parseJsons(DCMap,CommunicationMap, ThisDeviceIP)->
  nerl_tools:setup_logger(?MODULE),
  % create nerlnet_data ets
  ets:new(nerlnet_data,[named_table, set]),
  
  % ets name is nerlnet_data
  json_to_ets(ThisDeviceIP, DCMap),
  
  % use the nerlnet_data ets from this point
  %%This function returns a graph G, represents all the connections in nerlnet. each entitie should have a copy of it.
  NerlnetGraph = digraph:new(),
  ets:insert(nerlnet_data, {communicationGraph, NerlnetGraph}),

  NerlnetGraph = buildCommunicationGraph(DCMap, CommunicationMap),

  % Sources = ets:lookup_element(nerlnet_data, sources, ?ETS_DATA_IDX),
  % VerticesNames = digraph:vertices(NerlnetGraph),
  % SourcesInGraph = [S || S <- maps:keys(Sources), lists:member(S, VerticesNames)],

  % AllCreated = length(maps:keys(Sources)) == length(SourcesInGraph),
  % if AllCreated -> cont;
  % true -> throw("not all sources connected!!") end,

  % add graph to ets
  % save log of data extracted from json
  ets:tab2file(nerlnet_data, ?NERLNET_DATA_ETS_LOG_DST). % TODO consider adding a timestamp

%%---------------------------- Graph part ---------------------------------%%

%% This graph represents the connections withing NerlNet. edge=connection between two entities in the net, vertices = etities.
%The vaule of each of the vertices contains the tuple {Host,Port} with the host and port of the cowboy server connected to the entitie.
%TODO extract graph properties and info (longest path, vertices etc.)
buildCommunicationGraph(ArchitectureMap, CommunicationMap)->
  %Start building the graph one device at a time, than connect all routers with communicationMap json.
  HostsMap = ets:lookup_element(nerlnet_data, hosts, ?ETS_DATA_IDX), % Map of all hosts with their entities in ETS_DATA_IDX
  NerlnetGraph = ets:lookup_element(nerlnet_data, communicationGraph, ?ETS_DATA_IDX),
  % adding vertices to graph
  Func = fun(HostName, HostEntities) -> 
      add_host_vertices(NerlnetGraph, ArchitectureMap, HostName, HostEntities) end, % add all entities vertices include mainServer and serverAPI
  maps:foreach(Func , HostsMap),

  connectRouters(NerlnetGraph,ArchitectureMap,CommunicationMap),

  %%connect serverAPI to Main Server
  add_edges(NerlnetGraph,?API_SERVER_ATOM, ?MAIN_SERVER_ATOM),
  
  _ConnectedEntities = [digraph:vertex(NerlnetGraph,Vertex) || Vertex <- digraph:out_neighbours(NerlnetGraph,?MAIN_SERVER_ATOM)],
  %% TODO check if NerlGUI appears in list of vertices , if it appears a connection edge of NerlGUI to the main server here
  NerlnetGraph.


add_host_vertices(NerlnetGraph, ArchitectureMap, HostName, HostEntities)->
  HostNameStr = binary_to_list(HostName),
  
  HostRouters = get_host_routers(ArchitectureMap, HostEntities),
  HostSources = get_host_sources(ArchitectureMap, HostEntities),
  HostClients = get_host_clients(ArchitectureMap, HostEntities),
  HostSpecials = get_special_entities(ArchitectureMap, HostEntities),

  HostAllEntitiesMap = maps:from_list(HostRouters ++ HostSources ++ HostClients ++ HostSpecials),
  
  ?LOG_NOTICE("Adding host ~p vertices to graph",[HostNameStr]),

  AddEntityToGraph = fun(EntityName, EntityData) -> 
      EntityPort = element(?PORT_IDX, EntityData),
      ?LOG_NOTICE("Entity: ~p Port: ~p ~n",[EntityName,EntityPort]),
        case EntityName of 
          ?API_SERVER_ATOM ->   digraph:add_vertex(NerlnetGraph,EntityName, {element(?DATA_IDX, EntityData), EntityPort});
          _Else ->              digraph:add_vertex(NerlnetGraph,EntityName, {HostName, EntityPort})   %% TODO: atom_to_binary(EntityName)
        end
      end,

  maps:foreach(AddEntityToGraph , HostAllEntitiesMap),
  NerlnetGraph.



%%connects all the routers in the network by the json configuration received in CommunicationMapAdderess
connectRouters(Graph,_ArchitectureMap,CommunicationMap) -> 

    ConnectionsMapList = maps:to_list(maps:get(<<"connectionsMap">>,CommunicationMap)),
    TranslateJsonNamesToAtoms = fun({NameBin, EntitiesBin}) -> 
      Name = binary_to_atom(NameBin),
      [ {Name, binary_to_atom(EntityBin)} || EntityBin <- EntitiesBin]
    end,
    RoutersEdges = lists:flatten(lists:map(TranslateJsonNamesToAtoms, ConnectionsMapList)),
    ?LOG_INFO("RoutersEdges:~p~n",[RoutersEdges]),
    AddEdgesFunc = fun({Router,Entity}) -> add_edges(Graph,Router,Entity)  end,
    lists:foreach(AddEdgesFunc, RoutersEdges),
    StrongComponents = digraph_utils:strong_components(Graph),
    ?LOG_INFO("Graph update - strong components: ~p",[StrongComponents]),
    case length(StrongComponents) of
      ?SINGLE_STRONG_COMPONENT -> cont;
      _ -> throw("not all entities connected! Change connection map!!")
    end.

% returns true if edge appears in graph or false if not
edge_in_graph(Graph, Vertex1, Vertex2) when is_atom(Vertex1) and is_atom(Vertex2) ->  
  EdgesTmp = [ digraph:edge(Graph, Edge) || Edge <- digraph:edges(Graph, Vertex1)],
  Edges = lists:map(fun({_, V1, V2, _}) -> {V1, V2} end, EdgesTmp),
  lists:member({Vertex1, Vertex2},Edges).


add_edges(Graph, Vertex1, Vertex2) ->
  EdgeA = edge_in_graph(Graph,Vertex1,Vertex2),
  EdgeB = edge_in_graph(Graph,Vertex2,Vertex1),
  if 
    EdgeA -> skip; % appears in graph then don't add 
    true -> digraph:add_edge(Graph,Vertex1,Vertex2)
  end,
  if 
    EdgeB -> skip; % appears in graph then don't add 
    true -> digraph:add_edge(Graph,Vertex2,Vertex1)
  end.