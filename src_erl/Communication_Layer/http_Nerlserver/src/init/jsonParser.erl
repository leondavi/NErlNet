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
-export([getHostEntities/3, json_to_ets/2]).

-define(SERVER_API_STR, "serverAPI").
-define(MAIN_SERVER_STR, "mainServer").
-define(NERL_GUI_STR, "nerlGUI").
-define(ETS_DATA_IDX, 2).
-define(PORT_IDX, 1). % port is always at the first index of any entity that has a port!

is_special_entity(EntityName) ->  lists:member(EntityName, [?MAIN_SERVER_STR, ?SERVER_API_STR, ?NERL_GUI_STR]).

get_special_entities(ArchMap, HostEntities)->
  SpecialEntities = [ Entity || Entity <- HostEntities, is_special_entity(Entity)],
  Func = fun(SpecialEntityName) ->
    EntityMap = maps:get(list_to_binary(SpecialEntityName), ArchMap),
    Port = list_to_integer(binary_to_list(maps:get(<<"port">>, EntityMap))),
    Args = binary_to_list(maps:get(<<"args">>, EntityMap)),
    {SpecialEntityName, {Port, Args}}
  end,
  [Func(E) || E <- SpecialEntities].

get_clients_map([],ClientsMap) -> ClientsMap;
get_clients_map([Client|Clients],ClientsMap)->
  ClientName = list_to_atom(binary_to_list(maps:get(<<"name">>,Client))),
  Workers = re:split(binary_to_list(maps:get(<<"workers">>,Client)),",",[{return,list}]),
  Port =  list_to_integer(binary_to_list(maps:get(<<"port">>, Client))),
  NewMap = maps:put(ClientName, {Workers, Port}, ClientsMap),
  get_clients_map(Clients,NewMap).

get_devices(ArchMap) -> 
  Devices = maps:get(<<"devices">>,ArchMap),
  Func = fun(DeviceMap) ->
    HostName = maps:get(<<"host">>,DeviceMap),
    EntitiesBin = maps:get(<<"entities">>,DeviceMap),
    Entities = re:split(binary_to_list(EntitiesBin),",",[{return,list}]),
    {HostName, Entities}
  end,
  [Func(D) || D <- Devices].


get_host_clients(ArchMap, HostEntities) ->
  Clients = maps:get(<<"clients">>,ArchMap),
  HostClients = [C || C <- Clients, lists:member(C,HostEntities) ],
  Func = fun(ClientMap) -> 
    Name = maps:get(<<"name">>,ClientMap),
    Port = list_to_integer(binary_to_list(maps:get(<<"port">>, ClientMap))),
    Workers = re:split(binary_to_list(maps:get(<<"workers">>,ClientMap)),",",[{return,list}]),
    WorkersMap = generate_workers_map(Workers,#{},Name),
    {Name,{Port,WorkersMap}}
  end,
  [Func(S) || S <- HostClients]. % list of tuples: [Name,{Port,WorkersMap}]

generate_workers_map([],WorkersMap,_ClientName)->WorkersMap;
generate_workers_map([Worker|Workers],WorkersMap,ClientName)->
  generate_workers_map(Workers,maps:put(list_to_atom(Worker),ClientName,WorkersMap),ClientName).
  

%%returns a map of all workers  - key workerName, Value ClientName
get_workers_map([],WorkersMap)->WorkersMap;
get_workers_map([Client|Clients],WorkersMap)->
  ClientName = list_to_atom(binary_to_list(maps:get(<<"name">>,Client))),
  Workers = re:split(binary_to_list(maps:get(<<"workers">>,Client)),",",[{return,list}]),
  NewMap = generate_workers_map(Workers,WorkersMap,ClientName),
  get_workers_map(Clients,NewMap).

get_host_sources(ArchMap, HostEntities) ->
  Sources = maps:get(<<"sources">>, ArchMap),
  HostSources = [S || S <- Sources, lists:member(S,HostEntities) ],
  Func = fun(SourceMap) -> 
    SourceName = maps:get(<<"name">>,SourceMap),
    SourcePort = list_to_integer(binary_to_list(maps:get(<<"port">>, SourceMap))),
    SourceMethod = list_to_integer(binary_to_list(maps:get(<<"method">>, SourceMap))),
    {SourceName,{SourcePort,SourceMethod}}
  end,
  [Func(S) || S <- HostSources]. % list of tuples: [{SourceName,SourcePort,SourceMethod}]

get_host_routers(ArchMap, HostEntities) ->
  Routers = maps:get(<<"routers">>, ArchMap),
  HostRouters = [R || R <- Routers, lists:member(R,HostEntities) ],
  Func = fun(SourceMap) -> 
    RouterName = maps:get(<<"name">>,SourceMap),
    RouterPort = list_to_integer(binary_to_list(maps:get(<<"port">>, SourceMap))),
    RouterRouting = "", % TODO
    RouterFiltering = "", % TODO
    %RouterRouting = list_to_integer(binary_to_list(maps:get(<<"routing">>, SourceMap))),
    %RouterFiltering = list_to_integer(binary_to_list(maps:get(<<"filtering">>, SourceMap))),
    {RouterName,{RouterPort,RouterRouting,RouterFiltering}}
  end,
  [Func(R) || R <- HostRouters]. % list of tuples: [{RouterName,{RouterPort,RouterRouting,RouterFiltering}}]

getHostClientsWorkersList(NamedEts, HostEntities) ->
  [{clients, ClientsMaps}] = ets:lookup(NamedEts, clients),    %% = [{clients, #{c1 => {["w1"],8081}},....}]
  % io:format("Client map = ~p~n",[ClientsMaps]),
  Clients = maps:keys(ClientsMaps),
  WORKERS_TUPLE_IDX = 1, % numbering from 1
  [ {ClientName, maps:get(ClientName, element(WORKERS_TUPLE_IDX,ClientsMaps))} || ClientName <- Clients, lists:member(ClientName, HostEntities) ].   %% Tuple of {ClientName, ListOfWorkers}


%% ------------------- nerlnet data ets creation -----------------
%% Stores json files data into ets table called nerlnet_data
%% return the ets name
%% --------------------------------------------------------------
json_to_ets(HostName, JSONArchMap) ->

  ets:new(nerlnet_data,[named_table, set]),

  % update hostname
  ets:insert(nerlnet_data, {hostname, list_to_binary(HostName)}),

  % Get NerlNetSettings, batch size, frequency etc..
  NerlNetSettings = maps:get(<<"NerlNetSettings">>,JSONArchMap),
  BatchSize = list_to_integer(binary_to_list(maps:get(<<"batchSize">>,NerlNetSettings))),
  Frequency = list_to_integer(binary_to_list(maps:get(<<"frequency">>,NerlNetSettings))),
  
  ets:insert(nerlnet_data, {frequency, Frequency}),
  ets:insert(nerlnet_data, {batchSize, BatchSize}),

  JsonClients = maps:get(<<"clients">>,JSONArchMap),
  MapOfClients = get_clients_map(JsonClients, #{}), % each client has {WorkersList, Port}
  ets:insert(nerlnet_data, {clients, MapOfClients}),

  %%  get workers to clients map
  MapOfWorkers = get_workers_map(JsonClients, #{}),
  ets:insert(nerlnet_data, {workers, MapOfWorkers}),

  Hosts = get_devices(JSONArchMap), % get all hosts 
  ets:insert(nerlnet_data, {hosts,maps:from_list(Hosts)}),
  %%  retrive this device entities
  HostEntities = maps:get(list_to_binary(HostName), ets:lookup_element(nerlnet_data, hosts, ?ETS_DATA_IDX)), % List of host entities
  ets:insert(nerlnet_data, {hostEntities, HostEntities}),

  HostSpecialEntities = get_special_entities(JSONArchMap, HostEntities),
  SpecialEntityAttributeFunc = fun(SpecialEntity) -> ets:insert(nerlnet_data,SpecialEntity) end,
  lists:foreach(SpecialEntityAttributeFunc, HostSpecialEntities),

  %%  retrive THIS device Clients And Workers
  ets:insert(nerlnet_data, {hostClientsAndWorkers, getHostClientsWorkersList(nerlnet_data, HostEntities)}),

  %%  retrive this device of sources, [{SourceName, {Port, Method}}]
  Sources = get_host_sources(JSONArchMap, HostEntities),
  ets:insert(nerlnet_data, {sources, maps:from_list(Sources)}), % Stores list of sources in ets as {SourceName, {Port, Method}}

  %%  retrive THIS device Routers, returns a list of tuples:[{RoutersArgumentsMap, ConnectionMap},..]
  Routers = get_host_routers(JSONArchMap, HostEntities),
  ets:insert(nerlnet_data, {routers, maps:from_list(Routers)}), % Stores list of Routers in ets as {RouterName, {Port,Routing,Filtering}}

  nerlnet_data. % returns the ets name


getHostEntities(ArchitectureMap,CommunicationMap, HostName)->
  nerl_tools:setup_logger(?MODULE),

  % ets name is nerlnet_data
  % json_to_ets(HostName, ArchitectureMap),
  
  % use the nerlnet_data ets from this point
  %%This function returns a graph G, represents all the connections in nerlnet. each entitie should have a copy of it.
  NerlnetGraph = buildCommunicationGraph(ArchitectureMap, CommunicationMap),
  % add graph to ets
  ets:insert(nerlnet_data, {communicationGraph, NerlnetGraph}),
  nerlnet_data.

  %TODO add GUI edge to main server in the appropriate place addEdges(G, "mainServer", "nerlGUI");


%%---------------------------- Graph part ---------------------------------%%

%% This graph represents the connections withing NerlNet. edge=connection between two entities in the net, vertices = etities.
%The vaule of each of the vertices contains the tuple {Host,Port} with the host and port of the cowboy server connected to the entitie.
%TODO extract graph properties and info (longest path, vertices etc.)
buildCommunicationGraph(ArchitectureMap, CommunicationMap)->
  %Start building the graph one device at a time, than connect all routers with communicationMap json.
  NerlnetGraph = digraph:new(),
  HostsMap = ets:lookup_element(nerlnet_data, hosts, ?ETS_DATA_IDX), % Map of all hosts with their entities in ETS_DATA_IDX
  
  % adding vertices to graph
  Func = fun(HostName, HostEntities) -> 
      add_host_vertices(NerlnetGraph, ArchitectureMap, HostName, HostEntities) end, % add all entities vertices include mainServer and serverAPI
  maps:foreach(Func , HostsMap),

  connectRouters(NerlnetGraph,ArchitectureMap,CommunicationMap),

  %%connect serverAPI to Main Server
  addEdges(NerlnetGraph,?SERVER_API_STR, ?MAIN_SERVER_STR),
  
  %% TODO check if NerlGUI appears in list of vertices , if it appears a connection edge of NerlGUI to the main server here
  NerlnetGraph.


add_host_vertices(NerlnetGraph, ArchitectureMap, HostName, HostEntities)->
  HostNameStr = binary_to_list(HostName),
  ?LOG_NOTICE(?LOG_HEADER++"host ~p adds its entities: ~p to Nerlnet graph~n",[HostNameStr, HostEntities]),
  
  HostRouters = get_host_routers(ArchitectureMap, HostEntities),
  HostSources = get_host_sources(ArchitectureMap, HostEntities),
  HostClients = get_host_clients(ArchitectureMap, HostEntities),
  HostSpecials = get_special_entities(ArchitectureMap, HostEntities),

  HostAllEntitiesMap = maps:from_list(HostRouters ++ HostSources ++ HostClients ++ HostSpecials),

  AddEntityToGraph = fun(EntityName, EntityData) -> 
      EntityPort = element(?PORT_IDX, EntityData),
      ?LOG_NOTICE(?LOG_HEADER++"HostName: ~p Entity: ~p Port: ~p ~n",[HostName,HostName,EntityPort]),
      digraph:add_vertex(NerlnetGraph, EntityName,{HostName, EntityPort, list_to_binary(EntityName)})
      end,

  maps:foreach(AddEntityToGraph , HostAllEntitiesMap),
  NerlnetGraph.



%%connects all the routers in the network by the json configuration received in CommunicationMapAdderess
connectRouters(G,_ArchitectureMap,CommunicationMap) -> 

    ConnectionsMap = maps:to_list(maps:get(<<"connectionsMap">>,CommunicationMap)),
    [[addEdges(G,binary_to_list(Router),binary_to_list(Component))||Component<-Components]||{Router,Components}<-ConnectionsMap].
    % [[addEdges(G,binary_to_list(Router),binary_to_list(ListOfRouters))||ListOfRouters <- ListOfRouters]||{Router,ListOfRouters}<-ConnectionsMap].
    %io:format("ConnectionsMap:~n~p~n",[ConnectionsMap]).


addEdges(G,V1,V2) ->
  Edges = [digraph:edge(G,E) || E <- digraph:edges(G)],
  DupEdges = [E || {E, Vin, Vout, _Label} <- Edges, Vin == V1, Vout == V2],
  %io:format("DupEdges are: ~p~n",[DupEdges]),
  if length(DupEdges) /= 0 -> skip;
    true ->
      digraph:add_edge(G,V1,V2),
      digraph:add_edge(G,V2,V1)
  end.
