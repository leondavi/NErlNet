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
-include("../worker_definitions_ag.hrl").
-export([parseJsons/3, json_to_ets/2]).

-define(SINGLE_STRONG_COMPONENT,1). %% apiServer alone + all network 
-define(ETS_DATA_IDX, 2).
-define(PORT_IDX, 1). % port is always at the first index of any entity that has a port!
-define(NERLNET_DATA_ETS_LOG_DST, "/usr/local/lib/nerlnet-lib/log/nerlnet_data_ets.log").

is_special_entity(EntityName) ->  lists:member(EntityName, ?LIST_OF_SPECIAL_SERVERS).

get_special_entities(DCMap, DeviceEntities)->
  SpecialEntities = [ Entity || Entity <- DeviceEntities, is_special_entity(Entity)],
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
    IPv4 = binary_to_list(maps:get(?DC_IPV4_FIELD_STR_BIN,DeviceMap)),
    DeviceName = binary_to_atom(maps:get(?DC_NAME_FIELD_STR_BIN,DeviceMap)), 
    Entities = [ list_to_atom(EntityStr) || EntityStr <- re:split(binary_to_list(maps:get(?DC_ENTITIES_FIELD_STR_BIN,DeviceMap)),",",[{return,list}])],
    {DeviceName , {IPv4, Entities}}
  end,
  DeviceNameToIPv4EntitiesList = [DeviceNameToIPv4EntitiesFunc(D) || D <- Devices],
  IPv4ToDeviceNameMap = maps:from_list(lists:map(fun({DeviceName, {IPv4, _Entities}}) -> {IPv4, DeviceName} end, DeviceNameToIPv4EntitiesList)),
  DeviceNameToIPv4EntitiesMap = maps:from_list(DeviceNameToIPv4EntitiesList),
  {IPv4ToDeviceNameMap, DeviceNameToIPv4EntitiesMap}.
  



get_device_clients(DCMap, DeviceEntities) -> get_device_clients(DCMap, DeviceEntities, false).
get_device_clients(DCMap, DeviceEntities, PrintLog) ->
  AllClients = [ ClientMap || ClientMap <- maps:get(?DC_KEY_CLIENTS_STR_BIN,DCMap) ],
  DeviceClients = [ ClientMap || ClientMap <- AllClients, lists:member(binary_to_atom(maps:get(?DC_NAME_FIELD_STR_BIN, ClientMap)), DeviceEntities) ],
  Func = fun(ClientMap) -> 
    Name = binary_to_atom(maps:get(?DC_NAME_FIELD_STR_BIN,ClientMap)),
    Port = list_to_integer(binary_to_list(maps:get(?DC_PORT_FIELD_STR_BIN, ClientMap))),
    ClientWorkers = [ list_to_atom(WorkerStr) || WorkerStr <- re:split(binary_to_list(maps:get(?DC_WORKERS_FIELD_STR_BIN,ClientMap)),",",[{return,list}])],
    WorkersMaps = maps:get(?DC_WORKERS_FIELD_STR_BIN, DCMap), 
    WorkerShaList = lists:map(fun(WorkerMap) -> % Returns a list of tuples [{WorkerName, SHA},...]
                              { maps:get(?DC_NAME_FIELD_STR_BIN, WorkerMap) , 
                                maps:get(?DC_WORKER_MODEL_SHA_FIELD_STR_BIN , WorkerMap)} 
                                end, 
                              WorkersMaps),
    WorkerShaMap = maps:from_list(lists:map(fun({WorkerName, SHA}) -> {binary_to_atom(WorkerName), binary_to_list(SHA)} end, WorkerShaList)),
    if PrintLog ->
      ?LOG_NOTICE("Client Name: ~p Port: ~p Client Workers ~p",[Name,Port,ClientWorkers]);
      true -> skip
    end,
    {Name,{Port,ClientWorkers,WorkerShaMap,get_workers_map(AllClients, #{})}}
  end,
  [Func(S) || S <- DeviceClients]. % list of tuples: [Name,{Port,WorkersMap}]

get_models(ShaToModelMaps) ->
  Func = fun(_SHA,ModelParams) ->
    ModelType = binary_to_list(maps:get(?WORKER_FIELD_KEY_MODEL_TYPE_BIN,ModelParams)),
    ModelArgs = binary_to_list(maps:get(?WORKER_FIELD_KEY_MODEL_ARGS_BIN, ModelParams)),
    LayersSizes = binary_to_list(maps:get(?WORKER_FIELD_KEY_LAYER_SIZES_LIST_BIN,ModelParams)),
    LayersTypes = binary_to_list(maps:get(?WORKER_FIELD_KEY_LAYER_TYPES_LIST_BIN,ModelParams)),
    LayersFunctions = binary_to_list(maps:get(?WORKER_FIELD_KEY_LAYERS_FUNCTIONS_BIN,ModelParams)),
    LossMethod = binary_to_list(maps:get(?WORKER_FIELD_KEY_LOSS_METHOD_BIN,ModelParams)),
    LossArgs = binary_to_list(maps:get(?WORKER_FIELD_KEY_LOSS_ARGS_BIN,ModelParams)),
    LearningRate = binary_to_list(maps:get(?WORKER_FIELD_KEY_LEARNING_RATE_BIN,ModelParams)),
    Epochs = binary_to_list(maps:get(?WORKER_FIELD_KEY_EPOCHS_BIN,ModelParams)),
    Optimizer = binary_to_list(maps:get(?WORKER_FIELD_KEY_OPTIMIZER_TYPE_BIN,ModelParams)),
    OptimizerArgs = binary_to_list(maps:get(?WORKER_FIELD_KEY_OPTIMIZER_ARGS_BIN, ModelParams)),
    InfraType = binary_to_list(maps:get(?WORKER_FIELD_KEY_INFRA_TYPE_BIN,ModelParams)),
    DistributedSystemType = binary_to_list(maps:get(?WORKER_FIELD_KEY_DISTRIBUTED_SYSTEM_TYPE_BIN,ModelParams)),
    DistributedSystemArgs = binary_to_list(maps:get(?WORKER_FIELD_KEY_DISTRIBUTED_SYSTEM_ARGS_BIN,ModelParams)),
    DistributedSystemToken = binary_to_list(maps:get(?WORKER_FIELD_KEY_DISTRIBUTED_SYSTEM_TOKEN_BIN,ModelParams)),
    ModelTuple = {ModelType, ModelArgs , LayersSizes, LayersTypes, LayersFunctions, LossMethod, LossArgs, LearningRate, Epochs, Optimizer, OptimizerArgs, InfraType, DistributedSystemType, DistributedSystemArgs, DistributedSystemToken},
    ModelTuple
  end,
  ShaToModelArgsList = [{binary_to_list(ShaBin) , ModelParams} || {ShaBin , ModelParams} <- maps:to_list(maps:map(Func, ShaToModelMaps))],
  maps:from_list(ShaToModelArgsList).

generate_workers_map([],WorkersMap,_ClientName)->WorkersMap;
generate_workers_map([Worker|Workers],WorkersMap,ClientName)->
  generate_workers_map(Workers,maps:put(Worker, ClientName,WorkersMap),ClientName). 

%%returns a map for all workers  - key workerName, Value ClientName
get_workers_map([],WorkersMap)->WorkersMap;
get_workers_map([ClientMap|Clients],WorkersMap)->
  ClientName = list_to_atom(binary_to_list(maps:get(?DC_NAME_FIELD_STR_BIN,ClientMap))),
  Workers = [ list_to_atom(WorkerStr) || WorkerStr <- re:split(binary_to_list(maps:get(?DC_WORKERS_FIELD_STR_BIN,ClientMap)),",",[{return,list}])],    %% TODO: implement as function
  NewMap = generate_workers_map(Workers,WorkersMap,ClientName),
  get_workers_map(Clients,NewMap).

get_device_sources(DCMap, DeviceEntities) ->
  HostSources = [ SourceMap || SourceMap <- maps:get(?DC_KEY_SOURCES_STR_BIN,DCMap), lists:member(binary_to_atom(maps:get(?DC_NAME_FIELD_STR_BIN, SourceMap)), DeviceEntities) ],
  Func = fun(SourceMap) -> 
    SourceName = binary_to_atom(maps:get(?DC_NAME_FIELD_STR_BIN,SourceMap)),
    SourcePort = list_to_integer(binary_to_list(maps:get(?DC_PORT_FIELD_STR_BIN, SourceMap))),
    SourcePolicy = list_to_integer(binary_to_list(maps:get(?DC_POLICY_FIELD_STR_BIN, SourceMap))), 
    SourceEpochs = list_to_integer(binary_to_list(maps:get(?DC_EPOCHS_FIELD_STR_BIN, SourceMap))),
    SourceType = binary_to_atom(maps:get(?DC_TYPE_FIELD_STR_BIN, SourceMap)),
    SourceFrequency = list_to_integer(binary_to_list((maps:get(?DC_KEY_FREQUENCY_STR_BIN, SourceMap)))),
    {SourceName,{SourcePort,SourcePolicy,SourceFrequency,SourceEpochs,SourceType}}
  end,
  [Func(S) || S <- HostSources]. % list of tuples: [{SourceName,SourcePort,SourceMethod}]

get_device_routers(DCMap, DeviceEntities) ->
  RouterMapsList = maps:get(?DC_KEY_ROUTERS_STR_BIN,DCMap),
  RouterNamesAndMaps = lists:map(fun(RouterMap) -> {binary_to_atom(maps:get(?DC_NAME_FIELD_STR_BIN, RouterMap)) , RouterMap} end, RouterMapsList),
  DeviceRouters = [{RouterName , RouterMap} || {RouterName , RouterMap} <- RouterNamesAndMaps, lists:member(RouterName, DeviceEntities)], 
  Func = fun({RouterName , RouterMap}) -> 
    RouterPort = list_to_integer(binary_to_list(maps:get(?DC_PORT_FIELD_STR_BIN, RouterMap))),
    RouterPolicy = list_to_integer(binary_to_list(maps:get(?DC_POLICY_FIELD_STR_BIN, RouterMap))),
    {RouterName,{RouterPort,RouterPolicy}}
  end,
  [Func(R) || R <- DeviceRouters]. 

%% ------------------- nerlnet data ets creation -----------------
%% Stores json files data into ets table called nerlnet_data
%% return the ets name
%% --------------------------------------------------------------
json_to_ets(IPv4, JsonDCMap) ->
  % Auto generated definitions validation
  if ?DC_DISTRIBUTED_SYSTEM_TYPE_NONE_IDX_STR == "0" -> ok;
  true -> throw("Auto generated definitions are not valid, none-distributed system type should be 0") end,
  % update DeviceName
  ets:insert(nerlnet_data, {?DC_IPV4_FIELD_ATOM, IPv4}),
  ets:insert(nerlnet_data, {ipv4_bin, list_to_binary(IPv4)}), %% ? is this needed
  ?LOG_NOTICE("Device IP=~p~n",[IPv4]),

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

  MapSHAToModelArgs = maps:get(?DC_KEY_MODEL_SHA_STR_BIN, JsonDCMap), % Map of MapShaToModel to ModelArgs
  SHAToModelArgsMap = get_models(MapSHAToModelArgs), 
  ets:insert(nerlnet_data, {sha_to_models_map, SHAToModelArgsMap}),

  {IPv4ToDeviceNameMap, DeviceNameToIPv4EntitiesMap} = get_devices(JsonDCMap), % get all hosts 
  ets:insert(nerlnet_data, {ipv4_to_devices,IPv4ToDeviceNameMap}),
  ets:insert(nerlnet_data, {devices_map,DeviceNameToIPv4EntitiesMap}),
  %%  retrive this device entities
  DeviceName = maps:get(IPv4, IPv4ToDeviceNameMap), 
  ?LOG_NOTICE("Device Name: ~p IPv4: ~p~n", [DeviceName , IPv4]),
  ets:insert(nerlnet_data, {device_name, DeviceName}),

  {IPv4 , DeviceEntities} = maps:get(DeviceName, DeviceNameToIPv4EntitiesMap),
  ets:insert(nerlnet_data, {device_entities, DeviceEntities}),
  ?LOG_NOTICE("Device Entities: ~p", [DeviceEntities]),
  DeviceSpecialEntities = get_special_entities(JsonDCMap, DeviceEntities),
  SpecialEntityAttributeFunc = fun(SpecialEntity) -> ets:insert(nerlnet_data,SpecialEntity) end,
  lists:foreach(SpecialEntityAttributeFunc, DeviceSpecialEntities),
  ?LOG_NOTICE("Device special entities: ~p", [DeviceSpecialEntities]),
  %%  retrive THIS device Clients And Workers
  ?LOG_NOTICE("Adding Device Entities:"),
  ets:insert(nerlnet_data, {deviceClients, get_device_clients(JsonDCMap, DeviceEntities, true)}),

  %%  retrive this device of sources, [{SourceName, {Port, Method}}]
  Sources = get_device_sources(JsonDCMap, DeviceEntities),
  ets:insert(nerlnet_data, {sources, maps:from_list(Sources)}), % Stores list of sources in ets as {SourceName, {Port, Method}}

  %%  retrive THIS device Routers, returns a list of tuples:[{RoutersArgumentsMap, ConnectionMap},..]
  Routers = get_device_routers(JsonDCMap, DeviceEntities),
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

  ?LOG_NOTICE("Save nerlnet_data to log file: ~p", [?NERLNET_DATA_ETS_LOG_DST]),
  ets:tab2file(nerlnet_data, ?NERLNET_DATA_ETS_LOG_DST). % TODO consider adding a timestamp

%%---------------------------- Graph part ---------------------------------%%

%% This graph represents the connections withing NerlNet. edge=connection between two entities in the net, vertices = etities.
%The vaule of each of the vertices contains the tuple {Host,Port} with the host and port of the cowboy server connected to the entitie.
%TODO extract graph properties and info (longest path, vertices etc.)
buildCommunicationGraph(DCMap, CommunicationMap)->
  %Start building the graph one device at a time, than connect all routers with communicationMap json.
  DevicesMap = ets:lookup_element(nerlnet_data, devices_map, ?ETS_DATA_IDX), % Map of device name to {IPv4 , Entities} in ETS_DATA_IDX
  NerlnetGraph = ets:lookup_element(nerlnet_data, communicationGraph, ?ETS_DATA_IDX),
  % adding vertices to graph
  Func = fun(DeviceName, {IPv4 , DeviceEntities}) -> 
      add_device_vertices(NerlnetGraph, DCMap, DeviceName , IPv4, DeviceEntities) end, % add all entities vertices include mainServer and serverAPI
  maps:foreach(Func , DevicesMap),

  connectRouters(NerlnetGraph,DCMap,CommunicationMap),
  
  NerlnetGraph.

% add all entities vertices of given device include mainServer and serverAPI to NerlnetGraph
add_device_vertices(NerlnetGraph, DCMap , DeviceName , IPv4 , DeviceEntities)->
  DeviceRouters = get_device_routers(DCMap, DeviceEntities),
  DeviceSources = get_device_sources(DCMap, DeviceEntities),
  DeviceClients = get_device_clients(DCMap, DeviceEntities),
  DeviceSpecialEntities = get_special_entities(DCMap, DeviceEntities),

  DeviceEntitiesMap = maps:from_list(DeviceRouters ++ DeviceSources ++ DeviceClients ++ DeviceSpecialEntities),
  
  AddEntityToGraph = fun(EntityName, EntityData) -> 
      EntityPort = element(?PORT_IDX, EntityData),
      digraph:add_vertex(NerlnetGraph,EntityName, {IPv4, EntityPort , DeviceName})
      end,

  maps:foreach(AddEntityToGraph , DeviceEntitiesMap),
  NerlnetGraph.



%%connects all the routers in the network by the json configuration received in CommunicationMapAdderess
connectRouters(Graph,_DCMap,CommunicationMap) -> 

    ConnectionsMapList = maps:to_list(maps:get(?CONN_MAP_FIELD_BIN,CommunicationMap)),
    TranslateJsonNamesToAtoms = fun({NameBin, EntitiesBin}) -> 
      Name = binary_to_atom(NameBin),
      [ {Name, binary_to_atom(EntityBin)} || EntityBin <- EntitiesBin]
    end,
    RoutersEdges = lists:flatten(lists:map(TranslateJsonNamesToAtoms, ConnectionsMapList)),
    ?LOG_INFO("RoutersEdges:~p~n",[RoutersEdges]),
    AddEdgesFunc = fun({Router,Entity}) -> 
                      case Entity of 
                        ?MAIN_SERVER_ATOM -> add_edges(Graph , Router , ?API_SERVER_ATOM);
                        _ -> skip end, 
                      add_edges(Graph,Router,Entity)  end,
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

