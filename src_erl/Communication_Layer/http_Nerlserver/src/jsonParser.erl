%%%-------------------------------------------------------------------
%%% @author kapelnik
%%% @copyright (C) 2021, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 14. May 2021 4:48 AM
%%%-------------------------------------------------------------------
-module(jsonParser).
-author("kapelnik").
-export([getDeviceEntities/2]).

getDeviceEntities(JsonPath,HostName)->
  {ok, Data} = file:read_file(JsonPath),

%%TODO ADD CHECK FOR VALID INPUT:  io:format("~p~n",[jsx:is_json(Data)]),
  %%Decode Json to architecute map:
  ArchitectureMap = jsx:decode(Data,[]),

  %%  get workers to clients map
  WorkersMap = getWorkersMap(maps:get(<<"clients">>,ArchitectureMap),#{}),

  %%  retrive THIS device entities
  OnDeviceEntities1 = getOnDeviceEntities(maps:get(<<"devices">>,ArchitectureMap),HostName),
  OnDeviceEntities =re:split(binary_to_list(OnDeviceEntities1),",",[{return,list}]),
%%  io:format("OnDeviceEntities:~n~p~n",[OnDeviceEntities]),


  %%  retrive THIS device Clients And Workers, returns a list of tuples:[{ClientArgumentsMap,WorkersMap,ConnectionMap},..]
  ClientsAndWorkers = getClients(maps:get(<<"clients">>,ArchitectureMap),OnDeviceEntities , [],maps:get(<<"workers">>,ArchitectureMap),ArchitectureMap),

  %%  retrive THIS device Sources, returns a list of tuples:[{SourceArgumentsMap, ConnectionMap},..]
  Sources = {getSources(maps:get(<<"sources">>, ArchitectureMap), OnDeviceEntities, [], ArchitectureMap),WorkersMap},

  %%  retrive THIS device Routers, returns a list of tuples:[{RoutersArgumentsMap, ConnectionMap},..]
  Routers = getRouters(maps:get(<<"routers">>,ArchitectureMap),OnDeviceEntities , [],ArchitectureMap),



  %%  retrive THIS device MainServer, returns a map of arguments

%%  check if a mainServer needed to be opened on this device, retrive arguments for main server or return none atom
  OnDevice = lists:member("mainServer",OnDeviceEntities),
  if
    OnDevice == false -> MainServer = none;
    true -> MainServerArgs = maps:get(<<"mainServer">>,ArchitectureMap),
      ClientsNames = getAllClientsNames(maps:get(<<"clients">>,ArchitectureMap),[]),
      MainServerConnectionsMap=getConnectionMap(<<"mainServer">>,ArchitectureMap),
      MainServer = {MainServerArgs,MainServerConnectionsMap,WorkersMap,ClientsNames}
  end,



  %%  retrive  a map of arguments of the API Server
  ServerAPI = maps:get(<<"serverAPI">>,ArchitectureMap),

  io:format("On Device Entities to Open:~nMainServer: ~p~nServerAPI: ~p~nClientsAndWorkers: ~p~nSources: ~p~nRouter: ~p~n",[MainServer,ServerAPI,ClientsAndWorkers,Sources,Routers]),

  {MainServer,ServerAPI,ClientsAndWorkers,Sources,Routers}.



%%getEntities findes the right device from devices map and returns it's entities
getOnDeviceEntities([],_HostName) -> none;
getOnDeviceEntities([Device|Tail],HostName) ->
  DevHost = maps:get(<<"host">>,Device),
  if HostName == DevHost ->
    maps:get(<<"entities">>,Device);
    true -> getOnDeviceEntities(Tail,HostName)
  end.

%%getSources findes all sources needed to be opened on this device. returns [{sourceArgsMap,SourceConnectionMap},...]
getSources([],_OnDeviceSources,[],_ArchMap) ->none;
getSources([],_OnDeviceSources,Return,_ArchMap) ->Return;
getSources([Source|Sources],OnDeviceSources,Return,ArchMap) ->

  SourceName = maps:get(<<"name">>,Source),
  OnDevice = lists:member(binary_to_list(SourceName),OnDeviceSources),
  if  OnDevice == false->
    getSources(Sources,OnDeviceSources,Return,ArchMap);
    true ->
      ConnectionsMap = getConnectionMap(maps:get(<<"name">>,Source),ArchMap),
      getSources(Sources,OnDeviceSources,Return++[{Source,ConnectionsMap}],ArchMap)
  end.

%%getRouters findes all Routers needed to be opened on this device. returns [{RouterArgsMap,RoutersConnectionMap},...]
getRouters([],_OnDeviceRouters,[],_ArchMap) ->none;
getRouters([],_OnDeviceRouters,Return,_ArchMap) ->Return;
getRouters([Router|Routers],OnDeviceRouters,Return,ArchMap) ->

  RouterName = maps:get(<<"name">>,Router),
  OnDevice = lists:member(binary_to_list(RouterName),OnDeviceRouters),
  if  OnDevice == false->
    getRouters(Routers,OnDeviceRouters,Return,ArchMap);
    true ->
      ConnectionsMap = getRouterConnectionMap(maps:get(<<"name">>,Router),ArchMap),
      getRouters(Routers,OnDeviceRouters,Return++[{Router,ConnectionsMap}],ArchMap)
  end.

%%getClients findes all Clients And Workers needed to be opened on this device. returns [{ClientArgsMap,WorkersMap,ClientConnectionMap},...]
getClients([],_Entities,[],_ArchWorkers,_ArchMap) ->none;
getClients([],_Entities,ClientsAndWorkers,_ArchWorkers,_ArchMap) ->ClientsAndWorkers;
getClients([Client|Tail],Entities,ClientsAndWorkers,ArchWorkers,ArchMap) ->

  ClientName = maps:get(<<"name">>,Client),
  OnDevice = lists:member(binary_to_list(ClientName),Entities),
  if  OnDevice == false->
    getClients(Tail,Entities,ClientsAndWorkers,ArchWorkers,ArchMap);
    true ->
      ConnectionsMap = getConnectionMap(maps:get(<<"name">>,Client),ArchMap),
      ClientWorkers =re:split(binary_to_list(maps:get(<<"workers">>,Client)),",",[{return,list}]),
      Workers = getWorkers(ArchWorkers,ClientWorkers,[]),
      getClients(Tail,Entities,ClientsAndWorkers++[{Client,Workers,ConnectionsMap}],ArchWorkers,ArchMap)
  end.



getWorkers([],_ClientsWorkers,Workers) -> Workers;
getWorkers([Worker|Tail],ClientsWorkers,Workers) ->
  WorkerName = maps:get(<<"name">>,Worker),
  InClient = lists:member(binary_to_list(WorkerName),ClientsWorkers),
  if  InClient == false->
    getWorkers(Tail,ClientsWorkers,Workers);
    true ->
      getWorkers(Tail,ClientsWorkers,Workers++[Worker])
  end.

%%Builds a connnection map for the Router on this device to intreduce it later with all connected entities
getRouterConnectionMap(RouterName,ArchMap) ->
  ConnectionsList = maps:to_list(maps:get(RouterName,maps:get(<<"connectionsMap">>,ArchMap))),
  buildRouterConnectionMap(RouterName,ConnectionsList,ArchMap, #{}).


buildRouterConnectionMap(_MyRouterName,[],_ArchMap, ConnectionMap) -> ConnectionMap;
buildRouterConnectionMap(MyRouterName,[{EntityName,MyRouterName}|Entities],ArchMap, ConnectionMap) ->
  EntityHost = getHost(maps:get(<<"devices">>,ArchMap),EntityName),
  EntityPort = getPortUnknown(ArchMap,EntityName),
  buildRouterConnectionMap(MyRouterName,Entities,ArchMap, ConnectionMap#{list_to_atom(binary_to_list(EntityName)) => {EntityHost,EntityPort}});

buildRouterConnectionMap(MyRouterName,[{EntityName,RouterName}|Entities],ArchMap, ConnectionMap) ->
  EntityHost = getHost(maps:get(<<"devices">>,ArchMap),RouterName),
  EntityPort = getPort(maps:get(<<"routers">>,ArchMap),RouterName),
  buildRouterConnectionMap(MyRouterName,Entities,ArchMap, ConnectionMap#{list_to_atom(binary_to_list(EntityName)) => {EntityHost,EntityPort}}).

getConnectionMap(Name,ArchMap) ->
  ConnectionsList = maps:to_list(maps:get(Name,maps:get(<<"connectionsMap">>,ArchMap))),
%%  io:format("~p connection Map from json: ~p~n",[Name,ConnectionsList]),
  buildConnectionMap(ConnectionsList,ArchMap, #{}).

buildConnectionMap([],_ArchMap, ConnectionMap) -> ConnectionMap;
buildConnectionMap([{EntityName,RouterName}|Entities],ArchMap, ConnectionMap) ->
  EntityHost = getHost(maps:get(<<"devices">>,ArchMap),RouterName),
  EntityPort = getPort(maps:get(<<"routers">>,ArchMap),RouterName),
%%  io:format("@#@#Name- ~p, hostport - ~p~n",[EntityName,{EntityHost,EntityPort}]),
  buildConnectionMap(Entities,ArchMap, ConnectionMap#{list_to_atom(binary_to_list(EntityName)) => {EntityHost,EntityPort}}).

getHost([DeviceMap|Devices],EntityName) ->
  Entities = maps:get(<<"entities">>, DeviceMap),
  OnDeviceEntities =re:split(binary_to_list(Entities),",",[{return,list}]),

  OnDevice = lists:member(binary_to_list(EntityName),OnDeviceEntities),
  if  OnDevice == false->
    getHost(Devices,EntityName);
    true ->
      binary_to_list(maps:get(<<"host">>,DeviceMap))
  end.

getPort([],_EntityName) -> false;
getPort([EntityMap|Entities],EntityName) ->
  CurrName = maps:get(<<"name">>, EntityMap),
  if  CurrName == EntityName->
    list_to_integer(binary_to_list(maps:get(<<"port">>, EntityMap)));
    true ->
      getPort(Entities,EntityName)
  end.

getPortUnknown(ArchMap,<<"mainServer">>)->
  [MainServer] = maps:get(<<"mainServer">>,ArchMap),
  list_to_integer(binary_to_list(maps:get(<<"port">>, MainServer)));
getPortUnknown(ArchMap,<<"serverAPI">>)->
  [ServerAPI] = maps:get(<<"serverAPI">>,ArchMap),
  list_to_integer(binary_to_list(maps:get(<<"port">>, ServerAPI)));
getPortUnknown(ArchMap,EntityName)->
  Found = getPort(maps:get(<<"clients">>,ArchMap),EntityName),
  if  Found == false->
    getPort(maps:get(<<"sources">>,ArchMap),EntityName);
    true ->
      Found
  end.

%%returns a map of all workers  - key workerName, Value ClientName
getWorkersMap([],WorkersMap)->WorkersMap;
getWorkersMap([Client|Clients],WorkersMap)->
  ClientName = list_to_atom(binary_to_list(maps:get(<<"name">>,Client))),
  Workers = re:split(binary_to_list(maps:get(<<"workers">>,Client)),",",[{return,list}]),
  NewMap = addAll(Workers,WorkersMap,ClientName),
  getWorkersMap(Clients,NewMap).

addAll([],WorkersMape,_ClientName)->WorkersMape;
addAll([Worker|Workers],WorkersMap,ClientName)->
  addAll(Workers,maps:put(list_to_atom(Worker),ClientName,WorkersMap),ClientName).

%%returns a list of all clients names in nerlnet
getAllClientsNames([],ClientsNames) ->ClientsNames;
getAllClientsNames([Client|Tail],ClientsNames) ->
  ClientName = maps:get(<<"name">>,Client),
  getAllClientsNames(Tail,ClientsNames++[list_to_atom(binary_to_list(ClientName))]).