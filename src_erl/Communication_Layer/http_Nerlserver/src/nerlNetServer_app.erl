%%%-------------------------------------------------------------------
%% @doc nerlNetServer public API
%% @author Tal Kapelnik
%% @end
%%%-----------------------------------------------------------------------------------------------------------------------------------------------------------
%%%-----------------------------------------------------------------------------------------------------------------------------------------------------------
%%    http_NerlServer - generic http server for federated work environment based on http requests.
%%                      This server acquires its architecture from a JSON file found in input folder.
%%                      After parsing JSON, the following Cowboy-based machines will be initiated:
%%                               *Main Server - a cowboy server wrapped with a gen_server - controlling the creation and the operation of all Nerlnet
%%                               *NerlClient - a cowboy server wrapped with gen_stateM - serving as a client for cppSANN's handlers
%%                               *Source_server - a cowboy server wrapped with gen_stateM - feeding data to all listed nodes in Nerlnet
%%                               *Router - a cowboy server wrapped with gen_server - connecting listed nodes in the network - all http requests are routed
%%                                                                                   through this Server.
%%%-----------------------------------------------------------------------------------------------------------------------------------------------------------
%%%-----------------------------------------------------------------------------------------------------------------------------------------------------------

-module(nerlNetServer_app).

-behaviour(application).
-include_lib("kernel/include/logger.hrl").

-export([start/2, stop/1, getdeviceIP/0]).

-define(NERLNET_INIT_PORT,8484).
-define(PYTHON_SERVER_WAITING_TIMEOUT_MS, 1000).
-define(SUBNETS_CONFIG_ADDR, "/usr/local/lib/nerlnet-lib/NErlNet/config/subnets.nerlconfig").
%% *    Initiate rebar3 shell : rebar3 shell
%% **   send any request
%% ***  exit rebar3 shell: ctrl+g ->q

%%to make a request, send a url request to-    'http://localhost:PORT/*request* '
%%example for a request from a terminal:
%%curl -i http://localhost:8080/rasp/get/1

%% or - from erlang, init a basic httpc connection by typing in erlang shell-
%%inets:start(). -> ok.
%% set options for the http_Nerlserver -
%%httpc:set_options([{proxy, {{"localhost", PORT},["localhost"]}}]). -> ok.
%%now we can make requests from the server. for example -
%%httpc:request("http://localhost:8080/rasp/get/1"). -> {ok,{{"HTTP/1.1",200,"OK"},Headers, Body}.
%%httpc:request("http://localhost:8080/weight/set/1").

%%erlang request:   (using post method, because we want to receive a reply for our request)
%%httpc:request(post,{URL,Headers,content type,Body),HTTPOptions, Profile)
%%httpc:request(post,{"http://localhost:8080/weights_vector/update", [],"application/x-www-form-urlencoded","[33.20.2,120,2.<<23.2>>]"}, [], []).


start(_StartType, _StartArgs) ->
    logger:set_module_level(nerlNetServer_app, all),
    
    HostName = getdeviceIP(),
    %HostName = "127.0.0.1",        %TODO: update jsons with real ips
    ?LOG_INFO("This device IP: ~p~n", [HostName]),
    %Create a listener that waits for a message from python about the adresses of the wanted json
    createNerlnetInitiator(HostName),
    {ArchitectureAdderess,CommunicationMapAdderess} = waitForInit(),

    %Parse json and start nerlnet:
     
    ?LOG_INFO("ArchitectureAdderess: ~p~n CommunicationMapAdderess : ~p~n",[ArchitectureAdderess,CommunicationMapAdderess]),

    parseJsonAndStartNerlnet(HostName,ArchitectureAdderess,CommunicationMapAdderess),
    nerlNetServer_sup:start_link().

waitForInit() ->
    receive 
        {jsonAddress,MSG} -> {_ArchitectureAdderess,_CommunicationMapAdderess} = MSG;
        Other -> ?LOG_WARNING("Got bad message: ~p,~ncontinue listening for init Json~n",[Other]), waitForInit()
        after ?PYTHON_SERVER_WAITING_TIMEOUT_MS -> waitForInit()
    end.

createNerlnetInitiator(HostName) ->
    Port = ?NERLNET_INIT_PORT,
    NerlnetInitiatorDispatch = cowboy_router:compile([
        {'_', [

            {"/updateJsonPath",jsonHandler, [self()]},
            {"/isNerlnetDevice",iotHandler, [self()]}
        ]}
    ]),
    %% cowboy:start_clear(Name, TransOpts, ProtoOpts) - an http_listener
    %%An ok tuple is returned on success. It contains the pid of the top-level supervisor for the listener.
    init_cowboy_start_clear(nerlnetInitiator, {HostName,Port},NerlnetInitiatorDispatch).


parseJsonAndStartNerlnet(HostName,ArchitectureAdderess,CommunicationMapAdderess) ->
    %%Server that should be established on this machine from JSON architecture:
    % {MainServer,_ServerAPI,ClientsAndWorkers, {Sources,WorkersMap},Routers,{Federateds,WorkersMap},[NerlNetSettings]} = jsonParser:getDeviceEntities("./input/jsonArch1PC2Workers.json",list_to_binary(HostName)),
    %%    get json path from jsonPath file in main NErlNet directory
    

    %%Server that should be established on this machine from JSON architecture:
    {MainServer,_ServerAPI,ClientsAndWorkers, {Sources,WorkersMap},Routers,{Federateds,WorkersMap},[NerlNetSettings],_GUI} = jsonParser:getDeviceEntities(ArchitectureAdderess,CommunicationMapAdderess,list_to_binary(HostName)),

%  io:format("My NerlNetSettings: ~p~n",[NerlNetSettings]),

    BatchSize = list_to_integer(binary_to_list(maps:get(<<"batchSize">>,NerlNetSettings))),
    Frequency = list_to_integer(binary_to_list(maps:get(<<"frequency">>,NerlNetSettings))),
    %  io:format("My BatchSize: ~p~n",[BatchSize]),
    %  io:format("My Frequency: ~p~n",[Frequency]),

%%    Creating a Dispatcher for each Server from JSONs architecture - this dispatchers will rout http requests to the right handler.
%%    Each dispatcher will be listening to a different PORT
%%    Handling http requests will be managed by *Handler.erl and additional information given inside.
    %%these are the handler for any kind of request.
    %% {HostMatch, list({PathMatch, Handler, InitialState})}
    %The last arg becomes the State
    %arg in the *Handler's init() method.
    %%{"/req_name/:arg1/:arg2",[{arg1,constrains}, {arg2,int}], addHandler,[]}
    %%    each server gets the port map he will need inorder to make http requests. all requests are delivered via the router only

    createClientsAndWorkers(ClientsAndWorkers, HostName),
    createMainServer(MainServer,BatchSize,HostName),
    createRouters(Routers,HostName),
    createSources(Sources,WorkersMap, BatchSize, Frequency, HostName),
    createFederatedServer(Federateds,WorkersMap, HostName).


%% internal functions

createClientsAndWorkers(none,_HostName) -> none;
createClientsAndWorkers([], _HostName) -> okdone;
createClientsAndWorkers([{ClientArgs,WorkersArgs,ClientConnectionsGraph}|ClientsAndWorkers],HostName) ->
    ClientName = binary_to_list(maps:get(<<"name">>,ClientArgs)),
    Port = list_to_integer(binary_to_list(maps:get(<<"port">>,ClientArgs))),
    Federated = list_to_atom(binary_to_list(maps:get(<<"federated">>,ClientArgs))),

    %%Create a gen_StateM machine for maintaining Database for Client.
    %% all http requests will be handled by Cowboy which updates client_statem if necessary.
%%    WorkersArgsMap = #{Worker1Name => Worker1Args, Worker2Name => Worker2Args},
    ClientStatemArgs= {ClientName,Federated,WorkersArgs,ClientConnectionsGraph},        %%make this a list of client
    ClientStatemPid = clientStatem:start_link(ClientStatemArgs),


    %%Nerl Client
    %%Dispatcher for cowboy to rout each given http_request for the matching handler
    NerlClientDispatch = cowboy_router:compile([
        {'_', [
            %%first http request from main server should be about starting parameters, find more info inside initHandler
            {"/init",clientStateHandler, [init,ClientStatemPid]},
            {"/statistics",clientStateHandler, [statistics,ClientStatemPid]},
            {"/clientTraining",clientStateHandler, [training,ClientStatemPid]},
            {"/clientIdle",clientStateHandler, [idle,ClientStatemPid]},
            {"/clientPredict",clientStateHandler, [predict,ClientStatemPid]},
            {"/weightsVector",vectorHandler, [ClientStatemPid]},
            {"/federatedWeights",federatedHandler, [ClientStatemPid]}
        ]}
    ]),

    %% cowboy:start_clear(Name, TransOpts, ProtoOpts) - an http_listener
    %%An ok tuple is returned on success. It contains the pid of the top-level supervisor for the listener.
    init_cowboy_start_clear(ClientName, {HostName,Port},NerlClientDispatch),
    createClientsAndWorkers(ClientsAndWorkers,HostName).


createFederatedServer(none,_WorkersMap,_HostName) -> none;
createFederatedServer([],_WorkersMap,_HostName) -> okdone;
createFederatedServer([{FederateArgs,ConnectionsGraph}|Federated],WorkersMap,HostName) ->
    FederatedName = binary_to_list(maps:get(<<"name">>,FederateArgs)),
    Port = list_to_integer(binary_to_list(maps:get(<<"port">>,FederateArgs))),
    CounterLimit = list_to_integer(binary_to_list(maps:get(<<"counterLimit">>,FederateArgs))),
    %%Create a gen_StateM machine for maintaining Database for Federated Server.
    %% all http requests will be handled by Cowboy which updates source_statem if necessary.
    FederatedStatemArgs= {FederatedName,CounterLimit,WorkersMap,ConnectionsGraph},        %%TODO  make this a list of Sources
    FederatedStatemPid = cppSANNFedServStateM:start_link(FederatedStatemArgs),


    %%    Source server
    FederatedDispatch = cowboy_router:compile([
        {'_', [
            {"/federatedWeightsVector",weightsHandler, [federatedWeightsVector,FederatedStatemPid]},
            {"/statistics",weightsHandler, [statistics,FederatedStatemPid]}

        ]}
    ]),
    %% cowboy:start_clear(Name, TransOpts, ProtoOpts) - an http_listener
    %%An ok tuple is returned on success. It contains the pid of the top-level supervisor for the listener.
    init_cowboy_start_clear(FederatedName, {HostName,Port},FederatedDispatch),
    createFederatedServer(Federated,WorkersMap,HostName).


createSources(none,_WorkersMap,_BatchSize,_Frequency,_HostName) -> none;
createSources([],_WorkersMap,_BatchSize,_Frequency,_HostName) -> okdone;
createSources([{SourceArgs,ConnectionsGraph}|Sources],WorkersMap,BatchSize,Frequency,HostName) ->
    SourceName = binary_to_list(maps:get(<<"name">>,SourceArgs)),
    Port = list_to_integer(binary_to_list(maps:get(<<"port">>,SourceArgs))),
    Method = list_to_integer(binary_to_list(maps:get(<<"method">>,SourceArgs))),
    %%Create a gen_StateM machine for maintaining Database for Source.
    %% all http requests will be handled by Cowboy which updates source_statem if necessary.
    SourceStatemArgs= {SourceName, WorkersMap, ConnectionsGraph, Method, BatchSize, Frequency},        %%TODO  make this a list of Sources
    SourceStatemPid = sourceStatem:start_link(SourceStatemArgs),


    %%    Source server
    SourceDispatch = cowboy_router:compile([
        {'_', [

            {"/updateCSV",csvHandler, [SourceStatemPid]},
            {"/startCasting",castingHandler, [startCasting,SourceStatemPid]},
            {"/stopCasting",castingHandler, [stopCasting,SourceStatemPid]},
            {"/statistics",castingHandler, [statistics,SourceStatemPid]}
        ]}
    ]),
    %% cowboy:start_clear(Name, TransOpts, ProtoOpts) - an http_listener
    %%An ok tuple is returned on success. It contains the pid of the top-level supervisor for the listener.
    init_cowboy_start_clear(SourceName, {HostName,Port},SourceDispatch),
    createSources(Sources,WorkersMap,BatchSize,Frequency,HostName).



createRouters(none,_HostName) -> none;
createRouters([],_HostName) -> okdone;
createRouters([{RouterArgs,ConnectionsGraph}|Routers],HostName) ->
    RouterName = binary_to_list(maps:get(<<"name">>,RouterArgs)),
    Port =list_to_integer(binary_to_list( maps:get(<<"port">>,RouterArgs))),

    %%Create a gen_Server for maintaining Database for Router.
    %% all http requests will be handled by Cowboy which updates router_genserver if necessary.
    %%    connectivity map will be as follow:
    %%    name_atom of machine => {Host,Port} OR an atom router_name, indicating there is no direct http connection, and should pass request via router_name

    RouterGenServerArgs= {RouterName,ConnectionsGraph},        %%TODO  make this a list of Routers
    RouterGenServerPid = routerGenserver:start_link(RouterGenServerArgs),


    RouterDispatch = cowboy_router:compile([
        {'_', [
            {"/clientIdle",routingHandler, [clientIdle,RouterGenServerPid]},
            {"/lossFunction",routingHandler, [lossFunction,RouterGenServerPid]},
            {"/predictRes",routingHandler, [predictRes,RouterGenServerPid]},
            {"/statistics",routingHandler, [statistics,RouterGenServerPid]},
            {"/clientTraining",routingHandler, [clientTraining,RouterGenServerPid]},
            {"/clientPredict",routingHandler, [clientPredict,RouterGenServerPid]},
            {"/updateCSV",routingHandler, [updateCSV,RouterGenServerPid]},
            {"/csvReady",routingHandler, [csvReady,RouterGenServerPid]},
            {"/sourceDone",routingHandler, [sourceDone,RouterGenServerPid]},
            {"/clientReady",routingHandler, [clientReady,RouterGenServerPid]},
            {"/weightsVector",routingHandler, [rout,RouterGenServerPid]},
            {"/startCasting",routingHandler, [startCasting,RouterGenServerPid]},
            {"/stopCasting",routingHandler, [stopCasting,RouterGenServerPid]},
            {"/federatedWeightsVector",routingHandler, [federatedWeightsVector,RouterGenServerPid]},
            {"/federatedWeights",routingHandler, [federatedWeights,RouterGenServerPid]},

            %%GUI actions
            {"/getStats",routingHandler, [getStats,RouterGenServerPid]}
        ]}
    ]),
    %% cowboy:start_clear(Name, TransOpts, ProtoOpts) - an http_listener
    %%An ok tuple is returned on success. It contains the pid of the top-level supervisor for the listener.
   init_cowboy_start_clear(RouterName, {HostName,Port},RouterDispatch),
    createRouters(Routers,HostName).



createMainServer(none,_BatchSize,_HostName) -> none;
createMainServer({[MainServerArgsMap],ConnectionsGraph,WorkersMap,ClientsNames},BatchSize,HostName) ->
    MainName = "mainServer",
    Port = list_to_integer(binary_to_list(maps:get(<<"port">>,MainServerArgsMap))),

    %%Create a gen_Server for maintaining Database for Main Server.
    %% all http requests will be handled by Cowboy which updates main_genserver if necessary.
    MainGenServer_Args= {MainName,ClientsNames,BatchSize,WorkersMap,ConnectionsGraph},        %%TODO change from mainserverport to routerport . also make this a list of client
    MainGenServerPid = mainGenserver:start_link(MainGenServer_Args),

    MainServerDispatcher = cowboy_router:compile([
    {'_', [
        %Nerlnet actions
        {"/updateCSV",[],initHandler,[MainGenServerPid]},
        {"/lossFunction",[],actionHandler,[lossFunction,MainGenServerPid]},
        {"/predictRes",[],actionHandler,[predictRes,MainGenServerPid]},
        {"/csvReady",[],ackHandler,[source,MainGenServerPid]},
        {"/sourceDone",[],ackHandler,[sourceDone,MainGenServerPid]},
        {"/clientReady",[],ackHandler,[client,MainGenServerPid]},
        {"/clientsTraining",[],actionHandler,[clientsTraining,MainGenServerPid]},
        {"/statistics",[],actionHandler,[statistics,MainGenServerPid]},
        {"/clientsPredict",[],actionHandler,[clientsPredict,MainGenServerPid]},
        {"/startCasting",[],actionHandler, [startCasting, MainGenServerPid]},
        {"/stopCasting",[],actionHandler, [stopCasting, MainGenServerPid]},
        %GUI actions
        {"/getGraph",[],guiHandler, [getGraph, MainGenServerPid]},
        {"/getStats",[],guiHandler, [getStats, MainGenServerPid]},

        {"/[...]", [],noMatchingRouteHandler, [MainGenServerPid]}
        ]}
        ]),
    %% cowboy:start_clear(Name, TransOpts, ProtoOpts) - an http_listener
    %%An ok tuple is returned on success. It contains the pid of the top-level supervisor for the listener.
    init_cowboy_start_clear(MainName, {HostName,Port},MainServerDispatcher).



init_cowboy_start_clear(ListenerName,{_Host,Port},Dispatcher)->
%%    TODO check how to catch ! messages in listenerPid
    {ok, _listenerPid} = cowboy:start_clear(ListenerName,
        [{port,Port}], #{env => #{dispatch =>Dispatcher}}
    ).

stop(_State) ->
    ok.

% get this host ip
getdeviceIP() ->
    {ok, IFList} = inet:getifaddrs(),    % IFList format: [{IF_name, [{field, param},{},...]},...]
    SubnetsList = getNerlSubnets(),
    getdeviceIP(IFList, SubnetsList).

getdeviceIP([], SubnetsList) ->
    logger:error("No supported interface was found. Current supported interfaces list is: ~p.~nEdit subnets.nerlconfig file to include your network",[SubnetsList]);
getdeviceIP([IF|IFList], SubnetsList) ->
    {_IF_name, Params} = IF,
    try
        {addr, IF_addr} = lists:keyfind(addr, 1, Params),   % address format: {num, num, num, num}
        DeviceIP = isAddrInSubnets(IF_addr, SubnetsList),
        case DeviceIP of
            notFound -> getdeviceIP(IFList, SubnetsList);
            IP -> IP
        end
    catch error:_E -> getdeviceIP(IFList, SubnetsList)
    end.

getNerlSubnets() ->
    {ok, Data} = file:read_file(?SUBNETS_CONFIG_ADDR),
    Lines = string:split(binary_to_list(Data), "\n", all),
    Subnets = [Subnet || Subnet <- Lines, hd(Subnet) /= $#],
    lists:sort(Subnets).

isAddrInSubnets(_IF_addr, []) -> notFound;
isAddrInSubnets(IF_addr, [Subnet|SubnetsList]) ->
    %convert IF_addr to IP string
    IP_LIST = tuple_to_list(IF_addr),
    A = lists:flatten(io_lib:format("~p", [IP_LIST])),
    Subbed = lists:sublist(A,2,length(A)-2),
    IPString = lists:flatten(string:replace(Subbed,",",".",all)),
    % io:format("comparing ~p=~p~n",[IPString, Subnet]),
    IPMatch = lists:prefix(Subnet, IPString),
    case IPMatch of
        false -> isAddrInSubnets(IF_addr, SubnetsList);
        true -> IPString
    end.
