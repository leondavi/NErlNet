%%%-------------------------------------------------------------------
%% @doc nerlnetApp public API
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

-module(nerlnetApp_app).

-behaviour(application).
-include("nerl_tools.hrl").

-export([start/2, stop/1]).

-import(nerlNIF,[nif_preload/0]).

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

welcome_print() -> 
    io:format(
"

    __       __            __  __                                              
    /  |  _  /  |          /  |/  |                                             
    $$ | / \\ $$ |  ______  $$ |$$ |  _______   ______   _____  ____    ______   
    $$ |/$  \\$$ | /      \\ $$ |$$ | /       | /      \\ /     \\/    \\  /      \\  
    $$ /$$$  $$ |/$$$$$$  |$$ |$$ |/$$$$$$$/ /$$$$$$  |$$$$$$ $$$$  |/$$$$$$  | 
    $$ $$/$$ $$ |$$    $$ |$$ |$$ |$$ |      $$ |  $$ |$$ | $$ | $$ |$$    $$ | 
    $$$$/  $$$$ |$$$$$$$$/ $$ |$$ |$$ \\_____ $$ \\__$$ |$$ | $$ | $$ |$$$$$$$$/  
    $$$/    $$$ |$$       |$$ |$$ |$$       |$$    $$/ $$ | $$ | $$ |$$       | 
    $$/      $$/  $$$$$$$/ $$/ $$/  $$$$$$$/  $$$$$$/  $$/  $$/  $$/  $$$$$$$/  
                                                                                
                                                                            
                                                                            
                                                         __                 
                                                        /  |                
                                                       _$$ |_     ______    
                                                      / $$   |   /      \\   
                                                      $$$$$$/   /$$$$$$  |  
                                                        $$ | __ $$ |  $$ |  
                                                        $$ |/  |$$ \\__$$ |  
                                                        $$  $$/ $$    $$/   
                                                         $$$$/   $$$$$$/    
                                                                            
                                                                            
                                                                            
             __    __  ________            __  __    __              __     
            /  \\  /  |/        |          /  |/  \\  /  |            /  |    
            $$  \\ $$ |$$$$$$$$/   ______  $$ |$$  \\ $$ |  ______   _$$ |_   
            $$$  \\$$ |$$ |__     /      \\ $$ |$$$  \\$$ | /      \\ / $$   |  
            $$$$  $$ |$$    |   /$$$$$$  |$$ |$$$$  $$ |/$$$$$$  |$$$$$$/   
            $$ $$ $$ |$$$$$/    $$ |  $$/ $$ |$$ $$ $$ |$$    $$ |  $$ | __ 
            $$ |$$$$ |$$ |_____ $$ |      $$ |$$ |$$$$ |$$$$$$$$/   $$ |/  |
            $$ | $$$ |$$       |$$ |      $$ |$$ | $$$ |$$       |  $$  $$/ 
            $$/   $$/ $$$$$$$$/ $$/       $$/ $$/   $$/  $$$$$$$/    $$$$/  
                                                                                                                                                
~n~n").

start(_StartType, _StartArgs) ->
    welcome_print(),
    %% setup the erlang logger for this module 
    nerl_tools:setup_logger(?MODULE),
    %% make sure nif can be loaded:
    nerlNIF:nif_preload(),
    ThisDeviceIP = nerl_tools:getdeviceIP(),
    ?LOG_INFO("Installed Erlang OTP: ~s (Supported from 25)",[erlang:system_info(otp_release)]),
    ?LOG_INFO(?LOG_HEADER++"This device IP: ~p~n", [ThisDeviceIP]),
    %Create a listener that waits for a message from python about the adresses of the wanted json

    createNerlnetInitiator(ThisDeviceIP),
    {ArchitectureAdderess,CommunicationMapAdderess} = waitForInit(),

    %Parse json and start nerlnet:
    ?LOG_INFO(?LOG_HEADER++"ArchitectureAdderess: ~p, CommunicationMapAdderess : ~p~n",[ArchitectureAdderess,CommunicationMapAdderess]),

    parseJsonAndStartNerlnet(ThisDeviceIP),
    nerlnetApp_sup:start_link().

waitForInit() ->
    receive 
        {jsonAddress, MSG} -> {_ArchitectureAdderess,_CommunicationMapAdderess} = MSG;
        Other -> ?LOG_WARNING(?LOG_HEADER++"Got bad message: ~p,~ncontinue listening for init Json~n",[Other]), waitForInit()
        after ?PYTHON_SERVER_WAITING_TIMEOUT_MS -> waitForInit()
    end.

createNerlnetInitiator(HostName) ->
    DefaultPort = ?NERLNET_INIT_PORT,
    PortAvailable = nerl_tools:port_available(DefaultPort),
    if
        PortAvailable ->
            NerlnetInitiatorDispatch = cowboy_router:compile([
                {'_', [

                    {"/updateJsonPath",jsonHandler, [self()]},
                    {"/isNerlnetDevice",iotHandler, [self()]}
                ]}
            ]),
            %% cowboy:start_clear(Name, TransOpts, ProtoOpts) - an http_listener
            %% An ok tuple is returned on success. It contains the pid of the top-level supervisor for the listener.
            init_cowboy_start_clear(nerlnetInitiator, {HostName,DefaultPort},NerlnetInitiatorDispatch);
        true -> ?LOG_NOTICE("Nerlnet uses port ~p and it has to be unused before running Nerlnet server!", [DefaultPort]),
                ?LOG_NOTICE("Find the process that uses port ~p using the command: lsof -i:~p",[DefaultPort, DefaultPort]),
                ?LOG_ERROR("Port ~p is being used - can not start (definition NERLNET_INIT_PORT in nerl_tools.hrl)", [DefaultPort])
    end.



parseJsonAndStartNerlnet(ThisDeviceIP) ->
    %% Entities to open on device from reading arch.json: 
    {ok, DCJsonPath} = file:read_file(?JSON_ADDR++?LOCAL_ARCH_FILE_NAME),
    {ok, CommunicationMapPath} = file:read_file(?JSON_ADDR++?LOCAL_COMM_FILE_NAME),

    %%TODO: ADD CHECK FOR VALID INPUT:  
    % ?LOG_NOTICE("IS THIS A JSON? ~p~n",[jsx:is_json(ArchitectureAdderessData)]),

    %%Decode Json to architecute map and Connection map:
    DCMap = jsx:decode(DCJsonPath,[]),
    CommunicationMap = jsx:decode(CommunicationMapPath,[]),
    
    jsonParser:parseJsons(DCMap,CommunicationMap,ThisDeviceIP), % we use nerlnet_data ETS from this point

    %%    Creating a Dispatcher for each Server from JSONs architecture - this dispatchers will rout http requests to the right handler.
    %%    Each dispatcher will be listening to a different PORT
    %%    Handling http requests will be managed by *Handler.erl and additional information given inside.
    %%    these are the handler for any kind of request.
    %%    {HostMatch, list({PathMatch, Handler, InitialState})}
    %%    The last arg becomes the State
    %%    arg in the *Handler's init() method.
    %%    {"/req_name/:arg1/:arg2",[{arg1,constrains}, {arg2,int}], addHandler,[]}
    %%    each server gets the port map he will need inorder to make http requests. all requests are delivered via the router only

    Routers = ets:lookup_element(nerlnet_data, routers, ?DATA_IDX), % router map format: {RouterName => RouterPort,RouterRouting,RouterFiltering}
    BatchSize = ets:lookup_element(nerlnet_data, batchSize,?DATA_IDX),
    DefaultFrequency = ets:lookup_element(nerlnet_data, frequency, ?DATA_IDX),

    createClientsAndWorkers(), % TODO extract all of this args from ETS
    createRouters(Routers,ThisDeviceIP), % TODO extract all of this args from ETS
    createSources(BatchSize, DefaultFrequency, ThisDeviceIP), % TODO extract all of this args from ETS

    HostOfMainServer = ets:member(nerlnet_data, mainServer),
    createMainServer(HostOfMainServer,BatchSize,ThisDeviceIP).

%% internal functions
port_validator(Port, EntityName) ->
    PortAvailable = nerl_tools:port_available(Port),
    if PortAvailable ->
        ok;
        true -> ?LOG_ERROR("Nerlnet entity: ~p uses port ~p and it must be free", [EntityName, Port]),
                ?LOG_ERROR("You can take the following steps:",[]),
                ?LOG_ERROR("1. Change port in DC file to free port",[]),
                ?LOG_ERROR("2. Find the process that uses port ~p using the command: lsof -i:~p and terminate it (Risky approach)",[Port, Port]),
                erlang:error("Port ~p is being used - cannot start")
    end.

createClientsAndWorkers() ->
    ClientsAndWorkers = ets:lookup_element(nerlnet_data, deviceClients, ?DATA_IDX), % Each element is  {Name,{Port,ClientWorkers,ClientWorkersMaps}}
    % WorkerToClientMap = ets:lookup_element(nerlnet_data, workers, ?DATA_IDX),
    % io:format("Starting clients and workers locally with: ~p~n",[ClientsAndWorkers]),
    DeviceName = ets:lookup_element(nerlnet_data, device_name, ?DATA_IDX),
    NerlnetGraph = ets:lookup_element(nerlnet_data, communicationGraph, ?DATA_IDX),
    ShaToModelArgsMap = ets:lookup_element(nerlnet_data, shaToModelArgsMap, ?DATA_IDX),
    Func = 
        fun({Client,{Port,ClientWorkers,WorkerShaMap, WorkerToClientMap}}) ->
        port_validator(Port, Client),
        ClientStatemArgs = {Client, NerlnetGraph, ClientWorkers , WorkerShaMap, WorkerToClientMap , ShaToModelArgsMap},
        ClientStatemPid = clientStatem:start_link(ClientStatemArgs),
        %%Nerl Client
        %%Dispatcher for cowboy to rout each given http_request for the matching handler
        NerlClientDispatch = cowboy_router:compile([
            {'_', [
                %%first http request from main server should be about starting parameters, find more info inside initHandler
                {"/custom_worker_message",clientStateHandler, [custom_worker_message,ClientStatemPid]},
                {"/statistics",clientStateHandler, [statistics,ClientStatemPid]},
                {"/clientTraining",clientStateHandler, [training,ClientStatemPid]},
                {"/clientIdle",clientStateHandler, [idle,ClientStatemPid]},
                {"/clientPredict",clientStateHandler, [predict,ClientStatemPid]},
                {"/batch",clientStateHandler, [batch,ClientStatemPid]}
            ]}
        ]),
        init_cowboy_start_clear(Client, {DeviceName, Port},NerlClientDispatch)
    end,
    lists:foreach(Func, ClientsAndWorkers).


    %% cowboy:start_clear(Name, TransOpts, ProtoOpts) - an http_listener
    %%An ok tuple is returned on success. It contains the pid of the top-level supervisor for the listener.

createSources(BatchSize, DefaultFrequency, HostName) ->
    DATA_IDX = 2,
    NerlnetGraph = ets:lookup_element(nerlnet_data, communicationGraph, DATA_IDX),
    WorkersMap = ets:lookup_element(nerlnet_data, workers, DATA_IDX),
    SourcesMap = ets:lookup_element(nerlnet_data, sources, DATA_IDX),
    Func = 
    fun(SourceName,{SourcePort,SourceMethod, CustomFrequency}) -> 
        % SourceStatemArgs = {SourceName, WorkersMap, NerlnetGraph, SourceMethod, BatchSize},        %%TODO  make this a list of Sources
        port_validator(SourcePort, SourceName),
        SourceStatemArgs = 
            case CustomFrequency of 
                none -> {SourceName, WorkersMap, NerlnetGraph, SourceMethod, BatchSize, DefaultFrequency};
                _ -> {SourceName, WorkersMap, NerlnetGraph, SourceMethod, BatchSize, CustomFrequency}
            end,
        %%Create a gen_StateM machine for maintaining Database for Source.
        %% all http requests will be handled by Cowboy which updates source_statem if necessary.
        SourceStatemPid = sourceStatem:start_link(SourceStatemArgs),
        %%    Source server
        SourceDispatch = cowboy_router:compile([
        {'_', [

            {"/updateCSV",castingHandler, [csv,SourceStatemPid]},
            {"/startCasting",castingHandler, [startCasting,SourceStatemPid]},
            {"/stopCasting",castingHandler, [stopCasting,SourceStatemPid]},
            {"/statistics",castingHandler, [statistics,SourceStatemPid]}
        ]}
        ]),

        %% cowboy:start_clear(Name, TransOpts, ProtoOpts) - an http_listener
        %%An ok tuple is returned on success. It contains the pid of the top-level supervisor for the listener.
        init_cowboy_start_clear(SourceName, {HostName,SourcePort},SourceDispatch)
    end,
    maps:map(Func, SourcesMap).


createRouters(MapOfRouters, HostName) ->
    NerlnetGraph = ets:lookup_element(nerlnet_data, communicationGraph, ?DATA_IDX),
    Func = 
    fun(RouterName, {Port, _RouterRouting, _RouterFiltering}) -> 
        %%Create a gen_Server for maintaining Database for Router.
        %% all http requests will be handled by Cowboy which updates router_genserver if necessary.
        %%    connectivity map will be as follow:
        %%    name_atom of machine => {Host,Port} OR an atom router_name, indicating there is no direct http connection, and should pass request via router_name
        port_validator(Port, RouterName),
        RouterGenServerArgs= {RouterName, NerlnetGraph},        %%TODO  make this a list of Routers
        RouterGenServerPid = routerGenserver:start_link(RouterGenServerArgs),

        RouterDispatch = cowboy_router:compile([
            {'_', [
                {"/pass",routingHandler, [pass,RouterGenServerPid,NerlnetGraph,RouterName]},
            
                {"/custom_worker_message",routingHandler, [custom_worker_message,RouterGenServerPid, NerlnetGraph,RouterName]},
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
                {"/batch",routingHandler, [rout,RouterGenServerPid]},
                {"/startCasting",routingHandler, [startCasting,RouterGenServerPid]},
                {"/stopCasting",routingHandler, [stopCasting,RouterGenServerPid]},
                {"/federatedWeightsVector",routingHandler, [federatedWeightsVector,RouterGenServerPid]},
                {"/federatedWeights",routingHandler, [federatedWeights,RouterGenServerPid]},
                {"/unicast",routingHandler, [unicast,RouterGenServerPid]},
                {"/broadcast",routingHandler, [broadcast,RouterGenServerPid]},

                %%GUI actions
                {"/getStats",routingHandler, [getStats,RouterGenServerPid]}
            ]}
        ]),
        %% cowboy:start_clear(Name, TransOpts, ProtoOpts) - an http_listener
        %%An ok tuple is returned on success. It contains the pid of the top-level supervisor for the listener.
        init_cowboy_start_clear(RouterName, {HostName,Port},RouterDispatch)
    end,
    maps:foreach(Func, MapOfRouters). % iterates as key/values

createMainServer(false,_BatchSize,_HostName) -> none;
createMainServer(true,BatchSize,HostName) ->
    Name = mainServer,
    {Port, _Args} = ets:lookup_element(nerlnet_data, mainServer, ?DATA_IDX),
    port_validator(Port, Name),
        
    Clients = ets:lookup_element(nerlnet_data, clients, ?DATA_IDX),  % format of maps: {ClientName => {Workers, Port}, ClientsMap}
    ClientsNames = maps:keys(Clients),
    WorkersMap = ets:lookup_element(nerlnet_data, workers, ?DATA_IDX),
    NerlnetGraph = ets:lookup_element(nerlnet_data, communicationGraph, ?DATA_IDX),
    %%Create a gen_Server for maintaining Database for Main Server.
    %% all http requests will be handled by Cowboy which updates main_genserver if necessary.
    MainGenServer_Args= {Name,ClientsNames,BatchSize,WorkersMap,NerlnetGraph},        %%TODO change from mainserverport to routerport . also make this a list of client
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
    init_cowboy_start_clear(Name, {HostName,Port}, MainServerDispatcher).


%% cowboy:init_cowboy_start_clear  start_clear(Name, TransOpts, ProtoOpts) - an http_listener
%% An ok tuple is returned on success. It contains the pid of the top-level supervisor for the listener.
init_cowboy_start_clear(ListenerName,{_Host,Port},Dispatcher)->
%%    TODO check how to catch ! messages in listenerPid
    {ok, _listenerPid} = cowboy:start_clear(ListenerName,
        [{port,Port}], #{env => #{dispatch =>Dispatcher}}
    ).

stop(_State) ->
    ok.
