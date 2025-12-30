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

-define(NERLNET_APP_VERSION, "1.6.0").
-define(NERLPLANNER_TESTED_VERSION,"1.0.4").
-define(START_LOG, "/tmp/nerlnet/logs/start/nerlnet_start.log").

-export([start/2, stop/1]).

start_log(Tag, Data) ->
    ok = ensure_start_log_dir(),
    file:write_file(?START_LOG, io_lib:format("~p: ~p~n", [Tag, Data]), [append]).

ensure_start_log_dir() ->
    case filelib:ensure_dir(?START_LOG) of
        ok -> ok;
        {error, Reason} ->
            %% Logging must never fail; fall back to stdout if we cannot create directories.
            io:format("Failed creating start log dir (~p): ~p~n", [?START_LOG, Reason]),
            ok
    end.


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
╔═══════════════════════════════════════════════════════════════════════════════╗
║                                                                               ║
║    \033[38;5;88m███╗   ██╗███████╗██████╗ ██╗     ███╗   ██╗███████╗████████╗\033[0m              ║
║    \033[38;5;88m████╗  ██║██╔════╝██╔══██╗██║     ████╗  ██║██╔════╝╚══██╔══╝\033[0m              ║
║    \033[38;5;88m██╔██╗ ██║█████╗  ██████╔╝██║     ██╔██╗ ██║█████╗     ██║\033[0m                 ║
║    \033[38;5;88m██║╚██╗██║██╔══╝  ██╔══██╗██║     ██║╚██╗██║██╔══╝     ██║\033[0m                 ║
║    \033[38;5;88m██║ ╚████║███████╗██║  ██║███████╗██║ ╚████║███████╗   ██║\033[0m                 ║
║    \033[38;5;88m╚═╝  ╚═══╝╚══════╝╚═╝  ╚═╝╚══════╝╚═╝  ╚═══╝╚══════╝   ╚═╝\033[0m                 ║
║                                                                               ║
║                   \033[1mDistributed Neural Network Framework\033[0m                        ║
║                               \033[38;5;88mVersion ~s\033[0m                                   ║
║                              \033[32mDevice is Ready\033[0m                                  ║
║                                                                               ║
╚═══════════════════════════════════════════════════════════════════════════════╝

~n", [?NERLNET_APP_VERSION]).

legal_print() ->
    io:format("Nerlnet OTP Application is given without any warranty.~n"),
    io:format("There is no commitiment or responsibility for results, damage, loss that can be caused by using this tool.~n"),
    io:format("Please review the license of Nerlnet on Github repository: "),
    io:format("www.github.com/leondavi/NErlNet~n"),
    io:format("You must cite Nerlnet if you use any of its tools for academic/commercial/any purpose.~n~n").

start(_StartType, _StartArgs) ->
    _ = file:delete(?START_LOG),
    start_log(starting, ok),
    safe_call(fun legal_print/0, after_legal_print),
    safe_call(fun welcome_print/0, after_welcome_print),
    %% setup the erlang logger for this module 
    setup_logger_with_trap(),
    ?LOG_INFO("Logger initialized for release start", []),
    %% make sure nif can be loaded:
    start_log(loading_nifs, start),
    ensure_nif_loaded(nerlNIF),
    ensure_nif_loaded(sourceNIF),
    start_log(nifs_ready, ok),
    ThisDeviceIP = nerl_tools:getdeviceIP(),
    start_log(device_ip, ThisDeviceIP),
    ?LOG_INFO("Nerlnet version ~s",[?NERLNET_APP_VERSION]),
    ?LOG_INFO("Nerlplanner tested version ~s",[?NERLPLANNER_TESTED_VERSION]),
    ?LOG_INFO("Installed Erlang OTP: ~s (Supported from 25)",[erlang:system_info(otp_release)]),
    ?LOG_INFO("This device IP: ~p~n", [ThisDeviceIP]),
    ?LOG_INFO("About to create initiator on port ~p", [?NERLNET_INIT_PORT]),
    os:cmd("nohup sh -c 'sleep 5 && echo hey > /tmp/detached.txt' &"), %% ** FOR FUTURE RESET FUNCTIONALITY **
    %Create a listener that waits for a message from python about the adresses of the wanted json

    createNerlnetInitiator(ThisDeviceIP),
    ?LOG_INFO("Initiator listener started", []),
    start_log(initiator_ready, ok),
    {ArchitectureAdderess,CommunicationMapAdderess} = waitForInit(),
    ?LOG_INFO("Init payload received", []),
    start_log(init_received, {ArchitectureAdderess, CommunicationMapAdderess}),

    %Parse json and start nerlnet:
    ?LOG_INFO("DC file local path: ~p",[binary_to_list(ArchitectureAdderess)]),
    ?LOG_INFO("Communication map file local path: ~p",[binary_to_list(CommunicationMapAdderess)]),

    parseJsonAndStartNerlnet(ThisDeviceIP),
    start_log(parsing_done, ok),
    nerlnetApp_sup:start_link().

waitForInit() ->
    receive 
        {jsonAddress, MSG} -> {_ArchitectureAdderess,_CommunicationMapAdderess} = MSG; % TODO GUY this is the case for main server which spread the message using http direct requests to devices
        Other -> ?LOG_WARNING(?LOG_HEADER++"Got bad message: ~p,~ncontinue listening for init Json~n",[Other]), waitForInit()
        after ?PYTHON_SERVER_WAITING_TIMEOUT_MS -> waitForInit()
    end.

createNerlnetInitiator(HostName) ->
    DefaultPort = ?NERLNET_INIT_PORT,
    start_log(initiator_probe, {HostName, DefaultPort}),
    PortAvailable = nerl_tools:port_available(DefaultPort),
    if
        PortAvailable ->
            start_log(initiator_port_available, DefaultPort),
            NerlnetInitiatorDispatch = cowboy_router:compile([
                {'_', [

                    {"/sendJsons",jsonHandler, [self()]}, % ApiServer triggers sendJsons action by sending a request to the main server device
                    {"/isNerlnetDevice",iotHandler, [self()]}
                ]}
            ]),
            %% cowboy:start_clear(Name, TransOpts, ProtoOpts) - an http_listener
            %% An ok tuple is returned on success. It contains the pid of the top-level supervisor for the listener.
            try init_cowboy_start_clear(nerlnetInitiator, {HostName,DefaultPort},NerlnetInitiatorDispatch) of
                {ok, _} ->
                    start_log(initiator_listener_ready, {HostName, DefaultPort}),
                    ok
            catch
                Class:Reason ->
                    start_log(initiator_listener_failed, {DefaultPort, {Class, Reason}}),
                    erlang:error({initiator_listener_failed, DefaultPort, {Class, Reason}})
            end;
        true ->
            start_log(initiator_port_conflict, DefaultPort),
            ?LOG_NOTICE("Nerlnet uses port ~p and it has to be unused before running Nerlnet server!", [DefaultPort]),
            ?LOG_NOTICE("Find the process that uses port ~p using the command: sudo fuser -k ~p/tcp",[DefaultPort, DefaultPort]),
            ?LOG_ERROR("Port ~p is being used - can not start (definition NERLNET_INIT_PORT in nerl_tools.hrl)", [DefaultPort])
    end.



parseJsonAndStartNerlnet(ThisDeviceIP) ->
    %% Entities to open on device from reading arch.json: 
    {ok, DCJsonFileBytes} = file:read_file(?JSON_ADDR++?LOCAL_DC_FILE_NAME),
    {ok, CommunicationMapFileBytes} = file:read_file(?JSON_ADDR++?LOCAL_COMM_FILE_NAME),

    %%TODO: ADD CHECK FOR VALID INPUT:  
    % ?LOG_NOTICE("IS THIS A JSON? ~p~n",[jsx:is_json(ArchitectureAdderessData)]),

    %%Decode Json to architecute map and Connection map:
    DCMap = jsx:decode(DCJsonFileBytes,[]),
    CommunicationMap = jsx:decode(CommunicationMapFileBytes,[]),
    
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
    % DefaultFrequency = ets:lookup_element(nerlnet_data, frequency, ?DATA_IDX), TODO nerlplanner already assigns default frequency to sources that demand it.

    createClientsAndWorkers(), % TODO extract all of this args from ETS
    createRouters(Routers,ThisDeviceIP), % TODO extract all of this args from ETS
    createSources(BatchSize, ThisDeviceIP), % TODO extract all of this args from ETS

    HostOfMainServer = ets:member(nerlnet_data, mainServer),
    ThisDeviceName = maps:get(ThisDeviceIP , ets:lookup_element(nerlnet_data , ipv4_to_devices , ?DATA_IDX)),
    DevicesMap = ets:lookup_element(nerlnet_data, devices_map , ?DATA_IDX), % format of key value pairs: DeviceName => {Host,Port}
    DevicesListWithoutMainServerDevice = maps:to_list(maps:remove(ThisDeviceName, DevicesMap)), %% Form: [{DeviceNameAtom , {IPv4, Entities}}, ..]
    createMainServer(HostOfMainServer,BatchSize,ThisDeviceIP , ThisDeviceName),
    if 
        HostOfMainServer -> 
            send_jsons_to_other_devices(DCJsonFileBytes, CommunicationMapFileBytes, DevicesListWithoutMainServerDevice);
        true -> ok % Other devices get here and notify the main server they're ready
    end,
    NerlnetGraph = ets:lookup_element(nerlnet_data, communicationGraph, ?DATA_IDX),
    {?MAIN_SERVER_ATOM , {MainServerIP , MainServerPort , _MainServerDeviceName}} = digraph:vertex(NerlnetGraph, ?MAIN_SERVER_ATOM),
    URL = "http://" ++ MainServerIP ++ ":" ++ integer_to_list(MainServerPort) ++ "/jsonReceived",
    httpc:request(post , {URL , [] , "application/x-www-form-urlencoded" , term_to_binary({ThisDeviceName , length(DevicesListWithoutMainServerDevice)})}, [], []).

send_jsons_to_other_devices(_DCJsonFileBytes, _CommunicationMapFileBytes, []) -> ?LOG_INFO("This experiment is running on a single device!",[]);
send_jsons_to_other_devices(DCJsonFileBytes, CommunicationMapFileBytes, DevicesList) ->
    TorchPayloads = gather_torch_payloads(),
    Fun = fun({DeviceNameAtom, {IPv4, _Entities}}) ->
        ?LOG_INFO("Sending jsons to ~p",[DeviceNameAtom]),
        URL = "http://" ++ IPv4 ++ ":" ++ integer_to_list(?NERLNET_INIT_PORT) ++ "/sendJsons",
        Boundary = "------WebKitFormBoundaryUscTgwn7KiuepIr1",
        ContentType = lists:concat(["multipart/form-data; boundary=", Boundary]),
        Fields = [],
        BaseFiles = [{?JSON_ADDR++?LOCAL_DC_FILE_NAME, ?LOCAL_DC_FILE_NAME, binary_to_list(DCJsonFileBytes)},
                     {?JSON_ADDR++?LOCAL_COMM_FILE_NAME, ?LOCAL_COMM_FILE_NAME, binary_to_list(CommunicationMapFileBytes)}],
        Files = BaseFiles ++ TorchPayloads,
        ReqBody = nerl_tools:format_multipart_formdata(Boundary, Fields, Files),
        ReqHeader = [{"Content-Length", integer_to_list(length(ReqBody))}],
        {ok, _} = httpc:request(post, {URL, ReqHeader, ContentType, ReqBody}, [], [])
    end,
    lists:foreach(Fun, DevicesList).

gather_torch_payloads() ->
    case catch ets:lookup_element(nerlnet_data, torch_models_map, ?DATA_IDX) of
        {'EXIT', _} -> [];
        TorchMap ->
            Payloads = lists:flatmap(fun build_torch_payload/1, maps:to_list(TorchMap)),
            case Payloads of
                [] -> [];
                _ ->
                    ?LOG_INFO("Replicating ~p Torch artifact(s) with distributed config", [length(Payloads)]),
                    Payloads
            end
    end.

build_torch_payload({_Sha, TorchMeta}) ->
    Path0 = maps:get(path, TorchMeta, ""),
    case string:trim(Path0) of
        "" -> [];
        Path ->
            AbsPath = filename:absname(Path),
            case file:read_file(AbsPath) of
                {ok, Bin} ->
                    [{Path, filename:basename(Path), binary_to_list(Bin)}];
                {error, Reason} ->
                    ?LOG_ERROR("Failed to read Torch model at ~p: ~p", [AbsPath, Reason]),
                    []
            end
    end.

%% internal functions
port_validator(Port, EntityName) ->
    PortAvailable = nerl_tools:port_available(Port),
    if PortAvailable ->
        ok;
        true -> ?LOG_ERROR("Nerlnet entity: ~p uses port ~p and it must be free", [EntityName, Port]),
                ?LOG_ERROR("You can take the following steps:",[]),
                ?LOG_ERROR("1. Change port in DC file to free port",[]),
                ?LOG_ERROR("2. Find the process that uses port ~p using the command: sudo fuser -k ~p/tcp and terminate it (Risky approach)",[Port, Port]),
                ?LOG_ERROR("3. Kill Erlang beam instances on this machine: sudo pkill beam"),
                erlang:error("Port ~p is being used - cannot start")
    end.

createClientsAndWorkers() ->
    ClientsAndWorkers = ets:lookup_element(nerlnet_data, deviceClients, ?DATA_IDX), % Each element is  {Name,{Port,ClientWorkers,ClientWorkersMaps}}
    % WorkerToClientMap = ets:lookup_element(nerlnet_data, workers, ?DATA_IDX),
    % io:format("Starting clients and workers locally with: ~p~n",[ClientsAndWorkers]),
    DeviceName = ets:lookup_element(nerlnet_data, device_name, ?DATA_IDX),
    NerlnetGraph = ets:lookup_element(nerlnet_data, communicationGraph, ?DATA_IDX),
    ShaToModelArgsMap = ets:lookup_element(nerlnet_data, sha_to_models_map, ?DATA_IDX),
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
                {"/batch",clientStateHandler, [batch,ClientStatemPid]},
                {"/worker_to_worker_msg",clientStateHandler, [worker_to_worker_msg,ClientStatemPid]},
                {"/start_stream", clientStateHandler, [start_stream, ClientStatemPid]},
                {"/end_stream", clientStateHandler, [end_stream, ClientStatemPid]}
            ]}
        ]),
        init_cowboy_start_clear(Client, {DeviceName, Port},NerlClientDispatch)
    end,
    lists:foreach(Func, ClientsAndWorkers).


    %% cowboy:start_clear(Name, TransOpts, ProtoOpts) - an http_listener
    %%An ok tuple is returned on success. It contains the pid of the top-level supervisor for the listener.

createSources(BatchSize, HostName) ->
    DATA_IDX = 2,
    NerlnetGraph = ets:lookup_element(nerlnet_data, communicationGraph, DATA_IDX),
    WorkersMap = ets:lookup_element(nerlnet_data, workers, DATA_IDX),
    SourcesMap = ets:lookup_element(nerlnet_data, sources, DATA_IDX),
    Func = 
    fun(SourceName,{SourcePort,SourcePolicy,SourceFrequency,SourceEpochs,SourceType}) -> 
        % SourceStatemArgs = {SourceName, WorkersMap, NerlnetGraph, SourceMethod, BatchSize},        %%TODO  make this a list of Sources
        port_validator(SourcePort, SourceName),
        SourceStatemArgs = {SourceName, WorkersMap, NerlnetGraph, SourcePolicy, BatchSize, SourceFrequency , SourceEpochs, SourceType},        %%TODO  make this a list of Sources
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
    fun(RouterName, {Port, Policy}) -> 
        %%Create a gen_Server for maintaining Database for Router.
        %% all http requests will be handled by Cowboy which updates router_genserver if necessary.
        %%    connectivity map will be as follow:
        %%    name_atom of machine => {Host,Port} OR an atom router_name, indicating there is no direct http connection, and should pass request via router_name
        port_validator(Port, RouterName),
        RouterGenServerArgs= {RouterName , Policy , NerlnetGraph},        %%TODO  make this a list of Routers
        RouterGenServerPid = routerGenserver:start_link(RouterGenServerArgs),

        RouterDispatch = cowboy_router:compile([
            {'_', [
                {"/unicast",routingHandler, [unicast,RouterGenServerPid]},
                {"/broadcast",routingHandler, [broadcast,RouterGenServerPid]},
                {"/statistics",routingHandler, [statistics,RouterGenServerPid]}
            ]}
        ]),
        %% cowboy:start_clear(Name, TransOpts, ProtoOpts) - an http_listener
        %%An ok tuple is returned on success. It contains the pid of the top-level supervisor for the listener.
        init_cowboy_start_clear(RouterName, {HostName,Port},RouterDispatch)
    end,
    maps:foreach(Func, MapOfRouters). % iterates as key/values

createMainServer(false,_BatchSize,_HostName,_DeviceName) -> none;
createMainServer(true,BatchSize,HostName,DeviceName) ->
    Name = mainServer,
    {Port, _Args} = ets:lookup_element(nerlnet_data, mainServer, ?DATA_IDX),
    port_validator(Port, Name),
        
    Clients = ets:lookup_element(nerlnet_data, clients, ?DATA_IDX),  % format of maps: {ClientName => {Workers, Port}, ClientsMap}
    ClientsNames = maps:keys(Clients),
    WorkersMap = ets:lookup_element(nerlnet_data, workers, ?DATA_IDX),
    NerlnetGraph = ets:lookup_element(nerlnet_data, communicationGraph, ?DATA_IDX),
    %%Create a gen_Server for maintaining Database for Main Server.
    %% all http requests will be handled by Cowboy which updates main_genserver if necessary.
    MainGenServer_Args= {Name,ClientsNames,BatchSize,WorkersMap,NerlnetGraph,DeviceName},        %%TODO change from mainserverport to routerport . also make this a list of client
    MainGenServerPid = mainGenserver:start_link(MainGenServer_Args),
    
    MainServerDispatcher = cowboy_router:compile([
    {'_', [
        %Nerlnet actions
        {"/jsonReceived",[],ackHandler,[jsonReceived,MainGenServerPid]},
        {"/updateCSV",[],initHandler,[MainGenServerPid]},
        {"/lossFunction",[],actionHandler,[lossFunction,MainGenServerPid]},
        {"/predictRes",[],actionHandler,[predictRes,MainGenServerPid]},
        {"/dataReady",[],ackHandler,[dataReady,MainGenServerPid]},
        {"/sourceDone",[],ackHandler,[sourceDone,MainGenServerPid]},
        {"/clientReady",[],ackHandler,[clientAck,MainGenServerPid]}, % when client is ready for phase
        {"/clientsTraining",[],actionHandler,[clientsTraining,MainGenServerPid]},
        {"/statistics",[],actionHandler,[statistics,MainGenServerPid]},
        {"/clientsPredict",[],actionHandler,[clientsPredict,MainGenServerPid]},
        {"/startCasting",[],actionHandler, [startCasting, MainGenServerPid]},
        {"/stopCasting",[],actionHandler, [stopCasting, MainGenServerPid]},
        {"/clientsPhaseUpdate",[],actionHandler,[clientsPhaseUpdate,MainGenServerPid]},
        {"/apiserver_ack_validation", [], ackHandler, [apiserver_ack_validation, MainGenServerPid]},

        {"/[...]", [],noMatchingRouteHandler, [MainGenServerPid]}
        ]}
        ]),
    %% cowboy:start_clear(Name, TransOpts, ProtoOpts) - an http_listener
    %%An ok tuple is returned on success. It contains the pid of the top-level supervisor for the listener.
    init_cowboy_start_clear(Name, {HostName,Port}, MainServerDispatcher).


%% cowboy:init_cowboy_start_clear  start_clear(Name, TransOpts, ProtoOpts) - an http_listener
%% An ok tuple is returned on success. It contains the pid of the top-level supervisor for the listener.
init_cowboy_start_clear(ListenerName,{_Host,Port},Dispatcher)->
    %% Wrap cowboy:start_clear/3 so we can capture and persist any failure reasons.
    try cowboy:start_clear(ListenerName, [{port,Port}], #{env => #{dispatch => Dispatcher}}) of
        {ok, ListenerPid} ->
            start_log({listener_ready, ListenerName}, Port),
            {ok, ListenerPid}
    catch
        Class:Reason ->
            start_log({listener_failed, ListenerName}, {Port, {Class, Reason}}),
            ?LOG_ERROR("Listener ~p failed to bind port ~p: ~p", [ListenerName, Port, {Class, Reason}]),
            erlang:error({listener_failed, ListenerName, Port, {Class, Reason}})
    end.

stop(_State) ->
    ok.

ensure_nif_loaded(Module) ->
    start_log({nif_loading, Module}, start),
    case catch Module:nif_preload() of
        done ->
            start_log({nif_loading, Module}, ok),
            ok;
        {'EXIT', Reason} ->
            start_log({nif_failed, Module}, Reason),
            erlang:error({nif_load_failed, Module, Reason})
    end.

setup_logger_with_trap() ->
    case catch nerl_tools:setup_logger(?MODULE) of
        ok ->
            start_log(logger_ready, ok);
        {'EXIT', Reason} ->
            start_log(logger_failed, Reason),
            erlang:error({logger_setup_failed, Reason})
    end.

safe_call(Fun, Tag) ->
    Parent = self(),
    Worker = spawn(fun() -> Parent ! {self(), catch Fun()} end),
    receive
        {Worker, {'EXIT', Reason}} ->
            start_log({Tag, failed}, Reason);
        {Worker, _} ->
            start_log(Tag, ok)
    after 2000 ->
            start_log({Tag, timeout}, ok),
            exit(Worker, kill)
    end.
