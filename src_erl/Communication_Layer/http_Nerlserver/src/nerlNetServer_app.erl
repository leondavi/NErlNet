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

-export([start/2, stop/1]).

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
%%    HostName = getHostName(),
    HostName = "127.0.0.1",
    io:format("My HostName: ~p~n",[list_to_binary(HostName)]),

    %%Server that should be established on this machine from JSON architecture:
    {MainServer,_ServerAPI,ClientsAndWorkers, {Sources,WorkersMap},Routers} = jsonParser:getDeviceEntities("./input/jsonArch1PC.json",list_to_binary(HostName)),
    ChunkSize = 2,

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
    createMainServer(MainServer,HostName),
    createRouters(Routers,HostName),
    createSources(Sources,WorkersMap, ChunkSize, HostName),
%%    Worker1Args = {[3,2,1],0.01,[0,2,0],6,0,3,1,ChunkSize},
%%    Worker1Args = {[561,280,140,70,35,17,8,4,2,1],0.01,[0,2,2,2,2,2,2,2,2,0],6,0,561,1,ChunkSize},
%%    Worker2Args = {[3,2,1],0.01,[0,2,0],6,1,3,1,ChunkSize},
%%    Worker2Args = {[561,280,140,70,35,17,8,4,2,1],0.01,[0,2,2,2,2,2,2,2,2,0],6,1,561,1,ChunkSize},

%%    start supervisor
    nerlNetServer_sup:start_link().


%% internal functions

createClientsAndWorkers(none,_HostName) -> none;
createClientsAndWorkers([], _HostName) -> okdone;
createClientsAndWorkers([{ClientArgs,WorkersArgs,ClientConnectionsMap}|ClientsAndWorkers],HostName) ->
    ClientName = list_to_atom(binary_to_list(maps:get(<<"name">>,ClientArgs))),
    Port = list_to_integer(binary_to_list(maps:get(<<"port">>,ClientArgs))),

    %%Create a gen_StateM machine for maintaining Database for Client.
    %% all http requests will be handled by Cowboy which updates client_statem if necessary.
%%    WorkersArgsMap = #{Worker1Name => Worker1Args, Worker2Name => Worker2Args},
    ClientStatemArgs= {ClientName,WorkersArgs,ClientConnectionsMap},        %%make this a list of client
    ClientStatemPid = clientStatem:start_link(ClientStatemArgs),


    %%Nerl Client
    %%Dispatcher for cowboy to rout each given http_request for the matching handler
    NerlClientDispatch = cowboy_router:compile([
        {'_', [
            %%first http request from main server should be about starting parameters, find more info inside initHandler
            {"/init",clientStateHandler, [init,ClientStatemPid]},
            {"/clientTraining",clientStateHandler, [training,ClientStatemPid]},
            {"/clientIdle",clientStateHandler, [idle,ClientStatemPid]},
            {"/clientPredict",clientStateHandler, [predict,ClientStatemPid]},
            {"/weightsVector",vectorHandler, [ClientStatemPid]}
        ]}
    ]),

    %% cowboy:start_clear(Name, TransOpts, ProtoOpts) - an http_listener
    %%An ok tuple is returned on success. It contains the pid of the top-level supervisor for the listener.
    init_cowboy_start_clear(ClientName, {HostName,Port},NerlClientDispatch),
    createClientsAndWorkers(ClientsAndWorkers,HostName).

createSources(none,_WorkersMap,_ChunkSize,_HostName) -> none;
createSources([],_WorkersMap,_ChunkSize,_HostName) -> okdone;
createSources([{SourceArgs,SourceConnectionsMap}|Sources],WorkersMap,ChunkSize,HostName) ->
    SourceName = list_to_atom(binary_to_list(maps:get(<<"name">>,SourceArgs))),
    Port = list_to_integer(binary_to_list(maps:get(<<"port">>,SourceArgs))),

    %%Create a gen_StateM machine for maintaining Database for Source.
    %% all http requests will be handled by Cowboy which updates source_statem if necessary.
    SourceStatemArgs= {SourceName,WorkersMap,SourceConnectionsMap,ChunkSize},        %%TODO  make this a list of Sources
    SourceStatemPid = sourceStatem:start_link(SourceStatemArgs),


    %%    Source server
    SourceDispatch = cowboy_router:compile([
        {'_', [

            {"/updateCSV",csvHandler, [SourceStatemPid]},
            {"/startCasting",castingHandler, [startCasting,SourceStatemPid]},
            {"/stopCasting",castingHandler, [stopCasting,SourceStatemPid]}
        ]}
    ]),
    %% cowboy:start_clear(Name, TransOpts, ProtoOpts) - an http_listener
    %%An ok tuple is returned on success. It contains the pid of the top-level supervisor for the listener.
    init_cowboy_start_clear(SourceName, {HostName,Port},SourceDispatch),
    createSources(Sources,WorkersMap,ChunkSize,HostName).



createRouters(none,_HostName) -> none;
createRouters([],_HostName) -> okdone;
createRouters([{RouterArgs,RouterCommectopmsMap}|Routers],HostName) ->
    RouterName = maps:get(<<"name">>,RouterArgs),
    Port =list_to_integer(binary_to_list( maps:get(<<"port">>,RouterArgs))),

    %%Create a gen_Server for maintaining Database for Router.
    %% all http requests will be handled by Cowboy which updates router_genserver if necessary.
    %%    connectivity map will be as follow:
    %%    name_atom of machine => {Host,Port} OR an atom router_name, indicating there is no direct http connection, and should pass request via router_name

    RouterGenServerArgs= {RouterName,RouterCommectopmsMap},        %%TODO  make this a list of Routers
    RouterGenServerPid = routerGenserver:start_link(RouterGenServerArgs),


    RouterDispatch = cowboy_router:compile([
        {'_', [
            {"/clientIdle",routingHandler, [clientIdle,RouterGenServerPid]},
            {"/clientTraining",routingHandler, [clientTraining,RouterGenServerPid]},
            {"/clientPredict",routingHandler, [clientPredict,RouterGenServerPid]},
            {"/updateCSV",routingHandler, [updateCSV,RouterGenServerPid]},
            {"/csvReady",routingHandler, [csvReady,RouterGenServerPid]},
            {"/sourceDone",routingHandler, [sourceDone,RouterGenServerPid]},
            {"/clientReady",routingHandler, [clientReady,RouterGenServerPid]},
            {"/weightsVector",routingHandler, [rout,RouterGenServerPid]},
            {"/startCasting",routingHandler, [startCasting,RouterGenServerPid]},
            {"/stopCasting",routingHandler, [stopCasting,RouterGenServerPid]}
        ]}
    ]),
    %% cowboy:start_clear(Name, TransOpts, ProtoOpts) - an http_listener
    %%An ok tuple is returned on success. It contains the pid of the top-level supervisor for the listener.
   init_cowboy_start_clear(RouterName, {HostName,Port},RouterDispatch),
    createRouters(Routers,HostName).



createMainServer(none,_HostName) -> none;
createMainServer({[MainServerArgsMap],MainServerConnectionsMap,WorkersMap,ClientsNames},HostName) ->
    MainName = mainServer,
    Port = list_to_integer(binary_to_list(maps:get(<<"port">>,MainServerArgsMap))),

    %%Create a gen_Server for maintaining Database for Main Server.
    %% all http requests will be handled by Cowboy which updates main_genserver if necessary.
    MainGenServer_Args= {MainName,ClientsNames,WorkersMap,MainServerConnectionsMap},        %%TODO change from mainserverport to routerport . also make this a list of client
    MainGenServerPid = mainGenserver:start_link(MainGenServer_Args),

    MainServerDispatcher = cowboy_router:compile([
    {'_', [

    {"/updateCSV",[],initHandler,[MainGenServerPid]},
    {"/csvReady",[],ackHandler,[source,MainGenServerPid]},
    {"/sourceDone",[],ackHandler,[sourceDone,MainGenServerPid]},
    {"/clientReady",[],ackHandler,[client,MainGenServerPid]},
    {"/clientsTraining",[],actionHandler,[clientsTraining,MainGenServerPid]},
    {"/clientsPredict",[],actionHandler,[clientsPredict,MainGenServerPid]},
    {"/startCasting",actionHandler, [startCasting, MainGenServerPid]},
    {"/stopCasting",actionHandler, [stopCasting, MainGenServerPid]},
    {"/[...]", noMatchingRouteHandler, [MainGenServerPid]}
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


%%getHostName() ->
%%    {ok, L} = inet:getif(),
%%    IP = tuple_to_list(element(1, hd(L))),
%%    A = lists:flatten(io_lib:format("~p", [IP])),
%%    Subbed = lists:sublist(A,2,length(A)-2),
%%    lists:flatten(string:replace(Subbed,",",".",all)).
%%
%%

stop(_State) ->
    ok.
