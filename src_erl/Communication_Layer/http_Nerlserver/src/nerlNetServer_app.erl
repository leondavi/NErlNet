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


%%%%local servers to open, AFTER parsing json, this is the maps will be created:
    MainServerHostPort = {"localhost",8080},
    MainName = mainServer,

    ClientHostPort = {"localhost",8081},
    Client1Name = client1,

    Worker1Name = worker1,
    Worker1Args = [asd,asd,asd,asd,asd,asd,asd],
    Worker2Name = worker2,
    Worker2Args = [dsa,dsa,dsa,dsa,dsa,dsa,dsa],

    Source1HostPort = {"localhost",8082},
    Source1Name = source1,

    Router1HostPort = {"localhost",8083},
    Router1Name = router1,

%%    for mainserver only-
    Clients = [Client1Name],
%%Server that should be established on this machine
    MainServerPortMap =  #{MainName => MainServerHostPort},
    SourcePortMap =  #{Source1Name => Source1HostPort},
    RoutersPortMap =  #{Router1Name => Router1HostPort},
    ClientsPortMap =  #{Client1Name => ClientHostPort},

    PortsMap = #{ mainServer => MainServerPortMap,clients => ClientsPortMap, sources => SourcePortMap,routers => RoutersPortMap},

%%    connectivity map will be as follow:
%%    name_atom of machine => {Host,Port} OR an atom router_name, indicating there is no direct http connection, and should pass request via router_name
    MainServerConnectionsMap = #{Client1Name => Router1HostPort, Source1Name => Router1HostPort},
    ClientConnectionsMap = #{MainName => Router1HostPort},
    RouterConnectionsMap_router1 = #{MainName => MainServerHostPort, Client1Name=>ClientHostPort, Source1Name=>Source1HostPort},
    SourceConnectionsMap = #{MainName => Router1HostPort, Client1Name=>Router1HostPort, Source1Name=>Router1HostPort},

    WorkersMap = #{Worker1Name => Client1Name, Worker2Name => Client1Name},
%%    each server gets the port map he will need inorder to make http requests. all requests are delivered via the router only
%%    TODO add separation - each entity receives its own portmap of routers

    %%Create a gen_Server for maintaining Database for Main Server.
    %% all http requests will be handled by Cowboy which updates main_genserver if necessary.
    MainGenServer_Args= {MainName,Clients,WorkersMap,MainServerConnectionsMap},        %%TODO change from mainserverport to routerport . also make this a list of client
    MainGenServerPid = mainGenserver:start_link(MainGenServer_Args),

    %%Create a gen_StateM machine for maintaining Database for Client.
    %% all http requests will be handled by Cowboy which updates client_statem if necessary.
    ClientStatemArgs= {Client1Name,WorkersMap,ClientConnectionsMap},        %%make this a list of client
    ClientStatemPid = clientStatem:start_link(ClientStatemArgs),

    %%Create a gen_StateM machine for maintaining Database for Source.
    %% all http requests will be handled by Cowboy which updates source_statem if necessary.
    SourceStatemArgs= {Source1Name,WorkersMap,SourceConnectionsMap},        %%TODO  make this a list of Sources
    SourceStatemPid = sourceStatem:start_link(SourceStatemArgs),

    %%Create a gen_Server for maintaining Database for Router.
    %% all http requests will be handled by Cowboy which updates router_genserver if necessary.
    RouterGenServerArgs= {Router1Name,RouterConnectionsMap_router1},        %%TODO  make this a list of Routers
    RouterGenServerPid = routerGenserver:start_link(RouterGenServerArgs),



%%    Creating a Dispatcher for each Server from JSONs architecture - this dispatchers will rout http requests to the right handler.
%%    Each dispatcher will be listening to a different PORT
%%    Handling http requests will be managed by *Handler.erl and additional information given inside.
    %%these are the handler for any kind of request.
    %% {HostMatch, list({PathMatch, Handler, InitialState})}
    %The last arg becomes the State
    %arg in the *Handler's init() method.
    %%{"/req_name/:arg1/:arg2",[{arg1,constrains}, {arg2,int}], addHandler,[]}

    %%%Main Server
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



    %%Nerl Client
    %%Dispatcher for cowboy to rout each given http_request for the matching handler
    NerlClientDispatch = cowboy_router:compile([
        {'_', [
            %%first http request from main server should be about starting parameters, find more info inside initHandler
            {"/init",clientStateHandler, [init,ClientStatemPid]},
            {"/training",clientStateHandler, [training,ClientStatemPid]},
            {"/clientIdle",clientStateHandler, [idle,ClientStatemPid]},
            {"/predict",clientStateHandler, [predict,ClientStatemPid]},
            {"/weightsVector",vectorHandler, [ClientStatemPid]},
            {"/[...]", noMatchingRouteHandler, [MainGenServerPid]}
        ]}
    ]),

%%    Source server
    SourceDispatch = cowboy_router:compile([
        {'_', [

            {"/updateCSV",csvHandler, [SourceStatemPid]},
            {"/startCasting",castingHandler, [startCasting,SourceStatemPid]},
            {"/stopCasting",castingHandler, [stopCasting,SourceStatemPid]},
            {"/[...]", noMatchingRouteHandler, [MainGenServerPid]}
        ]}
    ]),

    %%    Source server
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
            {"/stopCasting",routingHandler, [stopCasting,RouterGenServerPid]},
            {"/[...]", noMatchingRouteHandler, [RouterGenServerPid]}
        ]}
    ]),

    %% cowboy:start_clear(Name, TransOpts, ProtoOpts) - an http_listener
    %%An ok tuple is returned on success. It contains the pid of the top-level supervisor for the listener.
%%    TODO: after parsing json, change from *1HostPort to the original list imported from json
    [init_cowboy_start_clear(Name,HostPort,MainServerDispatcher)||{Name,HostPort}<-maps:to_list(maps:get(mainServer,PortsMap))],
    [init_cowboy_start_clear(Name,HostPort,NerlClientDispatch)||{Name,HostPort}<-maps:to_list(maps:get(clients,PortsMap))],
    [init_cowboy_start_clear(Name,HostPort,SourceDispatch)||{Name,HostPort}<-maps:to_list(maps:get(sources,PortsMap))],
    [init_cowboy_start_clear(Name,HostPort,RouterDispatch)||{Name,HostPort}<-maps:to_list(maps:get(routers,PortsMap))],

%%    start supervisor
    nerlNetServer_sup:start_link().

init_cowboy_start_clear(ListenerName,{_Host,Port},Dispatcher)->
%%    TODO check how to catch ! messages in listenerPid
    {ok, _listenerPid} = cowboy:start_clear(ListenerName,
        [{port,Port}], #{env => #{dispatch =>Dispatcher}}
    ).
stop(_State) ->
    ok.





%% internal functions
