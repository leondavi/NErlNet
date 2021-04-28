%%%-------------------------------------------------------------------
%% @doc http_Nerlserver public API
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

-module(http_Nerlserver_app).

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
    MainServerPort = 8080,
    NerlClientPort = 8081,
    SourcePort = 8082,
    RouterPort = 8083,


    %%Create a gen_Server for maintaining Database for Main Server.
    %% all http requests will be handled by Cowboy which updates main_genserver if necessary.
    Main_genServer_Args= {self(),MainServerPort},        %%TODO change from mainserverport to routerport . also make this a list of client
    Main_genServer_Pid = main_genserver:start_link(Main_genServer_Args),



    %%Create a gen_StateM machine for maintaining Database for Client.
    %% all http requests will be handled by Cowboy which updates Client_StateM if necessary.
    Client_StateM_Args= {self(),MainServerPort},        %%make this a list of client
    Client_StateM_Pid = client_statem:start_link(Client_StateM_Args),

    Source_StateM_Args= {self(),SourcePort},        %%TODO  make this a list of Sources
    Source_StateM_Pid = source_statem:start_link([self(),Source_StateM_Args]),

    Router_genServer_Args= {self(),RouterPort},        %%TODO  make this a list of Routers
    Router_genServer_Pid = source_statem:start_link([self(),Router_genServer_Args]),



%%    Creating a Dispatcher for each Server from JSONs architecture - this dispatchers will rout http requests to the right handler.
%%    Each dispatcher will be listening to a different PORT
%%    Handling http requests will be managed by *_handler.erl and additional information given inside.
    %%these are the handler for any kind of request.
    %% {HostMatch, list({PathMatch, Handler, InitialState})}
    %The last arg becomes the State
    %arg in the *_handler's init() method.
    %%{"/req_name/:arg1/:arg2",[{arg1,constrains}, {arg2,int}], add_handler,[]}

    %%%Main Server
    MainServerDispatcher = cowboy_router:compile([
        {'_', [

            {"/initNerlnet",[],init_handler,[Main_genServer_Pid]},
            {"/start_training",action_handler, [Main_genServer_Pid,start]},
            {"/stop_training",action_handler, [Main_genServer_Pid,stop]},
            {"/[...]", no_matching_route_handler, [Main_genServer_Pid]}
        ]}
    ]),



    %%Nerl Client
    %%Dispatcher for cowboy to rout each given http_request for the matching handler
    NerlClientDispatch = cowboy_router:compile([
        {'_', [
            %%these are the handler for any kind of request. aditional infomation given inside *_handler.erl
            %% {HostMatch, list({PathMatch, Handler, InitialState})}
            %%%%%%%%The last arg becomes the State%%%%%%%%
            %arg in the *_handler's init() method.
            %%{"/req_name/:arg1/:arg2",[{arg1,constrains}, {arg2,int}], add_handler,[initial_state]}

            %%first http request from main server should be about starting parameters, find more info inside init_handler
            {"/init",client_init_handler, [Client_StateM_Pid]},
            {"/weights_vector",vector_handler, [Client_StateM_Pid]},
            {"/[...]", no_matching_route_handler, [Main_genServer_Pid]}
        ]}
    ]),

    SourceDispatch = cowboy_router:compile([
        {'_', [
            %%these are the handler for any kind of request. aditional infomation given inside *_handler.erl
            %% {HostMatch, list({PathMatch, Handler, InitialState})}
            %The last arg becomes the State
            %arg in the *_handler's init() method.
            %%{"/req_name/:arg1/:arg2",[{arg1,constrains}, {arg2,int}], add_handler,[]}
            {"/updateCSV",csv_handler, [Source_StateM_Pid,Client_StateM_Pid]},
            {"/start_training",casting_handler, [Source_StateM_Pid,Client_StateM_Pid,start]},
            {"/stop_training",casting_handler, [Source_StateM_Pid,Client_StateM_Pid,stop]},
            {"/[...]", no_matching_route_handler, [Main_genServer_Pid]}
        ]}
    ]),

    %% cowboy:start_clear(Name, TransOpts, ProtoOpts) - an http_listener
    %%An ok tuple is returned on success. It contains the pid of the top-level supervisor for the listener.TODO check how to catch ! messages in listenerPid
    {ok, _listenerPid} = cowboy:start_clear(main_http_listener,
        [{port,MainServerPort}], #{env => #{dispatch =>MainServerDispatcher}}
    ),

    {ok, _} = cowboy:start_clear(client_http_listener,
        [{port,NerlClientPort}], #{env => #{dispatch =>NerlClientDispatch}}
    ),

    {ok, _} = cowboy:start_clear(http_listener,
        [{port,SourcePort}], #{env => #{dispatch =>SourceDispatch}}
    ),

    http_Nerlserver_sup:start_link().



stop(_State) ->
    ok.





%% internal functions
