%%%-------------------------------------------------------------------
%% @doc nerlGUI public API
%% @end
%%%-------------------------------------------------------------------

-module(nerlMonitor_app).

-behaviour(application).

-include("../../Communication_Layer/http_Nerlserver/src/nerl_tools.hrl").

-export([start/2, stop/1 , link_GUI/1]).

-define(UTILNAME,nerlMonitor).
-define(IP , "192.168.64.7").
-define(PORT, 8096).  %port place holder
-define(MSADDRES,"192.168.64.7:8080" ). %place holder
-define(GUI , {'PyrlangProcess' , 'py@127.0.0.1'}). % Erlang node should be long name to communicate with pyrlang node

start(_StartType, _StartArgs) ->
    application:start(sasl),
    application:start(ranch),
    application:start(inets),
    
    Dispatch = cowboy_router:compile([
        {'_', [
            {"/utilInfo",nerlMonitor_handler, [utilInfo]},
            {"/stats" , nerlMonitor_handler , [stats]}
        
        ]}
    ]),
    {ok, _} = cowboy:start_clear(?UTILNAME,[{port, ?PORT}],#{env => #{dispatch => Dispatch}}),
    io:format("nerlMonitor started , opening GUI...~n"),
    erlang:register(recvPyrlang , self()),
    _GUI_PID = spawn_link(?MODULE , link_GUI , [self()]) , %% PyrlangNode: ('PyralngProcess' , 'py@127.0.0.1' , 'COOKIE') , sending message by: 'GUI ! HELLO.'
    URL = "http://" ++ ?MSADDRES ++ "/toolConnectionReq",
    mainServerPing(URL,term_to_binary([?UTILNAME , ?IP , integer_to_list(?PORT)])), %% TODO How to "import" nerl_tools
    nerlMonitor_sup:start_link().



%ping main server in 0.5 sec intervals with connection request. will stop when got valid response.
mainServerPing(URL,Body)->   
    io:format("pinging main server~n"),  
    io:format("Body: ~p~n" , [Body]),
    Response = httpc:request(post,{URL, [],"application/x-www-form-urlencoded",Body}, [], []),
    case Response of
        {error,_}->
            timer:sleep(1000),
            mainServerPing(URL,Body);
        {ok,{_ResCode, _Headers, Data}}-> 
            io:format("Sending graph to GUI , graph is ~p~n" , [Data]),
            ?GUI ! {graph , Data} , 
            recvLoop();
        {ok , _} -> 
            io:format("got2 response from main server~n")
    end.


recvLoop()-> %% MainServer replies with Nerlnet-Graph when nerlMonitor tool is used
    receive 
        {terminate , WorkerName} -> 
            io:format("got termination for Worker ~p from GUI~n" , [WorkerName]),
            URL = "http://" ++ ?MSADDRES ++ "/worker_kill",
            Body = term_to_binary(WorkerName),
            httpc:request(post,{URL, [],"application/x-www-form-urlencoded",Body}, [], []);
        _ -> io:format("got unknown message from GUI~n")
    end,
    recvLoop().
    

stop(_State) ->
    ok.

link_GUI(Pid) ->
    os:cmd('python3 src/MonitorGUI.py'),
    io:format("GUI Closed~n").


