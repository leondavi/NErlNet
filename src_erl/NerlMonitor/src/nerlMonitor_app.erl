%%%-------------------------------------------------------------------
%% @doc nerlGUI public API
%% @end
%%%-------------------------------------------------------------------

-module(nerlMonitor_app).

-behaviour(application).

-include("../../Communication_Layer/http_Nerlserver/src/nerl_tools.hrl").

-export([start/2, stop/1]).

-define(UTILLNAME,nerlMonitor).
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
            {"/utillInfo",nerlMonitor_handler, []}
        
        ]}
    ]),
    {ok, _} = cowboy:start_clear(?UTILLNAME,[{port, ?PORT}],#{env => #{dispatch => Dispatch}}),
    io:format("nerlMonitor started , opening GUI...~n"),
    %%os:cmd('python3 src/MonitorGUI.py'), %% PyrlangNode: ('PyralngProcess' , 'py@127.0.0.1' , 'COOKIE') , sending message by: 'GUI ! HELLO.'
    URL = "http://" ++ ?MSADDRES ++ "/toolConnectionReq",
    mainServerPing(URL,[list_to_binary(atom_to_list(?UTILLNAME) ++ "#") , list_to_binary(?IP ++ "#") , list_to_binary(integer_to_list(?PORT))]), %% TODO How to "import" nerl_tools
    nerlMonitor_sup:start_link().



%ping main server in 0.5 sec intervals with connection request. will stop when got valid response.
mainServerPing(URL,Body)->   
    io:format("pinging main server~n"),   
    Response = httpc:request(post,{URL, [],"application/x-www-form-urlencoded",Body}, [], []),
    case Response of
        {error,_}->
            timer:sleep(500),
            mainServerPing(URL,Body);
        {ok,{_ResCode, _Headers, Data}}-> 
            io:format("got1 response from main server~n"),
            initInfoProc(Data);
        {ok , _} -> 
            io:format("got2 response from main server~n")
    end.


initInfoProc(Body)-> %% MainServer replies with Nerlnet-Graph when nerlMonitor tool is used
    io:format("Sending graph to GUI , graph is ~p~n" , [Body]),
    ?GUI ! {graph , Body}.


%functionalty of the tool, will create a list of tuples when each tuple is {entity that will run the func (atom),function itself {func)}
functions()-> ok.
    

stop(_State) ->
    ok.


