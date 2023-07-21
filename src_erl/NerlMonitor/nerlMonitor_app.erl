%%%-------------------------------------------------------------------
%% @doc nerlGUI public API
%% @end
%%%-------------------------------------------------------------------

-module(nerlMonitor_app).

-behaviour(application).

-export([start/2, stop/1]).

-define(UTILLNAME,nerlMonitor ).
-define(PORT,8096 ).  %port place holder
-define(MSADDRES,"ip:port" ). %place holder
-define(GUI , {'PyrlangProcess' , 'py@127.0.0.1'}). % Erlang node should be long name to communicate with pyrlang node

start(_StartType, _StartArgs) ->
    application:start(sasl),
    application:start(ranch),
    application:start(inets),
    
    Dispatch = cowboy_router:compile([
        {'_', [
            {"/utillInfo",nerlUtill_info_handler, []}
        
        ]}
    ]),
    {ok, _} = cowboy:start_clear(?UTILLNAME,[{port, ?PORT}],#{env => #{dispatch => Dispatch}}),
    os:cmd('python3 MonitorGUI.py'), %% PyrlangNode: ('PyralngProcess' , 'py@127.0.0.1' , 'COOKIE') , sending message by: 'GUI ! HELLO.'
    URL = "http://" ++ ?MSADDRES ++ "/toolConnectionReq",
    mainServerPing(URL,[?UTILLNAME,functions(),nerl_tools:getdeviceIP() , ?PORT]). %% TODO How to "import" nerl_tools


%ping main server in 0.5 sec intervals with connection request. will stop when got valid response.
mainServerPing(URL,Body)->      
    Response = httpc:request(post,{URL, [],"application/x-www-form-urlencoded",Body}, [], []),
    case Response of
        {error,_}->
            timer:sleep(500),
            mainServerPing(URL,Body);
        {ok,{_ResCode, _Headers, Data}}-> 
            initInfoProc(Data)
    end.


initInfoProc(Body)-> %% MainServer replies with Nerlnet-Graph when nerlMonitor tool is used
    ?GUI ! {graph , Body}.


%functionalty of the tool, will create a list of tuples when each tuple is {entity that will run the func (atom),function itself {func)}
functions()->ok.
    

stop(_State) ->
    ok.


