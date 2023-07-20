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
    %GUI start
    URL = "http://" ++ ?MSADDRES ++ "/toolConnectionReq",
    mainServerPing(URL,{?UTILLNAME,functions()}).


%ping main server in 0.5 sec intervals with connection request. will stop when got valid response.
mainServerPing(URL,Body)->      
    Response=httpc:request(post,{URL, [],"application/x-www-form-urlencoded",Body}, [], []),
    case Response of
        {error,_}->
            timer:sleep(500),
            mainServerPing(URL,Body);
        {ok,{_ResCode, _Headers, Data}}->
            initInfoProc(Data)
    end.


initInfoProc(Body)->
    DevicesInfo = string:split(Body, "#", all),
    Devices = [string:split(DeviceInfo, ",", all) || DeviceInfo <- DevicesInfo, DevicesInfo /=[[]]],
    Edges = lists:droplast(lists:last(Devices)),
    DeviceList = lists:droplast(Devices),
    {DeviceList,Edges}. %need to send to gui for display

functions()->
    F1 = fun
            F(EtsRef) -> 
                WorkersNames = ets:lookup(EtsRef,workersNames),
                WorkerPids = [element(2,ets:lookup(EtsRef,Name))|| Name<-WorkersNames],

                lists:foreach(fun(Pid)->monitor(process , Pid)end , WorkerPids),
                
            F2 = fun
                     FF(EtsRef) ->
                        receive
                            {'DOWN',_,process,DownPid,Reason}->
                                
        end.
    

stop(_State) ->
    ok.


