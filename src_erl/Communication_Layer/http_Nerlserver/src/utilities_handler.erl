-module(utilities_handler).

-define(DATA_IDX , 2).

-export([init/2]).

init(Req0 , [MainServerPid]) ->
    {_,Body,_} = cowboy_req:read_body(Req0),
    [UtilityName , _Funcs , IP , Port] = binary_to_list(Body),
    case UtilityName of
        nerlMonitor -> 
           {reply , Graph , _ } = gen_server:call(MainServerPid , getGraph),
           WorkersMap = ets:lookup_element(nerlnet_data , workers , ?DATA_IDX),
           Reply = {Graph , WorkersMap}
    end,
    gen_server:cast(MainServerPid , {saveUtility , {UtilityName , IP , Port}}), %% TODO Add IP , Port to ets in main server record
    Req = cowboy_req:reply(200,
                            #{<<"content-type">> => <<"text/plain">>},
                            Reply,
                            Req0),
    {ok, Req, MainServerPid}.
