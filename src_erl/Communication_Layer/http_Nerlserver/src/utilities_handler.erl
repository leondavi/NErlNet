-module(utilities_handler).

-export([init/2]).

init(Req0 , [Pid]) ->
    {_,Body,_} = cowboy_req:read_body(Req0),
    [UtilityName , _Funcs , IP , Port] = binary_to_list(Body),
    case UtilityName of
        nerlMonitor -> 
           {reply , Reply , _ } = gen_server:call(Pid , getGraph) 

    end,
    gen_server:cast(), %% TODO Add IP , Port to ets in main server record
    Req = cowboy_req:reply(200,
                            #{<<"content-type">> => <<"text/plain">>},
                            Reply,
                            Req0),
    {ok, Req, Pid}.
