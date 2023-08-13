-module(utilities_handler).

-define(DATA_IDX , 2).

-export([init/2]).

init(Req0 , [MainServerPid]) ->
    {_,Body,_} = cowboy_req:read_body(Req0),
    io:format("Body: ~p~n" , [Body]) , 
    [UtilityName , IP , Port] = binary_to_term(Body),
    case UtilityName of
        nerlMonitor ->   
           Graph = gen_server:call(MainServerPid , getGraph),
           WorkersMap = ets:lookup_element(nerlnet_data , workers , ?DATA_IDX),
           WorkersClients = maps:to_list(WorkersMap),
           Workers = lists:flatten([atom_to_list(X)++"-"++atom_to_list(Y)++"!" || {X , Y} <- WorkersClients]),
           Reply = Graph ++ "," ++ Workers
    end,
    gen_server:cast(MainServerPid , {saveUtility , {UtilityName , IP , Port}}), %% TODO Add IP , Port to ets in main server record
    Req = cowboy_req:reply(200,
                            #{<<"content-type">> => <<"text/plain">>},
                            Reply,
                            Req0),
    {ok, Req, MainServerPid};

init(Req0 , [Action , MainServerPid]) -> 
    {_,Body,_} = cowboy_req:read_body(Req0),
    io:format("got kill-------------------messege: ~p~n",[Body]),
    case Action of
        worker_kill ->
            gen_statem:cast(MainServerPid , {worker_kill , Body});
        _ -> ok
    end,
    Reply = io_lib:format("ACK", []),
    Req = cowboy_req:reply(200,
                            #{<<"content-type">> => <<"text/plain">>},
                            Reply,
                            Req0),
    {ok, Req, MainServerPid}.
