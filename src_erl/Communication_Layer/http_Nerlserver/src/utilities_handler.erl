-module(utilities_handler).

-define(DATA_IDX , 2).

-export([init/2]).

init(Req0 , [MainServerPid]) ->
    io:format("Req is ~p~n" , [Req0]),
    {_,Body,_} = cowboy_req:read_body(Req0),
    io:format("Body: ~p~n" , [Body]) , 
    Formatted = binary_to_list(Body),
    [UtilityName , IP , Port] = [X || X <- re:split(Formatted , "#" , [{return , list}])],
    io:format("UtilityName: ~p , IP: ~p , Port: ~p~n" , [UtilityName , IP , Port]),
    case list_to_atom(UtilityName) of
        nerlMonitor ->  
           io:format("I'm HERE , args are: ~p , ~p , ~p~n" , [UtilityName , IP , Port]), 
           Graph = gen_server:call(MainServerPid , getGraph),
           io:format("Graph: ~p~n" , [Graph]),
           WorkersMap = ets:lookup_element(nerlnet_data , workers , ?DATA_IDX),
           WorkersClients = maps:to_list(WorkersMap),
           Workers = lists:flatten([atom_to_list(X)++"-"++atom_to_list(Y)++"!" || {X , Y} <- WorkersClients]),
           Reply = Graph ++ "," ++ Workers
    end,
    gen_server:cast(MainServerPid , {saveUtility , {list_to_atom(UtilityName) , IP , Port}}), %% TODO Add IP , Port to ets in main server record
    Req = cowboy_req:reply(200,
                            #{<<"content-type">> => <<"text/plain">>},
                            Reply,
                            Req0),
    {ok, Req, MainServerPid};

init(Req0 , [Action , MainServerPid]) -> 
    {_,Body,_} = cowboy_req:read_body(Req0),
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
