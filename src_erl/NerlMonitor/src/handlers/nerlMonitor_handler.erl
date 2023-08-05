-module(nerlMonitor_handler).

-export([init/2]).

-define(GUI , {'PyrlangProcess' , 'py@127.0.0.1'}).

init(Req0, [Msg]) ->
    {_,Body,_} = cowboy_req:read_body(Req0),
    Data = binary_to_list(Body),
    io:format("-------------------Data ~p--------------~n" , [Data]),
    case Msg of
      utilInfo -> io:format("Update graph with: worker_down ~p~n" , [Data]) , 
                     ?GUI ! {update ,Data};
      _ ->
        ok % got unknown messge, ignore.
    end,

    Req = cowboy_req:reply(200,
        #{<<"content-type">> => <<"text/plain">>},
        <<"Got that">>,
        Req0),
    {ok, Req, Msg}.

