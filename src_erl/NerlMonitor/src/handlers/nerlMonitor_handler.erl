-module(nerlMonitor_handler).

-export([init/2]).

-define(GUI , {'PyrlangProcess' , 'py@127.0.0.1'}).

init(Req0, [Msg]) ->
    {_,Body,_} = cowboy_req:read_body(Req0),
    Data = binary_to_list(Body),
    case Msg of
      utilInfo -> ?GUI ! {update ,Data};
      stats -> ?GUI ! {stats ,Data};
      _ ->
        ok % got unknown messge, ignore.
    end,

    Req = cowboy_req:reply(200,
        #{<<"content-type">> => <<"text/plain">>},
        <<"Got that">>,
        Req0),
    {ok, Req, Msg}.

