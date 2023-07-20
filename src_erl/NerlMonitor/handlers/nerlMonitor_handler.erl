-module(hello_handler).

-export([init/2]).

init(Req0, State = [MainScreen]) ->
    {_,Body,_} = cowboy_req:read_body(Req0),
    Data = binary_to_list(Body),
    case Data of
      worker_death_caught -> ok;%tell gui to change  graph display and write in event log
      _ ->
        ok % got unknown messge, ignore.
    end,

    Req = cowboy_req:reply(200,
        #{<<"content-type">> => <<"text/plain">>},
        <<"Got that">>,
        Req0),
    {ok, Req, State}.

