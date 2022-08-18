-module(hello_handler).

-export([init/2, http_request/4]).

init(Req0, State = [MainScreen]) ->


    Req = cowboy_req:reply(200,
        #{<<"content-type">> => <<"text/plain">>},
        <<"Hello Erlang!">>,
        Req0),
    {ok, Req, State}.

%%sending Body as an http request to {Host, Port} to path Path (=String)
%%Example:  http_request(RouterHost,RouterPort,"start_training", <<"client1,client2">>),
http_request(Host, Port,Path, Body)->
  URL = "http://" ++ Host ++ ":"++Port ++ "/" ++ Path,
  httpc:set_options([{proxy, {{Host, list_to_integer(Port)},[Host]}}]),
%%  io:format("sending:  ~p~nto HostPo: ~p~n",[Body,{Host, Port}]),
  {ok,Res} = httpc:request(post,{URL, [],"application/x-www-form-urlencoded",Body}, [], []),
  io:format("Got response ~p~n", [Res]),
  Approve = element(2,element(1,Res)),
  case Approve of
    404 ->
      %  io:format("sending:  ~p~nto HostPo: ~p~n Res: ~p",[Body,{Host, Port},R]),
      %  io:format("Trying again in 0.01 second~n"),
        timer:sleep(10),
        spawn(fun() ->http_request(Host, Port,Path, Body) end);
      _ -> ok
  end,
 % if py
  Ans = lists:sublist(element(3,Res),8),
  case Ans of
    "Somthing" ->
       % io:format("sending:  ~p~nto HostPo: ~p~nRes: ~p",[Body,{Host, Port},R]),
       % io:format("Trying again in 0.01 second~n"),
        timer:sleep(10),
        spawn(fun() ->http_request(Host, Port,Path, Body) end);
      _ -> Ans
  end.