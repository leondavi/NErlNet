%%%-------------------------------------------------------------------
%%% @author kapelnik
%%% @copyright (C) 2021, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 01. Jan 2021 4:58 AM
%%%-------------------------------------------------------------------
-module(noMatchingRouteHandler).
-author("kapelnik").


%% API
-export([init/2]).
%%Whoops! something probebly went wrong when sending this URL request
init(Req0, State) ->
  Scheme = cowboy_req:scheme(Req0),
  Host = cowboy_req:host(Req0),
  Port = cowboy_req:port(Req0),
  Path = cowboy_req:path(Req0),
  Qs = cowboy_req:qs(Req0),
  {ok,Body,_} = cowboy_req:read_body(Req0),
  Reply = io_lib:format("Somthing went wrong..Path attempted: ~p,~nBody: ~p~n", [Path,Body]),

  Req = cowboy_req:reply(200,
    #{<<"content-type">> => <<"text/plain">>},
    Reply,
    Req0),
  {ok, Req, State}.
%%httpc:request(post,{"http://localhost:8080/weights_vector/update, [],"application/x-www-form-urlencoded","[1.02,3.000005]"}, [], []).
