%%%-------------------------------------------------------------------
%%% @author kapelnik
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 31. Dec 2020 8:33 AM
%%%-------------------------------------------------------------------
-module(id_handler).
-author("kapelnik").
-behavior(cowboy_handler).
-export([init/2]).


%This module created to receive information about a given id
init(Req0, State) -> %State comes from last argument of the route
  MyObj = cowboy_req:binding(myobj, Req0),
  Id = cowboy_req:binding(id, Req0),
  {IP, Port} = cowboy_req:peer(Req0),
%%  now we can access CSV file and gather information about given object-id
  io:format("id_handler got body:~p~n",[{MyObj, Id}]),
  Reply = io_lib:format("information asked for object - ~p, with id - ~p~n", [MyObj, Id]),

  Req = cowboy_req:reply(200,
    #{<<"content-type">> => <<"text/plain">>},
    Reply,
    Req0),
  {ok, Req, State}.

