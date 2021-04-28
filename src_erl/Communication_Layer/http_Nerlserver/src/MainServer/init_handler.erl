%%%-------------------------------------------------------------------
%%% @author kapelnik
%%% @copyright (C) 2021, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 02. Jan 2021 4:05 AM
%%%-------------------------------------------------------------------
-module(init_handler).
-author("kapelnik").
-export([init/2]).
-behaviour(application).

%%setter handler for editing weights in CSV file, can also send a reply to sender
init(Req0, [Main_genServer_Pid]) ->
%%  Weight = cowboy_req:binding(weight, Req0),
%%  Id = cowboy_req:binding(id, Req0),
%%  Value = cowboy_req:binding(weight_value, Req0),
%%  can go to CSV file and edit weight

  %Bindings also can be accessed as once, giving a map of all bindings of Req0:
  {ok,Body,_} = cowboy_req:read_body(Req0),
  Decoded_body = binary_to_list(Body),
  Reply = io_lib:format("Body Received: ~p, Decoded Body = ~p ~n State:~p~n", [Body,Decoded_body, Main_genServer_Pid]),
  gen_server:cast(Main_genServer_Pid,{initCSV,"./input/input.csv"}),
  Req = cowboy_req:reply(200,
    #{<<"content-type">> => <<"text/plain">>},
    Reply,
    Req0),
  {ok, Req, Main_genServer_Pid}.
