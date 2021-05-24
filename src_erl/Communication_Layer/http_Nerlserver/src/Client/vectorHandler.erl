%%%-------------------------------------------------------------------
%%% @author kapelnik
%%% @copyright (C) 2021, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 02. Jan 2021 4:05 AM
%%%-------------------------------------------------------------------
-module(vectorHandler).
-author("kapelnik").
-export([init/2,  start/2, stop/1]).
-behaviour(application).


%%handler for receiveing vectors of samples from Sensors
init(Req0, [Client_StateM_Pid]) ->
  %Bindings also can be accesed as once, giving a map of all bindings of Req0:
  {ok,Body,_} = cowboy_req:read_body(Req0),
%%  Decoded_body = binary_to_list(Body),

  gen_statem:cast(Client_StateM_Pid,{sample,Body}),

  Reply = io_lib:format("Sample Received~n ", []),
  Req = cowboy_req:reply(200,
    #{<<"content-type">> => <<"text/plain">>},
    Reply,
    Req0),
  {ok, Req, Client_StateM_Pid}.


start(_StartType, _StartArgs) ->
  erlang:error(not_implemented).

stop(_State) ->
  erlang:error(not_implemented).