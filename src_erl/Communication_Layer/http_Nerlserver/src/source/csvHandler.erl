%%%-------------------------------------------------------------------
%%% @author kapelnik
%%% @copyright (C) 2021, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 02. Jan 2021 4:05 AM
%%%-------------------------------------------------------------------
-module(csvHandler).
-author("kapelnik").
-export([init/2,  start/2, stop/1]).
-behaviour(application).


%%setter handler for editing weights in CSV file, can also send a reply to sender
init(Req0, State = [Source_StateM_Pid]) ->
  %Bindings also can be accesed as once, giving a map of all bindings of Req0:
  {_,Body,_} = cowboy_req:read_body(Req0),
%%  [ClientName|CSV_Path] = re:split(binary_to_list(Body), ",", [{return, list}]),
  [_Myself|Splitted]  = re:split(binary_to_list(Body), ",", [{return, list}]),
  {Workers, CSV_Path} = getWorkerInput(Splitted,[]),
%%  io:format("csv handler got Body:~p~n",[Body]),
  gen_statem:cast(Source_StateM_Pid,{csvList,Workers,CSV_Path}),
  Reply = io_lib:format("ACKACK", []),
  Req = cowboy_req:reply(200,
    #{<<"content-type">> => <<"text/plain">>},
    Reply,
    Req0),
  {ok, Req, State}.


getWorkerInput([Input],Workers)->{Workers,Input};
getWorkerInput([Worker|WorkersAndInput],Workers) ->getWorkerInput(WorkersAndInput,Workers++[Worker]).


start(_StartType, _StartArgs) ->
  erlang:error(not_implemented).

stop(_State) ->
  erlang:error(not_implemented).