%%%-------------------------------------------------------------------
%%% @author kapelnik
%%% @copyright (C) 2021, Nerlnet
%%% @doc
%%%
%%% @endClient_StateM_Pid
%%% Created : 02. Jan 2021 4:05 AM
%%%-------------------------------------------------------------------
-module(initHandler).
-author("kapelnik").
-export([init/2, start/2, stop/1]).
-behaviour(application).

-define(DATA_LEN, 15*1000*1000). % default is 8MB, here set to 15MB

%%setter handler for editing weights in CSV file, can also send a reply to sender
init(Req0, [Main_genServer_Pid]) ->
%%  Weight = cowboy_req:binding(weight, Req0),
%%  Id = cowboy_req:binding(id, Req0),
%%  Value = cowboy_req:binding(weight_value, Req0),
%%  can go to CSV file and edit weight

  %Bindings also can be accessed as once, giving a map of all bindings of Req0:
  {_,Body,_} = cowboy_req:read_body(Req0, #{length => ?DATA_LEN}),  %read up to 10MB (default was 8MB)
  Decoded_body = binary_to_list(Body),
  %Decoded_body = read_all_data(Req0),
  [SourceName, _WorkersStr, Data] = string:split(Decoded_body, "#", all),
  %WorkersList = string:split(WorkersStr, ",", all),
  gen_server:cast(Main_genServer_Pid,{initCSV, SourceName, Body}),
  %[Source|WorkersAndInput] = re:split(binary_to_list(Body), "#", [{return, list}]),
  %{Workers,SourceData} = getWorkerInput(WorkersAndInput,[]),

  Reply = io_lib:format("Body Received: ~p, Decoded Body = ~p ~n State:~p~n", [Body,Decoded_body, Main_genServer_Pid]),

  Req = cowboy_req:reply(200,
    #{<<"content-type">> => <<"text/plain">>},
    Reply,
    Req0),
  {ok, Req, Main_genServer_Pid}.

getWorkerInput([Input],Workers)->{Workers,Input};
getWorkerInput([Worker|WorkersAndInput],Workers) ->getWorkerInput(WorkersAndInput,Workers++[Worker]).

read_all_data(Req0) -> read_all_data(Req0, []).
read_all_data(Req0, Got) ->
  io:format("length of read data so far: ~p~n",[length(Got)]),
  case cowboy_req:read_body(Req0) of
      {ok, Data, Req} ->
          Decoded = binary_to_list(Data),
          Got++Decoded;
      {more, Data, Req} ->
          Decoded = binary_to_list(Data),
          read_all_data(Req, Got++Decoded)
  end.

start(_StartType, _StartArgs) ->
  erlang:error(not_implemented).

stop(_State) ->
  erlang:error(not_implemented).