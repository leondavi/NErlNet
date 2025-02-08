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
-include("../nerl_tools.hrl").

%%setter handler for editing weights in CSV file
init(Req0, [Main_genServer_Pid]) ->
%%  Weight = cowboy_req:binding(weight, Req0),
%%  Id = cowboy_req:binding(id, Req0),
%%  Value = cowboy_req:binding(weight_value, Req0),
%%  can go to CSV file and edit weight

  %Bindings also can be accessed as once, giving a map of all bindings of Req0:
  {_,Body,_} = cowboy_req:read_body(Req0, #{length => ?DATA_LEN}),  %read up to X MB (default was 8MB)
  DecodedBody = binary_to_list(zlib:uncompress(Body)),
  [Index, TotalSources, SourceName, WorkersStr, Phase, NumOfBatches, NerlTensorType, Data] = string:split(DecodedBody, "#", all),
  DataCompressed = zlib:compress(list_to_binary(Data)),
  gen_server:cast(Main_genServer_Pid,{initCSV, Index, TotalSources, SourceName, WorkersStr, Phase, NumOfBatches, NerlTensorType, DataCompressed}),
  %[Source|WorkersAndInput] = re:split(binary_to_list(Body), "#", [{return, list}]),
  %{Workers,SourceData} = getWorkerInput(WorkersAndInput,[]),

  Req = cowboy_req:reply(200,
    #{<<"content-type">> => <<"text/plain">>},
    "OK",
    Req0),
  {ok, Req, Main_genServer_Pid}.

% getWorkerInput([Input],Workers)->{Workers,Input};
% getWorkerInput([Worker|WorkersAndInput],Workers) ->getWorkerInput(WorkersAndInput,Workers++[Worker]).

%% for reading a multipart message
% read_all_data(Req0) -> read_all_data(Req0, []).
% read_all_data(Req0, Got) ->
%   io:format("length of read data so far: ~p~n",[length(Got)]),
%   case cowboy_req:read_body(Req0) of
%       {ok, Data, _Req} ->
%           Decoded = binary_to_list(Data),
%           Got++Decoded;
%       {more, Data, Req} ->
%           Decoded = binary_to_list(Data),
%           read_all_data(Req, Got++Decoded)
%   end.

start(_StartType, _StartArgs) ->
  erlang:error(not_implemented).

stop(_State) ->
  erlang:error(not_implemented).