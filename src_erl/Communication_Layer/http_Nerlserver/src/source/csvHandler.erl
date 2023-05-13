%%%-------------------------------------------------------------------
%%% @author kapelnik
%%% @copyright (C) 2021, Nerlnet
%%% @doc
%%%
%%% @end
%%% Created : 02. Jan 2021 4:05 AM
%%%-------------------------------------------------------------------
-module(csvHandler).
-author("kapelnik").
-export([init/2,  start/2, stop/1]).
-behaviour(application).
-define(DATA_LEN, 15*1000*1000). % default is 8MB, here set to 15MB

%%setter handler for editing weights in CSV file, can also send a reply to sender
init(Req0, State = [Source_StateM_Pid]) ->
  {_,Body,_} = cowboy_req:read_body(Req0, #{length => ?DATA_LEN}),
  Decoded_body = binary_to_list(Body),
  %Decoded_body = read_all_data(Req0),
%%  [ClientName|CSV_Path] = re:split(binary_to_list(Body), ",", [{return, list}]),
  %% TODO: receive file data differently so it can be appended together / multipart
  [_SourceName, WorkersStr, CSVData] = string:split(Decoded_body, "#", all),
  WorkersList = string:split(WorkersStr, ",", all),
  gen_statem:cast(Source_StateM_Pid,{batchList,WorkersList,CSVData}),

  Reply = io_lib:format("ACKACK", []),
  Req = cowboy_req:reply(200,
    #{<<"content-type">> => <<"text/plain">>},
    Reply,
    Req0),
  {ok, Req, State}.

% read_all_data(Req0) -> read_all_data(Req0, []).
% read_all_data(Req0, Got) ->
%   %io:format("length of read data so far: ~p~n",[length(Got)]),
%   case cowboy_req:read_body(Req0) of
%       {ok, Data, Req} ->
%           Decoded = binary_to_list(Data),
%           Got++Decoded;
%       {more, Data, Req} ->
%           Decoded = binary_to_list(Data),
%           read_all_data(Req, Got++Decoded)
%   end.


% getWorkerInput([Input],Workers)->{Workers,Input};
% getWorkerInput([Worker|WorkersAndInput],Workers) ->getWorkerInput(WorkersAndInput,Workers++[Worker]).


start(_StartType, _StartArgs) ->
  erlang:error(not_implemented).

stop(_State) ->
  erlang:error(not_implemented).