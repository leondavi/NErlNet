%%%-------------------------------------------------------------------
%%% @author kapelnik
%%% @copyright (C) 2021, Nerlnet
%%% @doc
%%%
%%% @end
%%% Created : 02. Jan 2021 4:05 AM
%%%-------------------------------------------------------------------
-module(csvHandler).
-export([init/2,  start/2, stop/1]).
-behaviour(application).

init(Req0, State = [Source_StateM_Pid]) ->

  case cowboy_req:parse_header(<<"content-type">>, Req0) of
    {<<"multipart">>, <<"form-data">>, _} ->
        {_Req, Decoded_body} = nerl_tools:multipart(Req0, []);
    _Other -> 
        {_Req,Body} = nerl_tools:read_all_data(Req0, []),
        Decoded_body = binary_to_list(Body)
        %io:format("got Req: ~p~nData: ~p~n",[Req0, Body])
  end,
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


% getWorkerInput([Input],Workers)->{Workers,Input};
% getWorkerInput([Worker|WorkersAndInput],Workers) ->getWorkerInput(WorkersAndInput,Workers++[Worker]).


start(_StartType, _StartArgs) ->
  erlang:error(not_implemented).

stop(_State) ->
  erlang:error(not_implemented).