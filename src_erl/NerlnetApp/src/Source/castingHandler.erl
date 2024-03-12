%%%-------------------------------------------------------------------
%%% @author kapelnik
%%% @copyright (C) 2021, Nerlnet
%%% @doc
%%%
%%% @end
%%% Created : 02. Jan 2021 4:05 AM
%%%-------------------------------------------------------------------
-module(castingHandler).
-author("kapelnik").
-export([init/2,  start/2, stop/1]).
-behaviour(application).


%%setter handler for editing weights in CSV file, can also send a reply to sender
init(Req0, [Action,Source_StateM_Pid]) ->
  %Bindings also can be accesed as once, giving a map of all bindings of Req0:
%%  io:format("casting handler got Body:~p~n",[Body]),
  case Action of
    csv -> case cowboy_req:parse_header(<<"content-type">>, Req0) of 
                {<<"multipart">>, <<"form-data">>, _} ->
                    {_Req, Decoded_body} = nerl_tools:multipart(Req0, []);
                _Other -> 
                    {_Req,Body} = nerl_tools:read_all_data(Req0, <<>>),
                    Decoded_body = binary_to_list(Body)
                    %io:format("got Req: ~p~nData: ~p~n",[Req0, Body])
              end,
            [_SourceName, WorkersStr, NumOfBatches, CSVData] = string:split(Decoded_body, "#", all),
            WorkersList = string:split(WorkersStr, ",", all),
            gen_statem:cast(Source_StateM_Pid,{batchList,WorkersList, list_to_integer(NumOfBatches), CSVData});
    startCasting  ->  {_,Body,_} = cowboy_req:read_body(Req0),
                      gen_statem:cast(Source_StateM_Pid, {startCasting,Body});
    statistics    ->  gen_statem:cast(Source_StateM_Pid, {statistics});
    stopCasting   ->  gen_statem:cast(Source_StateM_Pid, {stopCasting})

  end,
  Reply = "Body Received",
  Req = cowboy_req:reply(200,
    #{<<"content-type">> => <<"text/plain">>},
    Reply,
    Req0),
  {ok, Req, Source_StateM_Pid}.


start(_StartType, _StartArgs) ->
  erlang:error(not_implemented).

stop(_State) ->
  erlang:error(not_implemented).