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
    csv ->  {_ , Body} = nerl_tools:read_all_data(Req0 , <<>>),
            {WorkersList, Phase, NumOfBatches, NerlTensorType, CompressedData} = binary_to_term(Body),
            gen_statem:cast(Source_StateM_Pid, {batchList, WorkersList, list_to_atom(Phase), list_to_integer(NumOfBatches), NerlTensorType , CompressedData});
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