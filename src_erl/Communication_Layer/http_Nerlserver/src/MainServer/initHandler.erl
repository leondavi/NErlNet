%%%-------------------------------------------------------------------
%%% @author kapelnik
%%% @copyright (C) 2021, <COMPANY>
%%% @doc
%%%
%%% @endClient_StateM_Pid
%%% Created : 02. Jan 2021 4:05 AM
%%%-------------------------------------------------------------------
-module(initHandler).
-author("kapelnik").
-export([init/2, start/2, stop/1]).
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
  [Source|WorkersAndInput] = re:split(binary_to_list(Body), ",", [{return, list}]),
  {Workers,_Input} = getWorkerInput(WorkersAndInput,[]),
  io:format("init _handler got body:~p~n",[Decoded_body]),
  gen_server:cast(Main_genServer_Pid,{initCSV, Source,Workers,Body}),
%%  gen_server:cast(Main_genServer_Pid,{initCSV,  splitbytriplets(Splitted,[])}),

  Reply = io_lib:format("Body Received: ~p, Decoded Body = ~p ~n State:~p~n", [Body,Decoded_body, Main_genServer_Pid]),

  Req = cowboy_req:reply(200,
    #{<<"content-type">> => <<"text/plain">>},
    Reply,
    Req0),
  {ok, Req, Main_genServer_Pid}.

getWorkerInput([Input],Workers)->{Workers,Input};
getWorkerInput([Worker|WorkersAndInput],Workers) ->getWorkerInput(WorkersAndInput,Workers++[Worker]).

%%splitbytriplets([],Ret) ->Ret;
%%splitbytriplets(ListofTriplets,Ret) ->
%%  L1 = lists:sublist(ListofTriplets,1,3),
%%  L2 = lists:sublist(ListofTriplets,4,length(ListofTriplets)-1),
%%  splitbytriplets(L2,Ret++[L1]).

start(_StartType, _StartArgs) ->
  erlang:error(not_implemented).

stop(_State) ->
  erlang:error(not_implemented).