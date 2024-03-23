%%%-------------------------------------------------------------------
%%% @author kapelnik
%%% @copyright (C) 2020, Nerlnet
%%% @doc
%%%
%%% @end
%%% Created : 31. Dec 2020 4:41 AM
%%%-------------------------------------------------------------------
-module(ackHandler).
-author("kapelnik").
-behavior(application).

-export([init/2, start/2, stop/1]).

%%setter handler for editing weights in CSV file, can also send a reply to sender
init(Req0, [Who,Main_genserver_Pid]) ->
  %Bindings also can be accesed as once, giving a map of all bindings of Req0:
  {_,Body,_} = cowboy_req:read_body(Req0),
  % io:format("mainserver got ACK: ~p~n",[Body]),
  case Who of
    dataReady -> gen_server:cast(Main_genserver_Pid, {sourceAckDataReady,Body});      %% when source data is ready
    sourceDone -> gen_server:cast(Main_genserver_Pid, {sourceDone,Body}); %% when source finished casting
    clientAck -> gen_server:cast(Main_genserver_Pid, {clientAck,Body});       %% when client received message (new state)
    jsonReceived -> gen_server:cast(Main_genserver_Pid, {jsonReceived,Body}); %% when other devices got the json and ready to start
    batch_ack -> Main_genserver_Pid ! {batch_ack, Body}; % This is a special ack to validate that all batches transmitted successfully
    apiserver_ack_validation -> Main_genserver_Pid ! {apiserver_ack_validation, Body} % This ack validates transmission with flask
  end,
  Reply = io_lib:format("Body Received: ~p ~n ", [Body]),
  Req = cowboy_req:reply(200,
    #{<<"content-type">> => <<"text/plain">>},
    Reply,
    Req0),
  {ok, Req, Main_genserver_Pid}.

start(_StartType, _StartArgs) ->
  erlang:error(not_implemented).

stop(_State) ->
  erlang:error(not_implemented).