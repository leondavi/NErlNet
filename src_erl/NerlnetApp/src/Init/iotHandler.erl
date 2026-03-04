%%%-------------------------------------------------------------------
%%% @author kapelnik
%%% @copyright (C) 2021, Nerlnet
%%% @doc
%%%
%%% @end
%%% Created : 01. Jan 2021 4:58 AM
%%%-------------------------------------------------------------------
-module(iotHandler).
-author("kapelnik").


%% API
-export([init/2]).
%this handler lets the python gui the option to make a broadcast http request and get all the nerlnet devices available on the subnet
% This handler waits for an http request from python. the syntax should be as follow:
%From python:
% response = requests.post('http://localhost:8484/isNerlnetDevice', data='')
%From erlang(maybe for debug):
%%httpc:request(post,{"http://localhost:8484/isNerlnetDevice", [],"application/x-www-form-urlencoded",[]]}, [], []).
%%
%% The route is used as a readiness probe by tooling and startup fanout.
%% Keep it tolerant to both legacy args ([Pid]) and explicit action args ([Action, Pid]).
init(Req0, [ApplicationPid]) ->
  init(Req0, [probe, ApplicationPid]);
init(Req0, [Action, ApplicationPid]) ->
  maybe_apply_action(Action),

  Method = cowboy_req:method(Req0),
  {ok, _Body, Req1} =
    case Method of
      <<"POST">> -> cowboy_req:read_body(Req0);
      _ -> {ok, <<>>, Req0}
    end,

  Reply = io_lib:format("nerlnet_available#host_name#~p", [nerl_tools:getdeviceIP()]),
  Req2 = cowboy_req:reply(
    200,
    #{<<"content-type">> => <<"text/plain">>},
    Reply,
    Req1
  ),
  {ok, Req2, ApplicationPid}.

maybe_apply_action(restart) ->
  os:cmd("nohup sh -c 'sleep 5 && /usr/local/lib/nerlnet-lib/NErlNet/NerlnetRun.sh --run-mode stop' &"),
  io:format("Nerlnet Stopped~n"),
  os:cmd("nohup sh -c 'sleep 20 && /usr/local/lib/nerlnet-lib/NErlNet/NerlnetRun.sh --run-mode release-bg' &"),
  io:format("Restarting Nerlnet~n");
maybe_apply_action(_Other) ->
  ok.
