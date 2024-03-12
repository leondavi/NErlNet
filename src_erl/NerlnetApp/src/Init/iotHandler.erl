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
init(Req0, [Action , ApplicationPid]) ->
  case Action of
    restart -> 
      os:cmd("nohup sh -c 'sleep 5 && /usr/local/lib/nerlnet-lib/NErlNet/NerlnetRun.sh --run-mode stop' &"),
      io:format("Nerlnet Stopped~n"),
      os:cmd("nohup sh -c 'sleep 20 && /usr/local/lib/nerlnet-lib/NErlNet/NerlnetRun.sh --run-mode release-bg' &"),
      io:format("Restarting Nerlnet~n")
  end,
  _Scheme = cowboy_req:scheme(Req0),
  _Host = cowboy_req:host(Req0),
  _Port = cowboy_req:port(Req0),
  _Path = cowboy_req:path(Req0),
  _Qs = cowboy_req:qs(Req0),
  {ok,Body,_} = cowboy_req:read_body(Req0),
  %% io:format("Body at iot Handler: ~p,~n", [Body]),
  %Notify the application that python is ready and send the addreses received in this http request:
  
  Reply = io_lib:format("nerlnet_available#host_name#~p", [nerl_tools:getdeviceIP()]),
  %% io:format("reply: ~p~n", [Reply]),

  Req = cowboy_req:reply(200,
    #{<<"content-type">> => <<"text/plain">>},
    Reply,
    Req0),
  {ok, Req, ApplicationPid}.