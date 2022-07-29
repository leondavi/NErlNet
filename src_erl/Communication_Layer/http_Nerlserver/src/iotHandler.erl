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
init(Req0, [ApplicationPid]) ->
  _Scheme = cowboy_req:scheme(Req0),
  _Host = cowboy_req:host(Req0),
  _Port = cowboy_req:port(Req0),
  _Path = cowboy_req:path(Req0),
  _Qs = cowboy_req:qs(Req0),
  {ok,Body,_} = cowboy_req:read_body(Req0),
  io:format("Body at iot Handler: ~p,~n", [Body]),
  %Notify the application that python is ready and send the addreses received in this http request:
  
  io:format("reply:~nnerlnet_available#host_name#~p", [getHostName()]),
  Reply = io_lib:format("nerlnet_available#host_name#~p", [getHostName()]),

  Req = cowboy_req:reply(200,
    #{<<"content-type">> => <<"text/plain">>},
    Reply,
    Req0),
  {ok, Req, ApplicationPid}.

%
%

getHostName() ->
   {ok, L} = inet:getif(),
   IP = tuple_to_list(element(1, hd(L))),
   A = lists:flatten(io_lib:format("~p", [IP])),
   Subbed = lists:sublist(A,2,length(A)-2),
   lists:flatten(string:replace(Subbed,",",".",all)).
%%
%%