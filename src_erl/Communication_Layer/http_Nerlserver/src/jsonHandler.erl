%%%-------------------------------------------------------------------
%%% @author kapelnik
%%% @copyright (C) 2021, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 01. Jan 2021 4:58 AM
%%%-------------------------------------------------------------------
-module(jsonHandler).
-author("kapelnik").


%% API
-export([init/2]).
% This handler waits for an http request from python. the syntax should be as follow:
%From python:
% response = requests.post('http://localhost:8484/updateJsonPath', data='../../../jsonPath')
%From erlang(maybe for debug):
%%httpc:request(post,{"http://localhost:8484/updateJsonPath", [],"application/x-www-form-urlencoded","../../../jsonPath"}, [], []).
init(Req0, [ApplicationPid]) ->
  _Scheme = cowboy_req:scheme(Req0),
  _Host = cowboy_req:host(Req0),
  _Port = cowboy_req:port(Req0),
  _Path = cowboy_req:path(Req0),
  _Qs = cowboy_req:qs(Req0),
  {ok,Body,_} = cowboy_req:read_body(Req0),
  [ArchitectureAdderess,CommunicationMapAdderess] = re:split(binary_to_list(Body),"#",[{return,list}]),
  io:format("Body at json Handler: ~p,~n sending to pid: ~p~n", [ArchitectureAdderess,CommunicationMapAdderess,ApplicationPid]),

  %Notify the application that python is ready and send the addreses received in this http request:
  ApplicationPid ! {jsonAddress,{ArchitectureAdderess,CommunicationMapAdderess}},
  
  Reply = io_lib:format("nerlnet starting", []),

  Req = cowboy_req:reply(200,
    #{<<"content-type">> => <<"text/plain">>},
    Reply,
    Req0),
  {ok, Req, ApplicationPid}.
