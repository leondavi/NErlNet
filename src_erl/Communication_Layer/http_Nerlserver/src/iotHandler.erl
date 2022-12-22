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
-export([init/2, http_request/4]).
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
  
  io:format("reply:~nnerlnet_available#host_name#~p", [nerlNetServer_app:getdeviceIP()]),
  Reply = io_lib:format("nerlnet_available#host_name#~p", [nerlNetServer_app:getdeviceIP()]),

  Req = cowboy_req:reply(200,
    #{<<"content-type">> => <<"text/plain">>},
    Reply,
    Req0),
  {ok, Req, ApplicationPid}.

  http_request(Host, Port,Path, Body)->
    %%  io:format("sending body ~p to path ~p to hostport:~p~n",[Body,Path,{Host,Port}]),
    URL = "http://" ++ Host ++ ":"++integer_to_list(Port) ++ "/" ++ Path,
    httpc:set_options([{proxy, {{Host, Port},[Host]}}]),
    httpc:request(post,{URL, [],"application/x-www-form-urlencoded",Body}, [], []).