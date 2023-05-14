%%%-------------------------------------------------------------------
%%% @author kapelnik
%%% @copyright (C) 2021, Nerlnet
%%% @doc
%%%
%%% @end
%%% Created : 01. Jan 2021 4:58 AM
%%%-------------------------------------------------------------------
-module(jsonHandler).
-include("../nerl_tools.hrl").

%% API
-export([init/2]).
% This handler waits for an http request from python. the syntax should be as follow:
%From python:
% response = requests.post('http://localhost:8484/updateJsonPath', data='../../../jsonPath')
%From erlang(maybe for debug):
%%httpc:request(post,{"http://localhost:8484/updateJsonPath", [],"application/x-www-form-urlencoded","../../../jsonPath"}, [], []).

%%%%%% Getting files in multipart format.
init(Req0, [ApplicationPid]) ->
  case cowboy_req:parse_header(<<"content-type">>, Req0) of
    {<<"multipart">>, <<"form-data">>, _} ->
        nerl_tools:deleteOldJson(?JSON_ADDR++"arch.json"),
        nerl_tools:deleteOldJson(?JSON_ADDR++"conn.json"),
        %% get files from Req
        {_Req, Data} = nerl_tools:multipart(Req0, []),       %% Data = [FileName1, FileName2]
        ApplicationPid ! {jsonAddress,{lists:nth(1, Data),lists:nth(2, Data)}};
    _Other -> 
        {ok,_Body,_} = cowboy_req:read_body(Req0)           %% shouldn't be here, files expected
        % io:format("got Req: ~p~nData: ~p~n",[Req0, Body])
  end,

  Reply = io_lib:format("nerlnet starting", []),

  Req2 = cowboy_req:reply(200,
    #{<<"content-type">> => <<"text/plain">>},
    Reply,
    Req0),
  {ok, Req2, ApplicationPid}.
