%%%-------------------------------------------------------------------
%%% @author kapelnik
%%% @copyright (C) 2021, Nerlnet
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

%%%%%% Getting files in multipart format.
init(Req0, [ApplicationPid]) ->

  {Req, Data} = multipart(Req0, []),  % gets
  io:format("got Req: ~p~nData: ~p~n",[Req, Data]),
  {ok,Body,_} = cowboy_req:read_body(Req0),
  %FullReq = multipart(Req0),
  % [ArchitectureAdderess,CommunicationMapAdderess] = re:split(binary_to_list(Body),"#",[{return,list}]),
  % io:format("Body at json Handler: ~p,~n sending to pid: ~p~n", [ArchitectureAdderess,CommunicationMapAdderess]),
  io:format("Headers are: ~p~n",[cowboy_req:header(<<"content-type">>, Req0)]),
  io:format("got now: ~p~n",[binary_to_list(Body)]),
  %io:format("Full message: ~p~n",[FullReq]),
  %Notify the application that python is ready and send the addreses received in this http request:
  %ApplicationPid ! {jsonAddress,{ArchitectureAdderess,CommunicationMapAdderess}},
  
  Reply = io_lib:format("nerlnet starting", []),

  Req = cowboy_req:reply(200,
    #{<<"content-type">> => <<"text/plain">>},
    Reply,
    Req0),
  {ok, Req, ApplicationPid}.


  % returns {FullReq, Data}
  multipart(Req0, Data) ->
    case cowboy_req:read_part(Req0) of
        {ok, Headers, Req1} ->
            {Req, BodyData} = case cow_multipart:form_data(Headers) of
                {data, _FieldName} ->
                    {ok, Body, Req2} = cowboy_req:read_part_body(Req1),
                    {Req2, Body};
                {file, _FieldName, Filename, _CType} ->
                    File = file:open(Filename, [append]),
                    Req2 = stream_file(Req1, File),
                    {Req2, fileReady}
            end,
            multipart(Req, Data++BodyData);
        {done, Req} ->
            {Req, Data}
    end.

stream_file(Req0, File) ->
    case cowboy_req:read_part_body(Req0) of
        {ok, LastBodyChunk, Req} ->
            file:write(File, LastBodyChunk),
            file:close(File),
            Req;
        {more, BodyChunk, Req} ->
            file:write(File, BodyChunk)
            stream_file(Req, File)
    end.