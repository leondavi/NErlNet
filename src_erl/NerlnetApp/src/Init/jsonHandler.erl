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
        nerl_tools:deleteOldJson(?JSON_ADDR++?LOCAL_DC_FILE_NAME),
        nerl_tools:deleteOldJson(?JSON_ADDR++?LOCAL_COMM_FILE_NAME),
      cleanup_torch_payloads(),
        %% get files from Req
        % io:format("parsing json of req with body: ~p~n",[cowboy_req:read_body(Req0)]),
      {_Req, Data} = nerl_tools:multipart(Req0, []), % multipart also save data to file      %% Data = [FileName1, FileName2]
      case Data of
        [ArchPath, ConnPath | TorchPaths] ->
          log_torch_payloads(TorchPaths),
          ApplicationPid ! {jsonAddress,{ArchPath,ConnPath}};
        _ ->
          ?LOG_ERROR("Multipart payload missing expected json files, received entries: ~p", [Data])
      end;
    _Other -> 
        {ok,Body,_} = cowboy_req:read_body(Req0),           %% shouldn't be here, files expected
        io:format("Error - Got an unknown request: ~p~nData: ~p~n",[Req0, Body])
  end,

  Reply = io_lib:format("nerlnet starting", []),

  Req2 = cowboy_req:reply(200,
    #{<<"content-type">> => <<"text/plain">>},
    Reply,
    Req0),
  {ok, Req2, ApplicationPid}.

cleanup_torch_payloads() ->
  BaseDir = ?TORCH_MODELS_BASE_DIR,
  ensure_directory(BaseDir),
  purge_directory_contents(BaseDir).

ensure_directory(Dir) ->
  ok = filelib:ensure_dir(filename:join(Dir, "dummy")),
  ok.

purge_directory_contents(Dir) ->
  case file:list_dir(Dir) of
    {ok, Entries} ->
      lists:foreach(fun(Name) ->
        Path = filename:join(Dir, Name),
        case filelib:is_dir(Path) of
          true ->
            purge_directory_contents(Path),
            delete_dir_safely(Path);
          false ->
            delete_file_safely(Path)
        end
      end, Entries);
    {error, enoent} -> ok;
    {error, Reason} -> ?LOG_WARNING("Failed to list Torch artifact dir ~p: ~p", [Dir, Reason])
  end.

delete_file_safely(File) ->
  case file:delete(File) of
    ok -> ok;
    {error, enoent} -> ok;
    {error, Reason} -> ?LOG_WARNING("Failed to delete stale torch artifact ~p: ~p", [File, Reason])
  end.

delete_dir_safely(Dir) ->
  case file:del_dir(Dir) of
    ok -> ok;
    {error, enoent} -> ok;
    {error, Reason} -> ?LOG_WARNING("Failed to delete Torch artifact directory ~p: ~p", [Dir, Reason])
  end.

log_torch_payloads([]) -> ok;
log_torch_payloads(TorchPaths) ->
  ?LOG_INFO("Received ~p Torch artifact(s) via multipart", [length(TorchPaths)]).
