%%%-------------------------------------------------------------------
%%% @author kapelnik
%%% @copyright (C) 2021, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 02. Jan 2021 4:03 AM
%%%-------------------------------------------------------------------
-module(erl_client).
-author("kapelnik").
%%state <- {machines - [rasp1,rasp2,..]}
%% API
-export([start_connection/0, updateCSV/0, start_training/0, stop_training/0]).
start_connection() ->
  inets:start(),
  httpc:set_options([{proxy, {{"localhost", 8080},["localhost"]}}]).


updateCSV() ->

%%erlang request:   (using post method, because we want to receive a reply for our request)
%%httpc:request(post,{URL,Headers,content type,Body),HTTPOptions, Profile)
%%for example:
%%httpc:request(post,{"http://localhost:8080/weights_vector/set", [],"application/x-www-form-urlencoded","[256]"}, [], []).

%%  {ok, {{Version, 200, ReasonPhrase}, Headers, Body}} =
    httpc:request(post,{"http://localhost:8080/initNerlnet", [],"application/x-www-form-urlencoded","./input/input.csv"}, [], []).

start_training()->
  httpc:request(post,{"http://localhost:8080/start_training", [],"application/x-www-form-urlencoded","listofsources"}, [], []).
stop_training()->
  httpc:request(post,{"http://localhost:8080/stop_training", [],"application/x-www-form-urlencoded","listofsources"}, [], []).




%%doubles_to_binary_list(Double_List)->[(X)||X<-Double_List].
%%receive a list of floats, returning a binary
encode(List_of_floats)->   Binary_List = [float_to_binary(X)||X<-List_of_floats],
  Binary =list_to_binary(Binary_List),
  io:format("encoded:~p   decoded: ~p~n",[Binary,decode(Binary)]), Binary.

%%receive a binary, returning a list of floats
decode(Binary)->
  Binary_list =binary_to_list(Binary),
  List_of_floats = [binary_to_float(X)||X<-Binary_list],
  List_of_floats.