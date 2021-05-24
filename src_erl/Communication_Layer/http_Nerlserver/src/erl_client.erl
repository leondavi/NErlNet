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
-export([start_connection/0, updateCSV/0, start_training/0, stop_training/0,  training/0]).
start_connection() ->
  inets:start(),
  httpc:set_options([{proxy, {{"localhost", 8080},["localhost"]}}]).



updateCSV() ->

%%erlang request:   (using post method, because we want to receive a reply for our request)
%%httpc:request(post,{URL,Headers,content type,Body),HTTPOptions, Profile)
%%for example:
%%httpc:request(post,{"http://localhost:8080/weights_vector/set", [],"application/x-www-form-urlencoded","[256]"}, [], []).

%%  {ok, {{Version, 200, ReasonPhrase}, Headers, Body}} =
%%Body here can be more than one source: for example "source1,./input/input99.csv,source2,./input/input123.csv":
    httpc:request(post,{"http://localhost:8080/initNerlnet", [],"application/x-www-form-urlencoded","source1,client1,./input/input2.csv"}, [], []).

%%sets all clients in training state
training()->
  httpc:request(post,{"http://localhost:8080/clientsTraining", [],"application/x-www-form-urlencoded",[]}, [], []).

start_training()->
  httpc:request(post,{"http://localhost:8080/startTraining", [],"application/x-www-form-urlencoded",<<"source1">>}, [], []).
stop_training()->
  httpc:request(post,{"http://localhost:8080/stopTraining", [],"application/x-www-form-urlencoded",<<"source1">>}, [], []).
  %%httpc:request(post,{"http://192.168.0.107:8082/updateCSV", [],"application/x-www-form-urlencoded","source1,worker1,worker2,./input/input2.csv"}, [], []).
%%httpc:set_options([{proxy, {{"192.168.0.107", 8082},["192.168.0.107"]}}]).

