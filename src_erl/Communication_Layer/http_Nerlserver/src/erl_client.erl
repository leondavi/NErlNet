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
-export([start_connection/0, updateCSV/0, start_training/0, stop_training/0, encodeMap/0, training/0]).
start_connection() ->
  inets:start(),
  httpc:set_options([{proxy, {{"localhost", 8080},["localhost"]}}]).


getHostName() ->
  {ok, L} = inet:getif(),
  IP = tuple_to_list(element(1, hd(L))),
  A = lists:flatten(io_lib:format("~p", [IP])),
  Subbed = lists:sublist(A,2,length(A)-2),
  lists:flatten(string:replace(Subbed,",",".",all)).






getNumbers([],List)->List;
getNumbers([Head|Tail], List) ->
%%  io:format("Head:~p~n",[Head]),
  try list_to_float(Head) of
    Float->    %io:format("~p~n",[Float]),
      getNumbers(Tail,[(Float)]++List)
  catch
    error:Error->
      %io:format("~p~n",[Error]),
      getNumbers(Tail,[list_to_integer(Head)]++List)

  end.




  getWorkerInput([Input],Workers)->{Workers,Input};
getWorkerInput([Worker|WorkersAndInput],Workers) ->getWorkerInput(WorkersAndInput,Workers++[Worker]).

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







encodeMap()->
  MainServerHostPort = {"localhost",8080},
  Client1HostPort = {"localhost",8081},
  Source1HostPort = {"localhost",8082},
  Router1HostPort = {"localhost",8083},

  MainServerPortMap =  #{mainServer => MainServerHostPort},
  ClientsPortMap =  #{client1 => Client1HostPort},
  SourcePortMap =  #{source1 => Source1HostPort},
  RoutersPortMap =  #{router1 => Router1HostPort},
  PortsMap = #{ mainServer => MainServerPortMap,clients => ClientsPortMap, sources => SourcePortMap,routers => RoutersPortMap},

%%    connectivity map will be as follow:
%%    name_atom of machine => {Host,Port} OR an atom router_name, indicating there is no direct http connection, and should pass request via router_name
  MainServerConnectionsMap = #{client1 => Router1HostPort, source1 => Router1HostPort},
  ClientConnectionsMap = #{mainServer => Router1HostPort},
  RouterConnectionsMap_router1 = #{mainServer => MainServerHostPort, client1=>Client1HostPort, source1=>Source1HostPort},
  SourceConnectionsMap = #{mainServer => Router1HostPort, client1=>Router1HostPort, source1=>Router1HostPort},
  io:format("sourcemap: ~p~n",[maps:get(mainServer,PortsMap)]).


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


