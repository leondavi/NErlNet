%%%-------------------------------------------------------------------
%%% @author kapelnik
%%% @copyright (C) 2021, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 02. Jan 2021 4:05 AM
%%%-------------------------------------------------------------------
-module(routing_handler).
-author("kapelnik").
-export([init/2,  start/2, stop/1]).
-behaviour(application).


%%handler for routing all messages in the network.
%%Action contains the information about the action performed, and Body contains the information needed for the action
init(Req0, State = [Action,Router_genserver_Pid]) ->
  %Bindings also can be accesed as once, giving a map of all bindings of Req0:
  {ok,Body,_} = cowboy_req:read_body(Req0),
  Decoded_body = binary_to_list(Body),
  io:format("routing handler got Body:~p~n",[Body]),
%%  gen_statem:cast(Client_StateM_Pid,{hello}),
  case Action of
      %%sends an cast for genserver to make an http request for updating CSV lists at all sensors found in Body.
    updateCSV ->  gen_server:cast(Router_genserver_Pid, {updateCSV,Body});

      %%sends an cast for genserver to make an http request for start feeding data.
    start_training ->  gen_statem:cast(Router_genserver_Pid, {start_training,Body});

      %%sends an cast for genserver to make an http request for updating CSV lists at all sensors found in Body.
    stop_training ->  gen_statem:cast(Router_genserver_Pid, {stop_training})
  end,
  Reply = io_lib:format("Body Received: ~p, Decoded Body = ~p ~n Client_StateM_Pid:~p, Handler's Pid: ~p~n ", [Body,Decoded_body,  Router_genserver_Pid,self()]),
  Req = cowboy_req:reply(200,
    #{<<"content-type">> => <<"text/plain">>},
    Reply,
    Req0),
  {ok, Req, State}.


start(StartType, StartArgs) ->
  erlang:error(not_implemented).

stop(State) ->
  erlang:error(not_implemented).