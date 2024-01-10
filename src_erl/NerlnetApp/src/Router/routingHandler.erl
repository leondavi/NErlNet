%%%-------------------------------------------------------------------
%%% @author Tal Kapelnik, Guy Perets, Gal Hilu, Haran Cohen
%%% @copyright (C) 2024, Nerlnet
%%% @doc
%%%
%%% @end
%%% Created : 02. Jan 2021 4:05 AM
%%%-------------------------------------------------------------------
-module(routingHandler).
-author("kapelnik").
-export([init/2]).
-include("../nerl_tools.hrl").

%% handler for routing all messages in the network.
%% Action contains the information about the action performed, and Body contains the information needed for the action

%%State = [Action, RouterPID, Graph, RouterName]
init(Req0, State) ->
  Action = lists:nth(1, State),
  Router_genserver_Pid = lists:nth(2, State),

  %Bindings also can be accesed as once, giving a map of all bindings of Req0:
  {_,Body,_} = cowboy_req:read_body(Req0, #{length => ?DATA_LEN}),
  %Decoded_body = binary_to_list(Body),
%  io:format("router got action ~p body:~p~n",[Action,Body]),
  case Action of
    unicast ->gen_server:cast(Router_genserver_Pid, {unicast,binary_to_term(Body)});
    broadcast ->gen_server:cast(Router_genserver_Pid, {broadcast,binary_to_term(Body)})
  end,
  Reply = io_lib:format(" ", []),
%%  Reply = io_lib:format("Body Received: ~p, Decoded Body = ~p ~n Client_StateM_Pid:~p, Handler's Pid: ~p~n ", [Body,Decoded_body,  Router_genserver_Pid,self()]),
  Req = cowboy_req:reply(200,
    #{<<"content-type">> => <<"text/plain">>},
    Reply,
    Req0),
  {ok, Req, State}.