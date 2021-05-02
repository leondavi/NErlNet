%%%-------------------------------------------------------------------
%%% @author kapelnik
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 31. Dec 2020 4:41 AM
%%%-------------------------------------------------------------------
-module(action_handler).
-author("kapelnik").
-behavior(cowboy_handler).

-export([init/2]).


%%setter handler for editing weights in CSV file, can also send a reply to sender
init(Req0, [Main_genserver_Pid,Action]) ->
  %Bindings also can be accesed as once, giving a map of all bindings of Req0:
  {_,Body,_} = cowboy_req:read_body(Req0),
  case Action of
    start ->  gen_statem:cast(Main_genserver_Pid, {start_casting,20});
    stop ->  gen_statem:cast(Main_genserver_Pid, {stop_casting})
  end,
  Reply = io_lib:format("Body Received: ~p ~n ", [Body]),
  Req = cowboy_req:reply(200,
    #{<<"content-type">> => <<"text/plain">>},
    Reply,
    Req0),
  {ok, Req, Main_genserver_Pid}.