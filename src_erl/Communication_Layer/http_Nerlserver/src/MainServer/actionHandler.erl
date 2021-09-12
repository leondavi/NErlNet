%%%-------------------------------------------------------------------
%%% @author kapelnik
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 31. Dec 2020 4:41 AM
%%%-------------------------------------------------------------------
-module(actionHandler).
-author("kapelnik").
-behavior(application).

-export([init/2, start/2, stop/1]).


%%setter handler for editing weights in CSV file, can also send a reply to sender
init(Req0, [Action, Main_genserver_Pid]) ->
  %Bindings also can be accesed as once, giving a map of all bindings of Req0:
  {_,Body,_} = cowboy_req:read_body(Req0),

  case Action of
    clientsTraining ->  gen_statem:cast(Main_genserver_Pid, {clientsTraining,Body});
    clientsPredict ->  gen_statem:cast(Main_genserver_Pid, {clientsPredict,Body});
    lossFunction ->  gen_statem:cast(Main_genserver_Pid, {lossFunction,Body});
    predictRes ->  gen_statem:cast(Main_genserver_Pid, {predictRes,Body});
    statistics ->  gen_statem:cast(Main_genserver_Pid, {statistics,Body});
    startCasting ->  gen_statem:cast(Main_genserver_Pid, {startCasting,Body});
    stopCasting ->  gen_statem:cast(Main_genserver_Pid, {stopCasting,Body})
  end,
  Reply = io_lib:format("Body Received: ~p ~n ", [Body]),
  Req = cowboy_req:reply(200,
    #{<<"content-type">> => <<"text/plain">>},
    Reply,
    Req0),
  {ok, Req, Main_genserver_Pid}.

start(_StartType, _StartArgs) ->
  erlang:error(not_implemented).

stop(_State) ->
  erlang:error(not_implemented).