%similar to actionHandler, but for GUI

-module(guiHandler).
-behavior(application).

-export([init/2, start/2, stop/1]).


%%setter handler for editing weights in CSV file, can also send a reply to sender
init(Req0, [Action, Main_genserver_Pid]) ->
  %Bindings also can be accesed as once, giving a map of all bindings of Req0:
  {_,_Body,_} = cowboy_req:read_body(Req0),

  Reply = case Action of
    %getGraph ->  gen_statem:call(Main_genserver_Pid, {getGraph,Body})
    getGraph ->  gen_statem:call(Main_genserver_Pid, getGraph);
    getStats ->  gen_statem:call(Main_genserver_Pid, getStats)
  end,
  %Reply = io_lib:format("Body Received: ~p ~n ", [Body]),
  io:format("replying to call with: ~p~n", [Reply]),
  Req = cowboy_req:reply(200,
    #{<<"content-type">> => <<"text/plain">>},
    Reply,
    Req0),
  {ok, Req, Main_genserver_Pid}.

start(_StartType, _StartArgs) ->
  erlang:error(not_implemented).

stop(_State) ->
  erlang:error(not_implemented).