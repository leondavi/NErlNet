%%%-------------------------------------------------------------------
%%% @author kapelnik
%%% @copyright (C) 2020, Nerlnet
%%% @doc
%%%
%%% @end
%%% Created : 31. Dec 2020 4:41 AM
%%%-------------------------------------------------------------------
-module(clientStateHandler).
-author("kapelnik").
-behavior(application).

-export([init/2, start/2, stop/1]).


%%init_handler handles http requests for starting nerlnet with given parameters.
init(Req0, [Action,Client_StateM_Pid]) ->

  {ok,Body,_} = cowboy_req:read_body(Req0),
  case Action of
    worker_to_worker_msg -> {worker_to_worker_msg , From , To , Data} = binary_to_term(Body),
                            gen_statem:cast(Client_StateM_Pid,{worker_to_worker_msg , From , To , Data});
    batch      -> gen_statem:cast(Client_StateM_Pid,{sample,Body});
    idle        -> gen_statem:cast(Client_StateM_Pid,{idle});
    training    -> gen_statem:cast(Client_StateM_Pid,{training});
    predict     -> gen_statem:cast(Client_StateM_Pid,{predict});
    statistics  -> gen_statem:cast(Client_StateM_Pid,{statistics})
  end,

  %% reply ACKnowledge to main server for initiating, later send finished initiating http_request from client_stateM
  Reply = io_lib:format("ACK", []),

  Req = cowboy_req:reply(200,
    #{<<"content-type">> => <<"text/plain">>},
    Reply,
    Req0),
  {ok, Req, Client_StateM_Pid}.



start(_StartType, _StartArgs) ->
  erlang:error(not_implemented).

stop(_State) ->
  erlang:error(not_implemented).
