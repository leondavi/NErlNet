%%%-------------------------------------------------------------------
%%% @author kapelnik
%%% @copyright (C) 2021, Nerlnet
%%% @doc
%%%
%%% @end
%%% Created : 02. Jan 2021 4:05 AM
%%%-------------------------------------------------------------------
-module(weightsHandler).
-author("kapelnik").
-export([init/2,  start/2, stop/1]).
-behaviour(application).


%%handler for receiveing vectors of samples from Sensors
init(Req0, [Action,FederatedStateM_Pid]) ->
  %Bindings also can be accesed as once, giving a map of all bindings of Req0:
  {ok,Body,_} = cowboy_req:read_body(Req0),
%%  Decoded_body = binary_to_list(Body),

  case Action of

    federatedWeightsVector -> gen_statem:cast(FederatedStateM_Pid,{federatedWeightsVector,Body});
    statistics ->gen_statem:cast(FederatedStateM_Pid,{statistics})
  end,


%%  Reply = io_lib:format("Weights Received~n ", []),
  Reply = io_lib:format(" ", []),
  Req = cowboy_req:reply(200,
    #{<<"content-type">> => <<"text/plain">>},
    Reply,
    Req0),
  {ok, Req, FederatedStateM_Pid}.


start(_StartType, _StartArgs) ->
  erlang:error(not_implemented).

stop(_State) ->
  erlang:error(not_implemented).