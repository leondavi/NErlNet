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
    worker_to_worker_msg -> case binary_to_term_safe(Body) of
                              {worker_to_worker_msg , From , To , Data} ->
                                gen_statem:cast(Client_StateM_Pid,{worker_to_worker_msg , From , To , Data});
                              _ -> ok
                            end;
    parallel_deliver -> case binary_to_term_safe(Body) of
                          {parallel_deliver, DeliveryId, From, To, Data} ->
                            gen_statem:cast(Client_StateM_Pid, {parallel_deliver, DeliveryId, From, To, Data});
                          {parallel_deliver, From, To, Data} ->
                            gen_statem:cast(Client_StateM_Pid, {parallel_deliver, From, To, Data});
                          {worker_to_worker_msg, From, To, Data} ->
                            gen_statem:cast(Client_StateM_Pid, {parallel_deliver, From, To, Data});
                          _ -> ok
                        end;
    batch      -> gen_statem:cast(Client_StateM_Pid,{sample,Body});
    idle        -> gen_statem:cast(Client_StateM_Pid,{idle});
    training    -> gen_statem:cast(Client_StateM_Pid,{training});
    predict     -> gen_statem:cast(Client_StateM_Pid,{predict});
    statistics  -> gen_statem:cast(Client_StateM_Pid,{statistics});
    parallel_mode -> case parse_parallel_mode(binary_to_term_safe(Body)) of
                       undefined -> ok;
                       Mode -> gen_statem:cast(Client_StateM_Pid,{set_parallel_mode, Mode, main_server})
                     end;
    parallel_execution -> ParallelExecution =
                            case binary_to_term_safe(Body) of
                              Map when is_map(Map) -> Map;
                              _ -> #{}
                            end,
                          gen_statem:cast(Client_StateM_Pid,{set_parallel_execution, ParallelExecution, main_server});
    parallel_super_command ->
      case binary_to_term_safe(Body) of
        undefined -> ok;
        SuperCommand -> gen_statem:cast(Client_StateM_Pid, {parallel_super_command, SuperCommand})
      end;
    start_stream -> gen_statem:cast(Client_StateM_Pid,{start_stream, Body});
    end_stream -> gen_statem:cast(Client_StateM_Pid,{end_stream, Body})
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

binary_to_term_safe(Body) ->
  try binary_to_term(Body, [safe]) of
    Term -> Term
  catch
    _:_ -> undefined
  end.

parse_parallel_mode("legacy") -> legacy;
parse_parallel_mode("pipeline") -> pipeline;
parse_parallel_mode("tensor") -> tensor;
parse_parallel_mode("pipeline_tensor") -> pipeline_tensor;
parse_parallel_mode(<<"legacy">>) -> legacy;
parse_parallel_mode(<<"pipeline">>) -> pipeline;
parse_parallel_mode(<<"tensor">>) -> tensor;
parse_parallel_mode(<<"pipeline_tensor">>) -> pipeline_tensor;
parse_parallel_mode(legacy) -> legacy;
parse_parallel_mode(pipeline) -> pipeline;
parse_parallel_mode(tensor) -> tensor;
parse_parallel_mode(pipeline_tensor) -> pipeline_tensor;
parse_parallel_mode(_) -> undefined.
