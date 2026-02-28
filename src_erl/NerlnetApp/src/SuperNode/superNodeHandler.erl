%%%-------------------------------------------------------------------
%%% @author Nerlnet
%%% @doc Cowboy handler for super node actions.
%%%-------------------------------------------------------------------
-module(superNodeHandler).
-behavior(application).

-export([init/2, start/2, stop/1]).

init(Req0, [Action, SuperNodePid]) ->
  {ok, Body, _} = cowboy_req:read_body(Req0),
  case Action of
    register_client ->
      case binary_to_term_safe(Body) of
        {register_client, ClientName, Workers} ->
          gen_server:cast(SuperNodePid, {register_client, ClientName, Workers});
        _ -> ok
      end;
    super_heartbeat ->
      case binary_to_term_safe(Body) of
        {heartbeat, ClientName, TsMs} ->
          gen_server:cast(SuperNodePid, {super_heartbeat, ClientName, TsMs});
        _ -> ok
      end;
    parallel_worker_message ->
      case binary_to_term_safe(Body) of
        {parallel_worker_message, FromWorker, ToWorker, Data} ->
          gen_server:cast(SuperNodePid, {parallel_worker_message, FromWorker, ToWorker, Data});
        _ -> ok
      end;
    parallel_deliver_ack ->
      case binary_to_term_safe(Body) of
        {parallel_deliver_ack, ClientName, DeliveryId, AckStatus} ->
          gen_server:cast(SuperNodePid, {parallel_deliver_ack, ClientName, DeliveryId, AckStatus});
        _ -> ok
      end;
    parallel_phase_update ->
      case binary_to_term_safe(Body) of
        {parallel_phase_update, PhaseName, ParallelMode, ParallelExecution, WorkerParallelMap} ->
          _ = gen_server:call(
            SuperNodePid,
            {parallel_phase_update, PhaseName, ParallelMode, ParallelExecution, WorkerParallelMap}
          );
        _ -> ok
      end;
    parallel_event ->
      case binary_to_term_safe(Body) of
        {parallel_event, FromWorker, Direction, BatchID, MicrobatchID, StageID, Meta} ->
          gen_server:cast(
            SuperNodePid,
            {parallel_event, FromWorker, Direction, BatchID, MicrobatchID, StageID, Meta}
          );
        _ -> ok
      end;
    parallel_phase_close ->
      case binary_to_term_safe(Body) of
        {parallel_phase_close, ClientName, PhaseEpoch} ->
          gen_server:cast(
            SuperNodePid,
            {parallel_phase_close, ClientName, PhaseEpoch}
          );
        _ -> ok
      end;
    scheduler_grant_rejected ->
      case binary_to_term_safe(Body) of
        {scheduler_grant_rejected, ClientName, WorkerName, Direction, BatchID, MicrobatchID, StageID, Reason, PhaseEpoch} ->
          gen_server:cast(
            SuperNodePid,
            {scheduler_grant_rejected, ClientName, WorkerName, Direction, BatchID, MicrobatchID, StageID, Reason, PhaseEpoch}
          );
        {scheduler_grant_rejected, ClientName, WorkerName, Direction, MicrobatchID, StageID, Reason, PhaseEpoch} ->
          gen_server:cast(
            SuperNodePid,
            {scheduler_grant_rejected, ClientName, WorkerName, Direction, MicrobatchID, StageID, Reason, PhaseEpoch}
          );
        _ -> ok
      end
  end,
  Req = cowboy_req:reply(
    200,
    #{<<"content-type">> => <<"text/plain">>},
    "ACK",
    Req0
  ),
  {ok, Req, SuperNodePid}.

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
