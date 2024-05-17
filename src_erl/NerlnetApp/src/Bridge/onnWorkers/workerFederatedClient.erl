-module(workerFederatedClient).

-export([controller/2]).

-include("/usr/local/lib/nerlnet-lib/NErlNet/src_erl/NerlnetApp/src/nerl_tools.hrl").
-include("workerDefinitions.hrl").
-include("w2wCom.hrl").

-define(WORKER_FEDERATED_CLIENT_ETS_FIELDS, [my_name, client_pid, server_name, sync_max_count, sync_count]).
-define(FEDERATED_CLIENT_ETS_KEY_IN_GENWORKER_ETS, fedrated_client_ets).
-define(DEFAULT_SYNC_MAX_COUNT_ARG, 1).

controller(FuncName, {GenWorkerEts, WorkerData}) -> 
  case FuncName of
    init        -> init({GenWorkerEts, WorkerData});
    pre_idle    -> pre_idle({GenWorkerEts, WorkerData});
    post_idle   -> post_idle({GenWorkerEts, WorkerData});
    pre_train   -> pre_train({GenWorkerEts, WorkerData});
    post_train  -> post_train({GenWorkerEts, WorkerData});
    pre_predict -> pre_predict({GenWorkerEts, WorkerData});
    post_predict-> post_predict({GenWorkerEts, WorkerData})
  end.

get_this_client_ets(GenWorkerEts) -> 
  ets:lookup_element(GenWorkerEts, federated_client_ets, ?ETS_KEYVAL_VAL_IDX).

parse_args(Args) -> 
  ArgsList = string:split(Args, "," , all),
  Func = fun(Arg) ->
    [Key, Val] = string:split(Arg, "="),
    {Key, Val}
  end,
  lists:map(Func, ArgsList). % Returns list of tuples [{Key, Val}, ...]

sync_max_count_init(FedClientEts , ArgsList) -> 
  case lists:keyfind("sync_max_count", 1, ArgsList) of
    false -> Val = ?DEFAULT_SYNC_MAX_COUNT_ARG;
    {_, Val} -> list_to_integer(Val)
  end,
  ets:insert(FedClientEts, {sync_max_count, Val}).

%% handshake with workers / server
init({GenWorkerEts, WorkerData}) ->
  % create an ets for this client and save it to generic worker ets
  FedratedClientEts = ets:new(federated_client,[set]),
  ets:insert(GenWorkerEts, {federated_client_ets, FedratedClientEts}),
  io:format("@FedClient: ~p~n",[WorkerData]),
  {MyName, Args, Token} = WorkerData,
  ArgsList = parse_args(Args),
  sync_max_count_init(FedratedClientEts, ArgsList),
  % create fields in this ets
  ets:insert(FedratedClientEts, {my_token, Token}),
  ets:insert(FedratedClientEts, {my_name, MyName}),
  ets:insert(FedratedClientEts, {server_name, none}), % update later
  ets:insert(FedratedClientEts, {sync_count, 0}),
  ets:insert(FedratedClientEts, {server_update, false}),
  ets:insert(FedratedClientEts, {handshake_done, false}),
  spawn(fun() -> handshake(FedratedClientEts) end).

handshake(EtsRef) ->
    w2wCom:sync_inbox(),
    InboxQueue = w2wCom:get_all_messages(),
    MessagesList = queue:to_list(InboxQueue),
    Func = 
      fun({?W2WCOM_ATOM, FromServer, MyName, {handshake, ServerToken}}) ->
        ets:insert(EtsRef, {server_name, FromServer}),
        ets:insert(EtsRef, {token , ServerToken}),
        MyToken = ets:lookup_element(EtsRef, my_token, ?ETS_KEYVAL_VAL_IDX),
        if 
          ServerToken =/= MyToken -> not_my_server; 
          true -> w2wCom:send_message(MyName, FromServer, {handshake, MyToken}) ,
                  ets:update_element(EtsRef, handshake_done, true)
        end
    end,
    lists:foreach(Func, MessagesList),
    % Check if handshake is done
    HandshakeDone = ets:lookup_element(EtsRef, handshake_done, ?ETS_KEYVAL_VAL_IDX),
    if HandshakeDone -> ok;
      true -> handshake(EtsRef)
    end.

pre_idle({_GenWorkerEts, _WorkerData}) -> ok.

post_idle({_GenWorkerEts, _WorkerData}) -> ok.

% After SyncMaxCount , sync_inbox to get the updated model from FedServer
pre_train({GenWorkerEts, NerlTensorWeights}) -> 
  ModelID = ets:lookup_element(GenWorkerEts, model_id, ?ETS_KEYVAL_VAL_IDX),
  nerlNIF:call_to_set_weights(ModelID, NerlTensorWeights).

%% every countLimit batches, send updated weights
post_train({GenWorkerEts, _WorkerData}) -> 
  ThisEts = get_this_client_ets(GenWorkerEts),
  SyncCount = ets:lookup_element(ThisEts, sync_count, ?ETS_KEYVAL_VAL_IDX),
  MaxSyncCount = ets:lookup_element(ThisEts, sync_max_count, ?ETS_KEYVAL_VAL_IDX),
  if SyncCount == MaxSyncCount ->
    ModelID = ets:lookup_element(GenWorkerEts, model_id, ?ETS_KEYVAL_VAL_IDX),
    Weights = nerlNIF:call_to_get_weights(GenWorkerEts, ModelID),
    ServerName = ets:lookup_element(ThisEts, server_name, ?ETS_KEYVAL_VAL_IDX), 
    MyName = ets:lookup_element(GenWorkerEts, worker_name, ?ETS_KEYVAL_VAL_IDX),
    io:format("@post_train: Worker ~p updates federated server ~p~n",[MyName , ServerName]),
    w2wCom:send_message(MyName, ServerName , Weights), %% ****** NEW - TEST NEEDED ******
    ets:update_element(ThisEts, sync_count, {?ETS_KEYVAL_VAL_IDX , 0});
  true ->
    ets:update_counter(ThisEts, sync_count, 1)
  end.

%% nothing?
pre_predict({_GenWorkerEts, WorkerData}) -> WorkerData.

%% nothing?
post_predict(Data) -> Data.

