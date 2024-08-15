-module(workerFederatedClient).

-export([controller/2]).

-include("/usr/local/lib/nerlnet-lib/NErlNet/src_erl/NerlnetApp/src/nerl_tools.hrl").
-include("/usr/local/lib/nerlnet-lib/NErlNet/src_erl/NerlnetApp/src/Bridge/Common/w2wCom.hrl").

-import(nerlNIF, [call_to_get_weights/2, call_to_set_weights/2]).

-define(WORKER_FEDERATED_CLIENT_ETS_FIELDS, [my_name, client_pid, server_name, sync_max_count, sync_count]).
-define(FEDERATED_CLIENT_ETS_KEY_IN_GENWORKER_ETS, fedrated_client_ets).
-define(DEFAULT_SYNC_MAX_COUNT_ARG, 100).

controller(FuncName, {GenWorkerEts, WorkerData}) -> 
  case FuncName of
    init          -> init({GenWorkerEts, WorkerData});
    pre_idle      -> pre_idle({GenWorkerEts, WorkerData});
    post_idle     -> post_idle({GenWorkerEts, WorkerData});
    pre_train     -> pre_train({GenWorkerEts, WorkerData});
    post_train    -> post_train({GenWorkerEts, WorkerData});
    pre_predict   -> pre_predict({GenWorkerEts, WorkerData});
    post_predict  -> post_predict({GenWorkerEts, WorkerData});
    start_stream  -> start_stream({GenWorkerEts, WorkerData});
    end_stream    -> end_stream({GenWorkerEts, WorkerData})
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
  case lists:keyfind("SyncMaxCount", 1, ArgsList) of
    false -> ValInt = ?DEFAULT_SYNC_MAX_COUNT_ARG;
    {_, Val} -> ValInt = list_to_integer(Val) % Val is a list (string) in the json so needs to be converted
  end,
  ets:insert(FedClientEts, {sync_max_count, ValInt}).

%% handshake with workers / server at the end of init
init({GenWorkerEts, WorkerData}) ->
  % create an ets for this client and save it to generic worker ets
  FederatedClientEts = ets:new(federated_client,[set, public]),
  ets:insert(GenWorkerEts, {federated_client_ets, FederatedClientEts}),
  {MyName, Args, Token} = WorkerData,
  ArgsList = parse_args(Args),
  sync_max_count_init(FederatedClientEts, ArgsList),
  W2WPid = ets:lookup_element(GenWorkerEts, w2wcom_pid, ?ETS_KEYVAL_VAL_IDX),
  % create fields in this ets
  ets:insert(FederatedClientEts, {my_token, Token}),
  ets:insert(FederatedClientEts, {my_name, MyName}),
  ets:insert(FederatedClientEts, {server_name, []}), % update later
  ets:insert(FederatedClientEts, {sync_count, 0}),
  ets:insert(FederatedClientEts, {server_update, false}),
  ets:insert(FederatedClientEts, {handshake_done, false}),
  ets:insert(FederatedClientEts, {handshake_wait, false}),
  ets:insert(FederatedClientEts, {w2wcom_pid, W2WPid}),
  ets:insert(FederatedClientEts, {active_streams, []}),
  ets:insert(FederatedClientEts, {stream_occuring, false}),
  ets:insert(FederatedClientEts, {wait_for_weights_update, false}),
  spawn(fun() -> handshake(FederatedClientEts) end).

handshake(FedClientEts) ->
  W2WPid = ets:lookup_element(FedClientEts, w2wcom_pid, ?ETS_KEYVAL_VAL_IDX),
  io:format("Handshake with server~n"),
  w2wCom:sync_inbox(W2WPid),
  InboxQueue = w2wCom:get_all_messages(W2WPid),
  MessagesList = queue:to_list(InboxQueue),
  Func = 
    fun({FedServer , {handshake, ServerToken}}) ->
      ets:insert(FedClientEts, {server_name, FedServer}),
      ets:insert(FedClientEts, {my_token , ServerToken}),
      MyToken = ets:lookup_element(FedClientEts, my_token, ?ETS_KEYVAL_VAL_IDX),
      MyName = ets:lookup_element(FedClientEts, my_name, ?ETS_KEYVAL_VAL_IDX),
      if 
        ServerToken =/= MyToken -> not_my_server; 
        true -> w2wCom:send_message(W2WPid, MyName, FedServer, {handshake, MyToken}),
                % io:format("@FedClient: Sent handshake to server ~p with token ~p~n", [FedServer, MyToken]),
                ets:update_element(FedClientEts, handshake_wait, {?ETS_KEYVAL_VAL_IDX, true})
      end
  end,
  lists:foreach(Func, MessagesList).

start_stream({GenWorkerEts, WorkerData}) ->  % WorkerData is currently a list of [SourceName, State]
  [_SourceName, ModelPhase] = WorkerData,
  FirstMsg = 1,
  case ModelPhase of
    train ->
        ThisEts = get_this_client_ets(GenWorkerEts),
        MyName = ets:lookup_element(ThisEts, my_name, ?ETS_KEYVAL_VAL_IDX),
        ServerName = ets:lookup_element(ThisEts, server_name, ?ETS_KEYVAL_VAL_IDX),
        W2WPid = ets:lookup_element(ThisEts, w2wcom_pid, ?ETS_KEYVAL_VAL_IDX),
        ActiveStreams = ets:lookup_element(GenWorkerEts, active_streams, ?ETS_KEYVAL_VAL_IDX),
        case length(ActiveStreams) of % Send to server an updater after got start_stream from the first source
          FirstMsg ->   w2wCom:send_message_with_event(W2WPid, MyName, ServerName , start_stream, MyName); % Server gets FedWorkerName instead of SourceName
          _ -> ok
        end;
      predict -> ok
  end.

end_stream({GenWorkerEts, WorkerData}) -> % WorkerData is currently a list of [SourceName]
  [_SourceName, ModelPhase] = WorkerData,
  case ModelPhase of
    predict -> ok;
    _ -> % train/wait
        ThisEts = get_this_client_ets(GenWorkerEts),
        MyName = ets:lookup_element(ThisEts, my_name, ?ETS_KEYVAL_VAL_IDX),
        ServerName = ets:lookup_element(ThisEts, server_name, ?ETS_KEYVAL_VAL_IDX),
        W2WPid = ets:lookup_element(ThisEts, w2wcom_pid, ?ETS_KEYVAL_VAL_IDX),
        ActiveStreams = ets:lookup_element(GenWorkerEts, active_streams, ?ETS_KEYVAL_VAL_IDX),
        case length(ActiveStreams) of % Send to server an updater after got start_stream from the first source
          0 ->  w2wCom:send_message_with_event(W2WPid, MyName, ServerName , end_stream, MyName); % Mimic source behavior
          _ -> ok
        end
  end.


pre_idle({_GenWorkerEts, _WorkerData}) -> ok.

post_idle({GenWorkerEts, _WorkerData}) -> 
  FedClientEts = get_this_client_ets(GenWorkerEts),
  W2WPid = ets:lookup_element(FedClientEts, w2wcom_pid, ?ETS_KEYVAL_VAL_IDX),
  Token = ets:lookup_element(FedClientEts, my_token, ?ETS_KEYVAL_VAL_IDX),
  HandshakeWait = ets:lookup_element(FedClientEts, handshake_wait, ?ETS_KEYVAL_VAL_IDX),
  _MyName = ets:lookup_element(FedClientEts, my_name, ?ETS_KEYVAL_VAL_IDX),
  case HandshakeWait of 
    true -> HandshakeDone = ets:lookup_element(FedClientEts, handshake_done, ?ETS_KEYVAL_VAL_IDX),
            case HandshakeDone of 
            false -> 
              w2wCom:sync_inbox(W2WPid),
              InboxQueue = w2wCom:get_all_messages(W2WPid),
              [{_FedServer, {handshake_done, Token}}] = queue:to_list(InboxQueue),
              io:format("Handshake done with server~n"),
              ets:update_element(FedClientEts, handshake_done, {?ETS_KEYVAL_VAL_IDX, true});
              % io:format("Worker is part of cluster with token ~p~n", [Token]);
            true -> ok
            end;
    false -> post_idle({GenWorkerEts, _WorkerData}) % busy waiting until handshake is done
  end.
  

pre_train({_GenWorkerEts, _NerlTensorWeights}) -> ok.

% post_train_update is a message from the server to update weights, so we need to wait for it
post_train({GenWorkerEts, {post_train_update, {_SyncIdx, UpdatedWeights}}}) ->
  FedClientEts = get_this_client_ets(GenWorkerEts),
  WeightsUpdateFlag = ets:lookup_element(FedClientEts, wait_for_weights_update, ?ETS_KEYVAL_VAL_IDX), % either true or false
  case WeightsUpdateFlag of
    false -> throw("Received weights update but not waiting for it");
    true -> 
      % io:format("Got updated weights from server~n"),
      ModelID = ets:lookup_element(GenWorkerEts, model_id, ?ETS_KEYVAL_VAL_IDX),
      nerlNIF:call_to_set_weights(ModelID, UpdatedWeights),
      ets:update_element(FedClientEts, wait_for_weights_update, {?ETS_KEYVAL_VAL_IDX, false}),
      ets:update_element(FedClientEts, sync_count, {?ETS_KEYVAL_VAL_IDX , 0})
  end,
  train;

%% every MaxSyncCount batches, send updated weights to server
post_train({GenWorkerEts, _Data}) -> 
  MyName = ets:lookup_element(GenWorkerEts, worker_name, ?ETS_KEYVAL_VAL_IDX),
  ActiveStreams = ets:lookup_element(GenWorkerEts, active_streams, ?ETS_KEYVAL_VAL_IDX),
  FedClientEts = get_this_client_ets(GenWorkerEts),
  ets:update_counter(FedClientEts, sync_count, 1),
  % io:format("Worker ~p ActiveStreams ~p~n",[MyName, ActiveStreams]),
  case ActiveStreams of
    [] -> train;
    _ ->
      SyncCount = ets:lookup_element(FedClientEts, sync_count, ?ETS_KEYVAL_VAL_IDX),
      MaxSyncCount = ets:lookup_element(FedClientEts, sync_max_count, ?ETS_KEYVAL_VAL_IDX),
      if SyncCount == MaxSyncCount ->
        % io:format("~p sent averaging request to server~n", [MyName]),
        ModelID = ets:lookup_element(GenWorkerEts, model_id, ?ETS_KEYVAL_VAL_IDX),
        WeightsTensor = nerlNIF:call_to_get_weights(ModelID),
        ServerName = ets:lookup_element(FedClientEts, server_name, ?ETS_KEYVAL_VAL_IDX), 
        W2WPid = ets:lookup_element(FedClientEts, w2wcom_pid, ?ETS_KEYVAL_VAL_IDX),
        w2wCom:send_message_with_event(W2WPid, MyName, ServerName , post_train_update, WeightsTensor),
        ets:update_element(FedClientEts, wait_for_weights_update, {?ETS_KEYVAL_VAL_IDX, true}),
        wait; % wait for server to send updated weights, workerGeneric should stay in wait state
      true -> train
      end
  end.

%% nothing?
pre_predict({_GenWorkerEts, WorkerData}) -> WorkerData.

%% nothing?
post_predict(Data) -> Data.



