
-module(clientWorkersFunctions).
-author("Guy").

-include("../nerl_tools.hrl").
-include("../worker_definitions_ag.hrl").

-export([create_workers/4]).
-export([get_worker_pid/2 , get_worker_stats_ets/2 , get_workers_names/1]).

get_distributed_worker_behavior(ClientEtsRef, DistributedSystemType , WorkerName , DistributedSystemArgs , DistributedSystemToken) ->
case DistributedSystemType of
      ?DC_DISTRIBUTED_SYSTEM_TYPE_TILES_IDX_STR ->
        DistributedBehaviorFunc = fun workerNN:controller/2,
        DistributedWorkerData = {tiles, DistributedSystemArgs, DistributedSystemToken};
      ?DC_DISTRIBUTED_SYSTEM_TYPE_NONE_IDX_STR ->
        DistributedBehaviorFunc = fun workerNN:controller/2,
        DistributedWorkerData = none;
      ?DC_DISTRIBUTED_SYSTEM_TYPE_FEDCLIENTAVG_IDX_STR ->
        DistributedBehaviorFunc = fun workerFederatedClient:controller/2,
        DistributedWorkerData = {_WorkerName = WorkerName , _Args = DistributedSystemArgs, _Token = DistributedSystemToken};
      %% Parse args eg. batch_sync_count
      ?DC_DISTRIBUTED_SYSTEM_TYPE_FEDSERVERAVG_IDX_STR ->
        WorkersMap = ets:lookup_element(ClientEtsRef, workerToClient, ?DATA_IDX),
        WorkersList = [Worker || {Worker, _Val} <- maps:to_list(WorkersMap)],
        DistributedBehaviorFunc = fun workerFederatedServer:controller/2,
        NumOfFedServers = ets:lookup_element(ClientEtsRef, num_of_fed_servers, ?DATA_IDX),
        UpdatedNumOfFedServers = NumOfFedServers + 1,
        ets:update_element(ClientEtsRef, num_of_fed_servers, {?DATA_IDX, UpdatedNumOfFedServers}),
        DistributedWorkerData = {_ServerName = WorkerName , _Args = DistributedSystemArgs, _Token = DistributedSystemToken , _WorkersList = WorkersList}
      end,
{DistributedBehaviorFunc , DistributedWorkerData}.

%% Create workers for clients
%% 
%% ets tree structure: ClientETS -> WorkersETS -> {WorkerName, {WorkerStatsETS , WorkerPid, WorkerArgs}}
create_workers(ClientName, ClientEtsRef , ShaToModelArgsMap , EtsStats) ->
  CLIENT_WORKES_MAPS_TUPLE_IDX = 2,
  ClientsMap = maps:from_list(ets:lookup_element(nerlnet_data, deviceClients, ?DATA_IDX)), % This is the format of hostClients {Name,{Port,ClientWorkers,ClientWorkersMaps}}
  ClientWorkers = element(CLIENT_WORKES_MAPS_TUPLE_IDX,maps:get(ClientName, ClientsMap)), 
  WorkersETS = ets:new(workers_ets,[set]),
  TorchModelsMap = get_torch_models_map(),

  Func = fun(WorkerName) -> 
    ModelID = erlang:unique_integer([positive]),
    WorkerStatsETS = stats:generate_workers_stats_ets(),
    {ok , SHA} = maps:find(WorkerName , ets:lookup_element(ClientEtsRef, workers_to_sha_map, ?DATA_IDX)),
    {ModelType, ModelArgs, LayersSizes, LayersTypes, LayersFunctions, LossMethod, LossArgs,
    LearningRate, Epochs, Optimizer, OptimizerArgs, InfraType, DistributedSystemType, 
    DistributedSystemArgs, DistributedSystemToken, TrainParams} = maps:get(SHA, ShaToModelArgsMap),
    ResolvedModelArgs = maybe_apply_torch_model_args(InfraType, ModelArgs, SHA, TorchModelsMap),
    PreparedTrainParams = prepare_train_params(InfraType, TrainParams, SHA, TorchModelsMap, ResolvedModelArgs),
    DistributedTypeInteger = list_to_integer(DistributedSystemType),
    if 
      DistributedTypeInteger > 0 -> % not none (distributed)
        if length(DistributedSystemToken) == 5 -> 
          ?LOG_INFO("~p Running a Distributed independent system on this device with Token: ~p",[WorkerName, DistributedSystemToken]);
          true -> throw("Distributed non-independent system (e.g., Federated Learning) must have a token which is NOT none. Add it to the Distributed Config json file under distributedSystemToken field, make sure it is 5 characters long")
        end;
      true -> ok
    end,
    MyClientPid = self(),
    % TODO add documentation about this case of 
    % move this case to module called client_controller
    {DistributedBehaviorFunc , DistributedWorkerData} = get_distributed_worker_behavior(ClientEtsRef, DistributedSystemType , WorkerName , DistributedSystemArgs , DistributedSystemToken),
    W2wComPid = w2wCom:start_link({WorkerName, MyClientPid}), % TODO Switch to monitor instead of link

        WorkerArgs = {ModelID , ModelType , ResolvedModelArgs , LayersSizes, LayersTypes, LayersFunctions, LearningRate , Epochs, 
          Optimizer, OptimizerArgs , LossMethod , LossArgs, InfraType, DistributedSystemType , DistributedSystemToken, DistributedSystemArgs, PreparedTrainParams},
    WorkerPid = workerGeneric:start_link({WorkerName , WorkerArgs , DistributedBehaviorFunc , DistributedWorkerData , MyClientPid , WorkerStatsETS , W2wComPid}),
    gen_server:cast(W2wComPid, {update_gen_worker_pid, WorkerPid}),
    ets:insert(WorkersETS, {WorkerName, {WorkerPid, WorkerArgs}}), 
    ets:insert(EtsStats, {WorkerName, WorkerStatsETS}),
    W2WPidMap = ets:lookup_element(ClientEtsRef, w2wcom_pids, ?DATA_IDX),
    W2WPidMapNew = maps:put(WorkerName, W2wComPid, W2WPidMap),
    ets:update_element(ClientEtsRef, w2wcom_pids, {?DATA_IDX, W2WPidMapNew}),

    WorkerName
  end,
  lists:foreach(Func, ClientWorkers),   %% TODO: collect forbidden names (keys of ets:insert)
  ets:insert(ClientEtsRef, {workers_ets, WorkersETS}).

get_worker_stats_ets(ClientEtsRef , WorkerName) ->
  WorkersETS = ets:lookup_element(ClientEtsRef, workers_ets, ?DATA_IDX),
  {WorkerStatsETS , _WorkerPid , _WorkerArgs} = ets:lookup_element(WorkersETS, WorkerName, ?DATA_IDX),
  WorkerStatsETS.

get_worker_pid(ClientEtsRef , WorkerName) ->
  WorkersETS = ets:lookup_element(ClientEtsRef, workers_ets, ?DATA_IDX),
  {WorkerPid , _WorkerArgs} = ets:lookup_element(WorkersETS, WorkerName, ?DATA_IDX),
  WorkerPid.

get_workers_names(ClientEtsRef) ->
  WorkersETS = ets:lookup_element(ClientEtsRef, workers_ets, ?DATA_IDX),
  _WorkersNames = [WorkerName || {WorkerName, _Val} <- ets:tab2list(WorkersETS)].

prepare_train_params(InfraType, TrainParams, SHA, TorchModelsMap, ModelPath) ->
  Normalized = ensure_string_map(TrainParams),
  case is_torch_infra(InfraType) of
    false -> Normalized;
    true ->
      ShaKey = normalize_string(SHA),
      TorchMeta = maps:get(ShaKey, TorchModelsMap, #{}),
      Format = maps:get(format, TorchMeta, "torchscript"),
      Checksum = maps:get(checksum, TorchMeta, ""),
      Description = maps:get(description, TorchMeta, ""),
      SystemParams = #{
        "model_path" => ModelPath,
        "model_sha" => ShaKey,
        "model_format" => Format,
        "model_checksum" => Checksum,
        "model_description" => Description
      },
      maps:merge(Normalized, SystemParams)
  end.

ensure_string_map(undefined) -> #{};
ensure_string_map(Map) when is_map(Map) ->
  maps:from_list([
    {normalize_string(Key), normalize_string(Value)}
    || {Key, Value} <- maps:to_list(Map)
  ]);
ensure_string_map(List) when is_list(List) ->
  ensure_string_map(maps:from_list(List));
ensure_string_map(_) -> #{}.

  



get_torch_models_map() ->
  case catch ets:lookup_element(nerlnet_data, torch_models_map, ?DATA_IDX) of
    {'EXIT', _} -> #{};
    Map -> Map
  end.

maybe_apply_torch_model_args(InfraType, ModelArgs, SHA, TorchModelsMap) ->
  case is_torch_infra(InfraType) of
    false -> ModelArgs;
    true ->
      ShaKey = normalize_string(SHA),
      case maps:get(ShaKey, TorchModelsMap, undefined) of
        undefined -> ModelArgs;
        TorchMeta ->
          case resolve_torch_model_path(ShaKey, TorchMeta) of
            undefined -> ModelArgs;
            ResolvedPath ->
              ensure_torch_model_available(ResolvedPath, ShaKey),
              ResolvedPath
          end
      end
  end.

is_torch_infra(InfraType) ->
  Lower = string:lowercase(normalize_string(InfraType)),
  lists:member(Lower, ["torch", "2"]).

normalize_string(Value) when is_list(Value) ->
  case io_lib:printable_list(Value) of
    true -> Value;
    false -> lists:flatten(io_lib:format("~p", [Value]))
  end;
normalize_string(Value) when is_binary(Value) -> binary_to_list(Value);
normalize_string(Value) when is_atom(Value) -> atom_to_list(Value);
normalize_string(Value) when is_integer(Value) -> integer_to_list(Value);
normalize_string(Other) -> lists:flatten(io_lib:format("~p", [Other])).

ensure_torch_model_available(Path, Sha) ->
  case filelib:is_file(Path) of
    true -> ok;
    false ->
      ?LOG_ERROR("Torch model ~p expected at ~p but not found", [Sha, Path]),
      throw({missing_torch_model, Sha, Path})
  end.

resolve_torch_model_path(ShaKey, TorchMeta) ->
  CandidatePaths = build_torch_model_candidate_paths(ShaKey, TorchMeta),
  case find_existing_candidate(CandidatePaths) of
    undefined ->
      case CandidatePaths of
        [First | _] -> First;
        [] -> undefined
      end;
    Found -> Found
  end.

build_torch_model_candidate_paths(ShaKey, TorchMeta) ->
  RawPath = maps:get(path, TorchMeta, ""),
  ArtifactName = torch_artifact_name(TorchMeta, RawPath),
  Primary = maybe_abs_path(RawPath),
  Staged = build_staged_candidate(ShaKey, ArtifactName),
  compact_candidates([Primary, Staged]).

build_staged_candidate(_ShaKey, undefined) -> undefined;
build_staged_candidate(_ShaKey, "") -> undefined;
build_staged_candidate(ShaKey, ArtifactName) ->
  SafeSha = sanitize_sha(ShaKey),
  case SafeSha of
    "" -> undefined;
    _ ->
      BaseDir = filename:absname(?TORCH_MODELS_BASE_DIR),
      filename:join([BaseDir, SafeSha, ArtifactName])
  end.

torch_artifact_name(TorchMeta, RawPath) ->
  case maps:get(artifact_name, TorchMeta, undefined) of
    undefined -> derive_artifact_name(RawPath);
    Name when is_list(Name) ->
      case string:trim(Name) of
        "" -> derive_artifact_name(RawPath);
        Trimmed -> Trimmed
      end;
    _Other -> derive_artifact_name(RawPath)
  end.

derive_artifact_name(RawPath) ->
  case normalize_and_trim(RawPath) of
    "" -> undefined;
    Trimmed -> filename:basename(Trimmed)
  end.

maybe_abs_path(Value) when is_binary(Value) -> maybe_abs_path(binary_to_list(Value));
maybe_abs_path(Value) when is_list(Value) ->
  case normalize_and_trim(Value) of
    "" -> undefined;
    Trimmed -> filename:absname(Trimmed)
  end;
maybe_abs_path(_) -> undefined.

normalize_and_trim(Value) when is_list(Value) -> string:trim(Value);
normalize_and_trim(Value) -> normalize_and_trim(normalize_string(Value)).

compact_candidates(Candidates) -> compact_candidates(Candidates, []).

compact_candidates([], Acc) -> lists:reverse(Acc);
compact_candidates([undefined | Rest], Acc) -> compact_candidates(Rest, Acc);
compact_candidates([Candidate | Rest], Acc) ->
  case lists:member(Candidate, Acc) of
    true -> compact_candidates(Rest, Acc);
    false -> compact_candidates(Rest, [Candidate | Acc])
  end.

find_existing_candidate([]) -> undefined;
find_existing_candidate([Candidate | Rest]) ->
  case filelib:is_file(Candidate) of
    true -> Candidate;
    false -> find_existing_candidate(Rest)
  end.

sanitize_sha(ShaKey) ->
  lists:filter(fun(Char) -> is_alphanumeric(Char) end, ShaKey).

is_alphanumeric(Char) when Char >= $0, Char =< $9 -> true;
is_alphanumeric(Char) when Char >= $A, Char =< $Z -> true;
is_alphanumeric(Char) when Char >= $a, Char =< $z -> true;
is_alphanumeric(_) -> false.






