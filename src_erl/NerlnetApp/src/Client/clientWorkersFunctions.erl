
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


  Func = fun(WorkerName) -> 
    ModelID = erlang:unique_integer([positive]),
    WorkerStatsETS = stats:generate_workers_stats_ets(),
    {ok , SHA} = maps:find(WorkerName , ets:lookup_element(ClientEtsRef, workers_to_sha_map, ?DATA_IDX)),
    {ModelType, ModelArgs, LayersSizes, LayersTypes, LayersFunctions, LossMethod, LossArgs,
    LearningRate, Epochs, Optimizer, OptimizerArgs, _InfraType, DistributedSystemType, 
    DistributedSystemArgs, DistributedSystemToken} = maps:get(SHA, ShaToModelArgsMap),
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

    WorkerArgs = {ModelID , ModelType , ModelArgs , LayersSizes, LayersTypes, LayersFunctions, LearningRate , Epochs, 
                  Optimizer, OptimizerArgs , LossMethod , LossArgs, DistributedSystemType , DistributedSystemToken, DistributedSystemArgs},
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

  




