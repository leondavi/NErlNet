
-module(clientWorkersFunctions).
-author("Guy").

-include("../nerl_tools.hrl").
-include("../worker_definitions_ag.hrl").

-export([createWorkers/3]).

createWorkers(ClientName, EtsRef , ShaToModelArgsMap) ->
  CLIENT_WORKES_MAPS_TUPLE_IDX = 2,
  ClientsMap = maps:from_list(ets:lookup_element(nerlnet_data, deviceClients, ?DATA_IDX)), % This is the format of hostClients {Name,{Port,ClientWorkers,ClientWorkersMaps}}
  io:format("ClientWorkersMaps: ~p~n", [ClientsMap]),
  ClientWorkers = element(CLIENT_WORKES_MAPS_TUPLE_IDX,maps:get(ClientName, ClientsMap)), 
  io:format("ClientWorkers: ~p~n", [ClientWorkers]),
  WorkerETS = ets:new(worker_ets,[set]),
  io:format("WorkerToSHAMap: ~p~n" , [ets:lookup_element(EtsRef, workers_to_sha_map, ?DATA_IDX)]),

  Func = fun(WorkerName) -> 
    ModelId = erlang:unique_integer([positive]),
    {ok , SHA} = maps:find(WorkerName , ets:lookup_element(EtsRef, workers_to_sha_map, ?DATA_IDX)),
    {ModelType, LayersSizes, LayersTypes, LayersFunctions, LossMethod, 
    LearningRate, Epochs, Optimizer, OptimizerArgs, _InfraType, DistributedSystemType, 
    DistributedSystemArgs, DistributedSystemToken} = maps:get(SHA, ShaToModelArgsMap),
    MyClientPid = self(),
    io:format("MyClientPid: ~p~n", [MyClientPid]),
    % TODO add documentation about this case of 
    % move this case to module called client_controller
    case DistributedSystemType of
      ?DC_DISTRIBUTED_SYSTEM_TYPE_NONE_IDX_STR ->
        CustomFunc = fun workerNN:controller/2,
        WorkerData = none;
      ?DC_DISTRIBUTED_SYSTEM_TYPE_FEDCLIENTAVG_IDX_STR ->
        CustomFunc = fun workerFederatedClient:controller/2,
        WorkerData = {_WorkerName = WorkerName , _Args = DistributedSystemArgs, _Token = DistributedSystemToken};
      %% Parse args eg. batch_sync_count
      ?DC_DISTRIBUTED_SYSTEM_TYPE_FEDSERVERAVG_IDX_STR ->
        CustomFunc = fun workerFederatedServer:controller/2,
        WorkerData = {_ServerName = WorkerName , _Args = DistributedSystemArgs, _Token = DistributedSystemToken, _WorkersNamesList = []}
      end,
    io:format("WorkerData: ~p~n", [WorkerData]),
    WorkerArgs = {WorkerName,ModelId,ModelType,LayersSizes,LayersTypes,LayersFunctions,
                  LossMethod,LearningRate,Epochs,Optimizer,OptimizerArgs, MyClientPid, CustomFunc, WorkerData},
    io:format("WorkerArgs: ~p~n", [WorkerArgs]),
    WorkerPid = workerGeneric:start_link(WorkerArgs),

    ets:insert(EtsRef, {WorkerName, WorkerPid, WorkerArgs, {0,0,0.0}, 0}), %% ! Deprecated

    ets:insert(WorkerETS, {worker_pid, WorkerPid}),
    %% TODO: Add more fields to worker ets
    WorkerName
  end,

  WorkersNames = lists:map(Func, ClientWorkers),   %% TODO: collect forbidden names (keys of ets:insert)

  ets:insert(EtsRef, {workersNames, WorkersNames}).  