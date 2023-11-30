
-module(clientWorkersFunctions).
-author("Guy").

-include("../nerl_tools.hrl").

-export([createWorkers/2]).


createWorkers(ClientName, EtsRef) ->
  CLIENT_WORKES_MAPS_TUPLE_IDX = 3,
  MapOfHostClients = maps:from_list(ets:lookup_element(nerlnet_data, hostClients, ?DATA_IDX)), % This is the format of hostClients {Name,{Port,ClientWorkers,ClientWorkersMaps}}
  ClientWorkersMaps = element(CLIENT_WORKES_MAPS_TUPLE_IDX,maps:get(ClientName, MapOfHostClients)),
  Func = fun(WorkerMap) -> 
    ModelId = erlang:unique_integer([positive]),
    %%  TODO: move to json parser
    WorkerName = list_to_atom(binary_to_list(maps:get(<<"name">>,WorkerMap))),
    ModelType = list_to_integer(binary_to_list(maps:get(<<"modelType">>,WorkerMap))),
    ScalingMethod = list_to_integer(binary_to_list(maps:get(<<"scalingMethod">>,WorkerMap))),
    LayerTypesList = nerl_tools:string_to_list_int(maps:get(<<"layerTypesList">>,WorkerMap)),
    LayersSizes = nerl_tools:string_to_list_int(maps:get(<<"layersSizes">>,WorkerMap)),
    LayersActivationFunctions = nerl_tools:string_to_list_int(maps:get(<<"layersActivationFunctions">>,WorkerMap)),
    Optimizer = list_to_integer(binary_to_list(maps:get(<<"optimizer">>,WorkerMap))),
    LossMethod = list_to_integer(binary_to_list(maps:get(<<"lossMethod">>,WorkerMap))),
    LearningRate = list_to_float(binary_to_list(maps:get(<<"learningRate">>,WorkerMap))),

    % validation of tensors:
    TensorsValidation = lists:all(fun(X) -> X end, [
      nerlNIF:validate_nerltensor_erl(LayerTypesList),
      nerlNIF:validate_nerltensor_erl(LayersSizes),
      nerlNIF:validate_nerltensor_erl(LayersActivationFunctions)
    ]),

    if
      TensorsValidation -> ok;
      true -> ?LOG_ERROR("Wrong NerlTensor dimensions declaration. XYZ != len(NerlTensor)"),
              throw("Wrong NerlTensor dimensions declaration")
    end,

    % TODO add documentation about this case of 
    % move this case to module called client_controller
    case ModelType of
      ?E_CUSTOMNN ->
        CustomFunc = fun workerNN:controller/2,
        WorkerData = none;
      ?E_FEDERATED_CLIENT ->
        CustomFunc = fun workerFederatedClient:controller/2,
        FedServerName = list_to_atom(binary_to_list(maps:get(<<"federatedServer">>,WorkerMap))),
        SyncCount = list_to_integer(binary_to_list(maps:get(<<"syncCount">>,WorkerMap))),
        WorkerData = {_SyncMaxCount = SyncCount, _MyName = WorkerName, _ServerName = FedServerName};
      ?E_FEDERATED_SERVER ->
        CustomFunc = fun workerFederatedServer:controller/2,
        SyncCount = list_to_integer(binary_to_list(maps:get(<<"syncCount">>,WorkerMap))),
        WorkerData = {_SyncMaxCount = SyncCount, _MyName = WorkerName, _WorkersNamesList = []}
      end,

    WorkerArgs = {WorkerName,ModelId,ModelType,ScalingMethod, LayerTypesList, LayersSizes,
      LayersActivationFunctions, Optimizer, LossMethod, LearningRate, self(), CustomFunc, WorkerData},

    WorkerPid = workerGeneric:start_link(WorkerArgs),

    ets:insert(EtsRef, {WorkerName, WorkerPid, WorkerArgs, {0,0,0.0}, 0}),
    WorkerName
  end,

  WorkersNames = lists:map(Func, ClientWorkersMaps),   %% TODO: collect forbidden names (keys of ets:insert)
  ets:insert(EtsRef, {workersNames, WorkersNames}).  