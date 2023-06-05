-module(workerFederatedServer).

-export([controller/2]).

-include("workerDefinitions.hrl").

-import(nerlNIF,[sum_nerltensors_lists/2, nerltensor_scalar_multiplication_nif/3]).

-define(ETS_WID_IDX, 1).
-define(ETS_TYPE_IDX, 2).
-define(ETS_WEIGHTS_AND_BIAS_NERLTENSOR_IDX, 3).
-define(ETS_NERLTENSOR_TYPE_IDX, 2).


controller(FuncName, {GenWorkerEts, WorkerData}) -> 
  case FuncName of
    init -> init({GenWorkerEts, WorkerData});
    pre_train -> pre_train({GenWorkerEts, WorkerData});
    post_train -> post_train({GenWorkerEts, WorkerData});
    pre_predict -> pre_predict({GenWorkerEts, WorkerData});
    post_predict -> post_predict({GenWorkerEts, WorkerData});
    pre_idle -> pre_idle({GenWorkerEts, WorkerData});
    post_idle -> post_idle({GenWorkerEts, WorkerData});
  end.

%% handshake with workers / server (DONE IN GENERIC)
init({GenWorkerEts, WorkerData}) -> ok,
  Type = float, % update from data
  #workerFederatedServer{clientPID = ClientPid, myName= MyName, workersNamesList = WorkersNamesList} = WorkerData,
  FederatedServerEts = ets:new(federated_server,[set]),
  insert(GenWorkerEts, federated_server_ets, FederatedServerEts),
   ets:insert(FederatedServerEts,{nerltensor_type, Type}).%TODO insert workers by their name {WID, worker, WeightsAndBiasNerlTensor}

%% every countLimit batches, get updated model (NOTHING FOR SERVER) ??? maybe he needs to go to state receive from other workers?
pre_train({GenWorkerEts, WorkerData}) -> ok.

%% every countLimit batches, send updated weights
post_train({GenWorkerEts, WorkerData}) -> ok,
    generate_avg_weights(). %TODO complete

%% nothing?
pre_predict({GenWorkerEts, WorkerData}) -> ok.

%% nothing?
post_predict({GenWorkerEts, WorkerData}) -> ok.

generate_avg_weights() ->
  BinaryType = ets:lookup_element(federated_server, nerltensor_type, ?ETS_NERLTENSOR_TYPE_IDX),
  ListOfWorkersNerlTensors = [ element(?ETS_WEIGHTS_AND_BIAS_NERLTENSOR_IDX, Attr) || Attr <- ets:tab2list(federated_server), element(?ETS_TYPE_IDX, Attr) == worker],
  NerlTensors = length(ListOfWorkersNerlTensors),
  FinalSumNerlTensor = nerlNIF:sum_nerltensors_lists(ListOfWorkersNerlTensors, BinaryType),
  nerltensor_scalar_multiplication_nif(FinalSumNerlTensor, BinaryType, 1.0/NerlTensors).

