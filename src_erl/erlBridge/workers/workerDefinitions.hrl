-define(ETS_KEYVAL_KEY_IDX, 2).
-define(ETS_KEYVAL_VAL_IDX, 2).
-define(TENSOR_DATA_IDX, 1).

-record(workerGeneric_state, {clientPid, features, labels, myName, modelId, nextState, currentBatchID=0, ackClient=0, missedBatchesCount = 0, missedTrainBatches= [],optimizer, lossMethod, learningRate, customFunc, workerData}).
-record(workerFederatedClient, {syncCount, syncMaxCount, clientPID, myName, serverName}).
-record(workerFederatedServer, {syncCount, syncMaxCount, clientPID, myName, workersNamesList}).
