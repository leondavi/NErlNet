-define(ETS_KEYVAL_KEY_IDX, 2).
-define(ETS_KEYVAL_VAL_IDX, 2).

-record(workerFederatedClient, {syncCount, syncMaxCount, clientPID, myName, serverName}).
-record(workerFederatedServer, {syncCount, syncMaxCount, clientPID, myName, workersNamesList}).
