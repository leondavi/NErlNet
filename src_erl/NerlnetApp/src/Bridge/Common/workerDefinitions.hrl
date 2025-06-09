-define(ETS_KEYVAL_KEY_IDX, 1).
-define(ETS_KEYVAL_VAL_IDX, 2).
-define(TENSOR_DATA_IDX, 1).

-record(workerGeneric_state, {myName , modelID , distributedBehaviorFunc , distributedWorkerData , currentBatchID , nextState , lastPhase, postBatchFunc}).
-define(EMPTY_FUNC, fun() -> ok end).