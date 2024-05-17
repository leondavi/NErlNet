% This is an auto generated .hrl file
% Worker Fields Generated by Nerlplanner version: 1.0.2

-define(WORKER_FIELD_KEY_MODEL_TYPE,modelType).
-define(WORKER_FIELD_KEY_MODEL_ARGS,modelArgs).
-define(WORKER_FIELD_KEY_LAYER_SIZES_LIST,layersSizes).
-define(WORKER_FIELD_KEY_LAYER_TYPES_LIST,layerTypesList).
-define(WORKER_FIELD_KEY_LAYERS_FUNCTIONS,layers_functions).
-define(WORKER_FIELD_KEY_LOSS_METHOD,lossMethod).
-define(WORKER_FIELD_KEY_LEARNING_RATE,lr).
-define(WORKER_FIELD_KEY_EPOCHS,epochs).
-define(WORKER_FIELD_KEY_OPTIMIZER_TYPE,optimizer).
-define(WORKER_FIELD_KEY_OPTIMIZER_ARGS,optimizerArgs).
-define(WORKER_FIELD_KEY_INFRA_TYPE,infraType).
-define(WORKER_FIELD_KEY_DISTRIBUTED_SYSTEM_TYPE,distributedSystemType).
-define(WORKER_FIELD_KEY_DISTRIBUTED_SYSTEM_TOKEN,distributedSystemToken).
-define(WORKER_FIELD_KEY_DISTRIBUTED_SYSTEM_ARGS,distributedSystemArgs).

-define(WORKER_FIELD_KEY_MODEL_TYPE_BIN,<<"modelType">>).
-define(WORKER_FIELD_KEY_MODEL_ARGS_BIN,<<"modelArgs">>).
-define(WORKER_FIELD_KEY_LAYER_SIZES_LIST_BIN,<<"layersSizes">>).
-define(WORKER_FIELD_KEY_LAYER_TYPES_LIST_BIN,<<"layerTypesList">>).
-define(WORKER_FIELD_KEY_LAYERS_FUNCTIONS_BIN,<<"layers_functions">>).
-define(WORKER_FIELD_KEY_LOSS_METHOD_BIN,<<"lossMethod">>).
-define(WORKER_FIELD_KEY_LEARNING_RATE_BIN,<<"lr">>).
-define(WORKER_FIELD_KEY_EPOCHS_BIN,<<"epochs">>).
-define(WORKER_FIELD_KEY_OPTIMIZER_TYPE_BIN,<<"optimizer">>).
-define(WORKER_FIELD_KEY_OPTIMIZER_ARGS_BIN,<<"optimizerArgs">>).
-define(WORKER_FIELD_KEY_INFRA_TYPE_BIN,<<"infraType">>).
-define(WORKER_FIELD_KEY_DISTRIBUTED_SYSTEM_TYPE_BIN,<<"distributedSystemType">>).
-define(WORKER_FIELD_KEY_DISTRIBUTED_SYSTEM_TOKEN_BIN,<<"distributedSystemToken">>).
-define(WORKER_FIELD_KEY_DISTRIBUTED_SYSTEM_ARGS_BIN,<<"distributedSystemArgs">>).

-define(DC_DISTRIBUTED_SYSTEM_TYPE_NONE_KEY_ATOM,none).
-define(DC_DISTRIBUTED_SYSTEM_TYPE_FEDCLIENTAVG_KEY_ATOM,fedClientAvg).
-define(DC_DISTRIBUTED_SYSTEM_TYPE_FEDSERVERAVG_KEY_ATOM,fedServerAvg).

-define(DC_DISTRIBUTED_SYSTEM_TYPE_NONE_IDX_STR,"0").
-define(DC_DISTRIBUTED_SYSTEM_TYPE_FEDCLIENTAVG_IDX_STR,"1").
-define(DC_DISTRIBUTED_SYSTEM_TYPE_FEDSERVERAVG_IDX_STR,"2").

-define(DC_DISTRIBUTED_SYSTEM_TYPE_NONE_IDX,0).
-define(DC_DISTRIBUTED_SYSTEM_TYPE_FEDCLIENTAVG_IDX,1).
-define(DC_DISTRIBUTED_SYSTEM_TYPE_FEDSERVERAVG_IDX,2).

-define(DC_INFRA_TYPE_OPENNN_KEY_ATOM,opennn).
-define(DC_INFRA_TYPE_WOLFENGINE_KEY_ATOM,wolfengine).

-define(DC_INFRA_TYPE_OPENNN_IDX_STR,"0").
-define(DC_INFRA_TYPE_WOLFENGINE_IDX_STR,"1").

-define(DC_INFRA_TYPE_OPENNN_IDX,0).
-define(DC_INFRA_TYPE_WOLFENGINE_IDX,1).
