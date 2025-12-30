%% erl logger
-include_lib("kernel/include/logger.hrl").

-define(MAIN_SERVER_ATOM, mainServer).
-define(MAIN_SERVER_STR, "mainServer").
-define(API_SERVER_ATOM, apiServer).
-define(API_SERVER_STR, "apiServer").
-define(LIST_OF_SPECIAL_SERVERS,[?API_SERVER_ATOM, ?MAIN_SERVER_ATOM]).
%% HEADER format example: "nerlNetServer_app/start@52: MES"
-define(LOG_HEADER, atom_to_list(?MODULE) ++ "/" ++ atom_to_list(?FUNCTION_NAME) ++ "@" ++ integer_to_list(?LINE) ++ ": " ).

-define(VALIDATION_OF_TRANSMISSION_WITH_API_SERVER_NUMOF_TRIALS, 5).
-define(VALIDATION_OF_TRANSMISSION_WITH_API_SERVER_INTERVAL_MS, 100). % how much between each resend
%% ETS definitions 

%% HTTP Content type definitions
-define(HTTP_CONTENT_TYPE_MULTI_PART_FORM_DATA, "multipart/form-data").
-define(HTTP_CONTENT_TYPE_JSON, "application/json").
-define(HTTP_CONTENT_TYPE_FORM_URLENCODED, "application/x-www-form-urlencoded").

% 2 elements ETS:
-define(KEY_IDX, 1).
-define(DATA_IDX, 2).

%% init process defines
-define(NERLNET_INIT_PORT,8484).
-define(PYTHON_SERVER_WAITING_TIMEOUT_MS, 1000).
-define(SUBNETS_CONFIG_ADDR, "/usr/local/lib/nerlnet-lib/NErlNet/config/subnets.nerlconfig").

%% init JSON defines
-define(JSON_ADDR, "/tmp/nerlnet/jsons/").
-define(LOCAL_DC_FILE_NAME, "dc.json").
-define(LOCAL_COMM_FILE_NAME, "conn.json").
-define(TORCH_MODELS_BASE_DIR, "/tmp/nerlnet/torch/models/pt/").

%% sorce tmpData name
-define(TMP_DIR_RUN,"/tmp/nerlnet/run").
-define(TMP_DATA_ADDR, "tmpData.csv").
%% max data length that cowboy server will accept (when passing big files)
-define(DATA_LEN, 1000*1000*1000). % default is 8MB, here set to 1000MB

%% workerFSM defines
-define(MODE_REGULAR, 0).
-define(MODE_FEDERATED, 1).

%% auto generated defintions:
%% TODO: implement this in python
-define(E_CUSTOMNN, 5).
-define(E_FEDERATED_CLIENT, 8).
-define(E_FEDERATED_SERVER, 9).

-define(CONN_MAP_FIELD_BIN , <<"connectionsMap">>).