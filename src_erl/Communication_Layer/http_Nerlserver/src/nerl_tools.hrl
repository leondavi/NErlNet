%% erl logger
-include_lib("kernel/include/logger.hrl").

-define(MAIN_SERVER_ATOM, mainServer).
-define(API_SERVER_ATOM, apiServer).
-define(NERLGUI_SERVER_ATOM, nerlGUI).
-define(LIST_OF_SPECIAL_SERVERS,[?API_SERVER_ATOM, ?NERLGUI_SERVER_ATOM, ?MAIN_SERVER_ATOM]).
%% HEADER format example: "nerlNetServer_app/start@52: MES"
-define(LOG_HEADER, atom_to_list(?MODULE) ++ "/" ++ atom_to_list(?FUNCTION_NAME) ++ "@" ++ integer_to_list(?LINE) ++ ": " ).

%% ETS definitions 

% 2 elements ETS:
-define(KEY_IDX, 1).
-define(DATA_IDX, 2).

%% init process defines
-define(NERLNET_INIT_PORT,8484).
-define(PYTHON_SERVER_WAITING_TIMEOUT_MS, 1000).
-define(SUBNETS_CONFIG_ADDR, "/usr/local/lib/nerlnet-lib/NErlNet/config/subnets.nerlconfig").

%% init JSON defines
-define(JSON_ADDR, "/usr/local/lib/nerlnet-lib/NErlNet/src_erl/Communication_Layer/http_Nerlserver/").
-define(LOCAL_ARCH_FILE_NAME, "arch.json").
-define(LOCAL_COMM_FILE_NAME, "conn.json").

%% sorce tmpData name
-define(TMP_DIR_RUN,"/tmp/nerlnet/run").
-define(TMP_DATA_ADDR, "tmpData.csv").
%% max data length that cowboy server will accept (when passing big files)
-define(DATA_LEN, 1000*1000*1000). % default is 8MB, here set to 1000MB
% -define(DATA_LEN, infinity).
%% sourceFSM defines
-define(SENDALL, 1).
-define(ROUNDROBIN, 2).

%% workerFSM defines
-define(MODE_REGULAR, 0).
-define(MODE_FEDERATED, 1).

% %% nerlNIF defines
% -define(NERLNET_LIB,"libnerlnet").
% -define(NERLNET_PATH,"/usr/local/lib/nerlnet-lib/NErlNet").
% -define(BUILD_TYPE_DEBUG,"debug").
% -define(BUILD_TYPE_RELEASE,"/build/release").

%% auto generated defintions:
%% TODO: implement this in python
-define(E_CUSTOMNN, 5).
-define(E_FEDERATED_CLIENT, 8).
-define(E_FEDERATED_SERVER, 9).