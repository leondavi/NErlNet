%% erl logger
-include_lib("kernel/include/logger.hrl").
%% HEADER format example: "nerlNetServer_app/start@52: MES"
-define(LOG_HEADER, atom_to_list(?MODULE) ++ "/" ++ atom_to_list(?FUNCTION_NAME) ++ "@" ++ integer_to_list(?LINE) ++ ": " ).

%% init process defines
-define(NERLNET_INIT_PORT,8484).
-define(PYTHON_SERVER_WAITING_TIMEOUT_MS, 1000).
-define(SUBNETS_CONFIG_ADDR, "/usr/local/lib/nerlnet-lib/NErlNet/config/subnets.nerlconfig").

%% init JSON defines
-define(JSON_ADDR, "/usr/local/lib/nerlnet-lib/NErlNet/src_erl/Communication_Layer/http_Nerlserver/").
-define(ARCH_FILE_NAME, "arch.json").
-define(COMM_FILE_NAME, "conn.json").

%% sorce tmpData name
-define(TMP_DATA_ADDR, "tmpData.csv").
%% sourceFSM defines
-define(SENDALL, 1).
-define(ROUNDROBIN, 2).

%% workerFSM defines
-define(MODE_REGULAR, 0).
-define(MODE_FEDERATED, 1).

%% nerlNIF defines
-define(NERLNET_LIB,"libnerlnet").
-define(NERLNET_PATH,"/usr/local/lib/nerlnet-lib/NErlNet").
-define(BUILD_TYPE_DEBUG,"debug").
-define(BUILD_TYPE_RELEASE,"/build/release").
