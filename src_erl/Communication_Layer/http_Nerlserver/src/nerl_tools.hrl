-include_lib("kernel/include/logger.hrl").

%% init process defines
-define(NERLNET_INIT_PORT,8484).
-define(PYTHON_SERVER_WAITING_TIMEOUT_MS, 1000).
-define(SUBNETS_CONFIG_ADDR, "/usr/local/lib/nerlnet-lib/NErlNet/config/subnets.nerlconfig").

%% init JSON defines
-define(JSON_ADDR, "/usr/local/lib/nerlnet-lib/NErlNet/src_erl/Communication_Layer/http_Nerlserver/").
-define(ARCH_FILE_NAME, "arch.json").
-define(COMM_FILE_NAME, "conn.json").

%% source defines
-define(SENDALL, 1).
-define(ROUNDROBIN, 2).

%% worker defines
-define(MODE_REGULAR, 0).
-define(MODE_FEDERATED, 1).