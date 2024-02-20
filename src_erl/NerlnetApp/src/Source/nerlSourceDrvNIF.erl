-module(nerlSourceDrvNIF).

-include_lib("kernel/include/logger.hrl").

-on_load(init/0).

-export([nif_preload/0]).

%% nerlNIF defines
-define(NERLNET_SOURCE_DRV_LIB,"libnerlnet_source_drv").
-define(NERLNET_PATH,"/usr/local/lib/nerlnet-lib/NErlNet").
-define(BUILD_TYPE_DEBUG,"debug").
-define(BUILD_TYPE_RELEASE,"/build/release").

init() ->
      NELNET_LIB_PATH = ?NERLNET_PATH++?BUILD_TYPE_RELEASE++"/"++?NERLNET_SOURCE_DRV_LIB,
      RES = erlang:load_nif(NELNET_LIB_PATH, 0),
      RES.

%% make sure nif can be loaded (activates on_load)
nif_preload() -> done.