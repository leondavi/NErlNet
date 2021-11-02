-module(niftest).

-export([init/0,hello/1]).

-define(DEBUG,false). % set here if it is debug or release  TODO change to read from hrl auto generated file
-if(DEBUG).
-define(BUILD_TYPE,"debug").
-else.
-define(BUILD_TYPE,"release").
-endif. 

-define(THIS_FILE_RELATIVE_TO_PROJECT_ROOT,"src_erl"). % if this file moves to inner place than update this define
-on_load(init/0).

init() ->
      {_,CWD} = file:get_cwd(), 
      CWD_UPPER_DIR = re:replace(CWD,"/"++?THIS_FILE_RELATIVE_TO_PROJECT_ROOT,"",[{return,list}]),
      FULL_PATH = CWD_UPPER_DIR++"/build/"++?BUILD_TYPE++"/libnerlnet",
      io:format("~p",[FULL_PATH]),
      erlang:load_nif(FULL_PATH, 0),
      ok.
      % io:format("hello"),
      %hello("hello").
      %erlang:hello().
      % erlang:nif_error("NIF library not loaded").

hello(Integer) when is_integer(Integer) ->
      exit(nif_library_not_loaded).