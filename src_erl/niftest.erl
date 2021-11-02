-module(niftest).

-export([init/0,hello/1]).

-on_load(init/0).

-define(DEBUG,false). % set here if it is debug or release  TODO change to read from hrl auto generated file
-ifdef(DEBUG).
-define(BUILD_TYPE,"debug").
-else.
-define(BUILD_TYPE,"release").
-endif. 

init() ->
      {_,CWD} = file:get_cwd(), 
      FULL_PATH = CWD ++ "/../build/"++?BUILD_TYPE++"/libnerlnet",
      erlang:load_nif(FULL_PATH, 0),
      ok.
      % io:format("hello"),
      %hello("hello").
      %erlang:hello().
      % erlang:nif_error("NIF library not loaded").

hello(Integer) when is_integer(Integer) ->
      exit(nif_library_not_loaded).