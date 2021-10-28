-module(niftest).

-export([init/0,hello/1]).

-on_load(init/0).

init() ->
      io:format("reached init"),
      erlang:load_nif("/home/evgeny/work_test/NErlNet/build/libnerlnet", 0),
      io:format("after init").
%jello() ->
      % io:format("hello"),
      %hello("hello").
      %erlang:hello().
      % erlang:nif_error("NIF library not loaded").

hello(Integer) when is_integer(Integer) ->
      exit(nif_library_not_loaded).