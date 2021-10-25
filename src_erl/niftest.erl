-module(niftest).

-export([init/0, hello/0]).

-on_load(init/0).

init() ->
      erlang:load_nif("/home/evgeny/work_test/NErlNet/build/libnerlnet", 0).

hello() ->
      erlang:nif_error("NIF library not loaded").
