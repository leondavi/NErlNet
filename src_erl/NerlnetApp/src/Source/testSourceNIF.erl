-module(testSourceNIF).
-author("David Leon").

-compile(sourceNIF).
-export([run_tests/0]).

run_tests() ->
    io:format("cwd is: ~s~n", [filename:absname(".")]),
    io:format("Running tests...~n"),
    ok.