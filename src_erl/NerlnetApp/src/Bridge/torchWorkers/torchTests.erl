-module(torchTests).
-author("David Leon").

-include_lib("kernel/include/logger.hrl").
-include("torchTestsDefs.hrl").

-define(NERLTEST_PRINT_STR, "[NERLTEST] ").

-export([run_tests/0]).

nerltest_print(String) ->
      logger:notice(?NERLTEST_PRINT_STR++String).

test_envelope(Func, TestName, Rounds) ->
      nerltest_print(nerl:string_format("~p test starts for ~p rounds",[TestName, Rounds])),
      {TimeTookMicro, _RetVal} = timer:tc(Func, [Rounds]),
      nerltest_print(nerl:string_format("Elapsed: ~p~p",[TimeTookMicro / 1000, ms])), ok.

test_envelope_nif_performance(Func, TestName, Rounds) ->
      nerltest_print(nerl:string_format("~p test starts for ~p rounds",[TestName, Rounds])),
      {TimeTookMicro, AccPerfromance} = timer:tc(Func, [Rounds]),
      AveragedPerformance = AccPerfromance/Rounds,
      nerltest_print(nerl:string_format("Elapsed: ~p~p Average nif performance: ~.3f~p",[TimeTookMicro/1000,ms, AveragedPerformance, ms])), ok.

run_tests()->
      nerl:logger_settings(nerlTests).