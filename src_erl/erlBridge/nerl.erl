-module(nerl).
-author("David Leon").
% nerl module contains generic functions

% math
-export([compare_floats_L/3]).
% time
-export([tic/0, toc/1]).
% string
-export([string_format/2]).
%logger
-export([logger_settings/1]).

% applies default nerlnet logger settings
logger_settings(Module) -> 
      logger:set_handler_config(default, formatter, {logger_formatter, #{}}),
      logger:set_module_level(Module, all).

%% A function that compares pairs of corresponding indexed floats from two lists
%% and returns true if the precision holds for each pair , false otherwise.
compare_floats_L(L1 , L2 , PrecisionDigits) -> Epsilon = math:pow(10, -PrecisionDigits), compare_floats_L(L1 , L2 , Epsilon , 0).
compare_floats_L([] , [] , _ , _) -> true;
compare_floats_L([H1|T1] , [H2|T2] , Epsilon , Index) ->
    case abs(H1 - H2) < Epsilon of
        true -> compare_floats_L(T1 , T2 , Epsilon , Index + 1);
        false -> false %io:format("Precision failed at index ~p~n" , [Index])
    end.

tic() -> erlang:timestamp().
toc(Tic) -> End = erlang:timestamp(), Diff = timer:now_diff(End , Tic) / 1000, {Diff, ms}.

string_format(Pattern, Values) ->
    lists:flatten(io_lib:format(Pattern, Values)).