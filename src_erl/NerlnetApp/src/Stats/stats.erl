-module(stats).

-include("stats.hrl").

-export([generate_stats_ets/0]).
-export([get_messages_received/1, increment_messages_received/1, increment_messages_received/2]).
-export([get_messages_sent/1, increment_messages_sent/1, increment_messages_sent/2]).
-export([get_messages_dropped/1, increment_messages_dropped/1, increment_messages_dropped/2]).
-export([get_bytes_received/1, increment_bytes_received/2]).
-export([get_bytes_sent/1, increment_bytes_sent/2]).
-export([get_bad_messages/1, increment_bad_messages/1]).
-export([get_value/2, increment_by_value/3]).
-export([set_value/3]).
-export([encode_ets_to_http_bin_str/1 , decode_http_bin_str_to_ets/1 , encode_workers_ets_to_http_bin_str/1]).
-export([update_workers_ets/4, increment_workers_ets/4 , generate_workers_stats_ets/0]).

% performance stats
-export([generate_performance_stats_ets/0]).
-export([start_os_mon/0]).
-export([performance_stats_reset/1]).
% perofmance stats getters/setters
-export([get_time_train_active/1, increment_time_train_active/2]).
-export([get_time_train_total/1, increment_time_train_total/2]).
-export([get_time_predict_active/1, increment_time_predict_active/2]).
-export([get_time_predict_total/1, increment_time_predict_total/2]).
-export([get_memory_peak_usage_train/1, update_memory_peak_usage_train/2]).
-export([get_memory_peak_usage_predict/1, update_memory_peak_usage_predict/2]).
-export([get_memory_train_ema_usage/1, update_memory_train_ema_usage/2]).
-export([get_memory_predict_ema_usage/1, update_memory_predict_ema_usage/2]).
-export([set_cpu_util_per_core/2]).

% performance stats queries
-export([query_memory_usage/0, query_cpu_util_cores/0]).

get_numeric_type(Value) ->
    case Value of
        _ when is_integer(Value) -> int;
        _ when is_float(Value) -> float;
        _ -> throw("Value is not numeric")
    end.

encode_ets_to_http_bin_str(StatsEts) -> 
    %% Takes value from ets and converts it to "<EntitiyName1>SEPERATOR<Value1 <EntityName2>SEPERATOR<Value2>" string.
    StatsList = ets:tab2list(StatsEts),
    Func = fun({Key , Value}) ->
                Type = get_numeric_type(Value),
                KeyStr = lists:flatten(io_lib:format("~p" , [Key])),
                ValueStr = lists:flatten(io_lib:format("~p" , [Value])),
                TypeStr = lists:flatten(io_lib:format("~p" , [Type])),
                %% StatsKey:StatsValue:StatsType#StatsKey:StatsValue:StatsType#...
                KeyStr ++ ?SEPERATOR_WITHIN_TRIPLET ++ ValueStr ++ ?SEPERATOR_WITHIN_TRIPLET ++ TypeStr ++ ?SEPERATOR_TRIPLETS
        end,
    lists:flatten(lists:map(Func , StatsList)).

encode_workers_ets_to_http_bin_str(StatsEts) -> 
    %% Takes value from ets and converts it to "<EntitiyName1>SEPERATOR<Value1 <EntityName2>SEPERATOR<Value2>" string.
    StatsList = ets:tab2list(StatsEts),
    Func = fun({Key , Value}) ->
                Type = get_numeric_type(Value),
                KeyStr = lists:flatten(io_lib:format("~p" , [Key])),
                ValueStr = lists:flatten(io_lib:format("~p" , [Value])),
                TypeStr = lists:flatten(io_lib:format("~p" , [Type])),
                %% StatsKey:StatsValue:StatsType#StatsKey:StatsValue:StatsType#...
                KeyStr ++ ?WORKER_SEPERATOR_WITHIN_TRIPLET ++ ValueStr ++ ?WORKER_SEPERATOR_WITHIN_TRIPLET ++ TypeStr ++ ?WORKER_SEPERATOR_TRIPLETS
        end,
    lists:flatten(lists:map(Func , StatsList)).

decode_http_bin_str_to_ets(EncodedStr) -> 
    ReturnedEts = ets:new(ets_to_merge , [set]),
    KeyValTypeTokens = string:tokens(EncodedStr , ?SEPERATOR_TRIPLETS),
    Func = fun(Triplet) ->
                [Key , ValueStr , Type] = string:tokens(Triplet , ?SEPERATOR_WITHIN_TRIPLET),
                TypeAtom = list_to_atom(Type),
                Value = case TypeAtom of
                    int -> list_to_integer(ValueStr);
                    float -> list_to_float(ValueStr);
                    _ -> throw("Type is not numeric")
                end,
                ets:insert(ReturnedEts , {Key , Value})
        end,
    lists:foreach(Func , KeyValTypeTokens),
    ReturnedEts.
    

generate_stats_ets() -> %% sources, clients , routers , mainserver...
    StatsEts = ets:new(stats_ets , [set, public]),
    ets:insert(StatsEts, {messages_received , 0}),
    ets:insert(StatsEts, {messages_sent , 0}),
    ets:insert(StatsEts, {messages_dropped , 0}),
    ets:insert(StatsEts, {bytes_received , 0}),
    ets:insert(StatsEts, {bytes_sent , 0}),
    ets:insert(StatsEts, {bad_messages , 0}),
    ets:insert(StatsEts, {batches_received , 0}), % related with client only
    ets:insert(StatsEts, {batches_dropped , 0}), % related with client only
    ets:insert(StatsEts, {batches_sent , 0}), % related with source only
    ets:insert(StatsEts, {actual_frequency, 0}), % related with source only
    StatsEts.

%% Starts the os_mon application if it is not already started.
%% This is necessary for monitoring system performance.
%% It also ensures that the sasl application is started, as os_mon depends on it.
start_os_mon() ->
    % check if os_mon is already started
    case application:which_applications() of
        [{os_mon, _, _} | _] ->
            ok;
        _ -> % start sasl application
            {ok,[sasl]} = application:ensure_all_started(sasl),
            % start os_mon application
            {ok,[os_mon]} = application:ensure_all_started(os_mon)
    end,
    ok.


generate_performance_stats_ets() -> %% clients
    start_os_mon(),
    PerformanceStatsEts = ets:new(performance_stats_ets , [set, public]),
    ets:insert(PerformanceStatsEts, {time_train_active , 0}), % Client Aggregate training times of workers
    ets:insert(PerformanceStatsEts, {time_train_total , 0}), % Client counts the total time spent in training state
    ets:insert(PerformanceStatsEts, {time_predict_active , 0}), % Client Aggregate prediction times of workers
    ets:insert(PerformanceStatsEts, {time_predict_total , 0}), % Client counts the total time spent in prediction state

    ets:insert(PerformanceStatsEts, {average_gpu_usage_train , 0}),
    ets:insert(PerformanceStatsEts, {average_gpu_memory_usage_predict , 0}),

    ets:insert(PerformanceStatsEts, {memory_train_ema_usage , 0}),
    ets:insert(PerformanceStatsEts, {memory_predict_ema_usage , 0}),
    ets:insert(PerformanceStatsEts, {memory_train_peak_usage , 0}),
    ets:insert(PerformanceStatsEts, {memory_predict_peak_usage , 0}),

    % cores usage
    NumberOfCores = length(cpu_sup:util([per_cpu])), % Important! this call also resets util since last util's call
    ets:insert(PerformanceStatsEts, {num_of_cores , NumberOfCores}),
    lists:foreach(fun(CoreIndex) ->
        KeyUtilTrainingPerCoreStr = lists:flatten(io_lib:format("cpu_train_util_core_~p" , [CoreIndex])),
        KeyUtilTrainingPerCoreAtom = list_to_atom(KeyUtilTrainingPerCoreStr),
        ets:insert(PerformanceStatsEts, {KeyUtilTrainingPerCoreAtom, 0}),
        KeyUtilPredictPerCoreStr = lists:flatten(io_lib:format("cpu_predict_util_core_~p" , [CoreIndex])),
        KeyUtilPredictPerCoreAtom = list_to_atom(KeyUtilPredictPerCoreStr),
        ets:insert(PerformanceStatsEts, {KeyUtilPredictPerCoreAtom, 0})
    end, 
    lists:seq(1, NumberOfCores)),
    PerformanceStatsEts.

% TODO add setters/getters 

generate_workers_stats_ets() -> %% workers..
    WorkersStatsEts = ets:new(workers_ets , [set, public]),
    ets:insert(WorkersStatsEts, {bytes_received , 0}),
    ets:insert(WorkersStatsEts, {bytes_sent , 0}),
    ets:insert(WorkersStatsEts, {bad_messages , 0}),
    ets:insert(WorkersStatsEts, {batches_received_train , 0}), % related with client only
    ets:insert(WorkersStatsEts, {batches_received_predict , 0}), % related with client only
    ets:insert(WorkersStatsEts, {batches_dropped_train , 0}), % related with client only
    ets:insert(WorkersStatsEts, {batches_dropped_predict , 0}), % related with client only
    ets:insert(WorkersStatsEts, {batches_sent_train , 0}), % related with source
    ets:insert(WorkersStatsEts, {empty_batches , 0}), % related with source
    ets:insert(WorkersStatsEts, {batches_sent_predict , 0}), % related with source
    ets:insert(WorkersStatsEts, {average_time_training , 0}),
    ets:insert(WorkersStatsEts, {average_time_prediction , 0}),
    ets:insert(WorkersStatsEts, {acc_time_training , 0}),
    ets:insert(WorkersStatsEts, {acc_time_prediction , 0}),
    ets:insert(WorkersStatsEts, {nan_loss_count , 0}),
    WorkersStatsEts.

%% ---- Workers Stats ETS Methods ----%%
update_workers_ets(StatsEts, WorkerName, WorkerAttribute, Value) ->
    WorkerStatsEts = ets:lookup_element(StatsEts, workers_ets, ?STATS_KEYVAL_VAL_IDX),
    Key = {WorkerName, WorkerAttribute},
    ets:insert(WorkerStatsEts, {Key, Value}).

increment_workers_ets(StatsEts, WorkerName, WorkerAttribute, Value) ->
    WorkerStatsEts = ets:lookup_element(StatsEts, workers_ets, ?STATS_KEYVAL_VAL_IDX),
    Key = {WorkerName, WorkerAttribute},
    ets:update_counter(WorkerStatsEts, Key, Value).

%% ---- Stats ETS Methods -----%%
set_value(StatsEts, Key, Value) ->
    ets:update_element(StatsEts, Key, {?STATS_KEYVAL_VAL_IDX, Value}).

increment_by_value(StatsEts, Key, Value) ->
    ets:update_counter(StatsEts, Key, Value).

get_value(StatsEts, Key) ->
    ets:lookup_element(StatsEts, Key , ?STATS_KEYVAL_VAL_IDX).

get_messages_received(StatsEts) ->
    ets:lookup_element(StatsEts, ?STATS_ATOM_MSG_RECV , ?STATS_KEYVAL_VAL_IDX).

increment_messages_received(StatsEts) ->
    ets:update_counter(StatsEts, ?STATS_ATOM_MSG_RECV, 1).

increment_messages_received(StatsEts , Value) ->
    ets:update_counter(StatsEts, ?STATS_ATOM_MSG_RECV, Value).

get_messages_sent(StatsEts) ->
    ets:lookup_element(StatsEts, ?STATS_ATOM_MSG_SENT , ?STATS_KEYVAL_VAL_IDX).

increment_messages_sent(StatsEts) ->
    ets:update_counter(StatsEts, ?STATS_ATOM_MSG_SENT, 1).

increment_messages_sent(StatsEts , Value) ->
    ets:update_counter(StatsEts, ?STATS_ATOM_MSG_SENT, Value).

get_messages_dropped(StatsEts) ->
    ets:lookup_element(StatsEts, ?STATS_ATOM_MSG_DROP , ?STATS_KEYVAL_VAL_IDX).

increment_messages_dropped(StatsEts) ->
    ets:update_counter(StatsEts, ?STATS_ATOM_MSG_DROP, 1).

increment_messages_dropped(StatsEts , Value) ->
    ets:update_counter(StatsEts, ?STATS_ATOM_MSG_DROP, Value).

get_bytes_received(StatsEts) ->
    ets:lookup_element(StatsEts, ?STATS_ATOM_BYTES_RECV , ?STATS_KEYVAL_VAL_IDX).

increment_bytes_received(StatsEts , Value) ->
    ets:update_counter(StatsEts, ?STATS_ATOM_BYTES_RECV, Value).

get_bytes_sent(StatsEts) ->
    ets:lookup_element(StatsEts, ?STATS_ATOM_BYTES_SENT , ?STATS_KEYVAL_VAL_IDX).

increment_bytes_sent(StatsEts , Value) ->
    ets:update_counter(StatsEts, ?STATS_ATOM_BYTES_SENT, Value).

get_bad_messages(StatsEts) ->
    ets:lookup_element(StatsEts, ?STATS_ATOM_BAD_MSG , ?STATS_KEYVAL_VAL_IDX).

increment_bad_messages(StatsEts) ->
    ets:update_counter(StatsEts, ?STATS_ATOM_BAD_MSG, 1).


performance_stats_reset(OldEts) ->
    % delete the old ETS table
    ets:delete(OldEts),
    % create a new ETS table
    NewEts = generate_performance_stats_ets(),
    NewEts.

%% Performance Stats Methods
ema_calc(OldValue, NewValue) ->
    %% Exponential Moving Average (EMA) calculation
    Coefficient = ?EMA_COEFFICIENT_HIST,
    NewValue * Coefficient + OldValue * (1 - Coefficient).

%% Perofrmance Stats Query Methods
query_memory_usage() ->
    %% Get the memory usage of the Erlang VM
    [{system_total_memory,SystemTotalMemory},
    {free_memory, FreeMemory},
    {total_memory, TotalMemory},
    {buffered_memory, BufferedMemory},
    {cached_memory, CachedMemory},
    {total_swap, TotalSwap},
    {free_swap, FreeSwap},
    {available_memory, AvailableMemory}] = memsup:get_system_memory_data(),
    %% Calculate the used memory
    UsedMemory = SystemTotalMemory - FreeMemory.

query_cpu_util_cores() -> 
    %% Get the CPU utilization of the Erlang VM
    CpuUtil = cpu_sup:util([per_cpu]),
    %% Convert the CPU utilization to a list of tuples
    lists:map(fun({CoreIndex, Busy, NonBusy, _ }) -> {CoreIndex, Busy} end, CpuUtil).

%% Performance Stats Getters/Setters
get_time_train_active(StatsEts) ->
    ets:lookup_element(StatsEts, ?STATS_ATOM_TIME_TRAIN_ACTIVE , ?STATS_KEYVAL_VAL_IDX).

increment_time_train_active(StatsEts, Value) ->
    ets:update_counter(StatsEts, ?STATS_ATOM_TIME_TRAIN_ACTIVE, Value).

get_time_train_total(StatsEts) ->
    ets:lookup_element(StatsEts, ?STATS_ATOM_TIME_TRAIN_TOTAL , ?STATS_KEYVAL_VAL_IDX).

increment_time_train_total(StatsEts, Value) ->
    ets:update_counter(StatsEts, ?STATS_ATOM_TIME_TRAIN_TOTAL, Value).

get_time_predict_active(StatsEts) ->
    ets:lookup_element(StatsEts, ?STATS_ATOM_TIME_PREDICT_ACTIVE , ?STATS_KEYVAL_VAL_IDX).  

increment_time_predict_active(StatsEts, Value) ->
    ets:update_counter(StatsEts, ?STATS_ATOM_TIME_PREDICT_ACTIVE, Value).

get_time_predict_total(StatsEts) ->
    ets:lookup_element(StatsEts, ?STATS_ATOM_TIME_PREDICT_TOTAL , ?STATS_KEYVAL_VAL_IDX).

increment_time_predict_total(StatsEts, Value) ->
    ets:update_counter(StatsEts, ?STATS_ATOM_TIME_PREDICT_TOTAL, Value).

get_memory_peak_usage_train(StatsEts) ->
    ets:lookup_element(StatsEts, ?STATS_MEMORY_TRAIN_PEAK_USAGE , ?STATS_KEYVAL_VAL_IDX).

update_memory_peak_usage_train(StatsEts, Value) ->
    % get the current peak usage
    CurrentPeak = get_memory_peak_usage_train(StatsEts),
    % update the peak usage if the new value is greater
    NewPeak = max(CurrentPeak, Value),
    ets:update_element(StatsEts, ?STATS_MEMORY_TRAIN_PEAK_USAGE, { ?STATS_KEYVAL_VAL_IDX, NewPeak }).

get_memory_peak_usage_predict(StatsEts) ->
    ets:lookup_element(StatsEts, ?STATS_MEMORY_PREDICT_PEAK_USAGE , ?STATS_KEYVAL_VAL_IDX).

update_memory_peak_usage_predict(StatsEts, Value) ->
    % get the current peak usage
    CurrentPeak = get_memory_peak_usage_predict(StatsEts),
    % update the peak usage if the new value is greater
    NewPeak = max(CurrentPeak, Value),
    ets:update_element(StatsEts, ?STATS_MEMORY_PREDICT_PEAK_USAGE, { ?STATS_KEYVAL_VAL_IDX, NewPeak }).

get_memory_train_ema_usage(StatsEts) ->
    ets:lookup_element(StatsEts, ?STATS_MEMORY_TRAIN_EMA_USAGE , ?STATS_KEYVAL_VAL_IDX).

update_memory_train_ema_usage(StatsEts, Value) ->
    % get the current EMA usage
    CurrentEma = get_memory_train_ema_usage(StatsEts),
    % calculate the new EMA usage
    NewEma = ema_calc(CurrentEma, Value),
    ets:update_element(StatsEts, ?STATS_MEMORY_TRAIN_EMA_USAGE, { ?STATS_KEYVAL_VAL_IDX, NewEma }).

get_memory_predict_ema_usage(StatsEts) ->
    ets:lookup_element(StatsEts, ?STATS_MEMORY_PREDICT_EMA_USAGE , ?STATS_KEYVAL_VAL_IDX).

update_memory_predict_ema_usage(StatsEts, Value) ->
    % get the current EMA usage
    CurrentEma = get_memory_predict_ema_usage(StatsEts),
    % calculate the new EMA usage
    NewEma = ema_calc(CurrentEma, Value),
    ets:update_element(StatsEts, ?STATS_MEMORY_PREDICT_EMA_USAGE, { ?STATS_KEYVAL_VAL_IDX, NewEma }). 

set_cpu_util_per_core(StatsEts, UtilList) ->
    %% UtilList is a list of tuples {CoreIndex, Util}
    lists:foreach(fun({CoreIndex, Util}) ->
        KeyUtilTrainingPerCoreStr = lists:flatten(io_lib:format("cpu_train_util_core_~p" , [CoreIndex])),
        KeyUtilTrainingPerCoreAtom = list_to_atom(KeyUtilTrainingPerCoreStr),
        ets:update_element(StatsEts, KeyUtilTrainingPerCoreAtom, { ?STATS_KEYVAL_VAL_IDX, Util }),
        KeyUtilPredictPerCoreStr = lists:flatten(io_lib:format("cpu_predict_util_core_~p" , [CoreIndex])),
        KeyUtilPredictPerCoreAtom = list_to_atom(KeyUtilPredictPerCoreStr),
        ets:update_element(StatsEts, KeyUtilPredictPerCoreAtom, { ?STATS_KEYVAL_VAL_IDX, Util })
    end, UtilList).