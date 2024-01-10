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
-export([encode_ets_to_http_bin_str/1, decode_http_bin_str_to_ets/3]).
-export([update_workers_ets/4, increment_workers_ets/4]).

encode_ets_to_http_bin_str(_StatsEts) -> ok. %% Takes value from ets and converts it to "<EntitiyName>SEPERATOR<Value>" string.
decode_http_bin_str_to_ets(_HTTPBinStr , _StatsEts , _OptsAtom) -> ok. %% Takes "<EntitiyName>SEPERATOR<Value>" string and updates ets value. OptsAtom = increment/overwrite.

generate_stats_ets() ->
    StatsEts = ets:new(stats_ets , [set]),
    WorkersStatsEts = ets:new(stats_ets , [set]),
    ets:insert(StatsEts, {message_received , 0}),
    ets:insert(StatsEts, {message_sent , 0}),
    ets:insert(StatsEts, {message_dropped , 0}),
    ets:insert(StatsEts, {bytes_received , 0}),
    ets:insert(StatsEts, {bytes_sent , 0}),
    ets:insert(StatsEts, {bad_messages , 0}),
    ets:insert(StatsEts, {batches_received , 0}), % related with client only
    ets:insert(StatsEts, {batches_dropped , 0}), % related with client only
    ets:insert(StatsEts, {batches_sent , 0}), % related with source
    ets:insert(StatsEts, {workers_ets, WorkersStatsEts}), % TODO Guy pay attention to this when you implement encode/decode methods
    StatsEts.

%% ---- Workers Stats ETS Methods ----%%
update_workers_ets(StatsEts, WorkerName, WorkerAttribute, Value) ->
    WorkerStatsEts = ets:lookup_element(StatsEts, workers_ets, ?STATS_KEYVAL_VAL_IDX),
    Key = {WorkerName, WorkerAttribute},
    ets:insert(WorkerStatsEts, Key, Value).

increment_workers_ets(StatsEts, WorkerName, WorkerAttribute, Value) ->
    WorkerStatsEts = ets:lookup_element(StatsEts, workers_ets, ?STATS_KEYVAL_VAL_IDX),
    Key = {WorkerName, WorkerAttribute},
    ets:update_counter(WorkerStatsEts, Key, Value).

%% ---- Stats ETS Methods -----%%
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

