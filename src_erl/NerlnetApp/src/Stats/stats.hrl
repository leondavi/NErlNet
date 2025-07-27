-define(STATS_ATOM_MSG_RECV, messages_received).
-define(STATS_ATOM_MSG_SENT, messages_sent).
-define(STATS_ATOM_MSG_DROP, messages_dropped).
-define(STATS_ATOM_BYTES_RECV, bytes_received).
-define(STATS_ATOM_BYTES_SENT, bytes_sent).
-define(STATS_ATOM_BAD_MSG, bad_messages).
-define(STATS_ATOM_BATCHES_RECEIVED, batches_received).
-define(STATS_ATOM_BATCHES_DROPPED, batches_dropped).
-define(STATS_ATOM_BATCHES_SENT, batches_sent).

-define(STATS_KEYVAL_KEY_IDX, 1).
-define(STATS_KEYVAL_VAL_IDX, 2).

-define(SEPERATOR_TRIPLETS , "#").
-define(SEPERATOR_WITHIN_TRIPLET , ":").

-define(API_SERVER_ENTITY_SEPERATOR , "|").
-define(PERF_STATS_SEPERATOR , ";").
-define(API_SERVER_WITHIN_ENTITY_SEPERATOR , "&").

-define(WORKER_SEPERATOR , "^").
-define(WORKER_SEPERATOR_TRIPLETS , "@").
-define(WORKER_SEPERATOR_WITHIN_TRIPLET , "$").

-define(STATS_ATOM_TIME_TRAIN_ACTIVE, time_train_active).
-define(STATS_ATOM_TIME_TRAIN_TOTAL, time_train_total).
-define(STATS_ATOM_TIME_PREDICT_ACTIVE, time_predict_active).
-define(STATS_ATOM_TIME_PREDICT_TOTAL, time_predict_total).
-define(STATS_AVG_GPU_USAGE_TRAIN, average_gpu_usage_train).
-define(STATS_AVG_GPU_MEMORY_USAGE_PREDICT, average_gpu_memory_usage_predict).
-define(STATS_MEMORY_TRAIN_EMA_USAGE, memory_train_ema_usage).
-define(STATS_MEMORY_PREDICT_EMA_USAGE, memory_predict_ema_usage).
-define(STATS_MEMORY_TRAIN_PEAK_USAGE, memory_train_peak_usage).
-define(STATS_MEMORY_PREDICT_PEAK_USAGE, memory_predict_peak_usage).

-define(EMA_COEFFICIENT_HIST, 0.4).
