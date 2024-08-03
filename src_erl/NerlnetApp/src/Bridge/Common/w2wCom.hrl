
-include_lib("kernel/include/logger.hrl").
-include("workerDefinitions.hrl").

-define(W2WCOM_INBOX_Q_ATOM, worker_to_worker_inbox_queue).
-define(W2WCOM_ATOM, worker_to_worker_msg).
-define(W2WCOM_TOKEN_CAST_ATOM, worker_to_worker_token_cast).
-define(SYNC_INBOX_TIMEOUT, 30000). % 30 seconds
-define(SYNC_INBOX_TIMEOUT_NO_LIMIT, 36000000). % 36000 seconds = 10 hours , no limit 
-define(DEFAULT_SYNC_INBOX_BUSY_WAITING_SLEEP, 5). % 5 milliseconds
-define(SUPPORTED_EVENTS , [post_train_update, start_stream, end_stream]).