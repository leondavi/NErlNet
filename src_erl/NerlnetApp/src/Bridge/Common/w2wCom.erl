-module(w2wCom). % Worker to Worker Communication Module
-behaviour(gen_server).

-include("w2wCom.hrl").

-export([start_link/1]).
-export([init/1, handle_cast/2, handle_call/3]).
-export([send_message/4, send_message_with_event/5, get_all_messages/1 , sync_inbox/1, sync_inbox_no_limit/1]). % methods that are used by worker


setup_logger(Module) ->
    logger:set_handler_config(default, formatter, {logger_formatter, #{}}),
    logger:set_module_level(Module, all).

%% @doc Spawns the server and registers the local name (unique)
-spec(start_link(args) ->
    {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link(Args = {WorkerName, _ClientStatemPid}) ->
    setup_logger(?MODULE),
    {ok,Gen_Server_Pid} = gen_server:start_link({local, WorkerName}, ?MODULE, Args, []),
    Gen_Server_Pid.

init({WorkerName, MyClientPid}) ->
    InboxQueue = queue:new(),
    W2wEts = ets:new(w2w_ets, [set]),
    put(worker_name, WorkerName),
    put(client_statem_pid, MyClientPid),
    % TODO Send init message to client with the {WorkerName , W2WCOMM_PID}
    put(w2w_ets, W2wEts),
    ets:insert(W2wEts, {inbox_queue, InboxQueue}),
    {ok, []}.

handle_cast({update_gen_worker_pid, GenWorkerPid}, State) ->
    put(gen_worker_pid, GenWorkerPid),
    {noreply, State};

handle_cast(Msg, State) ->
    io:format("@w2wCom: Wrong message received ~p~n", [Msg]),
    {noreply, State}.

handle_call({?W2WCOM_ATOM, FromWorkerName, ThisWorkerName, {msg_with_event, Event,  Data}}, _From, State) ->
    case get(worker_name) of
        ThisWorkerName -> ok;
        _ -> throw({error, "The provided worker name is not this worker"})
    end,
    GenWorkerPid = get(gen_worker_pid),
    case Event of
        post_train_update   -> gen_statem:cast(GenWorkerPid, {post_train_update, Data});
        start_stream        -> gen_statem:cast(GenWorkerPid, {start_stream, Data}); 
        end_stream          -> gen_statem:cast(GenWorkerPid, {end_stream, Data}) 
    end,
    % Saved messages are of the form: {FromWorkerName, , Data}
    Message = {FromWorkerName, Data},
    add_msg_to_inbox_queue(Message),
    {reply, {ok, Event}, State};

% Received messages are of the form: {worker_to_worker_msg, FromWorkerName, ThisWorkerName, Data}
handle_call({?W2WCOM_ATOM, FromWorkerName, ThisWorkerName, Data}, _From, State) ->
    case get(worker_name) of
        ThisWorkerName -> ok;
        _ -> throw({error, "The provided worker name is not this worker"})
    end,
    % Saved messages are of the form: {FromWorkerName, , Data}
    Message = {FromWorkerName, Data},
    add_msg_to_inbox_queue(Message),
    {reply, {ok, "Message received"}, State};

% Token messages are tupe of: {FromWorkerName, Token, Data}
handle_call({?W2WCOM_TOKEN_CAST_ATOM, FromWorkerName, ThisWorkerName, Token, Data}, _From, State) ->
    case get(worker_name) of
        ThisWorkerName -> ok;
        _ -> throw({error, "The provided worker name is not this worker"})
    end,
    Message = {FromWorkerName, Token, Data},
    add_msg_to_inbox_queue(Message),
    {reply, {ok, "Message received"}, State};


handle_call({is_inbox_empty}, _From, State) ->
    W2WEts = get(w2w_ets),
    InboxQueue = ets:lookup_element(W2WEts, inbox_queue, ?ETS_KEYVAL_VAL_IDX),
    IsInboxEmpty = queue:len(InboxQueue) == 0,
    {reply, {ok, IsInboxEmpty}, State};

handle_call({get_inbox_queue}, _From, State) ->
    W2WEts = get(w2w_ets),
    NewEmptyQueue = queue:new(),
    InboxQueue = ets:lookup_element(W2WEts, inbox_queue, ?ETS_KEYVAL_VAL_IDX),
    ets:update_element(W2WEts, inbox_queue, {?ETS_KEYVAL_VAL_IDX, NewEmptyQueue}),
    {reply, {ok, InboxQueue}, State};

handle_call({get_client_pid}, _From, State) ->
    {reply, {ok, get(client_statem_pid)}, State};

handle_call(_Call, _From, State) ->
    {noreply, State}.

get_all_messages(W2WPid) -> % Returns the InboxQueue and flush it 
    {ok , InboxQueue} = gen_server:call(W2WPid, {get_inbox_queue}),
    InboxQueue.

add_msg_to_inbox_queue(Message) -> % Only w2wCom process executes this function
    W2WEts = get(w2w_ets),
    InboxQueue = ets:lookup_element(W2WEts, inbox_queue, ?ETS_KEYVAL_VAL_IDX),
    InboxQueueUpdated = queue:in(Message, InboxQueue),
    ets:update_element(W2WEts, inbox_queue, {?ETS_KEYVAL_VAL_IDX, InboxQueueUpdated}).


send_message(W2WPid, FromWorker, TargetWorker, Data) -> 
    Msg = {?W2WCOM_ATOM, FromWorker, TargetWorker, Data},
    {ok, MyClient} = gen_server:call(W2WPid, {get_client_pid}),
    gen_statem:cast(MyClient, Msg).

send_message_with_event(W2WPid, FromWorker, TargetWorker, Event, Data) -> 
    ValidEvent = lists:member(Event, ?SUPPORTED_EVENTS),
    if ValidEvent -> ok;
        true -> ?LOG_ERROR("Event ~p is not supported!!",[Event]),
                throw({error, "The provided event is not supported"})
    end,
    Msg = {?W2WCOM_ATOM, FromWorker, TargetWorker, {msg_with_event, Event, Data}},
    {ok, MyClient} = gen_server:call(W2WPid, {get_client_pid}),
    gen_statem:cast(MyClient, Msg).


timeout_throw(Timeout) ->
    receive
        stop -> ok; 
        _ -> timeout_throw(Timeout)
    after Timeout -> throw("Timeout reached")
    end.

sync_inbox(W2WPid) ->
    TimeoutPID = spawn(fun() -> timeout_throw(?SYNC_INBOX_TIMEOUT) end),
    sync_inbox(TimeoutPID , W2WPid).

sync_inbox_no_limit(W2WPid) ->
    TimeoutPID = spawn(fun() -> timeout_throw(?SYNC_INBOX_TIMEOUT_NO_LIMIT) end),
    sync_inbox(TimeoutPID , W2WPid).

sync_inbox(TimeoutPID, W2WPid) ->
    timer:sleep(?DEFAULT_SYNC_INBOX_BUSY_WAITING_SLEEP), 
    {ok , IsInboxEmpty} = gen_server:call(W2WPid, {is_inbox_empty}),
    if 
        IsInboxEmpty -> sync_inbox(TimeoutPID, W2WPid);
        true -> TimeoutPID ! stop
    end.