-module(w2wCom). % Worker to Worker Communication Module
-behaviour(gen_server).

-include("w2wCom.hrl").

-export([start_link/1]).
-export([init/1, handle_cast/2, handle_call/3]).
-export([send_message/3, get_all_messages/0 , sync_inbox/0]). % methods that are used by worker

-define(SYNC_INBOX_TIMEOUT, 30000). % 30 seconds

%% @doc Spawns the server and registers the local name (unique)
-spec(start_link(args) ->
  {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link(Args = {WorkerName, _ClientStatemPid}) ->
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

% Received messages are of the form: {worker_to_worker_msg, FromWorkerName, ThisWorkerName, Data}
handle_cast({?W2WCOM_ATOM, FromWorkerName, ThisWorkerName, Data}, State) ->
    case get(worker_name) of
        ThisWorkerName -> ok;
        _ -> throw({error, "The provided worker name is not this worker"})
    end,
    % Saved messages are of the form: {FromWorkerName, , Data}
    Message = {FromWorkerName, Data},
    add_msg_to_inbox_queue(Message),
    io:format("Worker ~p received message from ~p: ~p~n", [ThisWorkerName, FromWorkerName, Data]), %TODO remove
    {noreply, State};

% Token messages are tupe of: {FromWorkerName, Token, Data}
handle_cast({?W2WCOM_TOKEN_CAST_ATOM, FromWorkerName, ThisWorkerName, Token, Data}, State) ->
    case get(worker_name) of
        ThisWorkerName -> ok;
        _ -> throw({error, "The provided worker name is not this worker"})
    end,
    Message = {FromWorkerName, Token, Data},
    add_msg_to_inbox_queue(Message),
     io:format("Worker ~p received token message from ~p: ~p~n", [ThisWorkerName, FromWorkerName, Data]), %TODO remove
    {noreply, State};

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_call(_Call, _From, State) ->
    {noreply, State}.

get_all_messages() ->
    W2WEts = get(w2w_ets),
    {_, InboxQueue} = ets:lookup(W2WEts, inbox_queue),
    NewEmptyQueue = queue:new(),
    ets:update_element(W2WEts, inbox_queue, {inbox_queue, NewEmptyQueue}),
    InboxQueue.

add_msg_to_inbox_queue(Message) ->
    W2WEts = get(w2w_ets),
    {_, InboxQueue} = ets:lookup(W2WEts, inbox_queue),
    InboxQueueUpdated = queue:in(Message, InboxQueue),
    ets:update_element(W2WEts, inbox_queue, {inbox_queue, InboxQueueUpdated}).

send_message(FromWorker, TargetWorker, Data) -> 
    Msg = {?W2WCOM_ATOM, FromWorker, TargetWorker, Data},
    MyClient = get(client_statem_pid),
    gen_server:cast(MyClient, Msg).

is_inbox_empty() ->
    W2WEts = get(w2w_ets),
    {_ , InboxQueue} = ets:lookup(W2WEts, inbox_queue),
    queue:len(InboxQueue) == 0.


% Think about better alternative to this method

timeout(Timeout) ->
    receive
        stop -> ok; 
        _ -> timeout(Timeout)
    after Timeout -> throw("Timeout reached")
    end.

sync_inbox() ->
    TimeoutPID = spawn(fun() -> timeout(?SYNC_INBOX_TIMEOUT) end),
    sync_inbox(TimeoutPID).

sync_inbox(TimeoutPID) ->
    timer:sleep(10), % 10 ms
    IsInboxEmpty = is_inbox_empty(),
    if 
        IsInboxEmpty -> sync_inbox(TimeoutPID);
        true -> TimeoutPID ! stop
    end.