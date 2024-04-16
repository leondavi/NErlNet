-module(w2wCom). % Worker to Worker Communication Module
-behaviour(gen_server).

-include("w2wCom.hrl").

-export([start_link/1]).
-export([init/1, handle_cast/2, handle_call/3]).
-export([send_message/3, get_all_messages/0]). % methods that are used by worker

%% @doc Spawns the server and registers the local name (unique)
-spec(start_link(args) ->
  {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link(Args = {WorkerName, _ClientStatemPid}) ->
  {ok,Gen_Server_Pid} = gen_server:start_link({local, WorkerName}, ?MODULE, Args, []),
  Gen_Server_Pid.

init({WorkerName, ClientStatemPid}) ->
    InboxQueue = queue:new(),
    W2wEts = ets:new(w2w_ets, [set]),
    put(worker_name, WorkerName),
    put(client_statem_pid, ClientStatemPid),
    put(w2w_ets, W2wEts),
    ets:insert(W2wEts, {inbox_queue, InboxQueue}),
    {ok, []}.

% Messages are of the form: {FromWorkerName, Data}
handle_cast({?W2WCOM_ATOM, FromWorkerName, ThisWorkerName, Data}, State) ->
    case get(worker_name) of
        ThisWorkerName -> ok;
        _ -> throw({error, "The provided worker name is not this worker"})
    end,
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
    InboxQueue.

add_msg_to_inbox_queue(Message) ->
    W2WEts = get(w2w_ets),
    {_, InboxQueue} = ets:lookup(W2WEts, inbox_queue),
    InboxQueueUpdated = queue:in(Message, InboxQueue),
    ets:insert(W2WEts, {inbox_queue, InboxQueueUpdated}).

send_message(FromWorkerName, ToWorkerName, Data) -> 
    Msg = {?W2WCOM_ATOM, FromWorkerName, ToWorkerName, Data},
    MyClient = client_name, % TODO
    gen_server:cast(MyClient, Msg).

    
    