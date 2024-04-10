-module(w2wCom). % Worker to Worker Communication Module
-behaviour(gen_server).

-include("w2wCom.hrl").
-define(SERVER, ?MODULE).

-export([start_link/1]).
-export([init/1, handle_cast/2, handle_call/3]).
-export([send_message/3, get_inbox_queue/1]). % methods that are used by worker

%% @doc Spawns the server and registers the local name (unique)
-spec(start_link(args) ->
  {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link(Args) ->
  {ok,Gen_Server_Pid} = gen_server:start_link({local, ?SERVER}, ?MODULE, Args, []),
  Gen_Server_Pid.

init({GenWorkerEts, WorkerName, ClientStatemPid}) ->
    InboxQueue = queue:new(),
    put(gen_worker_ets, GenWorkerEts),
    put(client_statem_pid, ClientStatemPid),
    ets:insert(GenWorkerEts, {get_ets_key(WorkerName), InboxQueue}),
    {ok, []}.

% Messages are of the form: {FromWorkerName, Data}
handle_cast({?W2WCOM_ATOM, FromWorkerName, ThisWorkerName, Data}, State) ->
    % TODO throw exception of ThisWorkerName is not this worker
    GenWorkerEts = get(gen_worker_ets),
    EtsKey = get_ets_key(ThisWorkerName),
    {_, InboxQueue} = ets:lookup(GenWorkerEts, EtsKey),
    InboxQueueUpdated = queue:in({FromWorkerName, Data}, InboxQueue),
    ets:insert(GenWorkerEts, {EtsKey, InboxQueueUpdated}),
    io:format("Worker ~p received message from ~p: ~p~n", [ThisWorkerName, FromWorkerName, Data]), %TODO remove
    {noreply, State};

% Token messages are tupe of: {FromWorkerName, Token, Data}
handle_cast({?W2WCOM_TOKEN_CAST_ATOM, FromWorkerName, ThisWorkerName, Token, Data}, State) ->
    % TODO throw exception of ThisWorkerName is not this worker
    GenWorkerEts = get(gen_worker_ets),
    EtsKey = get_ets_key(ThisWorkerName),
    {_, InboxQueue} = ets:lookup(GenWorkerEts, EtsKey),
    InboxQueueUpdated = queue:in({FromWorkerName, Token, Data}, InboxQueue),
    ets:insert(GenWorkerEts, {EtsKey, InboxQueueUpdated}),
    io:format("Worker ~p received token message from ~p: ~p~n", [ThisWorkerName, FromWorkerName, Data]), %TODO remove
    {noreply, State};

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_call(_Call, _From, State) ->
    {noreply, State}.


% Generic Functions for Worker to Worker Communication
get_ets_key(WorkerName) ->
    {?W2WCOM_INBOX_Q_ATOM, WorkerName}.

get_inbox_queue(WorkerName) ->
    GenWorkerEts = get(gen_worker_ets),
    EtsKey = get_ets_key(WorkerName),
    {_, InboxQueue} = ets:lookup(GenWorkerEts, EtsKey),
    InboxQueue.

send_message(FromWorkerName, ToWorkerName, Data) -> 
    Msg = {?W2WCOM_ATOM, FromWorkerName, ToWorkerName, Data},
    MyClient = client_name, % TODO
    gen_server:cast(MyClient, Msg).

    
    