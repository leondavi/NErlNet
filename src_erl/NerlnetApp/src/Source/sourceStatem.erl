%%%-------------------------------------------------------------------
%%% @authors Guy Perts, Tal Kapelnik, David Leon
%%% 
%%% Nerlnet - Source Entity FSM
%%% License: https://github.com/leondavi/NErlNet/blob/master/LICENSE
%%%-------------------------------------------------------------------
-module(sourceStatem).
-author("kapelnik").

-behaviour(gen_statem).

-include("../nerl_tools.hrl").
-include("../source_definitions_ag.hrl").

%% API
-export([start_link/1]).
%% gen_statem callbacks
-export([format_status/2, state_name/3, handle_event/4, terminate/3, code_change/4, callback_mode/0]).
%% states and misc
-export([init/1,  idle/3, castingData/3, transmitter/6]).
%% utils


%% defintions
-define(SENDING_FREQUENCY_OVERHEAD_FIX_FACTOR_PERC, 0.75).
-define(MICRO_TO_MILLI_FACTOR, 0.001).


-record(source_statem_state, {ets_ref, batchesList = [], castingTo=[], myName, source_pid, transmitter_pid = none,csvName="", nerlTensorType}).

%%%===================================================================
%%% API
%%%===================================================================

%% @doc Creates a gen_statem process which calls Module:init/1 to
%% initialize. To ensure a synchronized start-up procedure, this
%% function does not return until Module:init/1 has returned.

%%Arguments from Cowboy Server
%%return Pid to Cowboy Server
start_link(SourceStatemArgs) ->
    {ok,Pid} = gen_statem:start_link(?MODULE, SourceStatemArgs, []),
    Pid.

%%%===================================================================
%%% gen_statem callbacks
%%%===================================================================

%% @private
%% @doc Whenever a gen_statem is started using gen_statem:start/[3,4] or
%% gen_statem:start_link/[3,4], this function is called by the new
%% process to initialize.
%%initialize and go to state - idle
init({MyName, WorkersMap, NerlnetGraph, Policy, BatchSize, Frequency , Epochs, Type}) ->
  nerl_tools:setup_logger(?MODULE),
  inets:start(),
  ?LOG_INFO("Source ~p is connected to: ~p~n",[MyName, [digraph:vertex(NerlnetGraph,Vertex) || Vertex <- digraph:out_neighbours(NerlnetGraph,MyName)]]),
  ?LOG_INFO("BatchSize: ~p",[BatchSize]),
  put(myName , MyName),
  EtsRef = ets:new(source_data, [set]),
  EtsStatsRef = stats:generate_stats_ets(),

  ets:insert(EtsRef, {nerlnet_graph, NerlnetGraph}),
  ets:insert(EtsRef, {workers_map, WorkersMap}),
  ets:insert(EtsRef, {total_messages_ctr, 0}),
  ets:insert(EtsRef, {batches_sent_ctr, 0}),
  ets:insert(EtsRef, {new_batch_id_val, 0}),
  ets:insert(EtsRef, {my_name, MyName}),
  ets:insert(EtsRef, {frequency, Frequency}),
  ets:insert(EtsRef, {time_interval_ms, round(1000/Frequency)}), % time of delay before transmitter sends a batch
  ets:insert(EtsRef, {batch_size, BatchSize}),
  ets:insert(EtsRef, {method, Policy}),
  ets:insert(EtsRef, {epochs, Epochs}),
  ets:insert(EtsRef, {num_of_batches, 0}),
  ets:insert(EtsRef, {type, Type}), %% CSV/Camera
  ets:insert(EtsRef, {sample_size, none}),
  ets:insert(EtsRef, {workers_list, []}),
  ets:insert(EtsRef, {csv_name, ""}), % not in use
  
  {MyRouterHost,MyRouterPort} = nerl_tools:getShortPath(MyName,?MAIN_SERVER_ATOM, NerlnetGraph),
  ets:insert(EtsRef, {my_router,{MyRouterHost,MyRouterPort}}),

  % Updating dictionary
  put(nerlnetGraph, NerlnetGraph),
  put(source_ets, EtsRef),
  put(source_stats_ets, EtsStatsRef),
  {ok, idle, #source_statem_state{ets_ref = EtsRef, castingTo = []}}.

%% @private
%% @doc This function is called by a gen_statem when it needs to find out
%% the callback mode of the callback module.
callback_mode() ->
  state_functions.

%% @private
%% @doc Called (1) whenever sys:get_status/1,2 is called by gen_statem or
%% (2) when gen_statem terminates abnormally.
%% This callback is optional.
format_status(_Opt, [_PDict, _StateName, _State]) ->
  Status = some_term,
  Status.

%% @private
%% @doc There should be one instance of this function for each possible
%% state name.  If callback_mode is state_functions, one of these
%% functions is called when gen_statem receives and event from
%% call/2, cast/2, or as a normal process message.
state_name(_EventType, _EventContent, State = #source_statem_state{}) ->
  NextStateName = next_state,
  {next_state, NextStateName, State}.


%% This cast receive a list of samples to load to the records batchList
idle(cast, {batchList, WorkersList, NumOfBatches, NerlTensorType, Data}, State) ->
  EtsRef = get(source_ets),
  StatsEtsRef = get(source_stats_ets),
  MyName = ets:lookup_element(EtsRef, my_name, ?DATA_IDX),
  BatchSize = ets:lookup_element(EtsRef, batch_size, ?DATA_IDX),
  {NerlTensorBatchesList, SampleSize} = parser:parseCSV(MyName, BatchSize, NerlTensorType, Data), % TODO this is slow and heavy policy! pre parse in ETS a possible solution
  ets:update_element(EtsRef, workers_list, [{?DATA_IDX, WorkersList}]),
  ets:update_element(EtsRef, num_of_batches, [{?DATA_IDX, NumOfBatches}]),
  ets:insert(EtsRef, {nerlTensorType, NerlTensorType}),
  stats:increment_messages_received(StatsEtsRef),
  ?LOG_NOTICE("Source ~p, workers are: ~p", [MyName, WorkersList]),
  ?LOG_NOTICE("Source ~p, sample size: ~p", [MyName, SampleSize]),
  ets:update_element(EtsRef, sample_size, [{?DATA_IDX, SampleSize}]),
  ?LOG_INFO("Source ~p updated transmission list, total available batches to send: ~p~n",[MyName, NumOfBatches]),
  %%  send an ACK to mainserver that the CSV file is ready
  {RouterHost,RouterPort} = ets:lookup_element(EtsRef, my_router, ?DATA_IDX),
  nerl_tools:http_router_request(RouterHost, RouterPort, [?MAIN_SERVER_ATOM], atom_to_list(dataReady), MyName),
  ?LOG_INFO("~p Ready for casting",[MyName]),
  stats:increment_messages_sent(StatsEtsRef),
  {next_state, idle, State#source_statem_state{batchesList = NerlTensorBatchesList, nerlTensorType = NerlTensorType}};


%% This cast spawns a transmitter of data stream towards NerlClient by casting batches of data from parsed csv file given by cowboy source_server
idle(cast, {startCasting,_Body}, State = #source_statem_state{batchesList = BatchesList}) ->
  EtsRef = get(source_ets),
  StatsEtsRef = get(source_stats_ets),
  stats:increment_messages_received(StatsEtsRef),

  MyName = ets:lookup_element(EtsRef, my_name, ?DATA_IDX),
  Frequency = ets:lookup_element(EtsRef, frequency, ?DATA_IDX),
  BatchSize = ets:lookup_element(EtsRef, batch_size, ?DATA_IDX),
  SampleSize = ets:lookup_element(EtsRef, sample_size, ?DATA_IDX),
  Epochs = ets:lookup_element(EtsRef, epochs, ?DATA_IDX),
  BatchSize = ets:lookup_element(EtsRef, batch_size, ?DATA_IDX),
  WorkersList = ets:lookup_element(EtsRef, workers_list, ?DATA_IDX),

  %% UserLimitNumberOfBatchesToSendInt = list_to_integer(UserLimitNumberOfBatchesToSend),
  %% BatchesToSend = min(length(BatchesList), UserLimitNumberOfBatchesToSendInt),
  BatchesToSend = length(BatchesList),
  BatchesListFinal = lists:sublist(BatchesList, BatchesToSend), % Batches list with respect of constraint UserLimitNumberOfBatchesToSendInt

  % TODO consider add offset value from API
  ?LOG_NOTICE("~p - starts casting to workers: ~p",[MyName, WorkersList]),
  ?LOG_NOTICE("Frequency: ~pHz [Batches/Second]",[Frequency]),
  ?LOG_NOTICE("Batch size: ~p", [BatchSize]),
  ?LOG_NOTICE("Sample size = ~p",[SampleSize]),
  ?LOG_NOTICE("Rounds per data (epochs): ~p", [Epochs]),
  ?LOG_NOTICE("# of batches to send is ~p ",[BatchesToSend]),

  TransmitterPID =  spawnTransmitter(EtsRef, WorkersList, BatchesListFinal),
  {next_state, castingData, State#source_statem_state{transmitter_pid = TransmitterPID}};

idle(cast, {startCasting}, State) ->
  ?LOG_ERROR("Should not get start casting in idle - start casting is only after setting the phase type"),
  EtsRef = get(source_ets),
  StatsEtsRef = get(source_stats_ets),
  stats:increment_messages_received(StatsEtsRef),

  MyName = ets:lookup_element(EtsRef, my_name, ?DATA_IDX),
  ?LOG_WARNING("Source ~p receives message during casting!",[MyName]),
  {next_state, castingData, State};


idle(cast, {stopCasting}, State) ->
  StatsEtsRef = get(source_stats_ets),
  stats:increment_messages_received(StatsEtsRef),
  {next_state, idle, State};

idle(cast, {statistics}, State) ->
  EtsRef = get(source_ets),
  StatsEtsRef = get(source_stats_ets),
  stats:increment_messages_received(StatsEtsRef),

  MyName = ets:lookup_element(EtsRef, my_name, ?DATA_IDX),
  StatsEtsStr = stats:encode_ets_to_http_bin_str(StatsEtsRef),
  StatisticsBody = {MyName , StatsEtsStr}, 
  {RouterHost,RouterPort} = ets:lookup_element(EtsRef, my_router, ?DATA_IDX),
  nerl_tools:http_router_request(RouterHost, RouterPort, [?MAIN_SERVER_ATOM], atom_to_list(statistics), StatisticsBody),
  stats:increment_messages_sent(StatsEtsRef),
  {next_state, idle, State#source_statem_state{}};

idle(cast, _EventContent, State) ->
  EtsRef = get(source_ets),
  StatsEtsRef = get(source_stats_ets),
  stats:increment_messages_received(StatsEtsRef),
  MyName = ets:lookup_element(EtsRef, my_name, ?DATA_IDX),
  ?LOG_WARNING("Source ~p receives an unexpected cast event in idle state!",[MyName]),
  {next_state, idle, State#source_statem_state{}}.

%%waiting for ether data list of sample been sent to finish OR stop message from main server.
castingData(cast, {stopCasting}, State = #source_statem_state{transmitter_pid = TransmitterPID}) ->
  StatsEtsRef = get(source_stats_ets),
  stats:increment_messages_received(StatsEtsRef),
  ?LOG_ERROR("Unsupported yet"),
  TransmitterPID ! {stopCasting}, % TODO - kill transmitter on stop casting
  {next_state, idle, State#source_statem_state{transmitter_pid = none}};

castingData(cast, {startCasting}, State) ->
  EtsRef = get(source_ets),
  StatsEtsRef = get(source_stats_ets),
  stats:increment_messages_received(StatsEtsRef),
  MyName = ets:lookup_element(EtsRef, my_name, ?DATA_IDX),
  ?LOG_WARNING("~p is already casting, but received a startCasting message",[MyName]),
  {next_state, castingData, State};

castingData(cast, {leftOvers,_Tail}, State) ->
  EtsRef = get(source_ets),
  StatsEtsRef = get(source_stats_ets),
  stats:increment_messages_received(StatsEtsRef),
  MyName = ets:lookup_element(EtsRef, my_name, ?DATA_IDX),
  ?LOG_ERROR("Source ~p got leftOvers unhandled case of castingData state - Currently Deprecated!",[MyName]),
  {next_state, idle, State};

castingData(cast, {finishedCasting, BatchesSent}, State) ->
  EtsRef = get(source_ets),
  StatsEtsRef = get(source_stats_ets),
  stats:increment_messages_received(StatsEtsRef),
  %% source finished casting %%
  stats:increment_by_value(StatsEtsRef, batches_sent, BatchesSent),
  MyName = ets:lookup_element(EtsRef, my_name, ?DATA_IDX),
  {RouterHost,RouterPort} = ets:lookup_element(EtsRef, my_router, ?DATA_IDX),
  %%  send an ACK to mainserver that the CSV file is ready
  nerl_tools:http_router_request(RouterHost, RouterPort, [?MAIN_SERVER_ATOM], atom_to_list(sourceDone), MyName),
  stats:increment_messages_sent(StatsEtsRef),
  {next_state, idle, State#source_statem_state{transmitter_pid = none}};

castingData(cast, _EventContent, State = #source_statem_state{ets_ref = EtsRef}) ->
  StatsEtsRef = get(source_stats_ets),
  stats:increment_bad_messages(StatsEtsRef),
  MyName = ets:lookup_element(EtsRef, my_name, ?DATA_IDX),
  ?LOG_WARNING("Source ~p Received cast event during castingData state",[MyName]),
  {next_state, castingData, State#source_statem_state{}}.

%% @private
%% @doc If callback_mode is handle_event_function, then whenever a
%% gen_statem receives an event from call/2,  cast/2, or as a normal
%% process message, this function is called.
handle_event(_EventType, _EventContent, _StateName, State = #source_statem_state{}) ->
  NextStateName = the_next_state_name,
  {next_state, NextStateName, State}.

%% @private
%% @doc This function is called by a gen_statem when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_statem terminates with
%% Reason. The return value is ignored.
terminate(_Reason, _StateName, _State = #source_statem_state{}) ->
  ok.

%% @private
%% @doc Convert process state when code is changed
code_change(_OldVsn, StateName, State = #source_statem_state{}, _Extra) ->
  {ok, StateName, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%%% spawnTransmitter - creates a transmitter process to send batches
%%% Flow: transmitter -> transmit_func -> send_policy_<policy> -> prepare_send -> sendBatch
spawnTransmitter(SourceEtsRef, WorkersListOfNames, BatchesListToSend)->
  WorkersMap = ets:lookup_element(SourceEtsRef, workers_map, ?DATA_IDX),
  Method = ets:lookup_element(SourceEtsRef, method , ?DATA_IDX),
  TimeInterval_ms = ets:lookup_element(SourceEtsRef, time_interval_ms, ?DATA_IDX), % frequency to time interval duration in milliseconds between each send
  ClientWorkerPairs = nerl_tools:get_client_worker_pairs(WorkersListOfNames,WorkersMap,[]),
  SourcePid = self(),
  TimeIntervalWithOverheadFactor = TimeInterval_ms * ?SENDING_FREQUENCY_OVERHEAD_FIX_FACTOR_PERC,
  spawn_link(?MODULE,transmitter,[TimeIntervalWithOverheadFactor,SourceEtsRef, SourcePid ,ClientWorkerPairs, BatchesListToSend, Method]).

%% Sends batch of samples to a client
% A batch is always {NerlTensor, Type}
sendBatch(MyName,{NerlTensor, Type}, BatchID,ClientName,WorkerName,RouterHost,RouterPort)->
        ToSend = {MyName , ClientName, WorkerName, BatchID, {NerlTensor, Type}},
        nerl_tools:http_router_request(RouterHost, RouterPort, [ClientName], atom_to_list(batch), ToSend).

prepare_and_send(_TransmitterEts, _TimeInterval_ms, _Batch, _BatchIdx, []) -> ok;
prepare_and_send(TransmitterEts, TimeInterval_ms, Batch, BatchIdx, [ClientWorkerPair | ClientWorkerPairsTail]) ->
  Tic = erlang:timestamp(), % frequency relates to each send
  {RouterHost,RouterPort} = ets:lookup_element(TransmitterEts, my_router, ?DATA_IDX),
  case Batch of
  {NerlTensor, Type} ->
    {ClientName,WorkerName} = ClientWorkerPair,% TODO Gal is about to perform refactor here with casting support
    MyName = ets:lookup_element(TransmitterEts, my_name, ?DATA_IDX),
    % sending batch
    sendBatch(MyName,{NerlTensor, Type}, BatchIdx, ClientName,WorkerName,RouterHost,RouterPort),
    % timing handling
    Toc_millisec = timer:now_diff(Tic, erlang:timestamp()) * ?MICRO_TO_MILLI_FACTOR,
    SleepDuration = erlang:max(0,  round(TimeInterval_ms - Toc_millisec)),
    timer:sleep(SleepDuration);
  <<>> ->
    ets:update_counter(TransmitterEts, batches_skipped, 1);
  _ISSUE ->
    ets:update_counter(TransmitterEts, batches_issue, 1)
  end,
  prepare_and_send(TransmitterEts, TimeInterval_ms, Batch, BatchIdx, ClientWorkerPairsTail).

send_method_casting(TransmitterEts, TimeInterval_ms, ClientWorkerPairs, BatchesListToSend) ->
  % Sends the same batch to all
  BatchFunc = fun({BatchIdx, Batch}) ->
    prepare_and_send(TransmitterEts, TimeInterval_ms, Batch, BatchIdx, ClientWorkerPairs)
  end, % end of BatchFunc
  BatchesIndexes = lists:seq(0, length(BatchesListToSend)-1),
  BatchesWithIndexes = lists:zip(BatchesIndexes, BatchesListToSend),
  lists:foreach(BatchFunc, BatchesWithIndexes),
  % update batches sent
  SkippedBatches = ets:lookup_element(TransmitterEts, batches_skipped, ?DATA_IDX),
  ets:update_counter(TransmitterEts, batches_sent, length(ClientWorkerPairs) * length(BatchesListToSend) - SkippedBatches).

send_method_round_robin(TransmitterEts, TimeInterval_ms, ClientWorkerPairs, BatchesListToSend) ->
  % Sends a batch per each
  ClientWorkerPairsIndexes = lists:seq(0, length(ClientWorkerPairs)-1),
  ClientWorkerPairsWithIndexes = lists:zip(ClientWorkerPairsIndexes, ClientWorkerPairs), % Tuple {Idx, Triplet}
  ClientWorkerPairsMap = maps:from_list(ClientWorkerPairsWithIndexes),
  
  BatchFunc = fun({BatchIdx, Batch}) ->
    ClientWorkerPairIdx = BatchIdx rem length(ClientWorkerPairs),
    ClientWorkerPair = maps:get(ClientWorkerPairIdx, ClientWorkerPairsMap),
    prepare_and_send(TransmitterEts, TimeInterval_ms, Batch, BatchIdx, [ClientWorkerPair]) % improve by allowing casting messages
  end, % end of BatchFunc
  BatchesIndexes = lists:seq(0, length(BatchesListToSend)-1),
  BatchesWithIndexes = lists:zip(BatchesIndexes, BatchesListToSend),
  lists:foreach(BatchFunc, BatchesWithIndexes),
  % update batches sent
  SkippedBatches = ets:lookup_element(TransmitterEts, batches_skipped, ?DATA_IDX),
  ets:update_counter(TransmitterEts, batches_sent, length(BatchesListToSend) - SkippedBatches).

send_method_random(TransmitterEts, TimeInterval_ms, ClientWorkerPairs, BatchesListToSend) ->
  % Sends a batch per each
  ClientWorkerPairsIndexes = lists:seq(1, length(ClientWorkerPairs)),
  ClientWorkerPairsWithIndexes = lists:zip(ClientWorkerPairsIndexes, ClientWorkerPairs), % Tuple {Idx, Triplet}
  ClientWorkerPairsMap = maps:from_list(ClientWorkerPairsWithIndexes),
  BatchFunc = fun({BatchIdx, Batch}) ->
    ClientWorkerPairIdx = rand:uniform(length(ClientWorkerPairs)),
    ClientWorkerPair = maps:get(ClientWorkerPairIdx, ClientWorkerPairsMap),
    prepare_and_send(TransmitterEts, TimeInterval_ms, Batch, BatchIdx, [ClientWorkerPair]) % improve by allowing casting messages
  end, % end of BatchFunc
  BatchesIndexes = lists:seq(0, length(BatchesListToSend)-1),
  BatchesWithIndexes = lists:zip(BatchesIndexes, BatchesListToSend),
  lists:foreach(BatchFunc, BatchesWithIndexes),
  % update batches sent
  SkippedBatches = ets:lookup_element(TransmitterEts, batches_skipped, ?DATA_IDX),
  ets:update_counter(TransmitterEts, batches_sent, length(BatchesListToSend) - SkippedBatches).

transmitter(TimeInterval_ms, SourceEtsRef, SourcePid ,ClientWorkerPairs, BatchesListToSend, Method) ->
  MyName = ets:lookup_element(SourceEtsRef, my_name, ?DATA_IDX),
  TransmitterEts = ets:new(transmitter_ets, [set]), % allow transmitter process to edit
  {SourceRouterHost,SourceRouterPort} = ets:lookup_element(SourceEtsRef, my_router, ?DATA_IDX),
  ets:insert(TransmitterEts, {my_name, MyName}),
  ets:insert(TransmitterEts, {my_router, {SourceRouterHost,SourceRouterPort}}),
  ets:insert(TransmitterEts, {batches_sent, 0}),
  ets:insert(TransmitterEts, {batches_issue, 0}),
  ets:insert(TransmitterEts, {batches_skipped, 0}),
  ets:insert(TransmitterEts, {current_batch_id, 0}),
  TransmissionStart = erlang:timestamp(),
  case Method of
    ?SOURCE_POLICY_CASTING_ATOM -> send_method_casting(TransmitterEts, TimeInterval_ms, ClientWorkerPairs, BatchesListToSend);
    ?SOURCE_POLICY_ROUNDROBIN_ATOM -> send_method_round_robin(TransmitterEts, TimeInterval_ms, ClientWorkerPairs, BatchesListToSend);
    ?SOURCE_POLICY_RANDOM_ATOM -> send_method_random(TransmitterEts, TimeInterval_ms, ClientWorkerPairs, BatchesListToSend);
    _Default -> send_method_casting(TransmitterEts, TimeInterval_ms, ClientWorkerPairs, BatchesListToSend)
  end,
  TransmissionTimeTook_sec = timer:now_diff(erlang:timestamp(), TransmissionStart) / 1000000,
  ErrorBatches = ets:lookup_element(TransmitterEts, batches_issue, ?DATA_IDX),
  SkippedBatches = ets:lookup_element(TransmitterEts, batches_skipped, ?DATA_IDX),
  BatchesSent = ets:lookup_element(TransmitterEts, batches_sent, ?DATA_IDX),

  if 
    SkippedBatches > 0 -> ?LOG_WARNING("Source ~p skipped ~P Batches", [MyName, SkippedBatches]);
    true -> ok
  end,

  if 
    ErrorBatches > 0 -> ?LOG_ERROR("Source ~p had ~P Batches with errors", [MyName, ErrorBatches]);
    true -> ok
  end,

  gen_statem:cast(SourcePid,{finishedCasting,BatchesSent}),
  ActualFrequency = 1/(TransmissionTimeTook_sec/BatchesSent),
  ?LOG_INFO("Source ~p Actual Frequency: ~p [B/Sec]",[MyName, ActualFrequency]),
  ets:delete(TransmitterEts).