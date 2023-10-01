%%%-------------------------------------------------------------------
%%% @author kapelnik
%%% @copyright (C) 2021, Nerlnet
%%% @doc
%%%
%%% @end
%%% Created : 14. Apr 2021 9:57 AM
%%%-------------------------------------------------------------------
-module(sourceStatem).
-author("kapelnik").

-behaviour(gen_statem).

-include("../nerl_tools.hrl").
%% API
-export([start_link/1]).
%% gen_statem callbacks
-export([format_status/2, state_name/3, handle_event/4, terminate/3, code_change/4, callback_mode/0]).
%% states and misc
-export([init/1,  idle/3, castingData/3, transmitter/6]).
%% utils


%% defintions
-define(SENDING_FREQUENCY_OVERHEAD_FIX_FACTOR_PERC, 1.2).


-record(source_statem_state, {ets_ref, batchesList = [], castingTo=[], myName, source_pid, transmitter_pid = none,csvName="", nerlTensorType}).

%%%===================================================================
%%% API
%%%===================================================================

%% @doc Creates a gen_statem process which calls Module:init/1 to
%% initialize. To ensure a synchronized start-up procedure, this
%% function does not return until Module:init/1 has returned.

%%Arguments from Cowboy Server
%%return Pid to Cowboy Server
start_link(NerlnetGraph) ->
    {ok,Pid} = gen_statem:start_link(?MODULE, NerlnetGraph, []),
    Pid.

%%%===================================================================
%%% gen_statem callbacks
%%%===================================================================

%% @private
%% @doc Whenever a gen_statem is started using gen_statem:start/[3,4] or
%% gen_statem:start_link/[3,4], this function is called by the new
%% process to initialize.
%%initialize and go to state - idle
init({MyName,WorkersMap, NerlnetGraph, Method, BatchSize,Frequency}) ->

  nerl_tools:setup_logger(?MODULE),
  inets:start(),
  ?LOG_INFO("Source ~p is connected to: ~p~n",[MyName, [digraph:vertex(NerlnetGraph,Vertex) || Vertex <- digraph:out_neighbours(NerlnetGraph,MyName)]]),
  ?LOG_INFO("BatchSize: ~p",[BatchSize]),

  EtsRef = ets:new(source_data, [set]),
  ets:insert(EtsRef, {nerlnet_graph, NerlnetGraph}),
  ets:insert(EtsRef, {workers_map, WorkersMap}),
  ets:insert(EtsRef, {total_messages_ctr, 0}),
  ets:insert(EtsRef, {batches_sent_ctr, 0}),
  ets:insert(EtsRef, {new_batch_id_val, 0}),
  ets:insert(EtsRef, {my_name, MyName}),
  ets:insert(EtsRef, {frequency, Frequency}),
  ets:insert(EtsRef, {time_interval_ms, round(1000/Frequency)}), % time of delay before transmitter sends a batch
  ets:insert(EtsRef, {batch_size, BatchSize}),
  ets:insert(EtsRef, {method, Method}),
  ets:insert(EtsRef, {epochs, none}),
  ets:insert(EtsRef, {sample_size, none}),
  ets:insert(EtsRef, {workers_list, []}),
  ets:insert(EtsRef, {csv_name, ""}), % not in use

  put(nerlnetGraph, NerlnetGraph), % %TODO get rid of this put (it is used by nerl_tools:sendHTTP)


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
idle(cast, {batchList,WorkersList,Epochs, CSVData}, State = #source_statem_state{ets_ref = EtsRef}) ->
  MyName = ets:lookup_element(EtsRef, my_name, ?DATA_IDX),
  BatchSize = ets:lookup_element(EtsRef, batch_size, ?DATA_IDX),
  {NerlTensorBatchesList, NerlTensorType, SampleSize} = parser:parseCSV(MyName,BatchSize,CSVData), % TODO this is slow and heavy method! pre parse in ETS a possible solution
  ets:update_element(EtsRef, workers_list, [{?DATA_IDX, WorkersList}]),
  ets:update_element(EtsRef, epochs, [{?DATA_IDX, Epochs}]),
  ets:update_counter(EtsRef, total_messages_ctr, 1), % last is increment value
  ?LOG_NOTICE("Source ~p, workers are: ~p", [MyName, WorkersList]),
  ?LOG_NOTICE("Source ~p, sample size: ~p", [MyName, SampleSize]),
  ets:update_element(EtsRef, sample_size, [{?DATA_IDX, SampleSize}]),
  ?LOG_INFO("source updated transmition list, total avilable batches to send: ~p~n",[length(NerlTensorBatchesList)]),
  %%  send an ACK to mainserver that the CSV file is ready

  %% TODO fix exception here
  nerl_tools:sendHTTP(MyName,?MAIN_SERVER_ATOM,"csvReady",MyName),
  {next_state, idle, State#source_statem_state{batchesList = NerlTensorBatchesList, nerlTensorType = NerlTensorType}};


%% This cast spawns a transmitter of data stream towards NerlClient by casting batches of data from parsed csv file given by cowboy source_server
idle(cast, {startCasting,Body}, State = #source_statem_state{ets_ref = EtsRef, batchesList = BatchesList}) ->
  [_Source,UserLimitNumberOfBatchesToSend] = re:split(binary_to_list(Body), ",", [{return, list}]),
 
  ets:update_counter(EtsRef, total_messages_ctr, 1), % last is increment value

  MyName = ets:lookup_element(EtsRef, my_name, ?DATA_IDX),
  Frequency = ets:lookup_element(EtsRef, frequency, ?DATA_IDX),
  BatchSize = ets:lookup_element(EtsRef, batch_size, ?DATA_IDX),
  SampleSize = ets:lookup_element(EtsRef, sample_size, ?DATA_IDX),
  Epochs = ets:lookup_element(EtsRef, epochs, ?DATA_IDX),
  BatchSize = ets:lookup_element(EtsRef, batch_size, ?DATA_IDX),
  WorkersList = ets:lookup_element(EtsRef, workers_list, ?DATA_IDX),

  UserLimitNumberOfBatchesToSendInt = list_to_integer(UserLimitNumberOfBatchesToSend),
  BatchesToSend = min(length(BatchesList), UserLimitNumberOfBatchesToSendInt),
  BatchesListFinal = lists:sublist(BatchesList, BatchesToSend), % Batches list with respect of constraint UserLimitNumberOfBatchesToSendInt

  % TODO consider add offset value from API

  ?LOG_NOTICE("~p - starts casting to workers: ~p",[MyName, WorkersList]),
  ?LOG_NOTICE("Frequency: ~pHz [Batches/Second]",[Frequency]),
  ?LOG_NOTICE("Batch size: ~p", [BatchSize]),
  ?LOG_NOTICE("Sample size = ~p",[SampleSize]),
  ?LOG_NOTICE("Rounds per data (epochs): ~p", [Epochs]),
  ?LOG_NOTICE("Limit max # of batches by API is set to ~p ",[UserLimitNumberOfBatchesToSendInt]),
  ?LOG_NOTICE("# of batches to send is ~p ",[BatchesToSend]),

  TransmitterPID =  spawnTransmitter(EtsRef, WorkersList, BatchesListFinal),
  {next_state, castingData, State#source_statem_state{transmitter_pid = TransmitterPID}};

idle(cast, {startCasting}, State = #source_statem_state{ets_ref = EtsRef}) ->
  ets:update_counter(EtsRef, total_messages_ctr, 1), % last is increment value
  % io:format("im not suppose to be here"),
  MyName = ets:lookup_element(EtsRef, my_name, ?DATA_IDX),
  ?LOG_WARNING("Source ~p receives message during casting!",[MyName]),
  {next_state, castingData, State#source_statem_state{}};


idle(cast, {stopCasting}, State = #source_statem_state{ets_ref = EtsRef}) ->
  ets:update_counter(EtsRef, total_messages_ctr, 1), % last is increment value
  {next_state, idle, State#source_statem_state{}};

idle(cast, {statistics}, State = #source_statem_state{ets_ref = EtsRef, myName =  MyName}) ->
  NerlnetGraph = ets:lookup_element(EtsRef, nerlnet_graph, ?DATA_IDX),
  ets:update_counter(EtsRef, total_messages_ctr, 1), % last is increment value
  TotalMessagesCounter = ets:lookup_element(EtsRef, total_messages_ctr, ?DATA_IDX),

  {RouterHost,RouterPort} = nerl_tools:getShortPath(MyName,?MAIN_SERVER_ATOM,NerlnetGraph),
  nerl_tools:http_request(RouterHost,RouterPort,"statistics", list_to_binary(atom_to_list(MyName)++":"++integer_to_list(TotalMessagesCounter))),
%%  io:format("sending statistics casting to: ~p~n",[CastingTo]),
  {next_state, idle, State#source_statem_state{}};

idle(cast, _EventContent, State = #source_statem_state{ets_ref = EtsRef}) ->
  ets:update_counter(EtsRef, total_messages_ctr, 1), % last is increment value
  MyName = ets:lookup_element(EtsRef, my_name, ?DATA_IDX),
  ?LOG_WARNING("Source ~p receives an unexpected cast event in idle state!",[MyName]),
  {next_state, idle, State#source_statem_state{}}.

%%waiting for ether data list of sample been sent to finish OR stop message from main server.
castingData(cast, {stopCasting}, State = #source_statem_state{ets_ref = EtsRef, transmitter_pid = TransmitterPID}) ->
  ets:update_counter(EtsRef, total_messages_ctr, 1), % last is increment value
  TransmitterPID ! {stopCasting},
  {next_state, idle, State#source_statem_state{transmitter_pid = none}};

castingData(cast, {startCasting}, State = #source_statem_state{ets_ref = EtsRef}) ->
  ets:update_counter(EtsRef, total_messages_ctr, 1), % last is increment value
  MyName = ets:lookup_element(EtsRef, my_name, ?DATA_IDX),
  ?LOG_WARNING("~p is already casting, but received a startCasting message",[MyName]),
  {next_state, castingData, State#source_statem_state{}};

castingData(cast, {leftOvers,_Tail}, State = #source_statem_state{ets_ref = EtsRef}) ->
  MyName = ets:lookup_element(EtsRef, my_name, ?DATA_IDX),
  ets:update_counter(EtsRef, total_messages_ctr, 1), % last is increment value
  ?LOG_ERROR("Source ~p got leftOvers unhandled case of castingData state - Currently Deprecated!",[MyName]),
  {next_state, idle, State#source_statem_state{}};
  %{next_state, idle, State#source_statem_state{batchList = Tail}};

castingData(cast, {finishedCasting, BatchesSent}, State = #source_statem_state{ets_ref = EtsRef}) ->
   %% source finished casting %%
   ets:update_counter(EtsRef, total_messages_ctr, BatchesSent), % last is increment value
   ets:update_counter(EtsRef, batches_sent_ctr, BatchesSent),
   MyName = ets:lookup_element(EtsRef, my_name, ?DATA_IDX),
   NerlnetGraph = ets:lookup_element(EtsRef, nerlnet_graph, ?DATA_IDX),
  {RouterHost,RouterPort} = nerl_tools:getShortPath(MyName,?MAIN_SERVER_STR,NerlnetGraph),
  %%  send an ACK to mainserver that the CSV file is ready
  nerl_tools:http_request(RouterHost,RouterPort,"sourceDone", MyName),
  {next_state, idle, State#source_statem_state{transmitter_pid = none}};

castingData(cast, _EventContent, State = #source_statem_state{ets_ref = EtsRef}) ->
  ets:update_counter(EtsRef, total_messages_ctr, 1), % last is increment value
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
%%% Flow: transmitter -> transmit_func -> send_method_<method> -> prepare_send -> sendBatch
spawnTransmitter(SourceEtsRef, WorkersListOfNames, BatchesListToSend)->
  MyName = ets:lookup_element(SourceEtsRef, my_name, ?DATA_IDX),
  NerlnetGraph = ets:lookup_element(SourceEtsRef, nerlnet_graph, ?DATA_IDX),
  WorkersMap = ets:lookup_element(SourceEtsRef, workers_map, ?DATA_IDX),
  Method = ets:lookup_element(SourceEtsRef, method, ?DATA_IDX),
  TimeInterval_ms = ets:lookup_element(SourceEtsRef, time_interval_ms, ?DATA_IDX), % frequency to time interval duration in milliseconds between each send
  Triplets = nerl_tools:getHostPort(WorkersListOfNames,WorkersMap,NerlnetGraph,MyName,[]),
  SourcePid = self(),
  spawn_link(?MODULE,transmitter,[TimeInterval_ms,SourceEtsRef, SourcePid ,Triplets, BatchesListToSend, Method]).

%% Sends batch of samples to a client
% A batch is always {NerlTensor, Type}
sendBatch({NerlTensor, Type},CSVPath, BatchID,ClientName,WorkerName,RouterHost,RouterPort)->
        ToSend = term_to_binary({ClientName, WorkerName, CSVPath, BatchID, {NerlTensor, Type}}),
        nerl_tools:http_request(RouterHost, RouterPort,"batch",ToSend). % TODO add macro

prepare_and_send(TransmitterEts, TimeInterval_ms, Batch, Triplet) ->
  Tic = erlang:timestamp(), % frequency relates to each send
  if
    Batch == <<>> ->
      skip_batch;
    true ->
      BatchID = ets:lookup_element(TransmitterEts, current_batch_id, ?DATA_IDX),
      {ClientName,WorkerName,RouterHost,RouterPort} = Triplet,% TODO Gal is about to perform refactor here with casting support
      CSVPath = "",
      % sending batch
      sendBatch(Batch,CSVPath, BatchID, ClientName,WorkerName,RouterHost,RouterPort),
      % update counters
      ets:update_counter(TransmitterEts, batches_sent, 1),
      % timing handling
      Toc_millisec = timer:now_diff(Tic, erlang:timestamp()) * 0.001, % out of diff is micro
      SleepDuration = erlang:max(0,  round(TimeInterval_ms - ?SENDING_FREQUENCY_OVERHEAD_FIX_FACTOR_PERC*Toc_millisec)),
      timer:sleep(SleepDuration),
      ok
  end.


send_method_casting(TransmitterEts, TimeInterval_ms, Triplets, BatchesListToSend) ->
  % Sends the same batch to all
  BatchFunc = fun(Batch) ->
    CastingFunc = fun(Triplet) -> 
      Result = prepare_and_send(TransmitterEts, TimeInterval_ms, Batch, Triplet), % improve by allowing casting messages
      case Result of
        skip_batch -> 
          MyName = ets:lookup_element(TransmitterEts, my_name, ?DATA_IDX),
          ?LOG_WARNING("Source ~p Skipped Batch", [MyName]);
        _ELSE -> ok
      end
    end,
    lists:foreach(CastingFunc, Triplets), % TODO Gal - with casting no need for outer loop here
    ets:update_counter(TransmitterEts, current_batch_id, 1)
  end,
  lists:foreach(BatchFunc, BatchesListToSend).

send_method_round_robin(TransmitterEts, TimeInterval_ms, Triplets, BatchesListToSend) ->
  % Sends a batch per each
  BatchFunc = fun(Batch, TripletPos) ->
    Triplet = lists:nth(TripletPos),
    Result = prepare_and_send(TransmitterEts, TimeInterval_ms, Batch, Triplet), % improve by allowing casting messages
    ets:update_counter(TransmitterEts, current_batch_id, 1),
    case Result of
      skip_batch -> 
        MyName = ets:lookup_element(TransmitterEts, my_name, ?DATA_IDX),
        ?LOG_WARNING("Source ~p Skipped Batch", [MyName]);
      _ELSE -> ok
    end
  end,
  TripletPos = [(X rem length(Triplets)) + 1 || X <- lists:seq(0, length(BatchesListToSend)-1)],
  lists:zipwith(BatchFunc, BatchesListToSend, TripletPos).


transmitter(TimeInterval_ms, SourceEtsRef, SourcePid ,Triplets, BatchesListToSend, Method) ->
  MyName = ets:lookup_element(SourceEtsRef, my_name, ?DATA_IDX),
  ?LOG_INFO("Source ~p Transmitter Starts", [MyName]),
  TransmitterEts = ets:new(transmitter_ets, [set]), % allow transmitter process to edit
  ets:insert(TransmitterEts, {my_name, MyName}),
  ets:insert(TransmitterEts, {batches_sent, 0}),
  ets:insert(TransmitterEts, {current_batch_id, 0}),
  TransmissionStart = erlang:timestamp(),
  case Method of
    ?CASTING -> send_method_casting(TransmitterEts, TimeInterval_ms, Triplets, BatchesListToSend);
    ?ROUNDROBIN -> send_method_round_robin(TransmitterEts, TimeInterval_ms, Triplets, BatchesListToSend);
    _Default -> send_method_casting(TransmitterEts, TimeInterval_ms, Triplets, BatchesListToSend)
  end,
  TransmissionTimeTook_sec = timer:now_diff(erlang:timestamp(), TransmissionStart) / 1000000,
  BatchesSent = ets:lookup_element(TransmitterEts, batches_sent, ?DATA_IDX),

  gen_statem:cast(SourcePid,{finishedCasting,BatchesSent}),
  ActualFrequency = 1/(TransmissionTimeTook_sec/BatchesSent),
  ?LOG_INFO("Source ~p Transmitter Finished", [MyName]),
  ?LOG_INFO("Source ~p Actual Frequency: ~p [B/Sec]",[MyName, ActualFrequency]),
  ets:delete(TransmitterEts).