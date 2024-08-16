-module(sourceSendingPolicies).
-author("David Leon").

-define(MICRO_TO_MILLI_FACTOR, 0.001).

-include("../nerl_tools.hrl").
% exports
-export([send_method_casting/5, send_method_round_robin/5, send_method_random/5]).

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


generate_batch_indexes(NumOfBatches, EpochIdx) ->
  [ EpochIdx * NumOfBatches + BatchIdx || BatchIdx <- lists:seq(0, NumOfBatches-1)].


send_method_casting(TransmitterEts, Epochs, TimeInterval_ms, ClientWorkerPairs, BatchesListToSend) -> 
    send_method_casting(TransmitterEts, Epochs, TimeInterval_ms, ClientWorkerPairs, BatchesListToSend, 0).
send_method_casting(_TransmitterEts, Epochs, _TimeInterval_ms, _ClientWorkerPairs, _BatchesListToSend, EpochIdx) when EpochIdx == Epochs -> ok;
send_method_casting(TransmitterEts, Epochs, TimeInterval_ms, ClientWorkerPairs, BatchesListToSend, EpochIdx) ->
  io:format("Epoch ~p~n", [EpochIdx]),
  % Sends the same batch to all
  BatchFunc = fun({BatchIdx, Batch}) ->
    prepare_and_send(TransmitterEts, TimeInterval_ms, Batch, BatchIdx, ClientWorkerPairs)
  end, % end of BatchFunc
  TotalNumOfBatches = length(BatchesListToSend),
  BatchesIndexes = generate_batch_indexes(TotalNumOfBatches, EpochIdx),
  BatchesWithIndexes = lists:zip(BatchesIndexes, BatchesListToSend),
  lists:foreach(BatchFunc, BatchesWithIndexes),
  % update batches sent
  SkippedBatches = ets:lookup_element(TransmitterEts, batches_skipped, ?DATA_IDX),
  ets:update_counter(TransmitterEts, batches_sent, length(ClientWorkerPairs) * length(BatchesListToSend) - SkippedBatches),
  send_method_casting(TransmitterEts, Epochs, TimeInterval_ms, ClientWorkerPairs, BatchesListToSend , EpochIdx + 1).


send_method_round_robin(TransmitterEts, Epochs, TimeInterval_ms, ClientWorkerPairs, BatchesListToSend) -> 
  send_method_round_robin(TransmitterEts, Epochs, TimeInterval_ms, ClientWorkerPairs, BatchesListToSend, 0).
send_method_round_robin(_TransmitterEts, Epochs, _TimeInterval_ms, _ClientWorkerPairs, _BatchesListToSend, EpochIdx) when EpochIdx == Epochs -> ok;
send_method_round_robin(TransmitterEts, Epochs, TimeInterval_ms, ClientWorkerPairs, BatchesListToSend, EpochIdx) ->
  io:format("Epoch ~p~n", [EpochIdx]),
  % Sends a batch per each
  ClientWorkerPairsIndexes = lists:seq(0, length(ClientWorkerPairs)-1),
  ClientWorkerPairsWithIndexes = lists:zip(ClientWorkerPairsIndexes, ClientWorkerPairs), % Tuple {Idx, Triplet}
  ClientWorkerPairsMap = maps:from_list(ClientWorkerPairsWithIndexes),
  
  BatchFunc = fun({{BatchIdx, WorkerIdx}, Batch}) ->
    ClientWorkerPair = maps:get(WorkerIdx, ClientWorkerPairsMap),
    prepare_and_send(TransmitterEts, TimeInterval_ms, Batch, BatchIdx, [ClientWorkerPair])
  end, % end of BatchFunc
  TotalNumOfBatches = length(BatchesListToSend),
  BatchesIndexes = generate_batch_indexes(TotalNumOfBatches, EpochIdx),
  ClientWorkerPairsLength = length(ClientWorkerPairs),
  WorkerIdxBatchIdxTuples = [ {X , X rem ClientWorkerPairsLength} || X <- BatchesIndexes],
  BatchesWithIndexes = lists:zip(WorkerIdxBatchIdxTuples, BatchesListToSend),
  lists:foreach(BatchFunc, BatchesWithIndexes),
  % update batches sent
  SkippedBatches = ets:lookup_element(TransmitterEts, batches_skipped, ?DATA_IDX),
  ets:update_counter(TransmitterEts, batches_sent, length(BatchesListToSend) - SkippedBatches),
  send_method_round_robin(TransmitterEts, Epochs, TimeInterval_ms, ClientWorkerPairs, BatchesListToSend, EpochIdx + 1).


send_method_random(TransmitterEts, Epochs, TimeInterval_ms, ClientWorkerPairs, BatchesListToSend) -> 
  send_method_random(TransmitterEts, Epochs, TimeInterval_ms, ClientWorkerPairs, BatchesListToSend, 0).
send_method_random(_TransmitterEts, Epochs, _TimeInterval_ms, _ClientWorkerPairs, _BatchesListToSend, EpochIdx) when EpochIdx == Epochs -> ok;
send_method_random(TransmitterEts, Epochs, TimeInterval_ms, ClientWorkerPairs, BatchesListToSend, EpochIdx) ->
  io:format("Epoch ~p~n", [EpochIdx]),
  % Sends a batch per each
  ClientWorkerPairsIndexes = lists:seq(1, length(ClientWorkerPairs)),
  ClientWorkerPairsWithIndexes = lists:zip(ClientWorkerPairsIndexes, ClientWorkerPairs), % Tuple {Idx, Triplet}
  ClientWorkerPairsMap = maps:from_list(ClientWorkerPairsWithIndexes),
  BatchFunc = fun({{BatchIdx, WorkerIdx}, Batch}) ->
    ClientWorkerPair = maps:get(WorkerIdx, ClientWorkerPairsMap),
    prepare_and_send(TransmitterEts, TimeInterval_ms, Batch, BatchIdx, [ClientWorkerPair])
  end, % end of BatchFunc
  TotalNumOfBatches = length(BatchesListToSend),
  BatchesIndexes = generate_batch_indexes(TotalNumOfBatches, EpochIdx),
  ClientWorkerPairsLength = length(ClientWorkerPairs),
  WorkerIdxBatchIdxTuples = [ {X , rand:uniform(ClientWorkerPairsLength)} || X <- BatchesIndexes],
  BatchesWithIndexes = lists:zip(WorkerIdxBatchIdxTuples, BatchesListToSend), % Tuple {{BatchIdx, WorkerIdx}, Batch}
  lists:foreach(BatchFunc, BatchesWithIndexes),
  % update batches sent
  SkippedBatches = ets:lookup_element(TransmitterEts, batches_skipped, ?DATA_IDX),
  ets:update_counter(TransmitterEts, batches_sent, length(BatchesListToSend) - SkippedBatches),
  send_method_random(TransmitterEts, Epochs, TimeInterval_ms, ClientWorkerPairs, BatchesListToSend, EpochIdx + 1).