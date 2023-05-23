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
-export([init/1,  idle/3, castingData/3, sendSamples/10]).

-record(source_statem_state, {sendingMethod, frequency, batchSize, lengthOfSample, castingTo=[], myName,workersMap,nerlnetGraph, msgCounter=0, sourcePid=[],csvName="", batchList, nerlTensorType}).

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
  io:format("Source ~p Connecting to: ~p~n",[MyName, [digraph:vertex(NerlnetGraph,Vertex) || Vertex <- digraph:out_neighbours(NerlnetGraph,MyName)]]),
  nerl_tools:start_connection([digraph:vertex(NerlnetGraph,Vertex) || Vertex <- digraph:out_neighbours(NerlnetGraph,MyName)]),

  {ok, idle, #source_statem_state{sendingMethod = Method, frequency = Frequency, batchSize = BatchSize,myName = MyName, workersMap = WorkersMap, nerlnetGraph = NerlnetGraph, msgCounter = 1, castingTo = []}}.

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


%%This cast receive a list of samples to load to the records batchList
idle(cast, {batchList,Workers,CSVData}, State = #source_statem_state{batchSize = BatchSize, myName = MyName, msgCounter = Counter, nerlnetGraph = NerlnetGraph}) ->
  %io:format("CSVData - ~p~n",[CSVData]),
  ?LOG_INFO("Arch BatchSize = ~p~nWorkers under source = ~p~n",[BatchSize, Workers]),
  {NerlTensorList, NerlTensorType, SampleSize} = parser:parseCSV(MyName,BatchSize,CSVData),
  %%  CSVName = lists:last(re:split(CSVPath,"/",[{return,list}])),
  {RouterHost,RouterPort} = nerl_tools:getShortPath(MyName,"mainServer",NerlnetGraph),
  %%  send an ACK to mainserver that the CSV file is ready
  ?LOG_INFO("source updated transmition list, total avilable batches to send: ~p~n",[length(NerlTensorList)]),
  nerl_tools:http_request(RouterHost,RouterPort,"csvReady",MyName),
  {next_state, idle, State#source_statem_state{lengthOfSample = SampleSize, castingTo = Workers, msgCounter = Counter+1,batchList = NerlTensorList, nerlTensorType = NerlTensorType}};


%%This cast spawns a transmitter of data stream towards NerlClient by casting batches of data from parsed csv file given by cowboy source_server
idle(cast, {startCasting,Body}, State = #source_statem_state{myName = MyName, lengthOfSample = LengthOfSample, sendingMethod = Method, frequency = Frequency, batchSize = BatchSize, sourcePid = [],workersMap = WorkersMap, castingTo = CastingTo, nerlnetGraph = NerlnetGraph, msgCounter = Counter, csvName = CSVName, batchList =CSVlist}) ->
    [_Source,NumOfBatchesToSend] = re:split(binary_to_list(Body), ",", [{return, list}]),

  ?LOG_NOTICE("start casting to: ~p~nnumber of batches to send: ~p~ntotal casting list length: ~p~n ",[CastingTo,NumOfBatchesToSend, length(CSVlist)]),
  NumOfBatches = list_to_integer(NumOfBatchesToSend),
  BatchesToSend = if length(CSVlist) < NumOfBatches -> length(CSVlist); true -> list_to_integer(NumOfBatchesToSend) end,

  Transmitter =  spawnTransmitter(CastingTo,CSVName,CSVlist,NerlnetGraph,MyName,WorkersMap,BatchSize,LengthOfSample,Frequency,BatchesToSend,Method) ,
  {next_state, castingData, State#source_statem_state{msgCounter = Counter+1, sourcePid = Transmitter}};

idle(cast, {startCasting}, State = #source_statem_state{msgCounter = Counter}) ->
  % io:format("im not suppose to be here"),
  {next_state, castingData, State#source_statem_state{msgCounter = Counter+1}};



idle(cast, {stopCasting}, State = #source_statem_state{msgCounter = Counter}) ->
  % io:format("already idle~n",[]),
  {next_state, idle, State#source_statem_state{msgCounter = Counter+1}};

idle(cast, {statistics}, State = #source_statem_state{myName =  MyName, sourcePid = [], nerlnetGraph = NerlnetGraph, msgCounter = Counter}) ->
  {RouterHost,RouterPort} = nerl_tools:getShortPath(MyName,"mainServer",NerlnetGraph),
  nerl_tools:http_request(RouterHost,RouterPort,"statistics", list_to_binary(MyName++"#"++integer_to_list(Counter))),
%%  io:format("sending statistics casting to: ~p~n",[CastingTo]),
  {next_state, idle, State#source_statem_state{msgCounter = Counter+1}};

idle(cast, EventContent, State = #source_statem_state{msgCounter = Counter}) ->
  io:format("ignored: ~p~nstate - idle",[EventContent]),
  {next_state, idle, State#source_statem_state{msgCounter = Counter+1}}.

%%waiting for ether data list of sample been sent to finish OR stop message from main server.
castingData(cast, {stopCasting}, State = #source_statem_state{msgCounter = Counter,sourcePid = SourcePid}) ->
  SourcePid ! {stopCasting},
  {next_state, idle, State#source_statem_state{msgCounter = Counter+1,sourcePid = []}};

castingData(cast, {startCasting}, State = #source_statem_state{msgCounter = Counter}) ->
  % io:format("already casting~n",[]),
  {next_state, castingData, State#source_statem_state{msgCounter = Counter+1}};

castingData(cast, {leftOvers,Tail}, State = #source_statem_state{msgCounter = Counter}) ->
%%  io:format("received leftovers- ~p~n",[Tail]),
  {next_state, idle, State#source_statem_state{msgCounter = Counter+1,batchList = Tail}};

castingData(cast, {finishedCasting,CounterReceived,ListOfSamples}, State = #source_statem_state{myName = MyName, msgCounter = Counter, nerlnetGraph = NerlnetGraph}) ->
   %io:format("source finished casting~n"),
  {RouterHost,RouterPort} = nerl_tools:getShortPath(MyName,"mainServer",NerlnetGraph),
%%  send an ACK to mainserver that the CSV file is ready
  nerl_tools:http_request(RouterHost,RouterPort,"sourceDone", MyName),
  {next_state, idle, State#source_statem_state{msgCounter = Counter+CounterReceived+1,batchList = ListOfSamples, sourcePid = []}};

castingData(cast, EventContent, State = #source_statem_state{msgCounter = Counter}) ->
  io:format("ignored: ~p~nstate - casting data",[EventContent]),
  {next_state, castingData, State#source_statem_state{msgCounter = Counter+1}}.


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

%%send samples receive a batch size and casts the client with data. data received as a string containing floats and integers.
%%CLIENT NEED TO PROCESS DATA BEFORE FEEDING THE DNN

spawnTransmitter(WorkersNames,CSVPath,CSVlist,NerlnetGraph, MyName,WorkersMap,BatchSize,LengthOfSample, Frequency,NumOfBatchesToSend,Method)->
%%  ListOfWorkers = re:split(WorkersNames,",", [{return, list}]),
  Triplets = nerl_tools:getHostPort(WorkersNames,WorkersMap,NerlnetGraph,MyName,[]),
  %%[list of binarys from CSV file, Size of batch, 1/Hz (in milisecond), statem pid]
  Ms = round(1000/Frequency),
  spawn(?MODULE,sendSamples,[CSVlist,CSVPath,BatchSize,LengthOfSample,Ms,self(),Triplets,0,NumOfBatchesToSend,Method]).


sendSamples(ListOfSamples,_CSVPath,_BatchSize,_LengthOfSample, Ms,Pid,_Triplets,Counter,NumOfBatchesToSend,_Method) when NumOfBatchesToSend=<0 ->
  receive
  after Ms ->
    gen_statem:cast(Pid,{finishedCasting,Counter,ListOfSamples}), ?LOG_INFO("sent all samples~n")
  end;

sendSamples([],_CSVPath,_BatchSize,_LengthOfSample, Ms,Pid,_Triplets,Counter,_NumOfBatchesToSend,_Method)->
  receive
  after Ms ->
    gen_statem:cast(Pid,{finishedCasting,Counter,[]})
  end;

sendSamples(ListOfSamples,CSVPath,BatchSize,LengthOfSample, Ms,Pid,Triplets,Counter,NumOfBatchesToSend,Method)->
          %%this http request will be splitted at client's state machine by the following order:
          %%    Body:   ClientName#WorkerName#CSVName#BatchNumber#BatchOfSamples
  if NumOfBatchesToSend rem 10 == 0 ->
    ?LOG_INFO("~p samples left to send~n", [NumOfBatchesToSend]); true -> skip end,
  
  case Method of
    ?SENDALL ->
      %%sending batch to all clients"
      [Head|ListOfSamplesRest]=ListOfSamples,
      {_ListOfSamplesRest,NewCounter2} = sendToAll([Head],CSVPath,BatchSize,LengthOfSample, Ms,Pid,Triplets,Counter);

    ?ROUNDROBIN -> 
      %%sending batch to all clients with round robin"
      {ListOfSamplesRest,NewCounter2} = roundRobin(ListOfSamples,CSVPath,LengthOfSample,Counter,Triplets);
      
    %% default method is send to all
    _Default ->
      [Head|ListOfSamplesRest]=ListOfSamples,
      {_ListOfSamplesRest,NewCounter2} = sendToAll([Head],CSVPath,BatchSize,LengthOfSample,Ms,Pid,Triplets,Counter)
  end,

  %%main server might ask to stop casting,update source state with remaining lines. if no stop message received, continue casting after 1/Hz
  receive
    {stopCasting}  ->
      io:format("source stop casting",[]),
      gen_statem:cast(Pid,{leftOvers,ListOfSamplesRest})
   after Ms->
    if length(ListOfSamplesRest) == 0 -> gen_statem:cast(Pid,{finishedCasting,NewCounter2,[]});
      true -> sendSamples(ListOfSamplesRest,CSVPath,BatchSize,LengthOfSample,Ms,Pid,Triplets,NewCounter2,NumOfBatchesToSend-(NewCounter2-Counter),Method)
    end
  end.

roundRobin([],_CSVPath,_LengthOfSample,Counter,_Triplets)-> {[], Counter};
roundRobin(ListOfSamples,_CSVPath,_LengthOfSample,Counter,[])-> {ListOfSamples, Counter};
roundRobin(ListOfSamples,CSVPath,LengthOfSample,Counter,[{ClientName,WorkerName,RouterHost,RouterPort}|Triplets])->
  [Head|Rest]=ListOfSamples,
  if(Head ==<<>>)->
      roundRobin(Rest,CSVPath,LengthOfSample,Counter,[{ClientName,WorkerName,RouterHost,RouterPort}|Triplets]);
    true ->
      sendBatch(Head,CSVPath,LengthOfSample,Counter,ClientName,WorkerName,RouterHost,RouterPort),
      roundRobin(Rest,CSVPath,LengthOfSample,Counter+1,Triplets)
  end.



sendToAll([],_CSVPath,_BatchSize,_LengthOfSample,_Hz,_Pid,_Triplets,Counter)->
  {[], Counter};

sendToAll([Head|ListOfSamples],CSVPath,BatchSize,LengthOfSample,Hz,Pid,Triplets,Counter)->
  %%this http request will be splitted at client's state machine by the following order:
  [sendBatch(Head,CSVPath,LengthOfSample,Counter,ClientName,WorkerName,RouterHost,RouterPort)|| {ClientName,WorkerName,RouterHost,RouterPort}<-Triplets],
  receive
  %%main server might ask to stop casting,update source state with remaining lines. if no stop message received, continue casting after 1/Hz
    {stopCasting}  ->
      io:format("source stop casting",[]),
      gen_statem:cast(Pid,{leftOvers,ListOfSamples})
  after Hz-> sendToAll(ListOfSamples,CSVPath,BatchSize,LengthOfSample,Hz,Pid,Triplets,Counter+1)
  end.

%%Sends one batch of samples to a client
sendBatch(Sample,CSVPath,_LengthOfSample, BatchID,ClientName,WorkerName,RouterHost,RouterPort)->
  % when two workers(or more) are on the same device, they need a few miliseconds apart TODO remove this and manage on client
  % timer:sleep(5),
  % io:format("Source sending to Worker ~p: ~p~n",[WorkerName, Sample]),
  case Sample of
    {<<>>, _Type} -> done;    % no tensor to send
    {_Tensor, _Type} ->
        ToSend = term_to_binary({ClientName, WorkerName, CSVPath, BatchID, Sample}),
        nerl_tools:http_request(RouterHost, RouterPort,"weightsVector",ToSend)
    end.
