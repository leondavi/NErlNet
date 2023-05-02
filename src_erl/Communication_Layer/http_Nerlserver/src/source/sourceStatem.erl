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

%% API
-export([start_link/1]).

%% gen_statem callbacks
-export([init/1, format_status/2, state_name/3, handle_event/4, terminate/3,
  code_change/4, callback_mode/0, idle/3, castingData/3, sendSamples/10]).

-define(SERVER, ?MODULE).
-define(SENDALL, 1).
-define(ROUNDROBIN, 2).

-record(source_statem_state, {sendingMethod, frequency, chunkSize, lengthOfSample, castingTo=[], myName,workersMap,nerlnetGraph, msgCounter=0, sourcePid=[],csvName="", csvList="", num_of_features, num_of_labels}).

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
init({MyName,WorkersMap, NerlnetGraph, Method, ChunkSize,Frequency}) ->
  inets:start(),
    io:format("Source ~p Connecting to: ~p~n",[MyName, [digraph:vertex(NerlnetGraph,Vertex) || Vertex <- digraph:out_neighbours(NerlnetGraph,MyName)]]),
  start_connection([digraph:vertex(NerlnetGraph,Vertex) || Vertex <- digraph:out_neighbours(NerlnetGraph,MyName)]),


  {ok, idle, #source_statem_state{sendingMethod = Method, frequency = Frequency, chunkSize = ChunkSize,myName = MyName, workersMap = WorkersMap, nerlnetGraph = NerlnetGraph, msgCounter = 1, castingTo = []}}.

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


%%This cast receive a list of samples to load to the records csvList
idle(cast, {csvList,Workers,CSVData}, State = #source_statem_state{chunkSize = ChunkSize, myName = MyName, msgCounter = Counter, nerlnetGraph = NerlnetGraph}) ->
  %io:format("CSVData - ~p~n",[CSVData]),
  io:format("ChunkSize - ~p~n",[ChunkSize]),
  CSVlist = parser:parseCSV(ChunkSize,CSVData),
  [Head|_] = CSVlist,
  LengthOfSample = byte_size(Head),
  %%  CSVName = lists:last(re:split(CSVPath,"/",[{return,list}])),
  {RouterHost,RouterPort} = getShortPath(MyName,"mainServer",NerlnetGraph),
  %%  send an ACK to mainserver that the CSV file is ready
  io:format("source updated transmitting list, total batches to send: - ~p~n",[length(CSVlist)]),
  io:format("source updated Workers - ~p~n",[Workers]),
  http_request(RouterHost,RouterPort,"csvReady",MyName),
  {next_state, idle, State#source_statem_state{lengthOfSample = LengthOfSample, castingTo = Workers, msgCounter = Counter+1,csvList =CSVlist}};


%%This cast spawns a transmitter of data stream towards NerlClient by casting batches of data from parsed csv file given by cowboy source_server
idle(cast, {startCasting,Body}, State = #source_statem_state{myName = MyName, lengthOfSample = LengthOfSample, sendingMethod = Method, frequency = Frequency, chunkSize = ChunkSize, sourcePid = [],workersMap = WorkersMap, castingTo = CastingTo, nerlnetGraph = NerlnetGraph, msgCounter = Counter, csvName = CSVName, csvList =CSVlist}) ->
    [_Source,NumOfBatchesToSend] = re:split(binary_to_list(Body), ",", [{return, list}]),

  logger:notice("start casting to: ~p~nnumber of batches to send: ~p~ntotal casting list length: ~p~n ",[CastingTo,NumOfBatchesToSend, length(CSVlist)]),
  NumOfBatches = list_to_integer(NumOfBatchesToSend),
  BatchesToSend = if length(CSVlist) < NumOfBatches -> length(CSVlist); true -> list_to_integer(NumOfBatchesToSend) end,

  Transmitter =  spawnTransmitter(CastingTo,CSVName,CSVlist,NerlnetGraph,MyName,WorkersMap,ChunkSize,LengthOfSample,Frequency,BatchesToSend,Method) ,
  {next_state, castingData, State#source_statem_state{msgCounter = Counter+1, sourcePid = Transmitter}};

idle(cast, {startCasting}, State = #source_statem_state{msgCounter = Counter}) ->
  io:format("im not suppose to be here"),
  {next_state, castingData, State#source_statem_state{msgCounter = Counter+1}};



idle(cast, {stopCasting}, State = #source_statem_state{msgCounter = Counter}) ->
  io:format("already idle~n",[]),
  {next_state, idle, State#source_statem_state{msgCounter = Counter+1}};

idle(cast, {statistics}, State = #source_statem_state{myName =  MyName, sourcePid = [], nerlnetGraph = NerlnetGraph, msgCounter = Counter}) ->
  {RouterHost,RouterPort} = getShortPath(MyName,"mainServer",NerlnetGraph),
  http_request(RouterHost,RouterPort,"statistics", list_to_binary(MyName++"#"++integer_to_list(Counter))),
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
  io:format("already casting~n",[]),
  {next_state, castingData, State#source_statem_state{msgCounter = Counter+1}};

castingData(cast, {leftOvers,Tail}, State = #source_statem_state{msgCounter = Counter}) ->
%%  io:format("received leftovers- ~p~n",[Tail]),
  {next_state, idle, State#source_statem_state{msgCounter = Counter+1,csvList = Tail}};

castingData(cast, {finishedCasting,CounterReceived,ListOfSamples}, State = #source_statem_state{myName = MyName, msgCounter = Counter, nerlnetGraph = NerlnetGraph}) ->
   %io:format("source finished casting~n"),
  {RouterHost,RouterPort} = getShortPath(MyName,"mainServer",NerlnetGraph),
%%  send an ACK to mainserver that the CSV file is ready
  http_request(RouterHost,RouterPort,"sourceDone", MyName),
  {next_state, idle, State#source_statem_state{msgCounter = Counter+CounterReceived+1,csvList = ListOfSamples, sourcePid = []}};

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

spawnTransmitter(WorkersNames,CSVPath,CSVlist,NerlnetGraph, MyName,WorkersMap,ChunkSize,LengthOfSample, Frequency,NumOfBatchesToSend,Method)->
%%  ListOfWorkers = re:split(WorkersNames,",", [{return, list}]),
Triplets =getHostPort(WorkersNames,WorkersMap,NerlnetGraph,MyName,[]),
%%  io:format("~p~n",[Triplets]),
  %%[list of binarys from CSV file, Size of batch, 1/Hz (in milisecond), statem pid]
  Ms = round(1000/Frequency),
  spawn(?MODULE,sendSamples,[CSVlist,CSVPath,ChunkSize,LengthOfSample,Ms,self(),Triplets,0,NumOfBatchesToSend,Method]).


sendSamples(ListOfSamples,_CSVPath,_ChunkSize,_LengthOfSample, Ms,Pid,_Triplets,Counter,NumOfBatchesToSend,_Method) when NumOfBatchesToSend=<0 ->
  receive
  after Ms ->
    gen_statem:cast(Pid,{finishedCasting,Counter,ListOfSamples}), io:format("sent all samples~n")
  end;

sendSamples([],_CSVPath,_ChunkSize,_LengthOfSample, Ms,Pid,_Triplets,Counter,_NumOfBatchesToSend,_Method)->
  receive
  after Ms ->
    gen_statem:cast(Pid,{finishedCasting,Counter,[]})
  end;

sendSamples(ListOfSamples,CSVPath,ChunkSize,LengthOfSample, Ms,Pid,Triplets,Counter,NumOfBatchesToSend,Method)->
          %%this http request will be splitted at client's state machine by the following order:
          %%    Body:   ClientName#WorkerName#CSVName#BatchNumber#BatchOfSamples
  if NumOfBatchesToSend rem 10 == 0 ->
    io:format("~p samples left to send~n", [NumOfBatchesToSend]); true -> skip end,

  if Method == ?SENDALL ->
        %%sending batch to all clients"
        [Head|ListOfSamplesRest]=ListOfSamples,
        {_ListOfSamplesRest,NewCounter2} = sendToAll([Head],CSVPath,ChunkSize,LengthOfSample, Ms,Pid,Triplets,Counter);
    Method == ?ROUNDROBIN ->
      %%sending batch to all clients with round robin"

      {ListOfSamplesRest,NewCounter2} = roundRobin(ListOfSamples,CSVPath,LengthOfSample,Counter,Triplets);
    true ->%% default method is send to all
      %%"default method is send to all

      [Head|ListOfSamplesRest]=ListOfSamples,
      {_ListOfSamplesRest,NewCounter2} = sendToAll([Head],CSVPath,ChunkSize,LengthOfSample,Ms,Pid,Triplets,Counter)
  end,
%%        [http_request(RouterHost, RouterPort,"weightsVector",
%%                    list_to_binary([list_to_binary([list_to_binary(atom_to_list(ClientName)),<<"#">>,list_to_binary(WorkerName),<<"#">>,list_to_binary(CSVPath),<<"#">>,list_to_binary(integer_to_list(Counter)),<<"#">>]),list_to_binary(Head)]))
%%                  || {ClientName,WorkerName,RouterHost,RouterPort}<-Triplets],
  receive
      %%main server might ask to stop casting,update source state with remaining lines. if no stop message received, continue casting after 1/Hz
    {stopCasting}  ->
      io:format("source stop casting",[]),
      gen_statem:cast(Pid,{leftOvers,ListOfSamplesRest})
   after Ms->
    if length(ListOfSamplesRest)==0 ->
      gen_statem:cast(Pid,{finishedCasting,NewCounter2,[]});
      true ->
        sendSamples(ListOfSamplesRest,CSVPath,ChunkSize,LengthOfSample,Ms,Pid,Triplets,NewCounter2,NumOfBatchesToSend-(NewCounter2-Counter),Method)
    end
  end.

roundRobin([],_CSVPath,_LengthOfSample,Counter,_Triplets)-> {[], Counter};
roundRobin(ListOfSamples,_CSVPath,_LengthOfSample,Counter,[])-> {ListOfSamples, Counter};
roundRobin(ListOfSamples,CSVPath,LengthOfSample,Counter,[{ClientName,WorkerName,RouterHost,RouterPort}|Triplets])->
  [Head|Rest]=ListOfSamples,
  if(Head ==<<>>)->
    roundRobin(Rest,CSVPath,LengthOfSample,Counter,[{ClientName,WorkerName,RouterHost,RouterPort}|Triplets]);
    true ->
      sendSample(Head,CSVPath,LengthOfSample,Counter,ClientName,WorkerName,RouterHost,RouterPort),
      roundRobin(Rest,CSVPath,LengthOfSample,Counter+1,Triplets)
      end.



sendToAll([],_CSVPath,_ChunkSize,_LengthOfSample,_Hz,_Pid,_Triplets,Counter)->
  {[], Counter};

sendToAll([Head|ListOfSamples],CSVPath,ChunkSize,LengthOfSample,Hz,Pid,Triplets,Counter)->
  %%this http request will be splitted at client's state machine by the following order:
  [sendSample(Head,CSVPath,LengthOfSample,Counter,ClientName,WorkerName,RouterHost,RouterPort)|| {ClientName,WorkerName,RouterHost,RouterPort}<-Triplets],
  receive
  %%main server might ask to stop casting,update source state with remaining lines. if no stop message received, continue casting after 1/Hz
    {stopCasting}  ->
      io:format("source stop casting",[]),
      gen_statem:cast(Pid,{leftOvers,ListOfSamples})
  after Hz-> sendToAll(ListOfSamples,CSVPath,ChunkSize,LengthOfSample,Hz,Pid,Triplets,Counter+1)
  end.

%%Sends one batch of samples to a client
sendSample(Sample,CSVPath,LengthOfSample, BatchID,ClientName,WorkerName,RouterHost,RouterPort)->
  % when two workers(or more) are on the same device, they need a few miliseconds apart TODO remove this and manage on client
  timer:sleep(5),
  
  case byte_size(Sample) of
    LengthOfSample ->
        ToSend = term_to_binary({ClientName, WorkerName, CSVPath, BatchID, Sample}),
        http_request(RouterHost, RouterPort,"weightsVector",ToSend);
        _ -> ok
    end.



start_connection([])->ok;
start_connection([{_ServerName,{Host, Port}}|Tail]) ->
  httpc:set_options([{proxy, {{Host, Port},[Host]}}]),
  start_connection(Tail).


http_request(Host, Port,Path, Body)->
  URL = "http://" ++ Host ++ ":"++integer_to_list(Port) ++ "/" ++ Path,
  httpc:set_options([{proxy, {{Host, Port},[Host]}}]),
  httpc:request(post,{URL, [],"application/x-www-form-urlencoded",Body}, [], []).

getHostPort([],_WorkersMap,_NerlnetGraph,_MyName,Ret)-> Ret;
getHostPort([WorkerName|WorkersNames],WorkersMap,NerlnetGraph,MyName,Ret)->
  ClientName = maps:get(list_to_atom(WorkerName),WorkersMap),
  {RouterHost,RouterPort} = getShortPath(MyName,ClientName,NerlnetGraph),
  %{RouterHost,RouterPort} = maps:get(ClientName,PortMap),
  getHostPort(WorkersNames,WorkersMap, NerlnetGraph,MyName,Ret++[{ClientName,WorkerName,RouterHost,RouterPort}]).

getShortPath(From,To,NerlnetGraph) when is_atom(To)-> 
  	First = lists:nth(2,digraph:get_short_path(NerlnetGraph,From,atom_to_list(To))),

	{_First,{Host,Port}} = digraph:vertex(NerlnetGraph,First),
  {Host,Port};

getShortPath(From,To,NerlnetGraph) -> 
	First = lists:nth(2,digraph:get_short_path(NerlnetGraph,From,To)),
	{_First,{Host,Port}} = digraph:vertex(NerlnetGraph,First),
  {Host,Port}.
	