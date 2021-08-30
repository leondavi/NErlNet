%%%-------------------------------------------------------------------
%%% @author kapelnik
%%% @copyright (C) 2021, <COMPANY>
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
  code_change/4, callback_mode/0, idle/3, castingData/3, sendSamples/7]).

-define(SERVER, ?MODULE).

-record(source_statem_state, {chunkSize, castingTo=[], myName,workersMap,portMap, msgCounter=0, sourcePid=[],csvName="", csvList="", num_of_features, num_of_labels}).

%%%===================================================================
%%% API
%%%===================================================================

%% @doc Creates a gen_statem process which calls Module:init/1 to
%% initialize. To ensure a synchronized start-up procedure, this
%% function does not return until Module:init/1 has returned.

%%Arguments from Cowboy Server
%%return Pid to Cowboy Server
start_link(ConnectionsMap) ->
    {ok,Pid} = gen_statem:start_link(?MODULE, ConnectionsMap, []),
    Pid.

%%%===================================================================
%%% gen_statem callbacks
%%%===================================================================

%% @private
%% @doc Whenever a gen_statem is started using gen_statem:start/[3,4] or
%% gen_statem:start_link/[3,4], this function is called by the new
%% process to initialize.
%%initialize and go to state - idle
init({MyName,WorkersMap, ConnectionsMap,ChunkSize}) ->
  inets:start(),
  start_connection(maps:to_list(ConnectionsMap)),
  {ok, idle, #source_statem_state{chunkSize = ChunkSize,myName = MyName, workersMap = WorkersMap, portMap = ConnectionsMap, msgCounter = 1, castingTo = []}}.

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
idle(cast, {csvList,Workers,CSVPath}, State = #source_statem_state{chunkSize = ChunkSize, myName = Myname, msgCounter = Counter, portMap = PortMap}) ->
  CSVlist = parser:parse_file(CSVPath,ChunkSize),
  CSVName = lists:last(re:split(CSVPath,"/",[{return,list}])),
  {RouterHost,RouterPort} = maps:get(mainServer,PortMap),
%%  send an ACK to mainserver that the CSV file is ready
  io:format("source updated Workers - ~p~n",[Workers]),
  http_request(RouterHost,RouterPort,"csvReady",atom_to_list(Myname)),
   {next_state, idle, State#source_statem_state{csvName = CSVName, castingTo = Workers, msgCounter = Counter+1,csvList =CSVlist}};


%%This cast spawns a transmitter of data stream towards NerlClient by casting batches of data from parsed csv file given by cowboy source_server
idle(cast, {startCasting}, State = #source_statem_state{chunkSize = ChunkSize, sourcePid = [],workersMap = WorkersMap, castingTo = CastingTo, portMap = PortMap, msgCounter = Counter, csvName = CSVName, csvList =CSVlist}) ->
  io:format("start casting to: ~p~n",[CastingTo]),
  Transmitter =  spawnTransmitter(CastingTo,CSVName,CSVlist,PortMap,WorkersMap,ChunkSize) ,
  {next_state, castingData, State#source_statem_state{msgCounter = Counter+1, sourcePid = Transmitter}};

idle(cast, {startCasting}, State = #source_statem_state{msgCounter = Counter}) ->
  io:format("im not suppose to be here"),
  {next_state, castingData, State#source_statem_state{msgCounter = Counter+1}};



idle(cast, {stopCasting}, State = #source_statem_state{msgCounter = Counter}) ->
  io:format("already idle~n",[]),
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
  io:format("received leftovers- ~p~n",[Tail]),
  {next_state, idle, State#source_statem_state{msgCounter = Counter+1,csvList = Tail}};

castingData(cast, {finishedCasting}, State = #source_statem_state{myName = MyName, msgCounter = Counter, portMap = PortMap}) ->
  io:format("source finished casting- ~n",[]),
  {RouterHost,RouterPort} = maps:get(mainServer,PortMap),
%%  send an ACK to mainserver that the CSV file is ready
  http_request(RouterHost,RouterPort,"sourceDone", atom_to_list(MyName)),
  {next_state, idle, State#source_statem_state{msgCounter = Counter+1,sourcePid = [], csvList = []}};

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

spawnTransmitter(WorkersNames,CSVPath,CSVlist,PortMap,WorkersMap,ChunkSize)->
%%  ListOfWorkers = re:split(WorkersNames,",", [{return, list}]),
  Triplets =getHostPort(WorkersNames,WorkersMap,PortMap,[]),
%%  io:format("~p~n",[Triplets]),
  %%[list of binarys from CSV file, Size of batch, 1/Hz (in milisecond), statem pid]
  spawn(?MODULE,sendSamples,[CSVlist,CSVPath,ChunkSize,60,self(),Triplets,0]).


sendSamples([],_CSVPath,_ChunkSize,Hz,Pid,_Triplets,_Counter)->
  receive
    after Hz ->    gen_statem:cast(Pid,{finishedCasting})
end;

sendSamples([Head|ListOfSamples],CSVPath,ChunkSize,Hz,Pid,Triplets,Counter)->
          %%this http request will be splitted at client's state machine by the following order:
          %%    Body:   ClientName#WorkerName#CSVName#BatchNumber#BatchOfSamples
        [http_request(RouterHost, RouterPort,"weightsVector",
                    list_to_binary([list_to_binary([list_to_binary(atom_to_list(ClientName)),<<"#">>,list_to_binary(WorkerName),<<"#">>,list_to_binary(CSVPath),<<"#">>,list_to_binary(integer_to_list(Counter)),<<"#">>]),list_to_binary(Head)]))
                  || {ClientName,WorkerName,RouterHost,RouterPort}<-Triplets],
          receive
              %%main server might ask to stop casting,update source state with remaining lines. if no stop message received, continue casting after 1/Hz
            {stopCasting}  ->
              io:format("source stop casting",[]),
              gen_statem:cast(Pid,{leftOvers,[]})
           after Hz-> sendSamples(ListOfSamples,CSVPath,ChunkSize,Hz,Pid,Triplets,Counter+1)
          end.


start_connection([])->ok;
start_connection([{_ServerName,{Host, Port}}|Tail]) ->
  httpc:set_options([{proxy, {{Host, Port},[Host]}}]),
  start_connection(Tail).


http_request(Host, Port,Path, Body)->
  URL = "http://" ++ Host ++ ":"++integer_to_list(Port) ++ "/" ++ Path,
  httpc:set_options([{proxy, {{Host, Port},[Host]}}]),
  httpc:request(post,{URL, [],"application/x-www-form-urlencoded",Body}, [], []).

getHostPort([],_WorkersMap,_PortMap,Ret)-> Ret;
getHostPort([WorkerName|WorkersNames],WorkersMap,PortMap,Ret)->
  ClientName = maps:get(list_to_atom(WorkerName),WorkersMap),
  {RouterHost,RouterPort} = maps:get(ClientName,PortMap),
  getHostPort(WorkersNames,WorkersMap, PortMap,Ret++[{ClientName,WorkerName,RouterHost,RouterPort}]).


%%sendSamples([],_ChunkSize,_Hz,Pid,_Triplets)->gen_statem:cast(Pid,{finishedCasting});
%%sendSamples([Head|ListOfSamples],ChunkSize,Hz,Pid,Triplets)->
%%  [http_request(RouterHost, RouterPort,"weightsVector", list_to_binary([list_to_binary([list_to_binary(atom_to_list(ClientName)),<<"#">>,list_to_binary(WorkerName),<<"#">>]),Head]))|| {ClientName,WorkerName,RouterHost,RouterPort}<-Triplets],
%%
%%%%  NumOfSamples = length(ListOfSamples),
%%%%  if
%%%%      (NumOfSamples=<ChunkSize) ->
%%%%        BinaryHead = [list_to_binary(X)||X<-ListOfSamples],
%%%%%%                io:format("last chank : ~p~n",[BinaryHead]),
%%%%
%%%%%%        Last =  binary_to_list(lists:last(BinaryHead1)),
%%%%%%
%%%%%%        BinaryHead2 = lists:droplast(BinaryHead1),
%%%%%%        BinaryHead =BinaryHead2++[list_to_binary(lists:sublist(Last,1,length(Last)-1))],
%%%%%%        io:format("last chank after: ~p~n",[BinaryHead]),
%%%%%%        BinaryHead = list_to_binary(lists:flatten(ListOfSamples)),
%%%%        %%        ToSend = binary:part(BinaryHead, {byte_size(BinaryHead), -1}),
%%%%
%%%%        [http_request(RouterHost, RouterPort,"weightsVector", list_to_binary([list_to_binary([list_to_binary(atom_to_list(ClientName)),<<"#">>,list_to_binary(WorkerName),<<"#">>]),BinaryHead]))|| {ClientName,WorkerName,RouterHost,RouterPort}<-Triplets],
%%%%        gen_statem:cast(Pid,{finishedCasting});
%%%%      true ->
%%%%          {Head, Tail} = lists:split(ChunkSize,ListOfSamples),
%%%%%%        BinaryHead = [list_to_binary(X++[","])||X<-Head],
%%%%        BinaryHead = [list_to_binary(X)||X<-Head],
%%%%%%        io:format("a chank : ~p~n",[BinaryHead]),
%%%%
%%%%%%        io:format("BinaryHead: ~p~n",[BinaryHead]),
%%%%%%        BinaryHead = list_to_binary(lists:flatten(Head)),
%%%%        %%        ToSend = binary:part(BinaryHead, {byte_size(BinaryHead), -1}),
%%%%
%%%%        [http_request(RouterHost, RouterPort,"weightsVector", list_to_binary([list_to_binary([list_to_binary(atom_to_list(ClientName)),<<"#">>,list_to_binary(WorkerName),<<"#">>]),BinaryHead]))|| {ClientName,WorkerName,RouterHost,RouterPort}<-Triplets],
%%  receive
%%  %%main server might ask to stop casting,update source state with remaining lines. if no stop message received, continue casting after 1/Hz
%%    {stopCasting}  ->
%%      io:format("source stop casting",[]),
%%      gen_statem:cast(Pid,{leftOvers,[]})
%%  after Hz-> sendSamples(ListOfSamples,ChunkSize,Hz,Pid,Triplets)
%%  end.

%%  end.