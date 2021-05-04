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
  code_change/4, callback_mode/0, idle/3, casting_data/3, sendSamples/7]).

-define(SERVER, ?MODULE).

-record(source_statem_state, {castingTo, myName,portMap, msgCounter, sourcePids,csvList, num_of_features, num_of_labels}).

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
init({MyName,ConnectionsMap}) ->
  inets:start(),
  start_connection(maps:to_list(ConnectionsMap)),
  {ok, idle, #source_statem_state{myName = MyName, portMap = ConnectionsMap, msgCounter = 1, castingTo = []}}.

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
idle(cast, {csvList,ClientName,CSVlist}, State = #source_statem_state{castingTo = CastingTo, myName = Myname, msgCounter = Counter, portMap = PortMap}) ->
  {RouterHost,RouterPort} = maps:get(mainServer,PortMap),
%%  send an ACK to mainserver that the CSV file is ready
  http_request(RouterHost,RouterPort,"csvReady",atom_to_list(Myname)),
   {next_state, idle, State#source_statem_state{castingTo = CastingTo++[ClientName], msgCounter = Counter+1,csvList =CSVlist}};


%%This cast spawns a transmitter of data stream towards NerlClient by casting batches of data from parsed csv file given by cowboy source_server
idle(cast, {start_training}, State = #source_statem_state{castingTo = CastingTo, portMap = PortMap, msgCounter = Counter, csvList =CSVlist}) ->
  Transmitters = [ spawnTransmitters(ClientName,CSVlist,PortMap)  ||ClientName<-CastingTo],
  {next_state, casting_data, State#source_statem_state{msgCounter = Counter+1, sourcePids = Transmitters}};




idle(cast, {stop_training}, State = #source_statem_state{msgCounter = Counter}) ->
  io:format("already idle~n",[]),
  {next_state, idle, State#source_statem_state{msgCounter = Counter+1}};


idle(cast, EventContent, State = #source_statem_state{msgCounter = Counter}) ->
  io:format("ignored: ~p~nstate - idle",[EventContent]),
  {next_state, idle, State#source_statem_state{msgCounter = Counter+1}}.

%%waiting for ether data list of sample been sent to finish OR stop message from main server.
casting_data(cast, {stop_training,ClientName}, State = #source_statem_state{msgCounter = Counter,sourcePids = SourcePids}) ->
  SourcePids ! {stop_casting,ClientName},
  {next_state, idle, State#source_statem_state{msgCounter = Counter+1,sourcePids = SourcePids}};

casting_data(cast, {start_training}, State = #source_statem_state{msgCounter = Counter}) ->
  io:format("aleready casting~n",[]),
  {next_state, casting_data, State#source_statem_state{msgCounter = Counter+1}};

casting_data(cast, EventContent, State = #source_statem_state{msgCounter = Counter}) ->
  io:format("ignored: ~p~nstate - casting data",[EventContent]),
  {next_state, casting_data, State#source_statem_state{msgCounter = Counter+1}}.


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

spawnTransmitters(ClientName,CSVlist,PortMap)->
  {RouterHost,RouterPort} = maps:get(list_to_atom(ClientName),PortMap),
  %%[list of binarys from CSV file, Size of batch, 1/Hz, statem pid]
  spawn(?MODULE,sendSamples,[CSVlist,10,20,self(),RouterHost,RouterPort,ClientName]).


sendSamples([],_Batch_Size,_Hz,_Pid,_RouterHost,_RouterPort,_Name)->done;
sendSamples(ListOfSamples,Batch_Size,Hz,Pid,RouterHost,RouterPort,Name)->

  NumOfSamples = length(ListOfSamples),
  if
      (NumOfSamples=<Batch_Size) ->
        BinaryHead = [list_to_binary(X++[","])||X<-ListOfSamples],
        http_request(RouterHost, RouterPort,"weights_vector", list_to_binary([list_to_binary([Name,<<"#">>]),BinaryHead]));
      true ->
          {Head, Tail} = lists:split(Batch_Size,ListOfSamples),
        BinaryHead = [list_to_binary(X++[","])||X<-Head],
        http_request(RouterHost, RouterPort,"weights_vector", list_to_binary([list_to_binary([Name,<<"#">>]),BinaryHead])),
          receive
              %%main server might ask to stop casting,update source state with remaining lines. if no stop message received, continue casting after 1/Hz
            {stop_casting,_ClientName}  ->  gen_statem:cast(Pid,{csvList,Tail})
           after Hz-> sendSamples(Tail,Batch_Size,Hz,Pid,RouterHost,RouterPort,Name)
          end

  end.


start_connection([])->ok;
start_connection([{_ServerName,{Host, Port}}|Tail]) ->
  httpc:set_options([{proxy, {{Host, Port},[Host]}}]),
  start_connection(Tail).


http_request(Host, Port,Path, Body)->
  httpc:request(post,{"http://" ++ Host ++ ":"++integer_to_list(Port) ++ "/" ++ Path, [],"application/x-www-form-urlencoded",Body}, [], []).

