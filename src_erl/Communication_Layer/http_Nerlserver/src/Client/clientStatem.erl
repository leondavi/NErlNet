%%%-------------------------------------------------------------------
%%% @author kapelnik
%%% @copyright (C) 2021, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 14. Apr 2021 9:57 AM
%%%-------------------------------------------------------------------
-module(clientStatem).
-author("kapelnik").

-behaviour(gen_statem).

%%-import('nerlNetStatem', []).
%%-import('../../erlBridge/nerlNetStatem', []).

%% API
-export([start_link/1, predict/3]).

%% gen_statem callbacks
-export([init/1, format_status/2, state_name/3, handle_event/4, terminate/3,
  code_change/4, callback_mode/0, idle/3, training/3]).

-define(SERVER, ?MODULE).

%   myName - Client Name,
%   federatedServer - fed.server name,
%   workersMap - this clients workers on this machine,
%   portMap, all connections needed for this client
%   msgCounter - gather messages statistics
%   timingMap - gather Timing statistics: timingMap = #{{WorkerName1=>{LastBatchReceivedTime,totalBatches,AverageTrainingime},{Worker2,..}, ...}
-record(client_statem_state, {myName, federatedServer,workersMap, portMap, msgCounter,timingMap}).


%%%===================================================================
%%% API
%%%===================================================================

%% @doc Creates a gen_statem process which calls Module:init/1 to
%% initialize. To ensure a synchronized start-up procedure, this
%% function does not return until Module:init/1 has returned.

%%Arguments from Cowboy Server
%%return gen_statem's Pid to Cowboy Server
%%Client_StateM_Args= {self(),RouterPort},
start_link(Args) ->
    {ok,Pid} = gen_statem:start_link(?MODULE, Args, []),
    Pid.

%%%===================================================================
%%% gen_statem callbacks
%%%===================================================================

%% @private
%% @doc Whenever a gen_statem is started using gen_statem:start/[3,4] or
%% gen_statem:start_link/[3,4], this function is called by the new
%% process to initialize.
%%initialize and go to state - idle


%%NerlClientsArgs=[{MyName,Workers,ConnectionsMap},...], Workers = list of maps of name and args
%%  init nerlClient with given workers and parameters, and build a map :#{workerName=>WorkerPid,...}
init({MyName,Federated,Workers,ConnectionsMap}) ->
  inets:start(),
  start_connection(maps:to_list(ConnectionsMap)),
  io:format("loading niff~n",[]),
  %niftest:init(),
  io:format(" niff loaded~n",[]),

  % WorkersPids = createWorkers(Workers,self(),[]),

%%  [{WorkerName,nerlNetStatem:start_link({self(), WorkerName, CppSANNArgs})}||{WorkerName,CppSANNArgs}<-maps:to_list(Workers)],
  {WorkersMap,TimingMap} = createWorkers(Workers,586000901,self(),#{},#{}),
  io:format("TimingMap~p~n",[maps:to_list(TimingMap)]),
%  niftest:trainNifTest(10.0),

  {ok, idle, #client_statem_state{myName= MyName,timingMap = TimingMap, federatedServer = Federated, workersMap = WorkersMap, portMap = ConnectionsMap, msgCounter = 1}}.

createWorkers([],_WorkerModelID,_ClientPid,WorkersNamesPidsMap,TimingMap) ->{WorkersNamesPidsMap,TimingMap};
createWorkers([Worker|Workers],WorkerModelID,ClientPid,WorkersNamesPidsMap,TimingMap) ->
  %  FederatedMode="1", CountLimit="10",
    WorkerName = list_to_atom(binary_to_list(maps:get(<<"name">>,Worker))),
  ModelId = WorkerModelID,
  ModelType = list_to_integer(binary_to_list(maps:get(<<"modelType">>,Worker))),
  ScalingMethod = list_to_integer(binary_to_list(maps:get(<<"scalingMethod">>,Worker))),

  LayerTypesList = string_to_list_int(maps:get(<<"layerTypesList">>,Worker)),
  LayersSizes = string_to_list_int(maps:get(<<"layersSizes">>,Worker)),
  LayersActivationFunctions = string_to_list_int(maps:get(<<"layersActivationFunctions">>,Worker)),

  FederatedMode = list_to_integer(binary_to_list(maps:get(<<"federatedMode">>,Worker))),
  CountLimit = list_to_integer(binary_to_list(maps:get(<<"countLimit">>,Worker))),
  Optimizer = list_to_integer(binary_to_list(maps:get(<<"optimizer">>,Worker))),
  Features = list_to_integer(binary_to_list(maps:get(<<"features">>,Worker))),
  Labels = list_to_integer(binary_to_list(maps:get(<<"labels">>,Worker))),
  LossMethod = list_to_integer(binary_to_list(maps:get(<<"lossMethod">>,Worker))),
  LearningRate = float(list_to_integer(binary_to_list(maps:get(<<"learningRate">>,Worker)))),

  WorkerArgs = {WorkerName,ModelId,ModelType,ScalingMethod
                , LayerTypesList, LayersSizes
                , LayersActivationFunctions, FederatedMode
                , CountLimit, Optimizer, Features, Labels, LossMethod, LearningRate,  self() },
  WorkerPid = nerlNetStatem:start_link(WorkerArgs),
  % timingMap = #{{WorkerName1=>{LastBatchReceivedTime,totalBatches,AverageTrainingime},{Worker2,..}, ...}
  createWorkers(Workers,WorkerModelID+1,ClientPid,maps:put(WorkerName, WorkerPid,WorkersNamesPidsMap),maps:put(WorkerName,{0,0,0},TimingMap)).

%%return list of integer from string of lists of strings - "[2,2,2]" -> [2,2,2]
string_to_list_int(Binary) ->
  String = binary_to_list(Binary),
  NoParenthesis = lists:sublist(String,2,length(String)-2),
  Splitted = re:split(NoParenthesis,",",[{return,list}]),
  [list_to_integer(X)||X<-Splitted].
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
state_name(_EventType, _EventContent, State = #client_statem_state{}) ->
  NextStateName = next_state,
  {next_state, NextStateName, State}.



%%initiating nerlnet, given parameters in Body received by Cowboy init_handler
idle(cast, {init,CONFIG}, State = #client_statem_state{msgCounter = Counter}) ->
  % io:format("initiating, CONFIG received:~p ~n",[CONFIG]),
  {next_state, idle, State#client_statem_state{msgCounter = Counter+1}};

idle(cast, {statistics}, State = #client_statem_state{ myName = MyName,msgCounter = Counter,portMap = PortMap}) ->
  {RouterHost,RouterPort} = maps:get(mainServer,PortMap),
  http_request(RouterHost,RouterPort,"statistics", list_to_binary(atom_to_list(MyName)++"#"++integer_to_list(Counter))),

  {next_state, idle, State#client_statem_state{msgCounter = Counter+1}};

idle(cast, {training}, State = #client_statem_state{workersMap = WorkersMap, myName = MyName,msgCounter = Counter,portMap = PortMap}) ->
  Workers = maps:to_list(WorkersMap),
  [gen_statem:cast(WorkerPid,{training})|| {_WorkerName,WorkerPid}<-Workers],
%%  io:format("sending ACK   ~n",[]),
%%  {RouterHost,RouterPort} = maps:get(mainServer,PortMap),
%%  send an ACK to mainserver that the CSV file is ready
  ack(MyName,PortMap),
  {next_state, training, State#client_statem_state{msgCounter = Counter+1}};

idle(cast, {predict}, State = #client_statem_state{workersMap = WorkersMap,myName = MyName,msgCounter = Counter,portMap = PortMap}) ->
  io:format("client going to state predict",[]),
  Workers = maps:to_list(WorkersMap),
  [gen_statem:cast(WorkerPid,{predict})|| {_WorkerName,WorkerPid}<-Workers],
  %%  send an ACK to mainserver that the CSV file is ready
  ack(MyName,PortMap),
  {next_state, predict, State#client_statem_state{msgCounter = Counter+1}};

idle(cast, _EventContent, State = #client_statem_state{msgCounter = Counter}) ->
  %io:format("client training ignored:  ~p ~n",[EventContent]),
  {next_state, training, State#client_statem_state{msgCounter = Counter+1}}.


training(cast, {sample,[]}, State = #client_statem_state{msgCounter = Counter}) ->

  io:format("client got empty Vector~n",[]),

  {next_state, training, State#client_statem_state{msgCounter = Counter+1}};

training(cast, {sample,Body}, State = #client_statem_state{msgCounter = Counter,workersMap = WorkersMap,timingMap = TimingMap}) ->
  %%    Body:   {ClientName,WorkerName,CSVName,BatchNumber,BatchOfSamples}
  {_ClientName, WorkerName, _CSVName, _BatchNumber, BatchOfSamples} = binary_to_term(Body),
  ToSend =  decodeList(BatchOfSamples),
  Start = os:timestamp(),
  {_LastBatchReceivedTime,TotalBatches,AverageTrainingime} = maps:get(list_to_atom(WorkerName),TimingMap),
  NewTimingMap = maps:put(list_to_atom(WorkerName),{Start,TotalBatches+1,AverageTrainingime},TimingMap),
  WorkerPid = maps:get(list_to_atom(WorkerName),WorkersMap),
  gen_statem:cast(WorkerPid, {sample,ToSend}),
  {next_state, training, State#client_statem_state{msgCounter = Counter+1,timingMap = NewTimingMap}};

training(cast, {idle}, State = #client_statem_state{workersMap = WorkersMap,msgCounter = Counter}) ->
  io:format("client going to state idle",[]),
  Workers = maps:to_list(WorkersMap),
  [gen_statem:cast(WorkerPid,{idle})|| {_WorkerName,WorkerPid}<-Workers],
  {next_state, idle, State#client_statem_state{msgCounter = Counter+1}};

training(cast, {predict}, State = #client_statem_state{workersMap = WorkersMap,myName = MyName, portMap = PortMap, msgCounter = Counter}) ->
  io:format("client going to state predict",[]),
  Workers = maps:to_list(WorkersMap),
  [gen_statem:cast(WorkerPid,{predict})|| {_WorkerName,WorkerPid}<-Workers],
  ack(MyName,PortMap),
  {next_state, predict, State#client_statem_state{msgCounter = Counter+1}};

training(cast, {loss,WorkerName,nan}, State = #client_statem_state{myName = MyName,portMap = PortMap,  msgCounter = Counter}) ->
%%   io:format("LossFunction1: ~p   ~n",[LossFunction]),
  {RouterHost,RouterPort} = maps:get(mainServer,PortMap),
  http_request(RouterHost,RouterPort,"lossFunction", term_to_binary({WorkerName,"nan"})),
%%  http_request(RouterHost,RouterPort,"lossFunction", list_to_binary([list_to_binary(atom_to_list(WorkerName)),<<"#">>,<<"nan">>])),
  {next_state, training, State#client_statem_state{msgCounter = Counter+1}};

training(cast, {loss,WorkerName,LossFunction}, State = #client_statem_state{myName = MyName,portMap = PortMap,  msgCounter = Counter,timingMap = TimingMap}) ->
  % Start = maps:get(WorkerName,TimingMap),
  {Start,TotalBatches,AverageTrainingTime} = maps:get(WorkerName,TimingMap),
  Finish = os:timestamp(),
  TotalTrainingTime = (timer:now_diff(Finish, Start) / 1000),
  if(TotalBatches>0) ->
    NewAverage = ((AverageTrainingTime*(TotalBatches-1))+TotalTrainingTime)/TotalBatches,
    NewTimingMap = maps:put(WorkerName,{Start,TotalBatches,NewAverage},TimingMap);
      % io:format("AverageTrainingTime: ~p~n",[NewAverage])
    true ->       NewTimingMap = maps:put(WorkerName,{Start,TotalBatches,TotalTrainingTime},TimingMap)
    end,
   io:format("WorkerName: ~p , LossFunction1: ~p,  ~n",[WorkerName, LossFunction]),
  {RouterHost,RouterPort} = maps:get(mainServer,PortMap),
  http_request(RouterHost,RouterPort,"lossFunction", term_to_binary({WorkerName,LossFunction})),
  {next_state, training, State#client_statem_state{msgCounter = Counter+1,timingMap = NewTimingMap}};



%%Federated Mode:
training(cast, {loss,federated_weights, Worker, LOSS_FUNC, Ret_weights}, State = #client_statem_state{federatedServer = Federated,myName = MyName,portMap = PortMap,  msgCounter = Counter}) ->
%%  io:format("Worker: ~p~n, LossFunction: ~p~n,  Ret_weights_tuple: ~p~n",[Worker, LOSS_FUNC, Ret_weights_tuple]),
  {RouterHost,RouterPort} = maps:get(Federated,PortMap),

  ToSend = term_to_binary({Federated,encodeListOfLists(Ret_weights)}),

  http_request(RouterHost,RouterPort,"federatedWeightsVector", ToSend),
  {next_state, training, State#client_statem_state{msgCounter = Counter+1}};

training(cast, {loss, federated_weights, MyName, LOSS_FUNC}, State = #client_statem_state{myName = MyName,portMap = PortMap,  msgCounter = Counter}) ->
  {next_state, training, State#client_statem_state{msgCounter = Counter+1}};

training(cast, {federatedAverageWeights,Body}, State = #client_statem_state{myName = MyName,portMap = PortMap,workersMap = WorkersMap, msgCounter = Counter}) ->
%% io:format("federatedAverageWeights Body!!!!: ~p~n",[Body]),
  {_ClientName,WorkerName,BinaryWeights} = binary_to_term(Body),

%%  [_ClientName,WorkerName,Weights] = re:split(binary_to_list(Body),"#",[{return,list}]),
  WorkerPid = maps:get(WorkerName,WorkersMap),
  % io:format("client decoding weights!!!:   ~n!!!",[]),

  DecodedWeights = decodeListOfLists(BinaryWeights),
  % io:format("client finished decoding weights!!!:   ~n!!!",[]),
  gen_statem:cast(WorkerPid, {set_weights,  DecodedWeights}),
  {next_state, training, State#client_statem_state{msgCounter = Counter+1}};

training(cast, EventContent, State = #client_statem_state{msgCounter = Counter}) ->
  io:format("client training ignored!!!:  ~p ~n!!!",[EventContent]),
  {next_state, training, State#client_statem_state{msgCounter = Counter+1}}.


predict(cast, {sample,Body}, State = #client_statem_state{msgCounter = Counter,workersMap = WorkersMap, timingMap = TimingMap}) ->
  %%    Body:   ClientName#WorkerName#CSVName#BatchNumber#BatchOfSamples
  {_ClientName, WorkerName, CSVName, BatchNumber, BatchOfSamples} = binary_to_term(Body),
  ToSend =  decodeList(BatchOfSamples),
  Start = os:timestamp(),
  {_LastBatchReceivedTime,TotalBatches,AverageTrainingime} = maps:get(list_to_atom(WorkerName),TimingMap),
  NewTimingMap = maps:put(list_to_atom(WorkerName),{Start,TotalBatches+1,AverageTrainingime},TimingMap),
%%  io:format("CSVName: ~p, BatchNumber: ~p~n",[CSVName,BatchNumber]),
%%  io:format("Vector: ~p~n",[ToSend]),
  WorkerPid = maps:get(list_to_atom(WorkerName),WorkersMap),
  gen_statem:cast(WorkerPid, {sample,CSVName, BatchNumber,ToSend}),
%%  gen_statem:cast(WorkerPid, {sample,ToSend}),
  {next_state, predict, State#client_statem_state{msgCounter = Counter+1,timingMap = NewTimingMap}};

predict(cast, {predictRes,WorkerName,InputName,ResultID,[]}, State = #client_statem_state{msgCounter = Counter,portMap = PortMap}) ->
  {RouterHost,RouterPort} = maps:get(mainServer,PortMap),
  http_request(RouterHost,RouterPort,"predictRes", term_to_binary({InputName,ResultID,""})),
  {next_state, predict, State#client_statem_state{msgCounter = Counter+1}};

predict(cast, {predictRes,WorkerName,InputName,ResultID,Result}, State = #client_statem_state{msgCounter = Counter,portMap = PortMap,timingMap = TimingMap}) ->
    {Start,TotalBatches,AverageTrainingTime} = maps:get(WorkerName,TimingMap),
  Finish = os:timestamp(),
  TotalTrainingTime = (timer:now_diff(Finish, Start) / 1000),
  if(TotalBatches>0) ->
    NewAverage = ((AverageTrainingTime*(TotalBatches-1))+TotalTrainingTime)/TotalBatches,
    NewTimingMap = maps:put(WorkerName,{Start,TotalBatches,NewAverage},TimingMap);
      % io:format("AverageTrainingTime: ~p~n",[NewAverage])
    true ->       NewTimingMap = maps:put(WorkerName,{Start,TotalBatches,TotalTrainingTime},TimingMap)
    end,  
  {RouterHost,RouterPort} = maps:get(mainServer,PortMap),
  Result2 = lists:flatten(io_lib:format("~w",[Result])),",",[{return,list}],
  Result3 = lists:sublist(Result2,2,length(Result2)-2),
  % io:format("Client got result from predict-~nInputName: ~p,ResultID: ~p, ~nResult:~p~n",[InputName,ResultID,Result]),
  http_request(RouterHost,RouterPort,"predictRes", term_to_binary({InputName,ResultID,Result3})),
  {next_state, predict, State#client_statem_state{msgCounter = Counter+1}};

predict(cast, {training}, State = #client_statem_state{workersMap = WorkersMap,myName = MyName,portMap = PortMap,msgCounter = Counter}) ->
  Workers = maps:to_list(WorkersMap),
  [gen_statem:cast(WorkerPid,{training})|| {_WorkerName,WorkerPid}<-Workers],
  ack(MyName,PortMap),
  {next_state, training, State#client_statem_state{msgCounter = Counter+1}};

predict(cast, {idle}, State = #client_statem_state{workersMap = WorkersMap,msgCounter = Counter}) ->
  Workers = maps:to_list(WorkersMap),
  [gen_statem:cast(WorkerPid,{idle})|| {_WorkerName,WorkerPid}<-Workers],
  io:format("client going to state idle~n",[]),
  {next_state, idle, State#client_statem_state{msgCounter = Counter+1}};

predict(cast, EventContent, State = #client_statem_state{msgCounter = Counter}) ->
  io:format("client predict ignored:  ~p ~n",[EventContent]),
  {next_state, predict, State#client_statem_state{msgCounter = Counter+1}}.


%% @private
%% @doc If callback_mode is handle_event_function, then whenever a
%% gen_statem receives an event from call/2,  cast/2, or as a normal
%% process message, this function is called.
handle_event(_EventType, _EventContent, _StateName, State = #client_statem_state{}) ->
  NextStateName = the_next_state_name,
  {next_state, NextStateName, State}.

%% @private
%% @doc This function is called by a gen_statem when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_statem terminates with
%% Reason. The return value is ignored.
terminate(_Reason, _StateName, _State = #client_statem_state{}) ->
  ok.

%% @private
%% @doc Convert process state when code is changed
code_change(_OldVsn, StateName, State = #client_statem_state{}, _Extra) ->
  {ok, StateName, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
start_connection([])->ok;
start_connection([{_ServerName,{Host, Port}}|Tail]) ->
  httpc:set_options([{proxy, {{Host, Port},[Host]}}]),
  start_connection(Tail).

http_request(Host, Port,Path, Body)->
%%  io:format("sending body ~p to path ~p to hostport:~p~n",[Body,Path,{Host,Port}]),
  URL = "http://" ++ Host ++ ":"++integer_to_list(Port) ++ "/" ++ Path,
  httpc:set_options([{proxy, {{Host, Port},[Host]}}]),
  httpc:request(post,{URL, [],"application/x-www-form-urlencoded",Body}, [], []).


ack(MyName, PortMap) ->
    io:format("~p sending ACK   ~n",[MyName]),
  {RouterHost,RouterPort} = maps:get(mainServer,PortMap),
%%  send an ACK to mainserver that the CSV file is ready
  http_request(RouterHost,RouterPort,"clientReady",atom_to_list(MyName)).




%%This encoder receives a lists of lists: [[1.0,1.1,11.2],[2.0,2.1,22.2]] and returns a binary
encodeListOfLists(L)->encodeListOfLists(L,[]).
encodeListOfLists([],Ret)->term_to_binary(Ret);
encodeListOfLists([H|T],Ret)->encodeListOfLists(T,Ret++[encodeFloatsList(H)]).
encodeFloatsList(ListOfFloats)->
  ListOfBinaries = [<<X:64/float>>||X<-ListOfFloats],
  list_to_binary(ListOfBinaries).

%%This decoder receives a binary <<131,108,0,0,0,2,106...>> and returns a lists of lists: [[1.0,1.1,11.2],[2.0,2.1,22.2]]
decodeListOfLists(L)->decodeListOfLists(binary_to_term(L),[]).
decodeListOfLists([],Ret)->Ret;
decodeListOfLists([H|T],Ret)->decodeListOfLists(T,Ret++[decodeList(H)]).
decodeList(Binary)->  decodeList(Binary,[]).
decodeList(<<>>,L) -> L;
decodeList(<<A:64/float,Rest/binary>>,L) -> decodeList(Rest,L++[A]).

