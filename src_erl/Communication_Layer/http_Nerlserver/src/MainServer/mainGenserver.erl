%%%-------------------------------------------------------------------
%%% @author kapelnik
%%% @copyright (C) 2021, Nerlnet
%%% @doc
%%%
%%% @end
%%% Created : 27. Apr 2021 3:35 AM
%%%-------------------------------------------------------------------
-module(mainGenserver).
-author("kapelnik").

-behaviour(gen_server).
-include("../nerl_tools.hrl").

%% API
-export([start_link/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
  code_change/3]).

-define(SERVER, ?MODULE).


-record(main_genserver_state, {statisticsCounter = 0, myName, state, workersMap, clients, nerlnetGraph, sourcesCastingList = [], sourcesWaitingList = [], clientsWaitingList = [], statisticsMap, msgCounter = 0, batchSize}).

%%%===============================================================
%%% API
%%%===================================================================

%% @doc Spawns the server and registers the local name (unique)
-spec(start_link(args) ->
  {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link(Args) ->
  {ok,Gen_Server_Pid} = gen_server:start_link({local, ?SERVER}, ?MODULE, Args, []),
  Gen_Server_Pid.


%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%% @private
%% @doc Initializes the server
-spec(init(Args :: term()) ->
{ok, State :: #main_genserver_state{}} | {ok, State :: #main_genserver_state{}, timeout() | hibernate} |
{stop, Reason :: term()} | ignore).

init({MyName,Clients,BatchSize,WorkersMap,NerlnetGraph}) ->
  nerl_tools:setup_logger(?MODULE),
  inets:start(),
  MyNameStr = atom_to_list(MyName),
  ?MAIN_SERVER_ATOM = MyName, % must be identical
  ?LOG_NOTICE("Main Server starts"),
  ConnectedEntities = [digraph:vertex(NerlnetGraph,Vertex) || Vertex <- digraph:out_neighbours(NerlnetGraph,?MAIN_SERVER_ATOM)],
  ?LOG_NOTICE("Main Server is connected to: ~p~n",[ConnectedEntities]),
  put(nerlnetGraph, NerlnetGraph),
    % nerl_tools:start_connection([digraph:vertex(NerlnetGraph,Vertex) || Vertex <- digraph:out_neighbours(NerlnetGraph,MyName)]),
  
  NewStatisticsMap = getNewStatisticsMap([digraph:vertex(NerlnetGraph,Vertex) || Vertex <- digraph:vertices(NerlnetGraph)--?LIST_OF_SPECIAL_SERVERS]),
  % io:format("New StatisticsMap = ~p~n",[NewStatisticsMap]),
  {ok, #main_genserver_state{myName = MyNameStr, workersMap = WorkersMap, batchSize = BatchSize, state=idle, clients = Clients, nerlnetGraph = NerlnetGraph, msgCounter = 1,statisticsMap = NewStatisticsMap}}.

%% @private
%% @doc Handling call messages
-spec(handle_call(Request :: term(), From :: {pid(), Tag :: term()},
    State :: #main_genserver_state{}) ->
{reply, Reply :: term(), NewState :: #main_genserver_state{}} |
{reply, Reply :: term(), NewState :: #main_genserver_state{}, timeout() | hibernate} |
{noreply, NewState :: #main_genserver_state{}} |
{noreply, NewState :: #main_genserver_state{}, timeout() | hibernate} |
{stop, Reason :: term(), Reply :: term(), NewState :: #main_genserver_state{}} |
{stop, Reason :: term(), NewState :: #main_genserver_state{}}).

%% respond to GUI req
handle_call(getGraph, _From, State) ->
  NerlGraph = State#main_genserver_state.nerlnetGraph,
  FullNodes = [digraph:vertex(NerlGraph,Vertex) || Vertex <- digraph:vertices(NerlGraph)],
  %io:format("Full graph is: ~p~n", [FullNodes]),
  NodesList = [Entity++","++IP++","++integer_to_list(Port)++"#"||{Entity, {IP, Port}} <- FullNodes],
  EdgesList = [digraph:edge(NerlGraph,Edge) || Edge <- digraph:edges(NerlGraph)],
  %io:format("graph edges are: ~p~n", [EdgesList]),
  Nodes = nodeString(NodesList),
  Edges = edgeString(EdgesList),

  MsgCounter = State#main_genserver_state.msgCounter,
  {reply, Nodes++Edges, State#main_genserver_state{msgCounter = MsgCounter+1}};

handle_call(getStats, _From, State) ->
  Mode = State#main_genserver_state.state,
  RecvCounter = State#main_genserver_state.msgCounter,
  Conn = digraph:out_degree(State#main_genserver_state.nerlnetGraph, ?MAIN_SERVER_ATOM),
  %io:format("returning stats: ~p~n", [{Mode, RecvCounter}]),
  Mes = "mode="++atom_to_list(Mode)++",stats="++integer_to_list(RecvCounter)++",conn="++integer_to_list(Conn),

  MsgCounter = State#main_genserver_state.msgCounter,
  {reply, Mes, State#main_genserver_state{msgCounter = MsgCounter+1}}.

nodeString([Node |NodeList]) -> nodeString(NodeList, Node).
nodeString([], Str) -> Str;
nodeString([Node |NodeList], Str)-> nodeString(NodeList, Node++Str).

edgeString([Edge |EdgesList])-> {_ID, V1, V2, _Label} = Edge, edgeString(EdgesList, V1++"-"++V2).
edgeString([], Str)-> Str;
edgeString([Edge |EdgesList], Str)->
  {_ID, V1, V2, _Label} = Edge,
  edgeString(EdgesList, V1++"-"++V2++","++Str).

%% @private
%% @doc Handling cast messages
-spec(handle_cast(Request :: term(), State :: #main_genserver_state{}) ->
{noreply, NewState :: #main_genserver_state{}} |
{noreply, NewState :: #main_genserver_state{}, timeout() | hibernate} |
{stop, Reason :: term(), NewState :: #main_genserver_state{}}).


handle_cast({initCSV, Source,SourceData}, State = #main_genserver_state{state = idle, myName = MyName, sourcesWaitingList = SourcesWaitingList,nerlnetGraph = NerlnetGraph,msgCounter = MsgCounter}) ->
%%  send router http request, to rout this message to all sensors

  findroutAndsend(MyName, Source,SourceData,NerlnetGraph),
  io:format("WaitingList = ~p~n",[SourcesWaitingList++[list_to_atom(Source)]]),
  {noreply, State#main_genserver_state{sourcesWaitingList = SourcesWaitingList++[list_to_atom(Source)],msgCounter = MsgCounter+1}};

handle_cast({clientsTraining,Body}, State = #main_genserver_state{state = casting,clients = ListOfClients,msgCounter = MsgCounter}) ->
%%  send router http request, to rout this message to all sensors
  io:format("already casting~n",[]),
  io:format("Body:~p~n",[Body]),
  {noreply, State#main_genserver_state{clientsWaitingList = ListOfClients,msgCounter = MsgCounter+1}};

handle_cast({clientsTraining, _Body}, State = #main_genserver_state{myName = MyName, clients = ListOfClients, nerlnetGraph = NerlnetGraph, msgCounter = MsgCounter}) ->
%%  send router http request, to rout this message to all sensors
  % io:format("main server: setting all clients on training state: ~p~n",[ListOfClients]),
%%  io:format("Body:~p~n",[Body]),
%%  io:format("binary_to_list(Body):~p~n",[binary_to_list(Body)]),
%%  io:format("Splitted-(Body):~p~n",[re:split(binary_to_list(Body), ",", [{return, list}])]),
%%  TODO find the router that can send this request to Sources**
  io:format("setting clients ~p to training~n",[ListOfClients]),
  [{setClientState(clientTraining,ClientName, NerlnetGraph,MyName)}|| ClientName <- ListOfClients],
  {noreply, State#main_genserver_state{clientsWaitingList = ListOfClients,msgCounter = MsgCounter+1}};

handle_cast({clientsPredict,_Body}, State = #main_genserver_state{state = casting, clients = ListOfClients,msgCounter = MsgCounter}) ->
%%  send router http request, to rout this message to all sensors
  io:format("already casting~n",[]),
  {noreply, State#main_genserver_state{clientsWaitingList = ListOfClients,msgCounter = MsgCounter+1}};

handle_cast({clientsPredict,_Body}, State = #main_genserver_state{myName = MyName,clients = ListOfClients, nerlnetGraph = NerlnetGraph,msgCounter = MsgCounter}) ->
%%  send router http request, to rout this message to all sensors
  % io:format("main server: setting all clients on clientsPredict state: ~p~n",[ListOfClients]),
%%  TODO find the router that can send this request to Sources**
  [{setClientState(clientPredict,ClientName, NerlnetGraph,MyName)}|| ClientName<- ListOfClients],
  {noreply, State#main_genserver_state{clientsWaitingList = ListOfClients,msgCounter = MsgCounter+1}};

handle_cast({clientsIdle}, State = #main_genserver_state{state = idle, myName = MyName, clients = ListOfClients, nerlnetGraph = NerlnetGraph,msgCounter = MsgCounter}) ->
%%  send router http request, to rout this message to all sensors
  % io:format("main server: setting all clients on Idle state: ~p~n",[ListOfClients]),
%%  TODO find the router that can send this request to Sources**
  [{setClientState(clientIdle,ClientName, NerlnetGraph, MyName)}|| ClientName<- ListOfClients],
  {noreply, State#main_genserver_state{clientsWaitingList = ListOfClients,msgCounter = MsgCounter+1}};

%%%get Statistics from all Entities in the network
handle_cast({statistics,Body}, State = #main_genserver_state{myName = MyName, statisticsCounter = StatisticsCounter, nerlnetGraph = NerlnetGraph,statisticsMap = StatisticsMap,msgCounter = MsgCounter}) ->

    if Body == <<"getStatistics">> ->   %% initial message from APIServer, get stats from entities

          NewStatisticsMap = getNewStatisticsMap([digraph:vertex(NerlnetGraph,Vertex) || Vertex <- digraph:vertices(NerlnetGraph)--?LIST_OF_SPECIAL_SERVERS]),
          [findroutAndsendStatistics(MyName, Name,NerlnetGraph)||{Name,_Counter}<-maps:to_list(StatisticsMap)],
          NewState = State#main_genserver_state{msgCounter = MsgCounter+1,statisticsMap = NewStatisticsMap, statisticsCounter = length(maps:to_list(StatisticsMap))};

      Body == <<>> ->  io:format("in Statistcs, State has: StatsCount=~p, MsgCount=~p~n", [StatisticsCounter, MsgCounter]), NewState = State;

      true ->
          %%      statistics arrived from Entity
          [From|[NewCounter]] = re:split(binary_to_list(Body), ":", [{return, list}]),

          NewStatisticsMap = maps:put(From,NewCounter,StatisticsMap),
          NewState = State#main_genserver_state{msgCounter = MsgCounter+1,statisticsMap = NewStatisticsMap,statisticsCounter = StatisticsCounter-1},

          if StatisticsCounter == 1 ->  %% got stats from all entities
              Statistics = maps:to_list(NewStatisticsMap),
              S = mapToString(Statistics,[]) ,
              ?LOG_NOTICE("Sending stats: ~p~n",[S]),
              {RouterHost,RouterPort} = nerl_tools:getShortPath(MyName,?API_SERVER_ATOM,NerlnetGraph),
              nerl_tools:http_request(RouterHost,RouterPort,"statistics", S ++ "|mainServer:" ++integer_to_list(MsgCounter));

          %% wait for more stats
          true -> pass end
      end,
    {noreply, NewState};

%%handle_cast({startPredicting}, State = #main_genserver_state{clients = ListOfClients, nerlnetGraph = NerlnetGraph}) ->
%%%%  send router http request, to rout this message to all sensors
%%  io:format("main server: setting all clients on clientsPredict state: ~p~n",[ListOfClients]),
%%%%  TODO find the router that can send this request to Sources**
%%  [{setClientState(clientPredict,ClientName, ConnectionMap)}|| ClientName<- ListOfClients],
%%  {noreply, State#main_genserver_state{state = predict, clientsWaitingList = ListOfClients}};
%%
%%handle_cast({stopPredicting}, State = #main_genserver_state{clients = ListOfClients, nerlnetGraph = NerlnetGraph}) ->
%%%%  send router http request, to rout this message to all sensors
%%  io:format("main server: setting all clients on clientsPredict state: ~p~n",[ListOfClients]),
%%%%  TODO find the router that can send this request to Sources**
%%  [{setClientState(clientPredict,ClientName, ConnectionMap)}|| ClientName<- ListOfClients],
%%  {noreply, State#main_genserver_state{state = predict, clientsWaitingList = ListOfClients}};


handle_cast({sourceDone,Body}, State = #main_genserver_state{sourcesCastingList = CastingList,msgCounter = MsgCounter}) ->
  % io:format("~p done sending data ~n",[list_to_atom(binary_to_list(Body))]),
  NewCastingList = CastingList--[list_to_atom(binary_to_list(Body))],
  io:format("new Waiting List: ~p ~n",[NewCastingList]),

  case NewCastingList of
    [] -> NextState = State#main_genserver_state{state = idle, sourcesCastingList = NewCastingList,msgCounter = MsgCounter},
          gen_server:cast(self(),{clientsIdle}),
          ack();
    _ -> NextState = State#main_genserver_state{state = casting, sourcesCastingList = NewCastingList,msgCounter = MsgCounter+1}
  end,
  {noreply, NextState};

handle_cast({sourceAck,Body}, State = #main_genserver_state{nerlnetGraph = NerlnetGraph, sourcesWaitingList = WaitingList,msgCounter = MsgCounter}) ->
    io:format("~n~p sent ACK ~n",[list_to_atom(binary_to_list(Body))]),
%%  io:format("new Waiting List: ~p ~n",[WaitingList--[list_to_atom(binary_to_list(Body))]]),
    NewWaitingList = WaitingList--[list_to_atom(binary_to_list(Body))],
    if length(NewWaitingList) == 0 ->
        ack();
      true->
        io:format("~p sent ACK~n new sourceWaitinglist = ~p~n",[list_to_atom(binary_to_list(Body)),NewWaitingList])
    end,
  {noreply, State#main_genserver_state{sourcesWaitingList = NewWaitingList,msgCounter = MsgCounter+1}};


handle_cast({clientAck,Body}, State = #main_genserver_state{clientsWaitingList = WaitingList,msgCounter = MsgCounter,nerlnetGraph = NerlnetGraph}) ->
  NewWaitingList = WaitingList--[list_to_atom(binary_to_list(Body))],
  io:format("new Waiting List: ~p ~n",[NewWaitingList]),
  if length(NewWaitingList) == 0 ->
        ack();
    true->
        io:format("~p sent ACK~n new clientWaitinglist = ~p~n",[list_to_atom(binary_to_list(Body)),NewWaitingList])
    end,
  {noreply, State#main_genserver_state{clientsWaitingList = NewWaitingList, msgCounter = MsgCounter+1}};

%%TODO change Client_Names to list of clients
handle_cast({startCasting,Source_Names}, State = #main_genserver_state{state = idle,myName = MyName, sourcesCastingList=CastingList, nerlnetGraph = NerlnetGraph, sourcesWaitingList = [], clientsWaitingList = [],msgCounter = MsgCounter}) ->
  Splitted = re:split(binary_to_list(Source_Names), ",", [{return, list}]),
  NumOfSampleToSend = lists:last(Splitted),
  Sources = lists:sublist(Splitted,length(Splitted)-1),
  startCasting(Sources,NumOfSampleToSend,MyName, NerlnetGraph),
  SourcesAtoms = [list_to_atom(Source_Name)||Source_Name<-Sources],
  io:format("new Casting list: ~p~n",[SourcesAtoms]),
  {noreply, State#main_genserver_state{sourcesCastingList = CastingList++SourcesAtoms, state = casting,msgCounter = MsgCounter+1}};


handle_cast({startCasting,_Source_Names}, State = #main_genserver_state{sourcesWaitingList = SourcesWaiting, clientsWaitingList = ClientsWaiting}) ->
  io:format("Waiting for ~p~n",[{SourcesWaiting, ClientsWaiting}]),
  {noreply, State};

handle_cast({startCasting,_Source_Names}, State = #main_genserver_state{state = State}) ->
  io:format("not in training state. current state- ~p~n",[State]),
  {noreply, State};


handle_cast({stopCasting,Source_Names}, State = #main_genserver_state{state = casting, myName = MyName, nerlnetGraph = NerlnetGraph,msgCounter = MsgCounter}) ->
    {RouterHost,RouterPort} = nerl_tools:getShortPath(MyName,binary_to_list(Source_Names),NerlnetGraph),

    %{RouterHost,RouterPort} = maps:get(list_to_atom(binary_to_list(Source_Names)),ConnectionMap),
    nerl_tools:http_request(RouterHost,RouterPort,"stopCasting", Source_Names),
    {noreply, State#main_genserver_state{state = idle,msgCounter = MsgCounter+1}};

handle_cast({lossFunction,<<>>}, State = #main_genserver_state{msgCounter = MsgCounter}) ->
  {noreply, State#main_genserver_state{msgCounter = MsgCounter+1}};
handle_cast({lossFunction,Body}, State = #main_genserver_state{myName = MyName, nerlnetGraph = NerlnetGraph,msgCounter = MsgCounter}) ->
  try
    {RouterHost,RouterPort} = nerl_tools:getShortPath(MyName,?API_SERVER_ATOM,NerlnetGraph),
    % io:format("sending loss to serverAPI {RouterHost,RouterPort}:- ~p~n",[{RouterHost,RouterPort}]),
    case   binary_to_term(Body) of
      {WorkerName,{LossFunction,_Time}} ->
     % io:format("main server got loss function:- ~p~n",[binary_to_term(Body)]),
      %{RouterHost,RouterPort} = maps:get(serverAPI,ConnectionMap),
      % http_request(RouterHost,RouterPort,"lossFunc", atom_to_list(WorkerName)++"#"++float_to_list(1.01));
      nerl_tools:http_request(RouterHost,RouterPort,"lossFunc", atom_to_list(WorkerName)++"#"++float_to_list(LossFunction));
      {WorkerName,LossFunction} ->
     % io:format("main server got loss function:- ~p~n",[binary_to_term(Body)]),
      %{RouterHost,RouterPort} = maps:get(serverAPI,ConnectionMap),
      % http_request(RouterHost,RouterPort,"lossFunc", atom_to_list(WorkerName)++"#"++float_to_list(1.01))
      nerl_tools:http_request(RouterHost,RouterPort,"lossFunc", atom_to_list(WorkerName)++"#"++float_to_list(LossFunction))
      end
      %%  file:write_file("./output/"++WorkerName, LossFunction++"\n", [append]),

    
  catch Err:E ->
          io:format("Error receiving loss function ~p~n",[{Err,E}])
  end,
  {noreply, State#main_genserver_state{msgCounter = MsgCounter+1}};


handle_cast({predictRes,Body}, State = #main_genserver_state{batchSize = BatchSize, nerlnetGraph = NerlnetGraph,msgCounter = MsgCounter}) ->
  try 
      {RouterHost,RouterPort} = nerl_tools:getShortPath(?MAIN_SERVER_ATOM,?API_SERVER_ATOM,NerlnetGraph),
      % io:format("sending predictRes to serverAPI {RouterHost,RouterPort}:- ~p~n",[{RouterHost,RouterPort}]),

      {WorkerName, InputName, BatchID, {NerlTensor, Type}} = binary_to_term(Body),   %% TODO: add convention with client
      
      % io:format("Got Tensor: {~p, ~p}~n",[NerlTensor, Type]),

      {DecodedNerlTensor, _Type} =
      if (NerlTensor==<<>>) -> ?LOG_ERROR(?LOG_HEADER++"Got empty tensor"), empty_nerltensor_err;
          true ->  nerlNIF:nerltensor_conversion({NerlTensor, Type}, nerlNIF:erl_type_conversion(Type)) end,
      % io:format("Decoded NerlTensor: ~p~n",[DecodedNerlTensor]),    

      % XDim = integer_to_list(round(hd(DecodedNerlTensor))), %% returns XDim of this tensor
      %% IDX_WORKER = 0,IDX_BATCH_SIZE = 1,IDX_BATCHID = 2,IDX_CSVNAME = 3,IDX_PREDS = 4
      ListToSend = [WorkerName, integer_to_list(BatchID), integer_to_list(BatchSize), InputName, nerl_tools:string_format("~p",[DecodedNerlTensor])],

      ToSend = tl(lists:flatten(["#"++Item || Item <- ListToSend])),

      % io:format("predictResID- ~p~n",[ToSend]),
      nerl_tools:http_request(RouterHost,RouterPort,"predRes",ToSend)
  catch Err:E ->  ?LOG_ERROR(?LOG_HEADER++"Error receiving predict result ~p~n",[{Err,E}])
  end,
  {noreply, State#main_genserver_state{msgCounter = MsgCounter+1}};


handle_cast(Request, State = #main_genserver_state{}) ->

  io:format("main server cast ignored: ~p~n",[Request]),
  {noreply, State}.

%% @private
%% @doc Handling all non call/cast messages
-spec(handle_info(Info :: timeout() | term(), State :: #main_genserver_state{}) ->
{noreply, NewState :: #main_genserver_state{}} |
{noreply, NewState :: #main_genserver_state{}, timeout() | hibernate} |
{stop, Reason :: term(), NewState :: #main_genserver_state{}}).
handle_info(_Info, State = #main_genserver_state{}) ->
{noreply, State}.


%% @private
%% @doc This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
-spec(terminate(Reason :: (normal | shutdown | {shutdown, term()} | term()),
    State :: #main_genserver_state{}) -> term()).
terminate(_Reason, _State = #main_genserver_state{}) ->
ok.

%% @private
%% @doc Convert process state when code is changed
-spec(code_change(OldVsn :: term() | {down, term()}, State :: #main_genserver_state{},
Extra :: term()) ->
{ok, NewState :: #main_genserver_state{}} | {error, Reason :: term()}).
code_change(_OldVsn, State = #main_genserver_state{}, _Extra) ->
{ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================


setClientState(StateAtom,ClientName, NerlnetGraph,MyName) ->
  nerl_tools:sendHTTP(MyName, ClientName, atom_to_list(StateAtom), ClientName).
  %   {RouterHost,RouterPort} = nerl_tools:getShortPath(MyName,ClientName,NerlnetGraph),
  % %{RouterHost,RouterPort} =maps:get(ClientName, ConnectionMap),
  % nerl_tools:http_request(RouterHost,RouterPort,atom_to_list(StateAtom), atom_to_list(ClientName)).

%%find Router and send message: finds the path for the named machine from connection map and send to the right router to forword.
%%findroutAndsend([],_,_,WaitingList)->WaitingList;
%%Source,Workers,Input,WorkersMap,ConnectionMap
findroutAndsend(MyName,SourceName,Body,NerlnetGraph) ->
%%  io:format("WaitingList = ~p~n~n",[Workers]),
  {RouterHost,RouterPort} = nerl_tools:getShortPath(MyName,SourceName,NerlnetGraph),
  
  %{RouterHost,RouterPort} =maps:get(list_to_atom(SourceName),ConnectionsMap),
  nerl_tools:http_request(RouterHost, RouterPort,"updateCSV", Body).


findroutAndsendStatistics(_MyName, serverAPI,_ConnectionsMap) -> skip;
findroutAndsendStatistics(MyName,Entity,NerlnetGraph) ->
  nerl_tools:sendHTTP(MyName, Entity, "statistics", Entity).
  % {RouterHost,RouterPort} = nerl_tools:getShortPath(MyName,Entity,NerlnetGraph),
  % nerl_tools:http_request(RouterHost, RouterPort,"statistics", Entity).
	
getNewStatisticsMap(ConnectionList) -> getNewStatisticsMap(ConnectionList,#{}).
getNewStatisticsMap([],StatisticsMap) ->StatisticsMap;
getNewStatisticsMap([{Name,{_Host, _Port}}|Tail],StatisticsMap) ->
  getNewStatisticsMap(Tail,maps:put(atom_to_list(Name), 0, StatisticsMap)).


startCasting([],_NumOfSampleToSend,_MyName, _NerlnetGraph)->done;
startCasting([SourceName|SourceNames],NumOfSampleToSend, MyName, NerlnetGraph)->
    {RouterHost,RouterPort} = nerl_tools:getShortPath(MyName,SourceName,NerlnetGraph),
  ?LOG_NOTICE("~p sending start casting command to: ~p",[MyName, SourceName]),
  nerl_tools:http_request(RouterHost,RouterPort,"startCasting", SourceName++[","]++NumOfSampleToSend),
  startCasting(SourceNames,NumOfSampleToSend, MyName, NerlnetGraph).


ack() ->
  % io:format("mainserver sending ACK~n"),
  Response = nerl_tools:sendHTTP(?MAIN_SERVER_ATOM, ?API_SERVER_ATOM, "ackP", "ack"),
  io:format("ACK was ~p~n",[Response]).
%   % io:format("sending ACK to serverAPI~n"),
%   {_,{Host, Port}} = digraph:vertex(NerlnetGraph,?API_SERVER_ATOM),
% %%  send an ACK to mainserver that the CSV file is ready
%   nerl_tools:http_request(Host, Port,"ackP","ack").

% getCSVName(InputName) ->
%   lists:last(re:split(InputName, "/", [{return, list}])--[[]]).

%% encodes stats to string:
%% "Entity1:Stats,...|Entity2:Stats,...|....."
mapToString([],Ret) -> Ret;
mapToString([{Name,Data}|StatisticsList],[]) -> mapToString(StatisticsList,Name++":"++Data);
mapToString([{Name,Data}|StatisticsList],Ret) -> mapToString(StatisticsList,Ret++"|"++Name++":"++Data).