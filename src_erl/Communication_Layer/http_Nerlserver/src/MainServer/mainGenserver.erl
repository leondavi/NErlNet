%%%-------------------------------------------------------------------
%%% @author kapelnik
%%% @copyright (C) 2021, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 27. Apr 2021 3:35 AM
%%%-------------------------------------------------------------------
-module(mainGenserver).
-author("kapelnik").

-behaviour(gen_server).

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
  inets:start(),
    io:format("Main Server ~p Connecting to: ~p~n",[MyName, [digraph:vertex(NerlnetGraph,Vertex) || Vertex <- digraph:out_neighbours(NerlnetGraph,MyName)]]),
  start_connection([digraph:vertex(NerlnetGraph,Vertex) || Vertex <- digraph:out_neighbours(NerlnetGraph,MyName)]),
  
  NewStatisticsMap = getNewStatisticsMap([digraph:vertex(NerlnetGraph,Vertex) || Vertex <- (digraph:vertices(NerlnetGraph)--["serverAPI"])--["mainServer"]]),
  io:format("New StatisticsMap = ~p~n",[NewStatisticsMap]),
  {ok, #main_genserver_state{myName = MyName, workersMap = WorkersMap, batchSize = BatchSize, state=idle, clients = Clients, nerlnetGraph = NerlnetGraph, msgCounter = 1,statisticsMap = NewStatisticsMap}}.

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
handle_call(_Request, _From, State = #main_genserver_state{}) ->
{reply, ok, State}.

%% @private
%% @doc Handling cast messages
-spec(handle_cast(Request :: term(), State :: #main_genserver_state{}) ->
{noreply, NewState :: #main_genserver_state{}} |
{noreply, NewState :: #main_genserver_state{}, timeout() | hibernate} |
{stop, Reason :: term(), NewState :: #main_genserver_state{}}).


handle_cast({initCSV, Source,_Workers,Body}, State = #main_genserver_state{state = idle, myName = MyName, sourcesWaitingList = SourcesWaitingList,nerlnetGraph = NerlnetGraph,msgCounter = MsgCounter}) ->
%%  send router http request, to rout this message to all sensors
%%  TODO find the router that can send this request to Sources**
  findroutAndsend(MyName, Source,Body,NerlnetGraph),
  io:format("WaitingList = ~p~n",[SourcesWaitingList++[list_to_atom(Source)]]),
  {noreply, State#main_genserver_state{sourcesWaitingList = SourcesWaitingList++[list_to_atom(Source)],msgCounter = MsgCounter+1}};

handle_cast({clientsTraining,Body}, State = #main_genserver_state{state = casting,clients = ListOfClients,msgCounter = MsgCounter}) ->
%%  send router http request, to rout this message to all sensors
  io:format("already casting~n",[]),
  io:format("Body:~p~n",[Body]),
  {noreply, State#main_genserver_state{clientsWaitingList = ListOfClients,msgCounter = MsgCounter+1}};

handle_cast({clientsTraining, _Body}, State = #main_genserver_state{myName = MyName, clients = ListOfClients, nerlnetGraph = NerlnetGraph, msgCounter = MsgCounter}) ->
%%  send router http request, to rout this message to all sensors
  io:format("main server: setting all clients on training state: ~p~n",[ListOfClients]),
%%  io:format("Body:~p~n",[Body]),
%%  io:format("binary_to_list(Body):~p~n",[binary_to_list(Body)]),
%%  io:format("Splitted-(Body):~p~n",[re:split(binary_to_list(Body), ",", [{return, list}])]),
%%  TODO find the router that can send this request to Sources**
  [{setClientState(clientTraining,ClientName, NerlnetGraph,MyName)}|| ClientName<- ListOfClients],
  {noreply, State#main_genserver_state{clientsWaitingList = ListOfClients,msgCounter = MsgCounter+1}};

handle_cast({clientsPredict,_Body}, State = #main_genserver_state{state = casting, clients = ListOfClients,msgCounter = MsgCounter}) ->
%%  send router http request, to rout this message to all sensors
  io:format("already casting~n",[]),
  {noreply, State#main_genserver_state{clientsWaitingList = ListOfClients,msgCounter = MsgCounter+1}};

handle_cast({clientsPredict,_Body}, State = #main_genserver_state{myName = MyName,clients = ListOfClients, nerlnetGraph = NerlnetGraph,msgCounter = MsgCounter}) ->
%%  send router http request, to rout this message to all sensors
  io:format("main server: setting all clients on clientsPredict state: ~p~n",[ListOfClients]),
%%  TODO find the router that can send this request to Sources**
  [{setClientState(clientPredict,ClientName, NerlnetGraph,MyName)}|| ClientName<- ListOfClients],
  {noreply, State#main_genserver_state{clientsWaitingList = ListOfClients,msgCounter = MsgCounter+1}};

handle_cast({clientsIdle}, State = #main_genserver_state{state = idle, myName = MyName, clients = ListOfClients, nerlnetGraph = NerlnetGraph,msgCounter = MsgCounter}) ->
%%  send router http request, to rout this message to all sensors
  io:format("main server: setting all clients on Idle state: ~p~n",[ListOfClients]),
%%  TODO find the router that can send this request to Sources**
  [{setClientState(clientIdle,ClientName, NerlnetGraph, MyName)}|| ClientName<- ListOfClients],
  {noreply, State#main_genserver_state{clientsWaitingList = ListOfClients,msgCounter = MsgCounter+1}};

%%%get Statistics from all Entities in the network
handle_cast({statistics,Body}, State = #main_genserver_state{myName = MyName, statisticsCounter = StatisticsCounter, nerlnetGraph = NerlnetGraph,statisticsMap = StatisticsMap,msgCounter = MsgCounter}) ->
  %%  io:format("Body:~n~p~n",[Body]),
  
    if Body == <<"getStatistics">> ->
          [findroutAndsendStatistics(MyName, Name,NerlnetGraph)||{Name,_Counter}<-maps:to_list(StatisticsMap)],
          NewState = State#main_genserver_state{msgCounter = MsgCounter+1,statisticsCounter = length(maps:to_list(StatisticsMap))};
      true ->
  %%      statistics arrived from Entity
          [From|[NewCounter]] = re:split(binary_to_list(Body), "#", [{return, list}]),
  
        NewStatisticsMap = maps:put(From,NewCounter,StatisticsMap),
          NewState = State#main_genserver_state{msgCounter = MsgCounter+1,statisticsMap = NewStatisticsMap,statisticsCounter = StatisticsCounter-1},
  
        if StatisticsCounter == 1 ->
              Statistics = maps:to_list(NewStatisticsMap),
              StatisticsList = lists:flatten(io_lib:format("~w",[Statistics])),",",[{return,list}],
              io:format("new Statistics Map:~n~p~n",[StatisticsList]),
              S = mapToString(Statistics,[]) ,
              io:format("S: ~p~n",[S]),
              ack(NerlnetGraph),
              {RouterHost,RouterPort} = getShortPath(MyName,"serverAPI",NerlnetGraph),
  
              %{RouterHost,RouterPort} = maps:get(serverAPI,ConnectionMap),
              http_request(RouterHost,RouterPort,"statistics", "statistics#" ++ S ++ "@mainServer#" ++integer_to_list(MsgCounter));
            true ->
              ok
          end
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


handle_cast({sourceDone,Body}, State = #main_genserver_state{nerlnetGraph = NerlnetGraph, sourcesCastingList = CastingList,msgCounter = MsgCounter}) ->
  io:format("~p done sending data ~n",[list_to_atom(binary_to_list(Body))]),
  NewCastingList = CastingList--[list_to_atom(binary_to_list(Body))],
  io:format("new Waiting List: ~p ~n",[NewCastingList]),

  case NewCastingList of
    [] -> NextState = State#main_genserver_state{state = idle, sourcesCastingList = NewCastingList,msgCounter = MsgCounter},
          gen_server:cast(self(),{clientsIdle});
          % ack(NerlnetGraph);
    _ -> NextState = State#main_genserver_state{state = casting, sourcesCastingList = NewCastingList,msgCounter = MsgCounter+1}
  end,
  {noreply, NextState};

handle_cast({sourceAck,Body}, State = #main_genserver_state{nerlnetGraph = NerlnetGraph, sourcesWaitingList = WaitingList,msgCounter = MsgCounter}) ->
   io:format("~n~p sent ACK ~n",[list_to_atom(binary_to_list(Body))]),
%%  io:format("new Waiting List: ~p ~n",[WaitingList--[list_to_atom(binary_to_list(Body))]]),
  NewWaitingList = WaitingList--[list_to_atom(binary_to_list(Body))],
%%  if length(NewWaitingList) == 0 ->
    ack(NerlnetGraph),
%%    true->
%%      io:format("~p sent ACK~n new sourceWaitinglist = ~p~n",[list_to_atom(binary_to_list(Body)),NewWaitingList])
%%  end,
  {noreply, State#main_genserver_state{sourcesWaitingList = NewWaitingList,msgCounter = MsgCounter+1}};


handle_cast({clientAck,Body}, State = #main_genserver_state{ clientsWaitingList = WaitingList,msgCounter = MsgCounter,nerlnetGraph = NerlnetGraph}) ->
  NewWaitingList = WaitingList--[list_to_atom(binary_to_list(Body))],
  if length(NewWaitingList) == 0 ->
        ack(NerlnetGraph);
        % ack(NerlnetGraph);
          
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
    {RouterHost,RouterPort} = getShortPath(MyName,binary_to_list(Source_Names),NerlnetGraph),

    %{RouterHost,RouterPort} = maps:get(list_to_atom(binary_to_list(Source_Names)),ConnectionMap),
    http_request(RouterHost,RouterPort,"stopCasting", Source_Names),
    {noreply, State#main_genserver_state{state = idle,msgCounter = MsgCounter+1}};

handle_cast({lossFunction,<<>>}, State = #main_genserver_state{msgCounter = MsgCounter}) ->
  {noreply, State#main_genserver_state{msgCounter = MsgCounter+1}};
  handle_cast({lossFunction,Body}, State = #main_genserver_state{myName = MyName, nerlnetGraph = NerlnetGraph,msgCounter = MsgCounter}) ->
    %%  io:format("got loss function:- ~p~n",[Body]),
        {RouterHost,RouterPort} = getShortPath(MyName,"serverAPI",NerlnetGraph),
        io:format("{RouterHost,RouterPort}:- ~p~n",[{RouterHost,RouterPort}]),
      case   binary_to_term(Body) of
        {WorkerName,{LossFunction,_Time}} ->
          io:format("got loss function:- ~p~n",[{RouterHost,RouterPort,atom_to_list(WorkerName),float_to_list(LossFunction)}]),
        %{RouterHost,RouterPort} = maps:get(serverAPI,ConnectionMap),
        http_request(RouterHost,RouterPort,"lossFunction", atom_to_list(WorkerName)++"#"++float_to_list(LossFunction));
        {WorkerName,LossFunction} ->
          io:format("got loss function:- ~p~n",[{RouterHost,RouterPort,atom_to_list(WorkerName),float_to_list(LossFunction)}]),
        %{RouterHost,RouterPort} = maps:get(serverAPI,ConnectionMap),
        http_request(RouterHost,RouterPort,"lossFunction", atom_to_list(WorkerName)++"#"++float_to_list(LossFunction))
        end,
        %%  file:write_file("./output/"++WorkerName, LossFunction++"\n", [append]),

    

  {noreply, State#main_genserver_state{msgCounter = MsgCounter+1}};

% handle_cast({predictRes,Body}, State = #main_genserver_state{batchSize = BatchSize, nerlnetGraph = NerlnetGraph,msgCounter = MsgCounter}) ->
%    io:format("predict result : ~n~p~n",[Body]),

%   {InputName,BatchID,Result}=binary_to_term(Body),
%   if (Result==[]) ->
%         ListOfResults = ["error"||_<-lists:seq(1,BatchSize)];
%       true ->
%        ListOfResults = re:split(Result, ",", [{return, list}])
%   end,

%   %%  io:format("predictRes- length(ListOfResults): ~p~n{InputName,BatchID,Result} ~p ~n",[length(ListOfResults),{InputName,BatchID,Result}]),
% %%      io:format("predictResID- ~p~n",[BatchID]),

%       % io:format("Main Server got predictRes:InputName- ~p ResultID: ~p Result: ~p~n",[InputName,ResultID,Result]),
%       CSVName = getCSVName(InputName),
%       %%  file:write_file("./output/"++"predict"++CSVName, ResultID++" " ++Result++"\n", [append]),
%       io:format("writeToFile- ~p~n",[{ListOfResults,BatchID,CSVName,BatchSize}]),

%       writeToFile(ListOfResults,BatchID,CSVName,BatchSize),


%   {noreply, State#main_genserver_state{msgCounter = MsgCounter+1}};

handle_cast({predictRes,Body}, State = #main_genserver_state{batchSize = BatchSize, nerlnetGraph = NerlnetGraph,msgCounter = MsgCounter}) ->
  {InputName,BatchID,Result}=binary_to_term(Body),
  if (Result==[]) ->
        ListOfResults = ["error"||_<-lists:seq(1,BatchSize)];
      true ->
       ListOfResults = re:split(Result, ",", [{return, list}])
  end,

  %%  io:format("predictRes- length(ListOfResults): ~p~n{InputName,BatchID,Result} ~p ~n",[length(ListOfResults),{InputName,BatchID,Result}]),
%%      io:format("predictResID- ~p~n",[BatchID]),

      % io:format("Main Server got predictRes:InputName- ~p ResultID: ~p Result: ~p~n",[InputName,ResultID,Result]),
      CSVName = getCSVName(InputName),
      %%  file:write_file("./output/"++"predict"++CSVName, ResultID++" " ++Result++"\n", [append]),
      % writeToFile(ListOfResults,BatchID,CSVName,BatchSize),
        {RouterHost,RouterPort} = getShortPath("mainServer","serverAPI",NerlnetGraph),

      %{RouterHost,RouterPort} = maps:get(serverAPI,ConnectionMap),
      %%  send an ACK to mainserver that the CSV file is ready
      %FloatsString = [float_to_list(Float)++","||Float<-ListOfResults],
      ToSend=Result++"#"++integer_to_list(BatchID)++"#"++CSVName++"#"++integer_to_list(BatchSize),
      io:format("predictResID- ~p~n",[ToSend]),
      http_request(RouterHost,RouterPort,"predictRes",ToSend),

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
    {RouterHost,RouterPort} = getShortPath(MyName,ClientName,NerlnetGraph),
  %{RouterHost,RouterPort} =maps:get(ClientName, ConnectionMap),
  http_request(RouterHost,RouterPort,atom_to_list(StateAtom), atom_to_list(ClientName)).

%%find Router and send message: finds the path for the named machine from connection map and send to the right router to forword.
%%findroutAndsend([],_,_,WaitingList)->WaitingList;
%%Source,Workers,Input,WorkersMap,ConnectionMap
findroutAndsend(MyName,SourceName,Body,NerlnetGraph) ->
%%  io:format("WaitingList = ~p~n~n",[Workers]),
  {RouterHost,RouterPort} = getShortPath(MyName,SourceName,NerlnetGraph),
  
  %{RouterHost,RouterPort} =maps:get(list_to_atom(SourceName),ConnectionsMap),
  http_request(RouterHost, RouterPort,"updateCSV", Body).

findroutAndsendStatistics(_MyName, serverAPI,_ConnectionsMap) ->io:format("");

findroutAndsendStatistics(MyName,Entitie,NerlnetGraph) ->
%%  io:format("WaitingList = ~p~n~n",[Workers]),
  {RouterHost,RouterPort} = getShortPath(MyName,Entitie,NerlnetGraph),
% {RouterHost,RouterPort} =maps:get(Entitie,ConnectionsMap),
 case is_atom(Entitie) of
      true -> http_request(RouterHost, RouterPort,"statistics", list_to_binary(atom_to_list(Entitie)));
      _    -> http_request(RouterHost, RouterPort,"statistics", list_to_binary(Entitie))
  end.

%%sending Body as an http request to {Host, Port} to path Path (=String)
%%Example:  http_request(RouterHost,RouterPort,"start_training", <<"client1,client2">>),
http_request(Host, Port,Path, Body)->
  URL = "http://" ++ Host ++ ":"++integer_to_list(Port) ++ "/" ++ Path,
  httpc:set_options([{proxy, {{Host, Port},[Host]}}]),
%%  io:format("sending:  ~p~nto HostPo: ~p~n",[Body,{Host, Port}]),
  _R = httpc:request(post,{URL, [],"application/x-www-form-urlencoded",Body}, [], []).
  %io:format("request:~p~n",[{R, Host, Port,Path, Body}]).


%%Receives a list of routers and connects to them
start_connection([])->ok;
start_connection([{_ServerName,{Host, Port}}|Tail]) ->
  Res = httpc:set_options([{proxy, {{Host, Port},[Host]}}]),
  io:format("mainserver connecting to: ~p result: ~p~n",[{Host, Port},Res]),
start_connection(Tail).


getShortPath(From,To,NerlnetGraph) when is_atom(To)-> 
  	First = lists:nth(2,digraph:get_short_path(NerlnetGraph,From,atom_to_list(To))),

	{_First,{Host,Port}} = digraph:vertex(NerlnetGraph,First),
  {Host,Port};

getShortPath(From,To,NerlnetGraph) -> 
	First = lists:nth(2,digraph:get_short_path(NerlnetGraph,From,To)),
	{_First,{Host,Port}} = digraph:vertex(NerlnetGraph,First),
  {Host,Port}.

	
getNewStatisticsMap(ConnectionList) -> getNewStatisticsMap(ConnectionList,#{}).
getNewStatisticsMap([],StatisticsMap) ->StatisticsMap;
getNewStatisticsMap([{ServerName,{_Host, _Port}}|Tail],StatisticsMap) ->
  getNewStatisticsMap(Tail,maps:put(ServerName, 0, StatisticsMap)).

encodeDoubleVectorToString(ListOfFloats)->
  LL = lists:flatten(io_lib:format("~p",[ListOfFloats])),",",[{return,list}],
  lists:sublist(LL,2,length(LL)).

decode(L)->
%%  LL=lists:sublist(L,2,length(L)-2),
  LL=re:split(L,",",[{return,list}]),
  [list_to_float(X)||X<-LL].



startCasting([],_NumOfSampleToSend,_MyName, _NerlnetGraph)->done;
startCasting([SourceName|SourceNames],NumOfSampleToSend, MyName, NerlnetGraph)->
    {RouterHost,RouterPort} = getShortPath(MyName,SourceName,NerlnetGraph),

  %{RouterHost,RouterPort} = maps:get(list_to_atom(SourceName),ConnectionMap),
  io:format("sending StartCasting to: ~p~n",[{SourceName++[","]++NumOfSampleToSend}]),

  http_request(RouterHost,RouterPort,"startCasting", SourceName++[","]++NumOfSampleToSend),
  startCasting(SourceNames,NumOfSampleToSend, MyName, NerlnetGraph).


ack(NerlnetGraph) ->
  io:format("sending ACK to serverAPI~n"),
  {_,{Host, Port}} = digraph:vertex(NerlnetGraph,"serverAPI"),
%%  send an ACK to mainserver that the CSV file is ready
  http_request(Host, Port,"ack","ack").

getCSVName(InputName) ->
  lists:last(re:split(InputName, "/", [{return, list}])--[[]]).


%%this function takes a batch of samples, calculate the samples id and writes them to a file
writeToFile(ListOfSamples,BatchID,CSVName,BatchSize)->
  StartID = BatchID*BatchSize,
  SampleSize = round(length(ListOfSamples)/BatchSize),
  writeSamplesToFile(ListOfSamples,StartID,CSVName,SampleSize).

writeSamplesToFile([],_HeadID,_CSVName,_SampleSize)->ok;
writeSamplesToFile(ListOfSamples,HeadID,CSVName,SampleSize)->
  Head = lists:sublist(ListOfSamples,SampleSize),
  file:write_file("./output/"++"predict"++CSVName, integer_to_list(HeadID)++" " ++Head++"\n", [append]),
%%  io:format("./output/predict~p   ~p~p~n", [CSVName,integer_to_list(HeadID),Head]),
  writeSamplesToFile(lists:sublist(ListOfSamples,SampleSize+1,length(ListOfSamples)),HeadID+1,CSVName,SampleSize).


  mapToString([],Ret) -> Ret;
  mapToString([{Name,Counter}|StatisticsList],[]) -> mapToString(StatisticsList,Name++"#"++Counter);
  mapToString([{Name,Counter}|StatisticsList],Ret) -> 
      % io:format("~p~n",[{Name,Counter,Ret}]),
      mapToString(StatisticsList,Ret++"@"++Name++"#"++Counter).