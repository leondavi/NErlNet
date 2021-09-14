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

-record(main_genserver_state, {statisticsCounter=0,myName, state, workersMap, clients, connectionsMap, sourcesCastingList = [], sourcesWaitingList = [], clientsWaitingList = [],statisticsMap, msgCounter=0}).

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
init({MyName,Clients,WorkersMap,ConnectionsMap}) ->
  inets:start(),
  % io:format("connection map:~p~n",[ConnectionsMap]),
  start_connection(maps:to_list(ConnectionsMap)),
  NewStatisticsMap = getNewStatisticsMap(maps:to_list(ConnectionsMap)),
  io:format("New StatisticsMap = ~p~n",[NewStatisticsMap]),
  {ok, #main_genserver_state{myName = MyName, workersMap = WorkersMap, state=idle, clients = Clients, connectionsMap = ConnectionsMap,msgCounter = 1,statisticsMap = NewStatisticsMap}}.

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


handle_cast({initCSV, Source,_Workers,Body}, State = #main_genserver_state{state = idle, sourcesWaitingList = SourcesWaitingList,connectionsMap = ConnectionMap,msgCounter = MsgCounter}) ->
%%  send router http request, to rout this message to all sensors
%%  TODO find the router that can send this request to Sources**
  findroutAndsend(Source,Body,ConnectionMap),
  io:format("WaitingList = ~p~n",[list_to_atom(Source)]),
  {noreply, State#main_genserver_state{sourcesWaitingList = SourcesWaitingList++[list_to_atom(Source)],msgCounter = MsgCounter+1}};

handle_cast({clientsTraining,Body}, State = #main_genserver_state{state = casting,clients = ListOfClients,msgCounter = MsgCounter}) ->
%%  send router http request, to rout this message to all sensors
  io:format("already casting~n",[]),
  io:format("Body:~p~n",[Body]),
  {noreply, State#main_genserver_state{clientsWaitingList = ListOfClients,msgCounter = MsgCounter+1}};

handle_cast({clientsTraining,Body}, State = #main_genserver_state{clients = ListOfClients, connectionsMap = ConnectionMap,msgCounter = MsgCounter}) ->
%%  send router http request, to rout this message to all sensors
  io:format("main server: setting all clients on training state: ~p~n",[ListOfClients]),
  io:format("Body:~p~n",[Body]),
  io:format("binary_to_list(Body):~p~n",[binary_to_list(Body)]),
  io:format("Splitted-(Body):~p~n",[re:split(binary_to_list(Body), ",", [{return, list}])]),
%%  TODO find the router that can send this request to Sources**
  [{setClientState(clientTraining,ClientName, ConnectionMap)}|| ClientName<- ListOfClients],
  {noreply, State#main_genserver_state{clientsWaitingList = ListOfClients,msgCounter = MsgCounter+1}};

handle_cast({clientsPredict,_Body}, State = #main_genserver_state{state = casting, clients = ListOfClients,msgCounter = MsgCounter}) ->
%%  send router http request, to rout this message to all sensors
  io:format("already casting~n",[]),
  {noreply, State#main_genserver_state{clientsWaitingList = ListOfClients,msgCounter = MsgCounter+1}};

handle_cast({clientsPredict,_Body}, State = #main_genserver_state{clients = ListOfClients, connectionsMap = ConnectionMap,msgCounter = MsgCounter}) ->
%%  send router http request, to rout this message to all sensors
  io:format("main server: setting all clients on clientsPredict state: ~p~n",[ListOfClients]),
%%  TODO find the router that can send this request to Sources**
  [{setClientState(clientPredict,ClientName, ConnectionMap)}|| ClientName<- ListOfClients],
  {noreply, State#main_genserver_state{clientsWaitingList = ListOfClients,msgCounter = MsgCounter+1}};

handle_cast({clientsIdle}, State = #main_genserver_state{state = idle, clients = ListOfClients, connectionsMap = ConnectionMap,msgCounter = MsgCounter}) ->
%%  send router http request, to rout this message to all sensors
  io:format("main server: setting all clients on Idle state: ~p~n",[ListOfClients]),
%%  TODO find the router that can send this request to Sources**
  [{setClientState(clientIdle,ClientName, ConnectionMap)}|| ClientName<- ListOfClients],
  {noreply, State#main_genserver_state{clientsWaitingList = ListOfClients,msgCounter = MsgCounter+1}};

%%get Statistics from all Entities in the network
handle_cast({statistics,Body}, State = #main_genserver_state{statisticsCounter = StatisticsCounter, connectionsMap = ConnectionMap,statisticsMap = StatisticsMap,msgCounter = MsgCounter}) ->
  io:format("Body:~n~p~n",[Body]),

  if Body == <<"getStatistics">> ->
        [findroutAndsendStatistics(Name,ConnectionMap)||{Name,_Counter}<-maps:to_list(StatisticsMap)],
        NewState = State#main_genserver_state{msgCounter = MsgCounter+1,statisticsCounter = length(maps:to_list(StatisticsMap))};
    true ->
%%      statistics arrived from Entity
        [From|[NewCounter]] = re:split(binary_to_list(Body), "#", [{return, list}]),

      NewStatisticsMap = maps:put(list_to_atom(From),NewCounter,StatisticsMap),
        NewState = State#main_genserver_state{msgCounter = MsgCounter+1,statisticsMap = NewStatisticsMap,statisticsCounter = StatisticsCounter-1},

      if StatisticsCounter == 1 ->
            io:format("new Statistics Map:~n~p~n",[NewStatisticsMap]);
          true ->
            ok
        end
    end,
  {noreply, NewState};

%%handle_cast({startPredicting}, State = #main_genserver_state{clients = ListOfClients, connectionsMap = ConnectionMap}) ->
%%%%  send router http request, to rout this message to all sensors
%%  io:format("main server: setting all clients on clientsPredict state: ~p~n",[ListOfClients]),
%%%%  TODO find the router that can send this request to Sources**
%%  [{setClientState(clientPredict,ClientName, ConnectionMap)}|| ClientName<- ListOfClients],
%%  {noreply, State#main_genserver_state{state = predict, clientsWaitingList = ListOfClients}};
%%
%%handle_cast({stopPredicting}, State = #main_genserver_state{clients = ListOfClients, connectionsMap = ConnectionMap}) ->
%%%%  send router http request, to rout this message to all sensors
%%  io:format("main server: setting all clients on clientsPredict state: ~p~n",[ListOfClients]),
%%%%  TODO find the router that can send this request to Sources**
%%  [{setClientState(clientPredict,ClientName, ConnectionMap)}|| ClientName<- ListOfClients],
%%  {noreply, State#main_genserver_state{state = predict, clientsWaitingList = ListOfClients}};


handle_cast({sourceDone,Body}, State = #main_genserver_state{connectionsMap = ConnectionMap, sourcesCastingList = CastingList,msgCounter = MsgCounter}) ->
  io:format("~p done sending data ~n",[list_to_atom(binary_to_list(Body))]),
  io:format("new Waiting List: ~p ~n",[CastingList--[list_to_atom(binary_to_list(Body))]]),
  NewCastingList = CastingList--[list_to_atom(binary_to_list(Body))],
  case NewCastingList of
    [] -> NextState = State#main_genserver_state{state = idle, sourcesCastingList = NewCastingList,msgCounter = MsgCounter},
          gen_server:cast(self(),{clientsIdle}),
          ack(ConnectionMap);
    _ -> NextState = State#main_genserver_state{state = casting, sourcesCastingList = NewCastingList,msgCounter = MsgCounter+1}
  end,
  {noreply, NextState};

handle_cast({sourceAck,Body}, State = #main_genserver_state{connectionsMap = ConnectionMap, sourcesWaitingList = WaitingList,msgCounter = MsgCounter}) ->
  % io:format("~p sent ACK ~n",[list_to_atom(binary_to_list(Body))]),
%%  io:format("new Waiting List: ~p ~n",[WaitingList--[list_to_atom(binary_to_list(Body))]]),
  NewWaitingList = WaitingList--[list_to_atom(binary_to_list(Body))],
  if length(NewWaitingList) == 0 ->
    ack(ConnectionMap);
    true->
      io:format("~p sent ACK~n new sourceWaitinglist = ~p~n",[list_to_atom(binary_to_list(Body)),NewWaitingList])
  end,
  {noreply, State#main_genserver_state{sourcesWaitingList = NewWaitingList,msgCounter = MsgCounter+1}};


handle_cast({clientAck,Body}, State = #main_genserver_state{ clientsWaitingList = WaitingList,msgCounter = MsgCounter,connectionsMap = ConnectionMap}) ->
  NewWaitingList = WaitingList--[list_to_atom(binary_to_list(Body))],
  if length(NewWaitingList) == 0 ->
            ack(ConnectionMap);
    true->
            io:format("~p sent ACK~n new clientWaitinglist = ~p~n",[list_to_atom(binary_to_list(Body)),NewWaitingList])
    end,
  {noreply, State#main_genserver_state{clientsWaitingList = NewWaitingList, msgCounter = MsgCounter+1}};

%%TODO change Client_Names to list of clients
handle_cast({startCasting,Source_Names}, State = #main_genserver_state{state = idle,sourcesCastingList=CastingList, connectionsMap = ConnectionMap, sourcesWaitingList = [], clientsWaitingList = [],msgCounter = MsgCounter}) ->
  {RouterHost,RouterPort} = maps:get(list_to_atom(binary_to_list(Source_Names)),ConnectionMap),
  http_request(RouterHost,RouterPort,"startCasting", Source_Names),
  % io:format("old Casting list: ~p~n",[Source_Names]),
  Splitted = re:split(binary_to_list(Source_Names), ",", [{return, list}]),
  Sources = [list_to_atom(Source_Name)||Source_Name<-Splitted],
  io:format("new Casting list: ~p~n",[Sources]),
  {noreply, State#main_genserver_state{sourcesCastingList = CastingList++Sources, state = casting,msgCounter = MsgCounter+1}};


handle_cast({startCasting,_Source_Names}, State = #main_genserver_state{sourcesWaitingList = SourcesWaiting, clientsWaitingList = ClientsWaiting}) ->
  io:format("Waiting for ~p~n",[{SourcesWaiting, ClientsWaiting}]),
  {noreply, State};

handle_cast({startCasting,_Source_Names}, State = #main_genserver_state{state = State}) ->
  io:format("not in training state. current state- ~p~n",[State]),
  {noreply, State};


handle_cast({stopCasting,Source_Names}, State = #main_genserver_state{state = casting, connectionsMap = ConnectionMap,msgCounter = MsgCounter}) ->
  {RouterHost,RouterPort} = maps:get(list_to_atom(binary_to_list(Source_Names)),ConnectionMap),
  http_request(RouterHost,RouterPort,"stopCasting", Source_Names),
  {noreply, State#main_genserver_state{state = idle,msgCounter = MsgCounter+1}};

handle_cast({lossFunction,Body}, State = #main_genserver_state{connectionsMap = ConnectionMap,msgCounter = MsgCounter}) ->
%%  io:format("got loss function:- ~p~n",[Body]),
  [WorkerName|LossFunction]=re:split(binary_to_list(Body), "#", [{return, list}]),

  file:write_file("./output/"++WorkerName, LossFunction++"\n", [append]),

%%TODO add send to serverAPI
%%  {RouterHost,RouterPort} = maps:get(serverAPI,ConnectionMap),
%%  http_request(RouterHost,RouterPort,"lossFunction", Body),
  {noreply, State#main_genserver_state{state = idle,msgCounter = MsgCounter+1}};

handle_cast({predictRes,Body}, State = #main_genserver_state{connectionsMap = ConnectionMap,msgCounter = MsgCounter}) ->
  %io:format("Main Server got predictRes:- ~p~n",[Body]),
  [InputName,ResultID,Result]=re:split(binary_to_list(Body), "#", [{return, list}]),
  file:write_file("./output/"++"predict"++InputName, ResultID++" " ++Result++"\n", [append]),

%%%%TODO add send to serverAPI
%%  {RouterHost,RouterPort} = maps:get(serverAPI,ConnectionMap),
%%  http_request(RouterHost,RouterPort,"lossFunction", Body),
  {noreply, State#main_genserver_state{state = idle,msgCounter = MsgCounter+1}};



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


setClientState(StateAtom,ClientName, ConnectionMap) ->
  {RouterHost,RouterPort} =maps:get(ClientName, ConnectionMap),
  http_request(RouterHost,RouterPort,atom_to_list(StateAtom), atom_to_list(ClientName)).

%%find Router and send message: finds the path for the named machine from connection map and send to the right router to forword.
%%findroutAndsend([],_,_,WaitingList)->WaitingList;
%%Source,Workers,Input,WorkersMap,ConnectionMap
findroutAndsend(SourceName,Body,ConnectionsMap) ->
%%  io:format("WaitingList = ~p~n~n",[Workers]),
  {RouterHost,RouterPort} =maps:get(list_to_atom(SourceName),ConnectionsMap),
  http_request(RouterHost, RouterPort,"updateCSV", Body).

findroutAndsendStatistics(Entitie,ConnectionsMap) ->
%%  io:format("WaitingList = ~p~n~n",[Workers]),
  {RouterHost,RouterPort} =maps:get(Entitie,ConnectionsMap),
  http_request(RouterHost, RouterPort,"statistics", list_to_binary(atom_to_list(Entitie))).

%%sending Body as an http request to {Host, Port} to path Path (=String)
%%Example:  http_request(RouterHost,RouterPort,"start_training", <<"client1,client2">>),
http_request(Host, Port,Path, Body)->
  URL = "http://" ++ Host ++ ":"++integer_to_list(Port) ++ "/" ++ Path,
  httpc:set_options([{proxy, {{Host, Port},[Host]}}]),
%%  io:format("sending:  ~p~nto HostPo: ~p~n",[Body,{Host, Port}]),
  httpc:request(post,{URL, [],"application/x-www-form-urlencoded",Body}, [], []).


%%Receives a list of routers and connects to them
start_connection([])->ok;
start_connection([{_ServerName,{Host, Port}}|Tail]) ->
  Res = httpc:set_options([{proxy, {{Host, Port},[Host]}}]),
  io:format("mainserver connecting to: ~p result: ~p~n",[{Host, Port},Res]),
start_connection(Tail).

getNewStatisticsMap(ConnectionList)->getNewStatisticsMap(ConnectionList,#{}).
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

ack(PortMap) ->
  io:format("sending ACK to serverAPI"),
  {RouterHost,RouterPort} = maps:get(serverAPI,PortMap),
%%  send an ACK to mainserver that the CSV file is ready
  http_request(RouterHost,RouterPort,"ack","ack").