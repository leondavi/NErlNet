
-module(serverScreen).
-behaviour(wx_object).

-export([new/2, show/1, destroy/1, probe/1, startProbe/1]).  %% API
-export([init/1, handle_call/3, handle_cast/2, handle_event/2, handle_info/2]).

-include("gui_tools.hrl").

-define(PROBE_TIME, 1000).

new(Parent, Gen) ->
    wx_object:start(?MODULE, [Parent, Gen], []).

show(Frame) ->
    wx_object:call(Frame, show_modal).

startProbe(Frame) ->
    wx_object:cast(Frame, startProbe).

destroy(Frame) ->
    wx_object:call(Frame, destroy).

handle_call(show_modal, _From, State) ->
    wxFrame:show(State#state.frame),
    {reply, ok, State}.

handle_cast(startProbe, State) ->
    NerlGraph = deserialize(mainScreen:getGraph(State#state.mainGen)),
    NewState = State#state{nerlGraph = NerlGraph},
    {ok, _Timer} = timer:apply_interval(?PROBE_TIME, serverScreen, probe, [NewState]),
    {noreply, NewState}.

init([Parent, Gen])->
    ServerFrame = wxFrame:new(Parent, 200, "NerlNet Server", [{size, {1280, 720}}, {pos, {0,0}}]),

    Font = wxFrame:getFont(ServerFrame),
    wxFont:setPointSize(Font, ?FONT_SIZE),
    wxFrame:setFont(ServerFrame, Font),

    Title1 = wxStaticText:new(ServerFrame, 201, "CPU Usage of main server hosting device:",
        [?BUTTON_SIZE(1), ?BUTTON_LOC(0, 0)]),
    CPU_text = wxStaticText:new(ServerFrame, 211, "???%",
        [?BUTTON_SIZE(1), ?BUTTON_LOC(0.5, 0)]),

    Title2 = wxStaticText:new(ServerFrame, 202,"Number of active connections:",
        [?BUTTON_SIZE(1), ?BUTTON_LOC(0, 2)]),
    Conn_count = wxStaticText:new(ServerFrame, 212, "X",
        [?BUTTON_SIZE(1), ?BUTTON_LOC(0.5, 2)]),

    Title3 = wxStaticText:new(ServerFrame, 203, "Sent/Received messages:",
        [?BUTTON_SIZE(1), ?BUTTON_LOC(1, 0)]),
    Comm_stats = wxStaticText:new(ServerFrame, 213, "0/0",
        [?BUTTON_SIZE(1), ?BUTTON_LOC(1.5, 0)]),


    wxStaticText:wrap(Title1, ?TILE_W),wxStaticText:wrap(Title2, ?TILE_W),wxStaticText:wrap(Title3, ?TILE_W),

    State = #state{mainGen = Gen, frame = ServerFrame, objs = #{server_conn => Conn_count, stats => Comm_stats}},

    % ObjsMap = Wx_ObjsMap#{timer => Timer},


    wxFrame:show(ServerFrame),
    {ServerFrame, State}.


handle_event(Event, State) ->
    ID = Event#wx.id,
    case ID of
        Other ->        io:format("Got event with ID=~p~n",[Other])
    end,
    
    {noreply, State}.

handle_info(Info, State)->
    io:format("Got mes:~p~n",[Info]),
    {noreply, State}.


probe(State) ->
    NerlGraph = State#state.nerlGraph,
    io:format("probing.....~n"),
    case NerlGraph of
        undefined ->
            Mes = "no graph to diplay mainServer! reopen graph screen",
            mainScreen:addInfo(State#state.mainGen, Mes); %State#state.ppid ! {addInfo, "no graph to diplay mainServer! reopen graph screen"};
        Graph -> 

            {Name, {Host, Port}} = digraph:vertex(NerlGraph, "mainServer"),
            hello_handler:http_request(Host, Port, "statistics", <<"getStatistics">>),
            StatisticsMap = hello_handler:http_request(Host, Port, "getStats", ""),
            io:format("got stats: ~p~n", [StatisticsMap])
    end
    .

deserialize({VL, EL, NL, B}) ->       
    DG = {digraph, V, E, N, B} = case B of 
       true -> digraph:new();
       false -> digraph:new([acyclic])
    end,
    ets:delete_all_objects(V),
    ets:delete_all_objects(E),
    ets:delete_all_objects(N),
    ets:insert(V, VL),
    ets:insert(E, EL),
    ets:insert(N, NL),
    DG.