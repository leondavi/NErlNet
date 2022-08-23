
-module(serverScreen).
-behaviour(wx_object).

-export([new/2, show/1, destroy/1, probe/1, startProbe/1, updateText/2]).  %% API
-export([init/1, handle_call/3, handle_cast/2, handle_event/2, handle_info/2]).

-include("gui_tools.hrl").

new(Parent, Gen) ->
    wx_object:start(?MODULE, [Parent, Gen], []).

show(Frame) ->
    wx_object:call(Frame, show_modal).

startProbe(Frame) ->
    wx_object:cast(Frame, {startProbe, Frame}).

updateText(Frame, {ObjCode, Text}) ->
    wx_object:cast(Frame, {updateText, ObjCode, Text}).

destroy(Frame) ->
    wx_object:call(Frame, destroy).

handle_call(show_modal, _From, State) ->
    wxFrame:show(State#state.frame),
    {reply, ok, State}.

handle_cast({startProbe, ThisGen}, State) ->
    NerlGraph = gui_tools:deserialize(mainScreen:getGraph(State#state.mainGen)),
    NewState = State#state{nerlGraph = NerlGraph, frame = ThisGen},
    {ok, _Timer} = timer:apply_interval(?PROBE_TIME, serverScreen, probe, [NewState]),
    {noreply, NewState};

handle_cast({updateText, ObjCode, Text}, State) ->
    if Text /= "" ->
        Obj = maps:get(ObjCode, State#state.objs),
        wxStaticText:setLabel(Obj, Text);
    true -> skip end,
    {noreply, State}.

init([Parent, Gen])->
    ServerFrame = wxFrame:new(Parent, 200, "NerlNet Server", [{size, {920, 480}}, {pos, {10,10}}]),

    Font = wxFrame:getFont(ServerFrame),
    wxFont:setPointSize(Font, ?FONT_SIZE),
    wxFrame:setFont(ServerFrame, Font),

    Title1 = wxStaticText:new(ServerFrame, 201, "State (Mode) of Mainserver",
        [?BUTTON_SIZE(1), ?BUTTON_LOC(0, 0)]),
    Mode_text = wxStaticText:new(ServerFrame, 211, "???",
        [?BUTTON_SIZE(1), ?BUTTON_LOC(0.5, 0)]),

    Title2 = wxStaticText:new(ServerFrame, 202,"Number of active connections:",
        [?BUTTON_SIZE(1), ?BUTTON_LOC(0, 2)]),
    Connected_text = wxStaticText:new(ServerFrame, 212, "X",
        [?BUTTON_SIZE(1), ?BUTTON_LOC(0.5, 2)]),

    Title3 = wxStaticText:new(ServerFrame, 203, "Sent/Received messages:",
        [?BUTTON_SIZE(1), ?BUTTON_LOC(1, 0)]),
    MessStats_text = wxStaticText:new(ServerFrame, 213, "0/0",
        [?BUTTON_SIZE(1), ?BUTTON_LOC(1.5, 0)]),


    wxStaticText:wrap(Title1, ?TILE_W),wxStaticText:wrap(Title2, ?TILE_W),wxStaticText:wrap(Title3, ?TILE_W),

    State = #state{mainGen = Gen, frame = ServerFrame, objs = #{mode => Mode_text,conn => Connected_text, stats => MessStats_text}},

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
    
    io:format("probing mainserver.....~n"),
    case NerlGraph of
        undefined ->
            Mes = "No graph initiated! Re-open graph screen....",
            mainScreen:addInfo(State#state.mainGen, Mes);
        Graph -> 

            {Name, {Host, Port}} = digraph:vertex(NerlGraph, "mainServer"),
            %hello_handler:http_request(Host, Port, "statistics", <<"getStatistics">>),
            %StatisticsMap = hello_handler:http_request(Host, Port, "getStats", ""),
            Mes = hello_handler:http_request(Host, Port, "getStats", ""),
            [Mode, Count, Conn] = string:split(Mes, ",", all),
            io:format("got stats: ~p~n", [Mes]),

            serverScreen:updateText(State#state.frame, {mode, Mode}),
            serverScreen:updateText(State#state.frame, {stats, Count}),
            serverScreen:updateText(State#state.frame, {conn, Conn})
    end.
