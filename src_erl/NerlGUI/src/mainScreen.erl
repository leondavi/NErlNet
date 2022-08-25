
-module(mainScreen).
-behaviour(wx_object).

-export([new/2, show/1, destroy/1, setMainGenServer/1, addInfo/2, updateGraph/2, getGraph/1]).  %% API
-export([init/1, handle_call/3, handle_cast/2, handle_event/2, handle_info/2]).

-include("gui_tools.hrl").

-define(GRAPH_ID, 1).
-define(SERVER_ID, 2).
-define(ROUTER_ID, 3).
-define(COMMS_ID, 4).
-define(JSON_ID, 5).
-define(DEVCONTROL_ID, 6).

%%%%%%%%%%% Client API
new(Parent, _Msg) ->
    wx_object:start(?MODULE, [Parent, self()], []).

show(Frame) ->
    wx_object:call(Frame, show_modal).
destroy(Frame) ->
    wx_object:call(Frame, destroy).

setMainGenServer(Frame) ->
    wx_object:cast(Frame, {setGen, Frame}).

updateGraph(Frame, Graph) ->
    wx_object:cast(Frame, {updateGraph, Graph}).

addInfo(Frame, Mes) ->
    wx_object:cast(Frame, {addInfo, Mes}).

getGraph(Frame) ->
    wx_object:call(Frame, getGraph).

%%%%init the screen
init([Parent, PPID]) ->
    StartFrame = wxFrame:new(Parent, 0, "Main Screen", [{size, {1280, 720}}, {pos, {0,0}}]),

    Font = wxFrame:getFont(StartFrame),
    wxFont:setPointSize(Font, ?FONT_SIZE),
    wxFrame:setFont(StartFrame, Font),

    GraphButton = wxButton:new(StartFrame, 1, [{label, "Nerlnet Graph"}, ?BUTTON_SIZE(1), ?BUTTON_LOC(0,0)]),
    wxButton:connect(GraphButton, command_button_clicked, []),

    ServerStatsButton = wxButton:new(StartFrame, 2, [{label, "Main Server Status"}, ?BUTTON_SIZE(1), ?BUTTON_LOC(0,1)]), 
    %wxButton:connect(ServerStatsButton, command_button_clicked, [{callback, 
    %    fun(_Event, _Obj) -> spawn(serverScreen, init, []) end }]),
    wxButton:connect(ServerStatsButton, command_button_clicked, []),

    RouterStatsButton = wxButton:new(StartFrame, 3, [{label, "Router Stats"}, ?BUTTON_SIZE(1), ?BUTTON_LOC(0,2)]), 
    wxButton:connect(RouterStatsButton, command_button_clicked, []),

    CommsButton = wxButton:new(StartFrame, 4, [{label, "Communications\nPerformance"}, ?BUTTON_SIZE(1), ?BUTTON_LOC(1,0)]),
    wxButton:connect(CommsButton, command_button_clicked, []),

    JSONButton = wxButton:new(StartFrame, 5, [{label, "Generate\nnetwork JSON"}, ?BUTTON_SIZE(1), ?BUTTON_LOC(1,1)]),
    wxButton:connect(JSONButton, command_button_clicked, []),

    DevControlButton = wxButton:new(StartFrame, 6, [{label, "Device Control"}, ?BUTTON_SIZE(1), ?BUTTON_LOC(1,2)]),
    wxButton:connect(DevControlButton, command_button_clicked, []),

    wxStaticText:new(StartFrame, 7, "NerlNet Info:", [?BUTTON_SIZE(1), ?BUTTON_LOC(0, 3)]),

    NerlInfo = wxTextCtrl:new(StartFrame, 701, 
        [{size, {?TILE_W(2.5), ?TILE_H(3)}}, ?BUTTON_LOC(0.2, 3),
            {style, ?wxDEFAULT bor ?wxTE_MULTILINE}]),

    {StartFrame, #state{frame = StartFrame, objs=#{infoBox => NerlInfo}}}.

%%-----------------handle call / cast

handle_call(show_modal, _From, State) ->
    wxFrame:show(State#state.frame),
    {reply, ok, State};

handle_call(getGraph, _From, State) ->
    {reply, gui_tools:serialize(State#state.nerlGraph), State}.

handle_cast({passInfo, ScreenName, Info}, State) ->
    try
        ScreenGen = maps:get(ScreenName, State#state.objs),
        io:format("calling ~p with ~p~n", [ScreenGen, Info]),
        wx_object:cast(ScreenGen, {fromHandler,Info})
    catch badKey -> mainScreen:cast(State#state.mainGen, {addInfo, "Nowhere to display info"}) end,
    {noreply, State};

handle_cast({setGen, Frame}, State) ->
    {noreply, State#state{mainGen = Frame}};

handle_cast({updateGraph, Graph}, State) ->
    NerlGraph = gui_tools:deserialize(Graph),
    {noreply, State#state{nerlGraph = NerlGraph}};

handle_cast({addInfo, Mes}, State) ->
    ObjsMap = State#state.objs,
    NerlInfo = maps:get(infoBox, ObjsMap),
    LastLine = wxTextCtrl:getLineText(NerlInfo, wxTextCtrl:getNumberOfLines(NerlInfo)-2),
    %io:format("Last line is : ~p~n",[LastLine]),
    if LastLine /= Mes ->  wxTextCtrl:appendText(NerlInfo, Mes++"\n");
        true -> skip end,
    {noreply, State}.


handle_event(Event, State) ->
    Type = Event#wx.event,
    ID = Event#wx.id,
    io:format("Handling event type=~p~n",[Type]),
    ObjsMap = State#state.objs,
    NewState = 
    case Type of
        _Button ->
            case ID of
                ?GRAPH_ID ->       
                    io:format("starting graph screen~n"),
                    State#state{objs=ObjsMap#{graphScreen => graphScreen:new(State#state.frame, State#state.mainGen)}};
                ?SERVER_ID ->       
                    ServerScreen = serverScreen:new(State#state.frame, State#state.mainGen),
                    serverScreen:startProbe(ServerScreen),
                    State#state{objs=ObjsMap#{serverScreen => ServerScreen}};
                ?ROUTER_ID ->
                    RouterScreen = routerScreen:new(State#state.frame, State#state.mainGen),
                    routerScreen:startProbe(RouterScreen),
                    State#state{objs=ObjsMap#{routerScreen => RouterScreen}};
                ?COMMS_ID ->        State;
                ?JSON_ID ->         State;
                ?DEVCONTROL_ID ->   State#state{objs=ObjsMap#{devScreen => devScreen:new(State#state.frame, State#state.mainGen)}};
                Other ->        io:format("Got event with ID=~p~n",[Other]), State
            end
        end,
    
    {noreply, NewState}.

handle_info(Info, State)->
    io:format("Got mes: ~p~n",[Info]),
    {noreply, State}.