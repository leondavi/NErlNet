
-module(mainScreen).
-behaviour(wx_object).

-export([new/2, show/1, destroy/1]).  %% API
-export([init/1, handle_call/3, handle_event/2, handle_info/2]).

-include("gui_tools.hrl").

-define(GRAPH_ID, 1).
-define(SERVER_ID, 2).
-define(ROUTER_ID, 3).
-define(COMMS_ID, 4).
-define(JSON_ID, 5).
-define(DEVCONTROL_ID, 6).

%% Client API
new(Parent, _Msg) ->
    wx_object:start(?MODULE, [Parent, self()], []).

show(Frame) ->
    wx_object:call(Frame, show_modal).
destroy(Frame) ->
    wx_object:call(Frame, destroy).

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

    {StartFrame, #state{frame = StartFrame, objs=[NerlInfo]}}.


handle_call(show_modal, _From, State) ->
    wxFrame:show(State#state.frame),
    {reply, ok, State}.


handle_event(Event, State) ->
    Type = Event#wx.event,
    ID = Event#wx.id,
    %io:format("Handling event type=~p~n",[Type]),
    case Type of
        {wxClose, close_window} -> exit(normal);
        _Button ->
            case ID of
                ?GRAPH_ID ->        graphScreen:new(State#state.frame, self());
                ?SERVER_ID ->       serverScreen:new(State#state.frame, "");
                ?ROUTER_ID ->       routerScreen:new(State#state.frame, "");
                ?COMMS_ID ->        openGscreen;
                ?JSON_ID ->         openGscreen;
                ?DEVCONTROL_ID ->   devScreen:new(State#state.frame, "");
                Other ->        io:format("Got event with ID=~p~n",[Other])
            end
        end,
    
    {noreply, State}.

handle_info(Info, State)->
    io:format("Got mes: ~p~n",[Info]),
    NewState = 
    try 
        {Action, Data} = Info,
        case Action of
            addInfo -> 
                [NerlInfo] = State#state.objs,
                wxTextCtrl:appendText(NerlInfo, Data++"\n"),
                State;
            updateGraph ->
                State#state{nerlGraph = Data};
            getGraph ->
                
                Data ! State#state.nerlGraph,
                State
        end
        catch Err:Er -> io:format("bad action: ~p ~p", [Err,Er]), State
    end,

    {noreply, NewState}.