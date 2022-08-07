
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

init([Parent, _Str]) ->
    StartFrame = wxFrame:new(Parent, 0, "Main Screen", [{size, {1280, 720}}, {pos, {0,0}}]),

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

    JSONButton = wxButton:new(StartFrame, 5, [{label, "Generate network JSON"}, ?BUTTON_SIZE(1), ?BUTTON_LOC(1,1)]),
    wxButton:connect(JSONButton, command_button_clicked, []),

    DevControlButton = wxButton:new(StartFrame, 6, [{label, "Device Control"}, ?BUTTON_SIZE(1), ?BUTTON_LOC(1,2)]),
    wxButton:connect(DevControlButton, command_button_clicked, []),


    _InfoBox = wxTextCtrl:new(StartFrame, 7, [{value, "NerlNet Info"}, ?BUTTON_SIZE(2), ?BUTTON_LOC(0, 3)]),

    wxFrame:connect(StartFrame, close_window),
    wxFrame:show(StartFrame),
    {StartFrame, #state{parent = Parent, frame = StartFrame}}.


handle_call(show_modal, _From, State) ->
    wxFrame:show(State#state.frame),
    {reply, ok, State}.

handle_event(Event, State) ->
    Type = Event#wx.event,
    ID = Event#wx.id,
    %io:format("Handling event type=~p~n",[Type]),
    case Type of
        {wxClose, close_window} -> exit(normal);    %destroy(State#state.frame);
        _Button ->
            case ID of
                ?GRAPH_ID ->        openGscreen;
                ?SERVER_ID ->       serverScreen:new(State#state.frame, "");
                ?COMMS_ID ->        openGscreen;
                ?JSON_ID ->         openGscreen;
                ?DEVCONTROL_ID ->   openGscreen;
                ?ROUTER_ID ->       openGscreen;
                Other ->        io:format("Got event with ID=~p~n",[Other])
            end
        end,
    
    {noreply, State}.

handle_info(Info, State)->
    io:format("Got mes:~p~n",[Info]),
    {noreply, State}.

% loop(Frame)->
%     receive
%         {wx, _ID, _Ref, _Opt, Command}  ->
%             CMDString = element(2, Command),
%             case CMDString of
%                 close_window ->             wx:destroy(), exit(normal);
%                 Other ->                    io:format("bad event: ID=~p~n", [Other])
%             end;

%         Other -> io:format("Got other mes:~p~n", [Other])
%     end,
%     loop(Frame).
