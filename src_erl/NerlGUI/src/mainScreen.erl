
-module(mainScreen).
-behaviour(wx_object).

-export([new/2, show/1, destroy/1]).  %% API
-export([init/1, handle_call/3, handle_event/2]).

-record(wx, {id,     %% Integer Identity of object.
             obj,    %% Object reference that was used in the connect call.
             userData, %% User data specified in the connect call.
             event}).%% The event record 
-record(state, {frame}).

-define(PADDING_W, 10).
-define(PADDING_H, 20).
-define(BUTTON_W, 200).
-define(BUTTON_H, 200).
-define(BUTTON_SIZE(Mult), {size, {?BUTTON_W*Mult, ?BUTTON_H*Mult}}).
-define(BUTTON_LOC(Row, Col), 
    {pos, {(Col+1) * ?PADDING_H + Col * ?BUTTON_H, (Row+1) * ?PADDING_W + Row * ?BUTTON_W}}).


%% Client API
new(Parent, _Msg) ->
    wx_object:start(?MODULE, [Parent, self()], []).%self was ID?

show(Frame) ->
    wx_object:call(Frame, show_modal).

destroy(Frame) ->
    wx_object:call(Frame, destroy).

init([Parent, _Str]) ->
    StartFrame = wxFrame:new(Parent, 0, "Main Screen", [{size, {1280, 720}}, {pos, {0,0}}]),

    GraphButton = wxButton:new(StartFrame, 1, [{label, "Nerlnet Graph"}, ?BUTTON_SIZE(1), ?BUTTON_LOC(0,0)]),
    wxButton:connect(GraphButton, command_button_clicked, []),

    ServerStatsButton = wxButton:new(StartFrame, 2, [{label, "Main Server Status"}, ?BUTTON_SIZE(1), ?BUTTON_LOC(0,1)]), 
    wxButton:connect(ServerStatsButton, command_button_clicked, [{callback, 
        fun(_Event, _Obj) -> spawn(serverScreen, init, []) end }]),

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
    {StartFrame, #state{frame = StartFrame}}.


handle_call(show, _From, State) ->
    wxFrame:show(State#state.frame),
    {reply, ok, State}.

handle_event(#wx{}, State) ->
    io:format("Users clicked button~n",[]),
    {noreply, State}.

loop(Frame)->
    receive
        {wx, _ID, _Ref, _Opt, Command}  ->
            CMDString = element(2, Command),
            case CMDString of
                close_window ->             wx:destroy(), exit(normal);
                Other ->                    io:format("bad event: ID=~p~n", [Other])
            end;

        Other -> io:format("Got other mes:~p~n", [Other])
    end,
    loop(Frame).
