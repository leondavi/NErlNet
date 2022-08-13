
-module(devScreen).
-behaviour(wx_object).

-export([new/2, show/1, destroy/1]).  %% API
-export([init/1, handle_call/3, handle_event/2, handle_info/2]).

-define(SERVER_ID, 711).

-include("gui_tools.hrl").

%% Client API
new(Parent, _Msg) ->
    wx_object:start(?MODULE, [Parent, self()], []).

show(Frame) ->
    wx_object:call(Frame, show_modal).

destroy(Frame) ->
    wx_object:call(Frame, destroy).

init([Parent, _Str]) ->
    StartFrame = wxFrame:new(Parent, 700, "Main Screen", [{size, {1280, 720}}, {pos, {0,0}}]),

    Font = wxFrame:getFont(StartFrame),
    wxFont:setPointSize(Font, ?FONT_SIZE),
    wxFrame:setFont(StartFrame, Font),

    wxStaticText:new(StartFrame, 701, "MainServer:", [?BUTTON_SIZE(1), ?BUTTON_LOC(0, 0)]),

    ServerStatsButton = wxButton:new(StartFrame, 711, [{label, "Main Server Status"}, ?BUTTON_SIZE(1), ?BUTTON_LOC(0.1,0)]), 
    wxButton:connect(ServerStatsButton, command_button_clicked, []),

    wxStaticText:new(StartFrame, 702, "Routers:", [?BUTTON_SIZE(1), ?BUTTON_LOC(0, 1)]),
    %add button for each router

    wxStaticText:new(StartFrame, 703, "Workers:", [?BUTTON_SIZE(1), ?BUTTON_LOC(0, 2)]),
    %add button for each router

    wxStaticText:new(StartFrame, 704, "Sources:", [?BUTTON_SIZE(1), ?BUTTON_LOC(0, 3)]),
    %add button for each router

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
                ?SERVER_ID ->       serverScreen:new(State#state.frame, "");
                Other ->        io:format("Got event with ID=~p~n",[Other])
            end
        end,
    
    {noreply, State}.

handle_info(Info, State)->
    io:format("Got mes:~p~n",[Info]),
    {noreply, State}.


add_dev_button(_Frame, _Dev)->
    todo.