
-module(serverScreen).
-behaviour(wx_object).

-export([new/2, show/1, destroy/1]).  %% API
-export([init/1, handle_call/3, handle_event/2, handle_info/2]).

-include("gui_tools.hrl").

new(Parent, _Msg) ->
    wx_object:start(?MODULE, [Parent, self()], []).

show(Frame) ->
    wx_object:call(Frame, show_modal).

destroy(Frame) ->
    wx_object:call(Frame, destroy).

handle_call(show_modal, _From, State) ->
    wxFrame:show(State#state.frame),
    {reply, ok, State}.

init([Parent, _Str])->
    ServerFrame = wxFrame:new(Parent, 200, "NerlNet Server", [{size, {1280, 720}}, {pos, {0,0}}]),

    Title1 = wxStaticText:new(ServerFrame, 201, "CPU Usage of main server hosting device:",
        [?BUTTON_SIZE(1), ?BUTTON_LOC(0, 0)]),
    wxStaticText:new(ServerFrame, 211, "0%",
        [?BUTTON_SIZE(1), ?BUTTON_LOC(0.5, 0)]),

    Title2 = wxStaticText:new(ServerFrame, 202,"Number of active connections:",
        [?BUTTON_SIZE(1), ?BUTTON_LOC(0, 2)]),
    wxStaticText:new(ServerFrame, 212, "0",
        [?BUTTON_SIZE(1), ?BUTTON_LOC(0.5, 2)]),

    Title3 = wxStaticText:new(ServerFrame, 203, "Sent/Received messages:",
        [?BUTTON_SIZE(1), ?BUTTON_LOC(1, 0)]),
    wxStaticText:new(ServerFrame, 213, "0/0",
        [?BUTTON_SIZE(1), ?BUTTON_LOC(1.5, 0)]),

    Title4 = wxStaticText:new(ServerFrame, 204, "mainServer guardian (if applicable):",
        [?BUTTON_SIZE(1), ?BUTTON_LOC(1, 2)]),
    wxStaticText:new(ServerFrame, 214, "no",
        [?BUTTON_SIZE(1), ?BUTTON_LOC(1.5, 2)]),

    wxStaticText:wrap(Title1, ?TILE_W),wxStaticText:wrap(Title2, ?TILE_W),wxStaticText:wrap(Title3, ?TILE_W),wxStaticText:wrap(Title4, ?TILE_W),

    wxFrame:show(ServerFrame),
    {ServerFrame, #state{parent = parent, frame = ServerFrame}}.


handle_event(Event, State) ->
    ID = Event#wx.id,
    case ID of
        Other ->        io:format("Got event with ID=~p~n",[Other])
    end,
    
    {noreply, State}.

handle_info(Info, State)->
    io:format("Got mes:~p~n",[Info]),
    {noreply, State}.