
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

    %TODO: wrap text
    _InfoBox = wxTextCtrl:new(ServerFrame, 201, 
        [{value, "CPU Usage of main server hosting device"},
            ?BUTTON_SIZE(1), ?BUTTON_LOC(0, 0)]),
    wxTextCtrl:new(ServerFrame, 202,
        [{value, "CPU Usage of main server hosting device"},
            ?BUTTON_SIZE(1), ?BUTTON_LOC(0, 1)]),
    wxTextCtrl:new(ServerFrame, 203,
        [{value, "Sent/Received messages"},
            ?BUTTON_SIZE(1), ?BUTTON_LOC(0, 2)]),
    wxTextCtrl:new(ServerFrame, 204, 
        [{value, "mainServer guardian (if applicable)"}, 
            ?BUTTON_SIZE(1), ?BUTTON_LOC(0, 3)]),

    wxFrame:show(ServerFrame).


handle_event(Event, State) ->
    ID = Event#wx.id,
    case ID of
        Other ->        io:format("Got event with ID=~p~n",[Other])
    end,
    
    {noreply, State}.

handle_info(Info, State)->
    io:format("Got mes:~p~n",[Info]),
    {noreply, State}.