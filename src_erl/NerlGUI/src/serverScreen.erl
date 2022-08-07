
-module(serverScreen).

-export([init/0]).

-define(PADDING_W, 10).
-define(PADDING_H, 20).
-define(BUTTON_W, 200).
-define(BUTTON_H, 200).
-define(BUTTON_SIZE(Mult), {size, {?BUTTON_W*Mult, ?BUTTON_H*Mult}}).
-define(BUTTON_LOC(Row, Col), 
    {pos, {(Col+1) * ?PADDING_H + Col * ?BUTTON_H, (Row+1) * ?PADDING_W + Row * ?BUTTON_W}}).

init()->
    GUI = wx:new(),
    ServerFrame = wxFrame:new(GUI, 100, "NerlNet Server", [{size, {1280, 720}}, {pos, {0,0}}]),


    %TODO: wrap text
    _InfoBox = wxTextCtrl:new(ServerFrame, 101, 
        [{value, "CPU Usage of main server hosting device"},
            ?BUTTON_SIZE(1), ?BUTTON_LOC(0, 0)]),
    wxTextCtrl:new(ServerFrame, 102,
        [{value, "CPU Usage of main server hosting device"},
            ?BUTTON_SIZE(1), ?BUTTON_LOC(0, 1)]),
    wxTextCtrl:new(ServerFrame, 103,
        [{value, "Sent/Received messages"},
            ?BUTTON_SIZE(1), ?BUTTON_LOC(0, 2)]),
    wxTextCtrl:new(ServerFrame, 104, 
        [{value, "mainServer guardia (if applicable)"}, 
            ?BUTTON_SIZE(1), ?BUTTON_LOC(0, 3)]),

    wxFrame:show(ServerFrame),
    loop(ServerFrame).


loop(Frame)->
    get_stats,
    loop(Frame).