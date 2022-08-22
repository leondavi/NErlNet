
-module(routerScreen).
-behaviour(wx_object).

-export([new/2, show/1, destroy/1]).  %% API
-export([init/1, handle_call/3, handle_event/2, handle_info/2]).

-include("gui_tools.hrl").

new(Parent, Gen) ->
    wx_object:start(?MODULE, [Parent, Gen], []).

show(Frame) ->
    wx_object:call(Frame, show_modal).

destroy(Frame) ->
    wx_object:call(Frame, destroy).

handle_call(show_modal, _From, State) ->
    wxFrame:show(State#state.frame),
    {reply, ok, State}.

init([Parent, Gen])->
    ServerFrame = wxFrame:new(Parent, 300, "NerlNet Routers", [{size, {1280, 720}}, {pos, {0,0}}]),

    Font = wxFrame:getFont(ServerFrame),
    wxFont:setPointSize(Font, ?FONT_SIZE),
    wxFrame:setFont(ServerFrame, Font),

    % Title1 = wxStaticText:new(ServerFrame, 301, "Router #1:",
    %     [?BUTTON_SIZE(1), ?BUTTON_LOC(0, 0)]),
    % wxStaticText:new(ServerFrame, 311, "State/Mode:",
    %     [?BUTTON_SIZE(1), ?BUTTON_LOC(0.15, 0)]),
    % wxStaticText:new(ServerFrame, 331, "connected to:",
    %     [?BUTTON_SIZE(1), ?BUTTON_LOC(0.45, 0)]),
    % wxStaticText:new(ServerFrame, 321, "Sent/Recv messages:",
    %     [?BUTTON_SIZE(1), ?BUTTON_LOC(0.75, 0)]),

    %%init stat labels for 4 routers
    [wxStaticText:new(ServerFrame, 300 + Num, "Router #"++integer_to_list(Num)++":",[?BUTTON_SIZE(1), ?BUTTON_LOC(0, Num)]) || Num <- lists:seq(1,4)],
    [wxStaticText:new(ServerFrame, 300 + Num, "State/Mode:",[?BUTTON_SIZE(1), ?BUTTON_LOC(0.15, Num)]) || Num <- lists:seq(1,4)],
    [wxStaticText:new(ServerFrame, 300 + Num, "Connections:",[?BUTTON_SIZE(1), ?BUTTON_LOC(0.45, Num)]) || Num <- lists:seq(1,4)],
    [wxStaticText:new(ServerFrame, 300 + Num, "Sent/Recv messages:",[?BUTTON_SIZE(1), ?BUTTON_LOC(0.75, Num)]) || Num <- lists:seq(1,4)],


    %wxStaticText:wrap(Title1, ?TILE_W),
    
    wxFrame:show(ServerFrame),
    {ServerFrame, #state{mainGen = Gen, frame = ServerFrame}}.


handle_event(Event, State) ->
    ID = Event#wx.id,
    case ID of
        Other ->        io:format("Got event with ID=~p~n",[Other])
    end,
    
    {noreply, State}.

handle_info(Info, State)->
    io:format("Got mes:~p~n",[Info]),
    {noreply, State}.