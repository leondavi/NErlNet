
-module(devScreen).
-behaviour(wx_object).

-export([new/2, show/1, destroy/1]).  %% API
-export([init/1, handle_call/3, handle_cast/2, handle_event/2, handle_info/2]).

-define(SERVER_ID, 711).

-include("gui_tools.hrl").

%% Client API
new(Parent, Gen) ->
    wx_object:start(?MODULE, [Parent, Gen], []).

show(Frame) ->
    wx_object:call(Frame, show_modal).

destroy(Frame) ->
    wx_object:call(Frame, destroy).

init([Parent, Gen]) ->
    DevFrame = wxFrame:new(Parent, 700, "Main Screen", [{size, {1280, 720}}, {pos, {0,0}}]),

    Font = wxFrame:getFont(DevFrame),
    wxFont:setPointSize(Font, ?FONT_SIZE),
    wxFrame:setFont(DevFrame, Font),

    wxStaticText:new(DevFrame, 710, "MainServer:", [?BUTTON_SIZE(1), ?BUTTON_LOC(0, 0)]),

    ServerStatsButton = wxButton:new(DevFrame, 711, [{label, "Main Server Status"}, ?BUTTON_SIZE(1), ?BUTTON_LOC(0.1,0)]), 
    wxButton:connect(ServerStatsButton, command_button_clicked, []),
    io:format("ServerButton is:, ~p~n", [ServerStatsButton]),

    %PPID ! {getGraph, self()},

    wxStaticText:new(DevFrame, 720, "Routers:", [?BUTTON_SIZE(1), ?BUTTON_LOC(0, 1)]),
    %add button for each router

    wxStaticText:new(DevFrame, 730, "Clients:", [?BUTTON_SIZE(1), ?BUTTON_LOC(0, 2)]),
    %add button for each router

    wxStaticText:new(DevFrame, 740, "Sources:", [?BUTTON_SIZE(1), ?BUTTON_LOC(0, 3)]),
    %add button for each router

    {DevFrame, #state{mainGen = Gen, frame = DevFrame}}.


handle_call(show_modal, _From, State) ->
    wxFrame:show(State#state.frame),
    {reply, ok, State}.

handle_cast({updateGraph, Graph}, State) ->
    NerlGraph = gui_tools:deserialize(Graph),
    {noreply, State#state{nerlGraph = NerlGraph}}.

handle_event(Event, State) ->
    Type = Event#wx.event,
    ID = Event#wx.id,
    %io:format("Handling event type=~p~n",[Type]),
    case Type of
        {wxClose, close_window} -> exit(normal);    %destroy(State#state.frame);
        _Button ->
            case ID of
                ?SERVER_ID ->   serverScreen:new(State#state.frame, "");
                Other ->        io:format("Got event with ID=~p~n",[Other])
            end
        end,
    
    {noreply, State}.

handle_info(Info, State)->
    io:format("Got mes: ~p~n",[Info]),
    {Action, Data} = Info,
    NewState = 
    case Action of
        show -> wxFrame:show(State#state.frame), State;
        graphObj -> 
            NerlGraph = gui_tools:deserialize(Data),
            add_graph_buttons(State#state.frame, NerlGraph),
            self() ! {show, self()},
            State#state{nerlGraph = NerlGraph}
    end,
    {noreply, NewState}.

add_graph_buttons(Frame, NerlGraph)->
    Verts = digraph:vertices(NerlGraph)--["serverAPI, mainServer", "nerlGUI"],

    Routers = [digraph:vertex(NerlGraph, V) || V <- Verts, lists:member($r, V)],
    Clients = [digraph:vertex(NerlGraph, V) || V <- Verts, lists:member($c, V)],
    Sources = [digraph:vertex(NerlGraph, V) || V <- Verts, lists:member($s, V)],
    % io:format("Rbuttons are: ~p", [RButtons]),
    % io:format("Wbuttons are: ~p", [CButtons]),
    % io:format("Sbuttons are: ~p", [SButtons]),

    % Id = hd(lists:reverse(Name))-48
    
    RButtons = [wxButton:new(Frame, 720 + hd(lists:reverse(Name))-48, [{label, Name}, ?BUTTON_SIZE(1), ?BUTTON_LOC(0.1 + hd(lists:reverse(Name))-48,1)]) || {Name, Label} <- Routers],
    CButtons = [wxButton:new(Frame, 730 + hd(lists:reverse(Name))-48, [{label, Name}, ?BUTTON_SIZE(1), ?BUTTON_LOC(0.1 + hd(lists:reverse(Name))-48,2)]) || {Name, Label} <- Clients],
    SButtons = [wxButton:new(Frame, 740 + hd(lists:reverse(Name))-48, [{label, Name}, ?BUTTON_SIZE(1), ?BUTTON_LOC(0.1 + hd(lists:reverse(Name))-48,3)]) || {Name, Label} <- Sources].
    
