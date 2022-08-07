
-module(graphScreen).
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
    GraphFrame = wxFrame:new(Parent, 100, "NerlNet device graph", [{size, {1280, 720}}, {pos, {0,0}}]),

    %FileName = makeGraph(),
    FilePath = "out.png",
    %{ok, Data} = file:read_file(FilePath),

    Panel = wxPanel:new(GraphFrame, [?BUTTON_SIZE(2)]),
    Vbox = wxBoxSizer:new(?wxHORIZONTAL),
    wxSizer:add(Vbox, Panel, [{proportion, 1}, {flag, ?wxEXPAND}]),
    Image = wxBitmap:new(FilePath, [{type, ?wxBITMAP_TYPE_PNG}]),
    F = fun(I, _) -> redraw(Image,I) end,
    wxPanel:connect(Panel, paint, [{callback,F}]),

    _InfoBox = wxTextCtrl:new(GraphFrame, 102, 
        [{value, "CPU Usage of main server hosting device"},
            ?BUTTON_SIZE(1), ?BUTTON_LOC(0, 2)]),

    wxFrame:show(GraphFrame).

redraw(Image, #wx{obj=Panel}) ->
    DC = wxPaintDC:new(Panel),
    wxDC:drawBitmap(DC,Image,?BUTTON_LOC(0, 0)).

handle_event(Event, State) ->
    ID = Event#wx.id,
    case ID of
        Other ->        io:format("Got event with ID=~p~n",[Other])
    end,
    
    {noreply, State}.

makeGraph() ->
    %make the digraph
    %convert 
    graphviz:graph("G"),
    graphviz:add_vertex("G", "w1"),
    graphviz:add_vertex("G", "w2"),
    graphviz:add_vertex("G", "w3"),
    graphviz:add_vertex("G", "w4"),
    graphviz:add_vertex("G", "mainServer"),
    graphviz:add_vertex("G", "r1"),
    graphviz:add_vertex("G", "r2"),
    graphviz:add_edge("G", "w1", "r1"),
    graphviz:add_edge("G", "w2", "r1"),
    graphviz:add_edge("G", "mainServer", "r1"),
    graphviz:add_edge("G", "w3", "r2"),
    graphviz:add_edge("G", "w4", "r2"),
    graphviz:add_edge("G", "r1", "r2"),

    GraphPath = "graph.jpg",
    graphviz:to_file(GraphPath, jpg),
    GraphPath.


handle_info(Info, State)->
    io:format("Got mes:~p~n",[Info]),
    {noreply, State}.