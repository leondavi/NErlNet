
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
    FileName = "graph.png",
    %FilePath = "out.png",
    %{ok, Data} = file:read_file(FileName),

    %IMGPanel = wxPanel:new(GraphFrame, 101, [?BUTTON_SIZE(2), ?BUTTON_LOC(0, 0)]),
    Image = wxBitmap:new(FileName, [{type, ?wxBITMAP_TYPE_PNG}]),
    _StaticIMG = wxStaticBitmap:new(GraphFrame, 101, Image, [?BUTTON_SIZE(2), ?BUTTON_LOC(0, 0)]),

    wxStaticText:new(GraphFrame, 102, "Graph of devices in experiment:",
            [?BUTTON_SIZE(1), ?BUTTON_LOC(0, 0)]),

    wxFrame:show(GraphFrame),
    {GraphFrame, #state{parent = parent, frame = GraphFrame}}.

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
    graphviz:add_node("W1"),
    graphviz:add_node("W2"),
    graphviz:add_node("W3"),
    graphviz:add_node("W4"),
    graphviz:add_node("mainServer"),
    graphviz:add_node("R1"),
    graphviz:add_node("R2"),
    graphviz:add_edge("W1", "R1"),
    graphviz:add_edge("W2", "R1"),
    graphviz:add_edge("mainServer", "R1"),
    graphviz:add_edge("W3", "R2"),
    graphviz:add_edge("W4", "R2"),
    graphviz:add_edge("R1", "R2"),

    GraphPath = "graph.png",
    graphviz:to_file(GraphPath, png),
    graphviz:delete(),
    GraphPath.


handle_info(Info, State)->
    io:format("Got mes:~p~n",[Info]),
    {noreply, State}.