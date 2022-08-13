
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

    Font = wxFrame:getFont(GraphFrame),
    wxFont:setPointSize(Font, ?FONT_SIZE),
    wxFrame:setFont(GraphFrame, Font),

    {ok, {_ResCode, _Headers, Body}} = httpc:request(get, {?MAINSERVER_URL++"/getGraph", []}, [], []),
    Devices = string:split(Body, ",", all),
    %NerlGraph = httpc:request(post, {?MAINSERVER_URL++"/getGraph", [], [], body}, [], []),
    io:format("got graph: ~p~n", [Body]),

    FileName = makeGraph(Devices),
    receive _Any -> wait after 1000 -> done end, %wait for picture to process

    Image = wxBitmap:new(FileName, [{type, ?wxBITMAP_TYPE_PNG}]),
    _StaticIMG = wxStaticBitmap:new(GraphFrame, 101, Image, [?BUTTON_SIZE(5), ?BUTTON_LOC(0, 0)]),

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

%%generates graph from list and returns the filename
makeGraph(DeviceList) ->
    graphviz:graph("G"),
    graphviz:add_node("MainServer"),
    graphviz:add_node("ApiServer"),
    graphviz:add_edge("MainServer", "ApiServer"),
    FileName = "graph.png",
    makeGraph(DeviceList, FileName).

makeGraph([[]], FileName) -> makeGraph([], FileName);
makeGraph([], FileName) -> 
    graphviz:to_file(FileName, png),
    graphviz:delete(),
    FileName;
makeGraph([Device|DeviceList], FileName) -> 
    graphviz:add_node(Device),
    graphviz:add_edge(Device, "MainServer"),
    makeGraph(DeviceList, FileName).


handle_info(Info, State)->
    io:format("Got mes:~p~n",[Info]),
    {noreply, State}.