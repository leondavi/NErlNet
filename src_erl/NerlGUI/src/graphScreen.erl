
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
    GraphFrame = wxFrame:new(Parent, 100, "NerlNet device graph", [{size, {1080, 820}}, {pos, {0,0}}]),

    Font = wxFrame:getFont(GraphFrame),
    wxFont:setPointSize(Font, ?FONT_SIZE),
    wxFrame:setFont(GraphFrame, Font),
    {ok, {_ResCode, _Headers, Body}} = httpc:request(get, {?MAINSERVER_URL++"/getGraph", []}, [], []),
    io:format("got body: ~p~n", [Body]),
    DevicesInfo = string:split(Body, "#", all),
    Devices = [string:split(DeviceInfo, ",", all) || DeviceInfo <- DevicesInfo, DevicesInfo /=[[]]],
    Edges = lists:droplast(lists:last(Devices)),
    %NerlGraph = httpc:request(post, {?MAINSERVER_URL++"/getGraph", [], [], body}, [], []),
    io:format("got graph: ~p~n", [Devices]),
    DeviceList = lists:droplast(Devices),

    FileName = makeGraphIMG(DeviceList, Edges),
    receive _Any -> wait after 1000 -> done end, %wait for picture to process

    Image = wxBitmap:new(FileName, [{type, ?wxBITMAP_TYPE_PNG}]),
    _StaticIMG = wxStaticBitmap:new(GraphFrame, 101, Image, [?BUTTON_SIZE(4), ?BUTTON_LOC(0, 0)]),


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
%devices are: Name,IP,Port
makeGraphIMG(DeviceList, Edges) ->
    graphviz:graph("G"),
    createNodes(DeviceList),
    createEdges(Edges),
    FileName = "graph.png",
    graphviz:to_file(FileName, png),
    graphviz:delete(),
    FileName.

createNodes([])-> done;
createNodes([Device|DeviceList])->
    [Name, IP, Port] = Device,
    graphviz:add_node(Name),
    createNodes(DeviceList).

createEdges([]) -> done;
createEdges([Edge |Edges]) ->
    [V1, V2] = string:split(Edge, "-"),
    graphviz:add_edge(V1, V2),
    createEdges(Edges).


handle_info(Info, State)->
    io:format("Got mes:~p~n",[Info]),
    {noreply, State}.