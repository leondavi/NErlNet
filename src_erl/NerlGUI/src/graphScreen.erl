
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

init([Parent, PPID])->
    GraphFrame = wxFrame:new(Parent, 100, "NerlNet device graph", [{size, {1080, 820}}, {pos, {0,0}}]),

    Font = wxFrame:getFont(GraphFrame),
    wxFont:setPointSize(Font, ?FONT_SIZE),
    wxFrame:setFont(GraphFrame, Font),
    Response =
    try
        httpc:request(get, {?MAINSERVER_URL++"/getGraph", []}, [], [])
    catch Err:Er -> 
        {Reason, Trace} = Er,
        io:format("Err is: ~p",[Er]),
        Mes = atom_to_list(Reason),
        PPID ! {addInfo, "Graph error: "++ Mes},
        bad_response
    end,
    if Response /= bad_response ->
        {ok, {_ResCode, _Headers, Body}} = Response,
        io:format("got body: ~p~n", [Body]),

        DevicesInfo = string:split(Body, "#", all),
        Devices = [string:split(DeviceInfo, ",", all) || DeviceInfo <- DevicesInfo, DevicesInfo /=[[]]],
        Edges = lists:droplast(lists:last(Devices)),

        io:format("got graph: ~p~n", [Devices]),
        DeviceList = lists:droplast(Devices),

        {FileName, G} = makeGraphIMG(DeviceList, Edges),        %%save G globaly somewhere

        PPID ! {updateGraph, G},
        PPID ! {addInfo, "updated graph"},

        Image = wxBitmap:new(FileName, [{type, ?wxBITMAP_TYPE_PNG}]),
        _StaticIMG = wxStaticBitmap:new(GraphFrame, 101, Image, [?BUTTON_SIZE(4), ?BUTTON_LOC(0, 0)])
    true-> skip end,


    receive _Any -> wait after 500 -> done end, %wait for picture to process

    wxStaticText:new(GraphFrame, 102, "Graph of devices in experiment:",
            [?BUTTON_SIZE(1), ?BUTTON_LOC(0, 0)]),

    wxFrame:show(GraphFrame),
    {GraphFrame, #state{ppid = PPID, frame = GraphFrame}}.

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
    G = digraph:new(),
    createNodes(DeviceList, G),
    createEdges(Edges, G),
    FileName = "graph.png",
    graphviz:to_file(FileName, png),
    graphviz:delete(),
    {FileName, G}.

createNodes([], G)-> done;
createNodes([Device|DeviceList], G)->
    [Name, IP, Port] = Device,
    graphviz:add_node(Name),
    digraph:add_vertex(G, Name, {IP, Port}),
    createNodes(DeviceList, G).

createEdges([], G) -> done;
createEdges([Edge |Edges], G) ->
    [V1, V2] = string:split(Edge, "-"),
    graphviz:add_edge(V1, V2),
    digraph:add_edge(G, V1, V2),
    createEdges(Edges, G).


handle_info(Info, State)->
    io:format("Got mes:~p~n",[Info]),
    {noreply, State}.