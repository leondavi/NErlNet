
-module(graphScreen).
-behaviour(wx_object).

-export([new/2, show/1, destroy/1, probe/1, startProbe/1]).  %% API
-export([init/1, handle_call/3,  handle_cast/2, handle_event/2, handle_info/2]).

-include("gui_tools.hrl").

-define(GRAPH_FILENAME, "graph.png").

new(Parent, Gen) ->
    wx_object:start(?MODULE, [Parent, Gen], []).

show(Frame) ->
    wx_object:call(Frame, show_modal).

destroy(Frame) ->
    wx_object:call(Frame, destroy).

startProbe(Frame) ->
    wx_object:cast(Frame, {startProbe, Frame}).

handle_call(show_modal, _From, State) ->
    wxFrame:show(State#state.frame),
    {reply, ok, State}.

handle_cast({startProbe, ThisGen}, State) ->
    NerlGraph = gui_tools:deserialize(mainScreen:getGraph(State#state.mainGen)),
    NewState = State#state{nerlGraph = NerlGraph, frame = ThisGen},
    {ok, _Timer} = timer:apply_interval(?PROBE_TIME, graphScreen, probe, [NewState]),
    {noreply, NewState};

handle_cast(refresh, State) ->
    receive _Any -> wait after 500 -> done end,           %wait for picture to process

    Canvas = maps:get(canvas, State#state.objs),

    wxBitmap:destroy(maps:get(image, State#state.objs)),
    Image = wxBitmap:new(?GRAPH_FILENAME, [{type, ?wxBITMAP_TYPE_PNG}]),
    %wxBitmap:loadFile(Image, ?GRAPH_FILENAME, [{type, ?wxBITMAP_TYPE_PNG}]),
    wxStaticBitmap:setBitmap(Canvas, Image),
    wxStaticBitmap:refresh(Canvas),

    ObjsMap = State#state.objs,
    {noreply, State#state{objs = ObjsMap#{image := Image}}}.

init([Parent, Gen])->
    GraphFrame = wxFrame:new(Parent, 100, "NerlNet device graph", [{size, {1080, 820}}, {pos, {0,0}}]),

    Font = wxFrame:getFont(GraphFrame),
    wxFont:setPointSize(Font, ?FONT_SIZE),
    wxFrame:setFont(GraphFrame, Font),

    wxStaticText:new(GraphFrame, 102, "Graph of devices in experiment:",
            [?BUTTON_SIZE(1), ?BUTTON_LOC(0, 0)]),

    Response = httpc:request(get, {gui_tools:getMainServerURL()++"/getGraph", []}, [], []),

    case Response of
    {ok, Data} ->
        {_ResCode, _Headers, Body} = Data,
        io:format("got body: ~p~n", [Body]),

        DevicesInfo = string:split(Body, "#", all),
        Devices = [string:split(DeviceInfo, ",", all) || DeviceInfo <- DevicesInfo, DevicesInfo /=[[]]],
        Edges = lists:droplast(lists:last(Devices)),

        io:format("got graph: ~p~n", [Devices]),
        DeviceList = lists:droplast(Devices),

        {FileName, G} = gui_tools:makeGraphIMG(DeviceList, Edges),

        mainScreen:updateGraph(Gen, gui_tools:serialize(G)),
        mainScreen:addInfo(Gen, "updated graph"),

        receive _Any -> wait after 500 -> done end,           %wait for picture to process

        Image = wxBitmap:new(FileName, [{type, ?wxBITMAP_TYPE_PNG}]),
        Canvas = wxStaticBitmap:new(GraphFrame, 101, Image, [?BUTTON_SIZE(4), ?BUTTON_LOC(0, 0)]),
        wxStaticBitmap:connect(Canvas, paint, [callback]),

        {GraphFrame, #state{mainGen = Gen, frame = GraphFrame, objs = #{image => Image, canvas => Canvas}}}; 

    {error, Err} ->
        {Reason, Trace} = Err,
        io:format("Err is: ~p~n",[Err]),
        Mes = atom_to_list(Reason),
        mainScreen:addInfo(Gen, Mes),
        {GraphFrame, #state{mainGen = Gen, frame = GraphFrame}}
    end.

handle_event(Event, State) ->
    ID = Event#wx.id,
    case ID of
        Other ->        io:format("Got event with ID=~p~n",[Other])
    end,
    
    {noreply, State}.


handle_info(Info, State)->
    io:format("Got mes:~p~n",[Info]),
    {noreply, State}.

probe(State) ->
    io:format("probing for graph.....~n"),
    Graph = State#state.nerlGraph,
    Verts = digraph:vertices(Graph)--["serverAPI", "mainServer", "nerlGUI"],
    Devices = [digraph:vertex(Graph, V) || V <- Verts],
    get_Dev_stats(State, Devices),

    wx_object:cast(State#state.frame, refresh).

get_Dev_stats(State, Devices) ->
    graphviz:digraph("G"),
    check_online(Devices),
    addEdges(State#state.nerlGraph),
    graphviz:to_file(?GRAPH_FILENAME, png),
    graphviz:delete().

%%pings every device and adds a red/green node accordingly
check_online([]) -> done;
check_online([Device|Rest])->
    {Name, {Host,Port}} = Device,
    Response = gui_tools:sendReq(Host, Port, "getState", ""),
    case Response of
    "" ->
        %%got some response, TODO: get actual state?
        graphviz:add_node(Name, "green");
    {error, Err} ->
        {Reason, Trace} = Err,
        %Mes = "Device "++Name++" unresponsive: "++atom_to_list(Reason),
        %mainScreen:addInfo(State#state.mainGen, Mes),
        graphviz:add_node(Name, "red")
    end,
    check_online(Rest).

addEdges(G)->
    Edges = [digraph:edge(G,E) || E <- digraph:edges(G)],
    [graphviz:add_edge(V1, V2) || {_E, V1, V2, _Label} <- Edges].

handle_sync_event(#wx{event = #wxPaint{}}, _wxObj, #state{objs=ObjsMap}) ->
    Canvas = maps:get(canvas, ObjsMap),
    Bitmap = maps:get(image, ObjsMap),
    DC = wxPaintDC:new(Canvas),
    redraw(DC, Bitmap),
    wxPaintDC:destroy(DC),
    ok.

redraw(DC, Bitmap) ->
    MemoryDC = wxMemoryDC:new(Bitmap),
    wxDC:blit(DC, {0,0},
	      {wxBitmap:getWidth(Bitmap), wxBitmap:getHeight(Bitmap)},
	      MemoryDC, {0,0}),
    wxMemoryDC:destroy(MemoryDC).