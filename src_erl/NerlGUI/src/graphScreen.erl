
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
    try
        {ok, {_ResCode, _Headers, Body}} = httpc:request(get, {?MAINSERVER_URL++"/getGraph", []}, [], []),
        Devices = string:split(Body, "#", all),
        %NerlGraph = httpc:request(post, {?MAINSERVER_URL++"/getGraph", [], [], body}, [], []),
        io:format("got graph: ~p~n", [Body]),

        FileName = makeGraphIMG(Devices),
        receive _Any -> wait after 1000 -> done end, %wait for picture to process

        Image = wxBitmap:new(FileName, [{type, ?wxBITMAP_TYPE_PNG}]),
        _StaticIMG = wxStaticBitmap:new(GraphFrame, 101, Image, [?BUTTON_SIZE(4), ?BUTTON_LOC(0, 0)])

        catch Err:Er -> io:format("couldnt connect, got Err: ~p,~p~n", [Err, Er])
    end,

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
makeGraphIMG(DeviceList) ->
    %makeGraph(DeviceList),
    graphviz:graph("G"),
    graphviz:add_node("MainServer"),
    graphviz:add_node("ApiServer"),
    graphviz:add_edge("MainServer", "ApiServer"),
    FileName = "graph.png",
    makeGraphIMG(DeviceList, FileName).

makeGraphIMG([[]], FileName) -> makeGraphIMG([], FileName);
makeGraphIMG([], FileName) -> 
    graphviz:to_file(FileName, png),
    graphviz:delete(),
    FileName;
makeGraphIMG([Device|DeviceList], FileName) -> 
    {Name, IP, Port} = Device,
    graphviz:add_node(Name),
    graphviz:add_edge(Device, "MainServer"),
    makeGraphIMG(DeviceList, FileName).

%%%TODO: understand graph connections more, only routers connected? all connected? how to know what is device type?
% makeGraph(DeviceList) ->
%     G = digraph:new(),
%     PCs = groupByIP(DeviceList, #{}),
%     connectDevToRouter(PCs),
%     connectRouters(PCs)

%     .

% groupByIP([Device | Devices], Map) ->
%     {Name, IP, Port} = Device,
%     {DevicesInIP, OldMap} = maps:take(IP, Map),
%     NewMap = 
%         if DevicesInIP /= error ->
%             NewDevicesInIP = DevicesInIP ++ [{Name, Port}],
%             maps:put(IP, NewDevicesInIP, OldMap),
%         true -> maps:put(IP, {Name, Port}, OldMap),
            
%     groupByIP(Devices, NewMap).

handle_info(Info, State)->
    io:format("Got mes:~p~n",[Info]),
    {noreply, State}.