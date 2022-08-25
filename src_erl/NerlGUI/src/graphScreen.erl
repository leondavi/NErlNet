
-module(graphScreen).
-behaviour(wx_object).

-export([new/2, show/1, destroy/1, probe/1, startProbe/1]).  %% API
-export([init/1, handle_call/3,  handle_cast/2, handle_event/2, handle_info/2]).

-include("gui_tools.hrl").

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
    {ok, _Timer} = timer:apply_interval(?PROBE_TIME, graphScreen, probe, [State]),
    {noreply, State}.

init([Parent, Gen])->
    GraphFrame = wxFrame:new(Parent, 100, "NerlNet device graph", [{size, {1080, 820}}, {pos, {0,0}}]),

    Font = wxFrame:getFont(GraphFrame),
    wxFont:setPointSize(Font, ?FONT_SIZE),
    wxFrame:setFont(GraphFrame, Font),

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
        _StaticIMG = wxStaticBitmap:new(GraphFrame, 101, Image, [?BUTTON_SIZE(4), ?BUTTON_LOC(0, 0)]);

    {error, Err} ->
        {Reason, Trace} = Err,
        io:format("Err is: ~p~n",[Err]),
        Mes = atom_to_list(Reason),
        mainScreen:addInfo(Gen, Mes)
    end,


    wxStaticText:new(GraphFrame, 102, "Graph of devices in experiment:",
            [?BUTTON_SIZE(1), ?BUTTON_LOC(0, 0)]),

    wxFrame:show(GraphFrame),
    {GraphFrame, #state{mainGen = Gen, frame = GraphFrame}}.
    %{GraphFrame, #state{mainGen = Gen, frame = GraphFrame, objs = #{image => Image, staticIMG => StaticIMG}}}.

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
    MainServerURL = gui_tools:getMainServerURL(),
    Response = httpc:request(get, {MainServerURL++"/getGraph", []}, [], []),
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

        mainScreen:updateGraph(State#state.mainGen, gui_tools:serialize(G)),
        mainScreen:addInfo(State#state.mainGen, "updated graph"),

        receive _Any -> wait after 500 -> done end,           %wait for picture to process

        NewImage = wxBitmap:loadFile(maps:get(image, State#state.objs), FileName, [{type, ?wxBITMAP_TYPE_PNG}]),
        _StaticIMG = wxStaticBitmap:setBitmap(maps:get(staticIMG, State#state.objs), NewImage); 

    {error, Err} ->
        {Reason, Trace} = Err,
        io:format("Err is: ~p~n",[Err]),
        Mes = atom_to_list(Reason),
        mainScreen:addInfo(State#state.mainGen, Mes)

    end.