
-module(graphScreen).
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
    GraphFrame = wxFrame:new(Parent, 100, "NerlNet device graph", [{size, {1080, 820}}, {pos, {0,0}}]),

    Font = wxFrame:getFont(GraphFrame),
    wxFont:setPointSize(Font, ?FONT_SIZE),
    wxFrame:setFont(GraphFrame, Font),

    Response = httpc:request(get, {?MAINSERVER_URL++"/getGraph", []}, [], []),

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

        Image = wxBitmap:new(FileName, [{type, ?wxBITMAP_TYPE_PNG}]),
        _StaticIMG = wxStaticBitmap:new(GraphFrame, 101, Image, [?BUTTON_SIZE(4), ?BUTTON_LOC(0, 0)]),

        receive _Any -> wait after 200 -> done end           %wait for picture to process
        ; 

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

handle_event(Event, State) ->
    ID = Event#wx.id,
    case ID of
        Other ->        io:format("Got event with ID=~p~n",[Other])
    end,
    
    {noreply, State}.


handle_info(Info, State)->
    io:format("Got mes:~p~n",[Info]),
    {noreply, State}.
