
-module(routerScreen).
-behaviour(wx_object).

-export([new/2, show/1, destroy/1, probe/1, startProbe/1, updateText/2]).  %% API
-export([init/1, handle_call/3, handle_cast/2, handle_event/2, handle_info/2]).

-include("gui_tools.hrl").


new(Parent, Gen) ->
    wx_object:start(?MODULE, [Parent, Gen], []).

show(Frame) ->
    wx_object:call(Frame, show_modal).

destroy(Frame) ->
    wx_object:call(Frame, destroy).

startProbe(Frame) ->
    wx_object:cast(Frame, {startProbe, Frame}).

updateText(Frame, {ObjCode, Text}) ->
    wx_object:cast(Frame, {updateText, ObjCode, Text}).

handle_cast({startProbe, ThisGen}, State) ->
    NerlGraph = gui_tools:deserialize(mainScreen:getGraph(State#state.mainGen)),
    ObjsMap = init_labels(NerlGraph, State#state.frame),
    NewState = State#state{nerlGraph = NerlGraph, frame = ThisGen, objs = ObjsMap},
    {ok, _Timer} = timer:apply_interval(?PROBE_TIME, routerScreen, probe, [NewState]),
    {noreply, NewState};

handle_cast({fromHandler, Info}, State) ->
    [Name, Data] = string:split(Info, ","),
    %Info = [label=data;...]
    LabelDatas = string:split([Data], ";", all),
    updateRouterText(State, Name, LabelDatas),
    {noreply, State};

handle_cast({updateText, ObjCode, Text}, State) ->
    if Text /= "" ->
        Obj = maps:get(ObjCode, State#state.objs),
        wxStaticText:setLabel(Obj, Text);
    true -> skip end,
    {noreply, State}.

handle_call(show_modal, _From, State) ->
    wxFrame:show(State#state.frame),
    {reply, ok, State}.

init([Parent, Gen])->
    ServerFrame = wxFrame:new(Parent, 300, "NerlNet Routers", [{size, {1280, 720}}, {pos, {0,0}}]),

    Font = wxFrame:getFont(ServerFrame),
    wxFont:setPointSize(Font, 14),      %no ?FONT_SIZE
    wxFrame:setFont(ServerFrame, Font),

    %%init stat labels for 4 routers

    [wxStaticText:new(ServerFrame, ?wxID_ANY, "Router #"++integer_to_list(Num)++":",[?BUTTON_SIZE(1), ?BUTTON_LOC(0, Num)]) || Num <- lists:seq(1,4)],
    [wxStaticText:new(ServerFrame, ?wxID_ANY, "State/Mode:",[?BUTTON_SIZE(1), ?BUTTON_LOC(0.25, Num)]) || Num <- lists:seq(1,4)],
    [wxStaticText:new(ServerFrame, ?wxID_ANY, "Connections:",[?BUTTON_SIZE(1), ?BUTTON_LOC(0.5, Num)]) || Num <- lists:seq(1,4)],
    [wxStaticText:new(ServerFrame, ?wxID_ANY, "Sent/Recv messages:",[?BUTTON_SIZE(1), ?BUTTON_LOC(0.75, Num)]) || Num <- lists:seq(1,4)],

    % Names =         [wxStaticText:new(ServerFrame, ?wxID_ANY, "????",[?BUTTON_SIZE(1), ?BUTTON_LOC(0.1, Num)]) || Num <- lists:seq(1,4)],
    StateLabels =   [wxStaticText:new(ServerFrame, ?wxID_ANY, "ACTIVE/DOWN",[?BUTTON_SIZE(1), ?BUTTON_LOC(0.35, Num)]) || Num <- lists:seq(1,4)],
    % ConnList =      [wxStaticText:new(ServerFrame, ?wxID_ANY, "NONE",[?BUTTON_SIZE(1), ?BUTTON_LOC(0.6, Num)]) || Num <- lists:seq(1,4)],
    % CommStats =     [wxStaticText:new(ServerFrame, ?wxID_ANY, "0/0",[?BUTTON_SIZE(1), ?BUTTON_LOC(0.85, Num)]) || Num <- lists:seq(1,4)],

    % ObjsMap = #{routerName => Names,statesList => StateLabels, connList => ConnList, messStats => CommStats},

    %wxStaticText:wrap(Title1, ?TILE_W),
    
    wxFrame:show(ServerFrame),
    {ServerFrame, #state{mainGen = Gen, frame = ServerFrame}}.

init_labels(NerlGraph, Frame)->
    Verts = digraph:vertices(NerlGraph)--["serverAPI", "mainServer", "nerlGUI"],
    Routers = [digraph:vertex(NerlGraph, V) || V <- Verts, lists:member($r, V)],
    LabelMap = createLabelMap(Frame, Routers, #{}, 1).

createLabelMap(_Frame, [], Map, _Count)-> Map;
createLabelMap(Frame, [{Name, _Conn}|Routers], Map, Count)->
    wxStaticText:new(Frame, ?wxID_ANY, Name,[?BUTTON_SIZE(1), ?BUTTON_LOC(0.1, Count)]),
    ConnLabel = wxStaticText:new(Frame, ?wxID_ANY, "NONE",[?BUTTON_SIZE(1), ?BUTTON_LOC(0.6, Count)]),
    CommStats = wxStaticText:new(Frame, ?wxID_ANY, "0/0",[?BUTTON_SIZE(1), ?BUTTON_LOC(0.85, Count)]),
    NewMap = Map#{Name => #{connList => ConnLabel, messStats => CommStats}},
    createLabelMap(Frame, Routers, NewMap, Count+1).


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
    NerlGraph = State#state.nerlGraph,
    
    io:format("probing routers.....~n"),
    case NerlGraph of
        undefined ->
            Mes = "No graph initiated! Re-open graph screen....",
            mainScreen:addInfo(State#state.mainGen, Mes);
        Graph -> 
            Verts = digraph:vertices(NerlGraph)--["serverAPI", "mainServer", "nerlGUI"],
            Routers = [digraph:vertex(NerlGraph, V) || V <- Verts, lists:member($r, V)],
            io:format("routers are ~p~n", [Routers]),
            get_routers_stats(Routers, State#state.frame)
            
    end.

get_routers_stats([], _Frame) -> done;
get_routers_stats([Router | Routers], Frame)->
    io:format("probing ~p~n", [Router]),
    {Name, {Host, Port}} = Router,

    Mes = hello_handler:http_request(Host, Port, "getStats", ""),               %TODO replace with cast from hello_handler
    io:format("got stats: ~p~n", [Mes]),
    % [Mode, Conn, Comm] = string:split(Mes, ",", all),
    % routerScreen:updateText(Frame, {routerName, Name}),
    % routerScreen:updateText(Frame, {statesList, Mode}),
    % routerScreen:updateText(Frame, {connList, Conn}),
    % routerScreen:updateText(Frame, {messStats, Comm}),
    get_routers_stats(Routers, Frame).

updateRouterText(_State,_Name, [])-> done;
updateRouterText(State, Name, [Info|MoreInfo])->
    [Field, Data] = string:split(Info, "="),
    RouterLabels = maps:get(Name, State#state.objs),
    Obj = maps:get(list_to_atom(Field), RouterLabels),
    wxStaticText:setLabel(Obj, Data),
    updateRouterText(State, Name, MoreInfo).