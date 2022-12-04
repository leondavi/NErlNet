-module(gui_tools).

-export([serialize/1, deserialize/1, makeGraphIMG/2, getMainServerURL/0, sendReq/4]).


serialize(undefined) -> undefined;
serialize({digraph, V, E, N, B}) ->
    {ets:tab2list(V),
     ets:tab2list(E),
     ets:tab2list(N),
     B}.

deserialize(undefined) -> undefined;
deserialize({VL, EL, NL, B}) ->
    DG = {digraph, V, E, N, B} = case B of 
       true -> digraph:new();
       false -> digraph:new([acyclic])
    end,
    ets:delete_all_objects(V),
    ets:delete_all_objects(E),
    ets:delete_all_objects(N),
    ets:insert(V, VL),
    ets:insert(E, EL),
    ets:insert(N, NL),
    DG.


%%generates graph from list and returns the filename
%devices are: Name,IP,Port
makeGraphIMG(DeviceList, Edges) ->
    graphviz:digraph("G"),
    G = digraph:new(),
    createNodes(DeviceList, G),
    createEdges(Edges, G),
    FileName = "graph.png",
    graphviz:to_file(FileName, png),
    graphviz:delete(),
    {FileName, G}.

createNodes([], _G)-> done;
createNodes([Device|DeviceList], G)->
    [Name, IP, Port] = Device,
    graphviz:add_node(Name),
    digraph:add_vertex(G, Name, {IP, Port}),
    createNodes(DeviceList, G).

createEdges([], _G) -> done;
createEdges([Edge |Edges], G) ->
    [V1, V2] = string:split(Edge, "-"),
    graphviz:add_edge(V1, V2),
    digraph:add_edge(G, V1, V2),
    createEdges(Edges, G).

getMainServerURL() ->
   {ok, L} = inet:getif(),
   IP = tuple_to_list(element(1, hd(L))),
   A = lists:flatten(io_lib:format("~p", [IP])),
   Subbed = lists:sublist(A,2,length(A)-2),
   MainIP = lists:flatten(string:replace(Subbed,",",".",all)),
   Port = "8080",
   URL = "http://"++MainIP++":"++Port.

sendReq(Host, Port, Action, Body) ->
    URL = "http://" ++ Host ++ ":"++Port ++ "/" ++ Action,
    httpc:set_options([{proxy, {{Host, list_to_integer(Port)},[Host]}}]),
    Res = httpc:request(post,{URL, [],"application/x-www-form-urlencoded",Body}, [], []),
    case Res of
        {ok, Response} -> element(3, Response);
        {error, Err} -> {error, Err}
    end.