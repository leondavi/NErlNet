-module(gui_tools).

-export([serialize/1, deserialize/1, makeGraphIMG/2]).


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