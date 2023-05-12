-module(nerl_tools).

-export([start_connection/1, http_request/4, getHostPort/5, getShortPath/3, string_to_list_int/1]).

start_connection([])->ok;
start_connection([{_ServerName,{Host, Port}}|Tail]) ->
  httpc:set_options([{proxy, {{Host, Port},[Host]}}]),
  start_connection(Tail).

%% send message between entities
http_request(Host, Port,Path, Body)->
  URL = "http://" ++ Host ++ ":"++integer_to_list(Port) ++ "/" ++ Path,
  httpc:set_options([{proxy, {{Host, Port},[Host]}}]),
  httpc:request(post,{URL, [],"application/x-www-form-urlencoded",Body}, [], []).

getHostPort([],_WorkersMap,_NerlnetGraph,_MyName,Ret)-> Ret;
getHostPort([WorkerName|WorkersNames],WorkersMap,NerlnetGraph,MyName,Ret)->
  ClientName = maps:get(list_to_atom(WorkerName),WorkersMap),
  {RouterHost,RouterPort} = getShortPath(MyName,ClientName,NerlnetGraph),
  %{RouterHost,RouterPort} = maps:get(ClientName,PortMap),
  getHostPort(WorkersNames,WorkersMap, NerlnetGraph,MyName,Ret++[{ClientName,WorkerName,RouterHost,RouterPort}]).

getShortPath(From,To,NerlnetGraph) when is_atom(To)->  getShortPath(From,atom_to_list(To),NerlnetGraph);
getShortPath(From,To,NerlnetGraph) -> 
	First = lists:nth(2,digraph:get_short_path(NerlnetGraph,From,To)),
	{_First,{Host,Port}} = digraph:vertex(NerlnetGraph,First),
  {Host,Port}.
	

%%return list of integer from string of lists of strings - "[2,2,2]" -> [2,2,2]
string_to_list_int(Binary) ->
  String = binary_to_list(Binary),
  NoParenthesis = lists:sublist(String,2,length(String)-2),
  Splitted = re:split(NoParenthesis,",",[{return,list}]),
  [list_to_integer(X)||X<-Splitted].