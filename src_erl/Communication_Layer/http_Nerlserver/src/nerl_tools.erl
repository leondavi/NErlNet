-module(nerl_tools).

-include("nerl_tools.hrl").

-export([setup_logger/1, string_format/2]).
-export([http_request/4, getHostPort/5, getShortPath/3]).
-export([string_to_list_int/1, deleteOldJson/1]).
-export([multipart/2, read_all_data/2]).
-export([getdeviceIP/0]).
-export([list_to_numeric/1]).

setup_logger(Module) ->
  logger:add_handler(Module, Module, #{}), 
  logger:set_module_level(Module, all).

% start_connection([])->ok;
% start_connection([{_ServerName,{Host, Port}}|Tail]) ->
%   httpc:set_options([{proxy, {{Host, Port},[Host]}}]),
%   start_connection(Tail).

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


% returns {FullReq, Data} / {FullReq, [fileReady]}
multipart(Req0, Data) ->
  case cowboy_req:read_part(Req0) of
      {ok, Headers, Req1} ->
          {Req, BodyData} =
              case cow_multipart:form_data(Headers) of
                  %% The multipart message contains normal/basic data
                  {data, _FieldName} -> {_Req2, _BodyData} = read_all_parts(Req1,[]);
                  %% The message contains a file, write it to "FieldName"
                  {file, FieldName, _Filename, _CType} ->
                      {ok, File} = file:open(FieldName, [append]),
                      Req2 = stream_file(Req1, File),
                      {Req2, [FieldName]}
              end,
          multipart(Req, Data++BodyData);
      {done, Req} -> {Req, Data}
  end.

%% writes the input stream to file, "File" needs to be opened with 'append' option
stream_file(Req0, File) ->
    case cowboy_req:read_part_body(Req0) of
        {ok, LastBodyChunk, Req} ->
            file:write(File, LastBodyChunk),
            file:close(File),
            Req;
        {more, BodyChunk, Req} ->
            file:write(File, BodyChunk),
            stream_file(Req, File)
    end.

%% gets multipart data and combines it
read_all_parts(Req0, Got) ->
  %io:format("length of read data so far: ~p~n",[length(Got)]),
  case cowboy_req:read_part_body(Req0) of
      {more, Data, Req} -> read_all_data(Req, Got++Data);
      {ok, Data, Req} -> {Req, Got++Data}
  end.
  %% gets multipart data and combines it

read_all_data(Req0, Got) ->
  %io:format("length of read data so far: ~p~n",[length(Got)]),
  case cowboy_req:read_body(Req0) of
      {more, Data, Req} -> read_all_data(Req, Got++Data);
      {ok, Data, Req} -> {Req, Got++Data}
  end.

deleteOldJson(FilePath) ->
  try   file:delete(FilePath)
  catch {error, E} -> io:format("couldn't delete file ~p, beacuse ~p~n",[FilePath, E])
  end.

% get this host ip 
getdeviceIP() ->
    {ok, IFList} = inet:getifaddrs(),    % IFList format: [{IF_name, [{field, param},{},...]},...]
    SubnetsList = getNerlSubnets(),
    getdeviceIP(IFList, SubnetsList).

getdeviceIP([], SubnetsList) ->
    ?LOG_ERROR(?LOG_HEADER++"No supported interface was found. Current supported interfaces list is: ~p.~nEdit subnets.nerlconfig file to include your network",[SubnetsList]);
getdeviceIP([IF|IFList], SubnetsList) ->
    {_IF_name, Params} = IF,
    try
        {addr, IF_addr} = lists:keyfind(addr, 1, Params),   % address format: {num, num, num, num}
        DeviceIP = isAddrInSubnets(IF_addr, SubnetsList),
        case DeviceIP of
            notFound -> getdeviceIP(IFList, SubnetsList);
            IP -> IP
        end
    catch error:_E -> getdeviceIP(IFList, SubnetsList)
    end.

getNerlSubnets() ->
    {ok, Data} = file:read_file(?SUBNETS_CONFIG_ADDR),
    Lines = string:split(binary_to_list(Data), "\n", all),
    Subnets = [Subnet || Subnet <- Lines, hd(Subnet) /= $#],
    lists:sort(Subnets).

isAddrInSubnets(_IF_addr, []) -> notFound;
isAddrInSubnets(IF_addr, [Subnet|SubnetsList]) ->
    %convert IF_addr to IP string
    IP_LIST = tuple_to_list(IF_addr),
    A = lists:flatten(io_lib:format("~p", [IP_LIST])),
    Subbed = lists:sublist(A,2,length(A)-2),
    IPString = lists:flatten(string:replace(Subbed,",",".",all)),
    % io:format("comparing ~p=~p~n",[IPString, Subnet]),
    IPMatch = lists:prefix(Subnet, IPString),
    case IPMatch of
        false -> isAddrInSubnets(IF_addr, SubnetsList);
        true -> IPString
    end.

string_format(Pattern, Values) ->
    lists:flatten(io_lib:format(Pattern, Values)).

list_to_numeric(Num) when is_float(Num) -> {Num, float};
list_to_numeric(Num) when is_integer(Num) -> {Num, integer};
list_to_numeric(L) ->
  Float = (catch erlang:list_to_float(L)),
  Int = (catch erlang:list_to_integer(L)),
  if is_number(Float) -> {Float, float};
    is_number(Int) -> {Int, integer};
    true -> throw("couldnt_convert "++L)
  end.

%% TODO: add another timing map for NIF of each worker action

%% TODO: create create_body func for standard message passing