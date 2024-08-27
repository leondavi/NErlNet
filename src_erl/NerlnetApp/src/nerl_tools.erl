-module(nerl_tools).

-include("nerl_tools.hrl").

-export([setup_logger/1, string_format/2]).
-export([http_request/4, sendHTTP/4, get_client_worker_pairs/3, getShortPath/3]).
-export([string_to_list_int/1, deleteOldJson/1]).
-export([multipart/2, read_all_data/2]).
-export([getdeviceIP/0, port_available/1]).
-export([calculate_size/1]).
-export([make_routing_table/4]).
-export([http_router_request/5]).
-export([format_multipart_formdata/3]).

setup_logger(Module) ->
  logger:set_handler_config(default, formatter, {logger_formatter, #{}}),
  logger:set_module_level(Module, all).


http_router_request(RouterHost, RouterPort, DestinationsList, ActionStr, Body) ->
  if 
    length(DestinationsList) == 1 -> % unicast
        Dest = hd(DestinationsList),
        nerl_tools:http_request(RouterHost, RouterPort, "unicast", term_to_binary({Dest,{ActionStr, Body}}));
    length(DestinationsList) > 1 -> % Broadcast
        nerl_tools:http_request(RouterHost, RouterPort, "broadcast", term_to_binary({DestinationsList,{ActionStr , Body}}));
    true ->
        ?LOG_ERROR("Empty DestinationsList is given!"),
        throw("Empty DestinationsList is given!")
  end.


http_request(Host, Port, Path, {json, Body}) -> 
  JsonContentType = ?HTTP_CONTENT_TYPE_JSON,
  Json = jsx:encode(Body),
  http_request(Host, Port,Path, JsonContentType, Json);
http_request(Host, Port, Path, Body) -> 
  DefaultContentType = ?HTTP_CONTENT_TYPE_FORM_URLENCODED,
  http_request(Host, Port,Path, DefaultContentType, Body).

%% send message between entities
http_request(Host, Port, Path, ContentType, Body) when is_atom(Body) -> http_request(Host, Port,Path, ContentType, atom_to_list(Body));
http_request(Host, Port, Path, ContentType, Body) when is_binary(Host) -> http_request(binary_to_list(Host), Port,Path, ContentType, Body);
http_request(Host, Port, Path, ContentType, Body) ->
  URL = "http://" ++ Host ++ ":" ++ integer_to_list(Port) ++ "/" ++ Path, % Path is the action
  httpc:set_options([{proxy, {{Host, Port},[Host]}}]),
  httpc:request(post,{URL, [], ContentType, Body}, [], []).

get_client_worker_pairs([],_WorkersMap,Ret)-> Ret;
get_client_worker_pairs([WorkerName|WorkersNames],WorkersMap,Ret)->
  ClientName = maps:get(list_to_atom(WorkerName),WorkersMap),
  get_client_worker_pairs(WorkersNames,WorkersMap, Ret++[{ClientName,WorkerName}]).

getShortPath(From,To,NerlnetGraph) when is_list(To) -> getShortPath(From,list_to_atom(To),NerlnetGraph);
getShortPath(From,To,NerlnetGraph) when is_list(From) -> getShortPath(list_to_atom(From),To,NerlnetGraph);
getShortPath(From,To,NerlnetGraph) when is_atom(To) and is_atom(From) ->  % TODO use only atoms list conversions should be removed in the future!
  ShortPath = digraph:get_short_path(NerlnetGraph,From,To),
  case ShortPath of
    false -> false;
    ShortPath ->
      NextHop = lists:nth(2,ShortPath),
      {_Name,{Host,Port,_DeviceName}} = digraph:vertex(NerlnetGraph,NextHop),
      {Host,Port}
  end.
  

sendHTTP(From, To, Action, Body) ->
  case nerl_tools:getShortPath(From,To,get(nerlnetGraph)) of
    false -> ?LOG_WARNING("No path to entity ~p!",[To]);
    {RouterHost,RouterPort} -> nerl_tools:http_request(RouterHost, RouterPort, Action, Body)
  end.
	

%%return list of integer from string of lists of strings - "[2,2,2]" -> [2,2,2]
string_to_list_int(Binary) ->
  String = binary_to_list(Binary),
  NoParenthesis = lists:sublist(String,2,length(String)-2),
  Splitted = re:split(NoParenthesis,",",[{return,list}]),
  [list_to_integer(X)||X<-Splitted].

%% This gets a Cowboy HTTP Request object and reads the multipart (type of form) data from it
%% automatically streams to file data is file
% returns {FullReq, Data} / {FullReq, [fileReady]}
multipart(Req0, Data) ->
  case cowboy_req:read_part(Req0) of
      {ok, Headers, Req1} ->
          {Req, BodyData} =
              case cow_multipart:form_data(Headers) of
                  %% The multipart message contains normal/basic data
                  {data, _FieldName} -> {_Req2, _BodyData} = read_all_parts(Req1,[]);
                  %% The message contains a file, write it to "FieldName"
                  {file, FieldName, _Filename, _CType} -> % TODO understand this function
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
  % This function is useful when you’re dealing with large amounts of data that can’t be read all at once.
  % By reading the data in chunks and using recursion, it can handle any amount of data without running out of memory.
  % However, keep in mind that this function assumes that the data is either a binary or a list, and it may not work correctly
  % if the data is of a different type. It’s always a good idea to validate the data before processing it.
  % Cowboy website - https://ninenines.eu/docs/en/cowboy/2.10/guide/req_body/
  case cowboy_req:read_body(Req0) of
      {more, Data, Req} -> 
        if 
          Got == <<>>     ->  read_all_data(Req, <<Data/binary>>);
          is_binary(Data) ->  read_all_data(Req, <<Got/binary, Data/binary>>);
          true            ->  read_all_data(Req, Got++Data) end;
      {ok, Data, Req}   -> 
        if is_binary(Data) -> {Req, <<Got/binary, Data/binary>>};
        true ->               {Req, Got++Data} end
  end.

deleteOldJson(FilePath) ->
  try   file:delete(FilePath)
  catch {error, E} -> ?LOG_ERROR("couldn't delete file ~p, beacuse ~p~n",[FilePath, E])
  end.

% get this host ip 
getdeviceIP() ->
    {ok, IFList} = inet:getifaddrs(),    % IFList format: [{IF_name, [{field, param},{},...]},...]
    SubnetsList = getNerlSubnets(),
    getdeviceIP(IFList, SubnetsList).

getdeviceIP([], SubnetsList) ->
    ?LOG_ERROR(?LOG_HEADER++"No supported interface was found. Current supported interfaces list is: ~p.~nEdit subnets.nerlconfig file to include your network",[SubnetsList]), no_device_ip_in_subnet;
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

ip_list_to_str(IpList) ->
  IpIntListAsStr = lists:flatten(io_lib:format("~p", [IpList])),
  IpIntListAsStrRemovedParenthesis = lists:sublist(IpIntListAsStr,2,length(IpIntListAsStr)-2), % [IP] to IP
  IpString = lists:flatten(string:replace(IpIntListAsStrRemovedParenthesis,",",".",all)), % x,x,x,x to x.x.x.x
  IpString.

isAddrInSubnets(_IF_addr, []) -> notFound;
isAddrInSubnets(IF_addr, [Subnet|SubnetsList]) ->
    %convert IF_addr to IP string
    IpList = tuple_to_list(IF_addr),
    IPString = ip_list_to_str(IpList),
    IPMatch = lists:prefix(Subnet, IPString),
    case IPMatch of
        false -> isAddrInSubnets(IF_addr, SubnetsList);
        true -> IPString
    end.

string_format(Pattern, Values) ->
    lists:flatten(io_lib:format(Pattern, Values)).

port_available(Port) ->
    case gen_tcp:listen(Port, []) of
        {ok, Sock} ->
            ok = gen_tcp:close(Sock),
            true;
        _ ->
            false
    end.

%% calculate the number of bytes of term

calculate_size(Term) when is_tuple(Term) -> calculate_size(tuple_to_list(Term));
calculate_size(List) when is_list(List) ->
    Sizes = lists:map(fun(Term) -> byte_size(term_to_binary(Term)) end, List), 
    lists:sum(Sizes).

%% TODO: add another timing map for NIF of each worker action

%% TODO: create create_body func for standard message passing

%% Entities List must not contain ThisRouter
%% NextHop is the next router for all entities that are not connected with ThisRouter
%% Otherwise , NextHop is an entity of ThisRouter
make_routing_table(Ets,EntitiesList,ThisRouter,NerlnetGraph)->
  GenerateTablesFunc = fun(Entity) -> 
    case digraph:get_short_path(NerlnetGraph,ThisRouter,Entity) of
      false -> ok;
      ShortPath -> NextHop = lists:nth(2,ShortPath),
                  {Name , {Host , Port , _DeviceName}} = digraph:vertex(NerlnetGraph,NextHop),
                  ets:insert(Ets,{Entity,{Name,Host,Port}})
    end % case end
  end, % fun end
  lists:foreach(GenerateTablesFunc, EntitiesList),
  {ThisRouter , {RouterHost , RouterPort , _DeviceName}} = digraph:vertex(NerlnetGraph , ThisRouter),
  ets:insert(Ets , {ThisRouter , {ThisRouter , RouterHost , RouterPort}}).


%% Reference: https://stackoverflow.com/questions/39222517/how-to-httppost-file-with-httpcrequest-in-erlang
format_multipart_formdata(Boundary, Fields, Files) ->
    FieldParts = lists:map(fun({FieldName, FieldContent}) ->
        [lists:concat(["--", Boundary]),
            lists:concat(["Content-Disposition: form-data; name=\"",FieldName,"\""]),
            "", FieldContent]
                           end, Fields),

    FieldParts2 = lists:append(FieldParts),


    FileParts = lists:map(fun({FieldName, FileName, FileContent}) ->

        [lists:concat(["--", Boundary]),
            lists:concat(["Content-Disposition: form-data; name=\"",FieldName,"\"; filename=\"",FileName,"\""]), %FieldName is a list
            lists:concat(["Content-Type: ", "application/octet-stream"]), "", FileContent]
                          end, Files),
    FileParts2 = lists:append(FileParts),
    EndingParts = [lists:concat(["--", Boundary, "--"]), ""],
    Parts = lists:append([FieldParts2, FileParts2, EndingParts]),
    string:join(Parts, "\r\n").