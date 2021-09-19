%%%-------------------------------------------------------------------
%%% @author kapelnik
%%% @copyright (C) 2021, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 19. Apr 2021 4:27 AM
%%%-------------------------------------------------------------------
-module(newparser).
-author("kapelnik").

%% API
-export([parse_file/0]).


%%parsing a given CSV file
parse_file() ->
  io:format("~n~n~n~n~n~n~n~n~n~n~n~n~n~n~n~n~n~n~n~n~n~n~n~n~n~n~n~n~n",[]),

  File_Address="./../input/inputNew.csv",
  ChunkSize=2,
  {ok, Data} = file:read_file(File_Address),%%TODO change to File_Address
  Lines = re:split(Data, "\r|\n|\r\n", [{return,binary}] ),
%%  io:format("Lines : ~p~n~n~n",[Lines]),

%%  Splitted = [ re:split(Line, "\t", [{return,binary}] )||Line<-Lines],
%%  io:format("Splitted : ~p~n~n~n",[Splitted]),

  ListsOfListsOfFloats = encodeListOfLists(Lines),
%%  [HH|_T]=ListsOfListsOfFloats,
%%  io:format("decodeList(HH) : ~p~n~n~n",[decodeList(HH)]),
%%  io:format("ListsOfListsOfFloats : ~p~n~n~n",[ListsOfListsOfFloats]),

  [EncodedLine|_T]=ListsOfListsOfFloats,
  io:format("EncodedLine: ~p~n~n~n~n",[EncodedLine]),

%%  testing(),


  Chunked= makeChunks(ListsOfListsOfFloats,ChunkSize,ChunkSize,<<>>,[]),
  io:format("Chunked: ~p~n~n~n~n",[Chunked]),


%%  [BinaryHead|_T]=Chunked,
%%  io:format("BinaryHead: ~p~n~n~n~n",[BinaryHead]),

%%  Binary = term_to_binary(Head ),
%%  io:format("Binary: ~p~n~n~n",[Binary]),

%%  [TermHead|_Tail] = binary_to_term(BinaryHead),
%%  io:format("Term: ~p~n",[Term]),

  Decoded = decodeListOfLists(Chunked ),

  io:format("Decoded!!!: ~p~n",[Decoded]).


%%chank(L,ChunkSize)->chank(L,ChunkSize,[]).
%%chank(L,ChunkSize,Ret) when length(L)>ChunkSize->chank(lists:sublist(L,ChunkSize+1,length(L)),[Ret|[lists:sublist(L,ChunkSize)]]);
%%chank(L,_ChunkSize,Ret) ->Ret++[L].

%%L =[ [[1.1,2.2,3.3,4.4],[1.1,2.2,3.3,4.4]]++[[5.5,6.6,7.7,8.8],[1.1,2.2,3.3,4.4]]].
encodeListOfLists(L)->encodeListOfLists(L,[]).
encodeListOfLists([],Ret)->Ret;
encodeListOfLists([[<<>>]|Tail],Ret)->
      encodeListOfLists(Tail,Ret);
encodeListOfLists([Head|Tail],Ret)->
      encodeListOfLists(Tail,Ret++[encodeFloatsList(Head)]).


%%return a binary representing a list of floats: List-> <<binaryofthisList>>
encodeFloatsList(L)->
  Splitted = re:split(binary_to_list(L), ",", [{return,list}]),
  A = encodeFloatsList(Splitted,<<>>)
  .
encodeFloatsList([],Ret)->Ret;
encodeFloatsList([<<>>|ListOfFloats],Ret)->
  encodeFloatsList(ListOfFloats,Ret);
encodeFloatsList([[]|ListOfFloats],Ret)->
  encodeFloatsList(ListOfFloats,Ret);
encodeFloatsList([H|ListOfFloats],Ret)->
  H3=list_to_float(H),
  encodeFloatsList(ListOfFloats,<<Ret/binary,H3:64/float>>).

%%This decoder receives a binary <<131,108,0,0,0,2,106...>> and returns a lists of lists: [[1.0,1.1,11.2],[2.0,2.1,22.2]]
decodeListOfLists(L)->decodeListOfLists(L,[]).
decodeListOfLists([],Ret)->Ret;
decodeListOfLists([H|T],Ret)->decodeListOfLists(T,Ret++[decodeList(H)]).
decodeList(Binary)->  decodeList(Binary,[]).
decodeList(<<>>,L) -> L;
decodeList(<<A:64/float,Rest/binary>>,L) -> decodeList(Rest,L++[A]).




makeChunks(L,1,1,_,_) ->L;
makeChunks([],_Left,_ChunkSize,Acc,Ret) ->
  Ret++[Acc];

makeChunks([Head|Tail],1,ChunkSize,Acc,Ret) ->
  makeChunks(Tail,ChunkSize,ChunkSize,<<>>,Ret++[<<Acc/binary,Head/binary>>]);

makeChunks([Head|Tail],Left,ChunkSize,Acc,Ret) ->
%%  io:format("Acc:~p~nHead:~p~n",[Acc,Head]),
%%  io:format("<<Acc/binary,Head/binary>>:~p~n",[<<Acc/binary,Head/binary>>]),
  makeChunks(Tail,Left-1,ChunkSize,<<Acc/binary,Head/binary>>,Ret).








testing()->
  Bin1 = <<63,196,253,243,182,69,161,203,191,206,249,219,34,208,229,96,63,200,147,116,188,106,126,250,63,195,116,188,106,126,249,219>>,
  Bin2 = <<63,196,253,243,182,69,161,203,191,206,249,219,34,208,229,96,63,200,147,116,188,106,126,250,63,195,116,188,106,126,249,219>>,
  Concat = <<Bin1/binary,Bin2/binary>>,

  io:format("Concat = ~p~n",[Concat]),
  io:format("decodeList = ~p~n",[decodeList(Concat)]).