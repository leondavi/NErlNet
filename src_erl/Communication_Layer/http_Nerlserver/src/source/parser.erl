%%%-------------------------------------------------------------------
%%% @author kapelnik
%%% @copyright (C) 2021, Nerlnet
%%% @doc
%%%
%%% @end
%%% Created : 19. Apr 2021 4:27 AM
%%%-------------------------------------------------------------------
-module(parser).
-author("kapelnik").

-import(nerlNIF,[decode_nif/2, nerltensor_binary_decode/2]).
-import(nerlNIF,[encode_nif/2, nerltensor_encode/5, nerltensor_conversion/2, get_all_binary_types/0]).
-import(nerlNIF,[erl_type_conversion/1]).

%% API
-define(TMP_DATA_ADDR, "tmpData.csv").
-export([parse/2, parseCSV/3, deleteTMPData/1]).

%% unused functions
-export([decodeEncodeFloatsListBin/4]).


parseCSV(SourceName, BatchSize, CSVData)->
  %io:format("curr dir: ~p~n",[file:get_cwd()]),
  deleteTMPData(SourceName),    % ideally do this when getting a fresh CSV (finished train -> start predict)

  FileName = SourceName++?TMP_DATA_ADDR,
  try
    file:write_file(FileName, CSVData),
    logger:notice("created tmpData.csv"), parse_file(BatchSize, FileName)
  catch
    {error,Er} -> logger:error("couldn't write file ~p, beacuse ~p",[FileName, Er])
  end.

deleteTMPData(SourceName) ->
  {ok, Dir} = file:get_cwd(),
  {ok, Files} = file:list_dir(Dir),
  DataFiles = [File || File <- Files, string:find(File, ".csv") /= nomatch, string:prefix(File, SourceName) /= nomatch],
  try [file:delete(File) || File <- DataFiles]
  catch
    {error, E} -> logger:notice("couldn't delete file ~p, ~p",[DataFiles, E])
  end.

%%this parser takes a CSV folder containing chunked data, parsing into a list of binary.
%%each record in the line is a batch of samples
parse(BatchSize,FolderName)->
  io:format("curr dir: ~p~n",[file:get_cwd()]),
%%  FolderName="./input/shuffled-input1_splitted/",
  parse_all(BatchSize,FolderName,1,[]).


parse_all(BatchSize,FolderName,Counter,Ret)->
  Name = lists:last(re:split(FolderName,"/",[{return,list}])),

  try   parse_file(BatchSize,"../../../inputDataDir/"++FolderName++"_splitted/"++Name++"_splitted"++integer_to_list(Counter)++".csv") of

    L ->
      parse_all(BatchSize,FolderName,Counter+1,Ret++L)
  catch error: E->
    if length(Ret) == 0  ->
        io:format("#####Error at Parser: ~n~p~n",[E]);
    true -> Ret
  end
  end.

%%parsing a given CSV file
parse_file(BatchSize,File_Address) ->

  io:format("File_Address:~p~n~n",[File_Address]),

  {ok, Data} = file:read_file(File_Address),
  Lines = re:split(Data, "\r|\n|\r\n", [{return,binary}] ),

  SampleSize = length(re:split(binary_to_list(hd(Lines)), ",", [{return,list}])),
%%  get binary lines
  ListsOfListsOfData = decodeListOfLists(Lines),
  ListOfGroupedBatches = generateListOfBatches(ListsOfListsOfData, BatchSize),
  UserType = float,   %% TODO: support given type from json 
  DimZ = 1,
  ErlType = nerlNIF:get_type_conversion(UserType),
  ListOfTensors = 
    case ErlType of 
          erl_float -> encodeListOfListsNerlTensor(ListOfGroupedBatches, UserType, float(BatchSize),float(SampleSize),float(DimZ));
          erl_int -> encodeListOfListsNerlTensor(ListOfGroupedBatches, UserType, BatchSize,SampleSize,DimZ);
          _Other -> throw("wrong ErlType")
    end,
  {ListOfTensors, UserType}.


generateListOfBatches(ListOfList, BatchSize) ->
  generateListOfBatches(ListOfList, BatchSize, []).

generateListOfBatches([], _BatchSize, Ret) -> Ret;
generateListOfBatches(ListOfList, BatchSize, Ret) ->
  Sublist = lists:sublist(ListOfList, BatchSize),
  generateListOfBatches(ListOfList -- Sublist,Ret++[lists:flatten(Sublist)]).

decodeListOfLists(L)->decodeListOfLists(L,[]).
decodeListOfLists([],Ret)-> Ret;
decodeListOfLists([[<<>>]|Tail],Ret)->
  decodeListOfLists(Tail,Ret);
decodeListOfLists([Head|Tail],Ret)->
  decodeListOfLists(Tail,Ret++[decodeFloatsList(Head)]).

encodeListOfListsNerlTensor(L, TargetBinaryType, XDim, YDim, ZDim)->
  {_Num, Type} = is_numeric(hd(hd(hd(L)))),
  
  ErlType =
  case Type of 
      float -> erl_float;
      integer -> erl_int;
      _Other -> throw("bad type in conversion") end,
  encodeListOfListsNerlTensor(L, ErlType, TargetBinaryType, [], XDim, YDim, ZDim).

encodeListOfListsNerlTensor([], _ErlType, _TargetBinaryType, Ret, _XDim, _YDim, _ZDim)-> Ret;
encodeListOfListsNerlTensor([Head|Tail], ErlType, TargetBinaryType, Ret, XDim, YDim, ZDim)->
  encodeListOfListsNerlTensor(Tail,Ret++[nerlNIF:nerltensor_conversion({[XDim, YDim, ZDim | Head], ErlType}, TargetBinaryType)], XDim, YDim, ZDim).


%%return a binary representing a list of floats: List-> <<binaryofthisList>>
decodeEncodeFloatsListBin(L, XDim, YDim, ZDim)->
  Splitted = re:split(binary_to_list(L), ",", [{return,list}]),
  decodeEncodeFloatsListBin(Splitted, <<>>, XDim, YDim, ZDim).
decodeEncodeFloatsListBin([],Ret, XDim, YDim, ZDim) -> <<XDim:64/float, YDim:64/float, ZDim:64/float, Ret/binary>>;
decodeEncodeFloatsListBin([<<>>|ListOfFloats],Ret, XDim, YDim, ZDim)->
  decodeEncodeFloatsListBin(ListOfFloats,Ret, XDim, YDim, ZDim);
decodeEncodeFloatsListBin([[]|ListOfFloats],Ret, XDim, YDim, ZDim)->
  decodeEncodeFloatsListBin(ListOfFloats,Ret, XDim, YDim, ZDim);
decodeEncodeFloatsListBin([H|ListOfFloats],Ret, XDim, YDim, ZDim)->
  %% numbers sometime appear as ".7" / "-.1" 
  Num = case H of
    [$-,$.|Rest]  -> "-0."++Rest;
    [$.|Rest]     -> "0."++Rest;
    List          -> List
  end,
  {NumToAdd, _Type} = is_numeric(Num),

    
  decodeEncodeFloatsListBin(ListOfFloats,<<Ret/binary,NumToAdd:64/float>>, XDim, YDim, ZDim).

%%return a binary representing a list of floats: List-> <<binaryofthisList>>
decodeFloatsList(L)->
  Splitted = re:split(binary_to_list(L), ",", [{return,list}]),
  decodeFloatsList(Splitted,[]).
decodeFloatsList([],Ret)->Ret;
decodeFloatsList([[]|ListOfFloats],Ret)->
  decodeFloatsList(ListOfFloats,Ret);
decodeFloatsList([H|ListOfFloats],Ret)->
  %% numbers sometime appear as ".7" / "-.1" 
  Num = case H of
    [$-,$.|Rest]  -> "-0."++Rest;
    [$.|Rest]     -> "0."++Rest;
    List          -> List
  end,
  {NumToAdd, _Type} = is_numeric(Num),
    
  decodeFloatsList(ListOfFloats,Ret++[NumToAdd]).

is_numeric(L) ->
  Float = (catch erlang:list_to_float(L)),
  Int = (catch erlang:list_to_integer(L)),
  if is_number(Float) -> {Float, float};
    is_number(Int) -> {Int, integer};
    true -> throw("invalid type") end.
