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
-include("../nerl_tools.hrl").
-import(nerlNIF,[decode_nif/2, nerltensor_binary_decode/2]).
-import(nerlNIF,[encode_nif/2, nerltensor_encode/5, nerltensor_conversion/2, get_all_binary_types/0]).
-import(nerlNIF,[erl_type_conversion/1]).

%% API
-export([parseCSV/3]).
%% unused functions
% -export([decodeEncodeFloatsListBin/4, get_batch/2]).


parseCSV(SourceName, BatchSize, CSVData)->
  SourceNameStr = atom_to_list(SourceName),
  nerl_tools:setup_logger(?MODULE),
  %io:format("curr dir: ~p~n",[file:get_cwd()]),
  %deleteTMPData(SourceName),    % ideally do this when getting a fresh CSV (finished train -> start predict)
  FileName = ?TMP_DIRECTORY++SourceNameStr++?TMP_DATA_ADDR,
  try
    file:write_file(FileName, CSVData),
    parse_file(SourceName, BatchSize, FileName) %% change so read data only when sending (currently loading all data)
  catch
    {error,Er} -> logger:error("couldn't write file ~p, beacuse ~p",[FileName, Er])
  end.

%% read batch from file, dont keep all in memory:
% get_batch(File, BatchSize) ->
%   Batch = get_N_lines(File, BatchSize),
%   DecodedBatch = decode(Batch, <<>>),
%   encode_tensor(DecodedBatch).

% get_N_lines(File, 0) -> [];
% get_N_lines(File, N) ->
%   case file:read_line(File) of
%       {ok, Data} -> [[Data] | get_N_lines(File, N - 1)];
%       eof        -> []
%   end.

% decode([], Ret) -> Ret;
% decode([H | Rest], Ret) ->
%   %% numbers sometime appear as ".7" / "-.1" 
%   Num = case H of
%     [$-,$.|Rest]  -> "-0."++Rest;
%     [$.|Rest]     -> "0."++Rest;
%     List -> List
%   end,
%   {NumToAdd, _Type} = nerl_tools:list_to_numeric(Num),
%   case Ret of
%     <<>>  -> decode(Rest,<<float(NumToAdd)>>);
%     _     -> decode(Rest,<<Ret/binary,NumToAdd:32/float>>)
%   end.

% encode_tensor(Batch) ->
%   XDim = length(hd(Batch)),
%   YDim = length(Batch),
%   ZDim = 1,
%   ErlType = erl_float,
%   TargetBinaryType = float,
%   nerlNIF:nerltensor_conversion({[XDim, YDim, ZDim | Head], ErlType}, TargetBinaryType)

% deleteTMPData(SourceName) ->
%   SourceNameStr = atom_to_list(SourceName),
%   {ok, Dir} = file:get_cwd(),
%   {ok, Files} = file:list_dir(Dir),
%   DataFiles = [File || File <- Files, string:find(File, ".csv") /= nomatch, string:prefix(File, SourceNameStr) /= nomatch], % TODO Haran - it's a very very dangerous function if you wannt to delete a specific file - then delete it
%   try [file:delete(File) || File <- DataFiles]
%   catch
%     {error, E} -> logger:notice("couldn't delete files ~p, ~p",[DataFiles, E])
%   end.

%%this parser takes a CSV folder containing chunked data, parsing into a list of binary.
%%each record in the line is a batch of samples
parse_file(SourceName, BatchSize,File_Address) ->
  {ok, Data} = file:read_file(File_Address),
  Lines = re:split(Data, "\r|\n|\r\n", [{return,binary}] ),

  SampleSize = length(re:split(binary_to_list(hd(Lines)), ",", [{return,list}])),
  UserType = float,   %% TODO: support given type from json  --- JsonParser should be updated too
  DimZ = 1,
  ListOfLinesOfData = decodeListOfLists(Lines),
  ListOfGroupedBatches = generateListOfBatches(ListOfLinesOfData, BatchSize),
  % ErlType = nerlNIF:erl_type_conversion(UserType),        
  ErlType = erl_float,        %% TODO: delete so can accept int as well
  ListOfTensors = 
    case ErlType of 
          erl_float -> encodeListOfListsNerlTensor(ListOfGroupedBatches, UserType, float(BatchSize),float(SampleSize),float(DimZ));
          % erl_int -> encodeListOfListsNerlTensor(ListOfGroupedBatches, UserType, BatchSize,SampleSize,DimZ);
          _Other -> throw("wrong ErlType")
    end,
  ?LOG_NOTICE("Source ~p generated list of NerlTensors from file: ~p",[SourceName, File_Address]),
  {ListOfTensors, UserType, SampleSize}.


generateListOfBatches(ListOfList, BatchSize) -> generateListOfBatches(ListOfList, BatchSize, []).

generateListOfBatches([], _BatchSize, Ret) -> Ret;
generateListOfBatches(ListOfList, BatchSize, Ret) when BatchSize >= length(ListOfList) -> Ret++[lists:flatten(ListOfList)];
generateListOfBatches(ListOfList, BatchSize, Ret) ->
  {NewBatch, Rest} = lists:split(BatchSize, ListOfList),
  generateListOfBatches(Rest, BatchSize, Ret ++ [lists:flatten(NewBatch)]).

decodeListOfLists(L) -> decodeListOfLists(L,[]).

decodeListOfLists([],Ret) -> Ret;
decodeListOfLists([[<<>>]|Tail],Ret) -> decodeListOfLists(Tail,Ret);
decodeListOfLists([Head|Tail],Ret) ->
  decodeListOfLists(Tail,Ret++[decodeFloatsList(Head)]).

encodeListOfListsNerlTensor(L, TargetBinaryType, XDim, YDim, ZDim)->
  {_Num, Type} = nerl_tools:list_to_numeric(hd(hd(L))),
  
  ErlType =
    case Type of 
        float -> erl_float;
        integer -> erl_int;
        _Other -> throw("bad type in conversion")
    end,
  encodeListOfListsNerlTensor(L, ErlType, TargetBinaryType, [], XDim, YDim, ZDim).

encodeListOfListsNerlTensor([], _ErlType, _TargetBinaryType, Ret, _XDim, _YDim, _ZDim)-> Ret;
encodeListOfListsNerlTensor([Head|Tail], ErlType, TargetBinaryType, Ret, _XDim, YDim, ZDim)->
  XDim = length(Head)/YDim,  %% XDim is the number of samples in batch
  if XDim == 0 -> encodeListOfListsNerlTensor(Tail,ErlType,TargetBinaryType,Ret, XDim, YDim, ZDim);   %% skip empty tensor
  true ->
    NewTensor = nerlNIF:nerltensor_conversion({[XDim, YDim, ZDim | Head], ErlType}, TargetBinaryType),%% create new tensor
    encodeListOfListsNerlTensor(Tail,ErlType,TargetBinaryType,Ret++[NewTensor], XDim, YDim, ZDim)
  end.


%%return a binary representing a list of floats: List-> <<binaryofthisList>>
%%%%%%% this function is for FLOATS, converts int data to float
decodeFloatsList(L)->
  Splitted = re:split(binary_to_list(L), ",", [{return,list}]),
  decodeFloatsList(Splitted,[]).
decodeFloatsList([],Ret)->Ret;
decodeFloatsList([[]|ListOfFloats],Ret)-> decodeFloatsList(ListOfFloats,Ret);
decodeFloatsList([H|ListOfFloats],Ret)->
  %% numbers sometime appear as ".7" / "-.1" 
  Num = case H of
    [$-,$.|Rest]  -> "-0."++Rest;
    [$.|Rest]     -> "0."++Rest;
    List -> List
  end,
  {NumToAdd, _Type} = nerl_tools:list_to_numeric(Num),
    
  decodeFloatsList(ListOfFloats,Ret++[float(NumToAdd)]).     %% remove float() to keep mixed data type

%% UNUSED: return a binary representing a list of floats: List-> <<binaryofthisList>>
% decodeEncodeFloatsListBin(L, XDim, YDim, ZDim)->
%   Splitted = re:split(binary_to_list(L), ",", [{return,list}]),
%   decodeEncodeFloatsListBin(Splitted, <<>>, XDim, YDim, ZDim).
% decodeEncodeFloatsListBin([],Ret, XDim, YDim, ZDim) -> <<XDim:64/float, YDim:64/float, ZDim:64/float, Ret/binary>>;
% decodeEncodeFloatsListBin([<<>>|ListOfFloats],Ret, XDim, YDim, ZDim)->
%   decodeEncodeFloatsListBin(ListOfFloats,Ret, XDim, YDim, ZDim);
% decodeEncodeFloatsListBin([[]|ListOfFloats],Ret, XDim, YDim, ZDim)->
%   decodeEncodeFloatsListBin(ListOfFloats,Ret, XDim, YDim, ZDim);
% decodeEncodeFloatsListBin([H|ListOfFloats],Ret, XDim, YDim, ZDim)->
%   %% numbers sometime appear as ".7" / "-.1" 
%   Num = case H of
%     [$-,$.|Rest]  -> "-0."++Rest;
%     [$.|Rest]     -> "0."++Rest;
%     List          -> List
%   end,
%   {NumToAdd, _Type} = nerl_tools:list_to_numeric(Num),

    
%   decodeEncodeFloatsListBin(ListOfFloats,<<Ret/binary,NumToAdd:64/float>>, XDim, YDim, ZDim).