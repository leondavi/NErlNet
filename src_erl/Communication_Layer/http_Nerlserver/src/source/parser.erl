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

-define(CORE_NUM, erlang:system_info(logical_processors_available)).
-define(PARALLELIZATION_FACTOR, 4).

%% API
-export([parseCSV/3, batchesProcFunc/3]).
-export([dataStrToNumeric_NumHandler/1,dataStrToNumeric_lineHandler/4]).
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
  CleanLines = [Line || Line <- Lines, Line /= []],
  ?LOG_INFO("split in data"),
  %% TODO: create NIF that does dataStrToNumericData. In = ["1,2,3,4", "5,6,7,8", ....] => out = [[1,2,3,4],[5,6,7,8],....] (make sure to take type into account)
  ListOfLinesOfData = dataStrToNumericData(CleanLines),
  ?LOG_INFO("converted str to float"),
  SampleSize = length(hd(ListOfLinesOfData)),
  {_Num, DataType} = nerl_tools:list_to_numeric(hd(hd(ListOfLinesOfData))),
  ListOfBatches = generateListOfBatches(ListOfLinesOfData, BatchSize),
  ?LOG_INFO("grouped batches"),
  % ErlType = nerlNIF:erl_type_conversion(UserType),        
  ErlType = erl_float,        %% TODO: delete so can accept int as well
  ListOfTensors = 
    case ErlType of 
          erl_float ->
            A = encodeListOfBatchesToNerlTensorsBinBatches(ListOfBatches, ErlType, DataType, SampleSize),
            ?LOG_INFO("converted to tensors"),
            % TestTensor = nerlNIF:nerltensor_conversion(hd(tl(A)), erl_float),
            % B = encodeListOfListsNerlTensor(ListOfBatches, DataType, SampleSize, 1),
            % io:format("A = ~p~n~nB = ~p ~n", [A, B]),
            % io:format("Test Tensor = ~p ~n", [TestTensor]),
            A;
          % erl_int -> encodeListOfListsNerlTensor(ListOfGroupedBatches, UserType, BatchSize,SampleSize,DimZ);
          _Other -> throw("wrong ErlType")
    end,
  ?LOG_NOTICE("Source ~p generated list of NerlTensors from file: ~p",[SourceName, File_Address]),
  file:write_file("Decoded"++File_Address, term_to_binary(ListOfTensors)),    %% write decoded and grouped samples to file
  {ListOfTensors, DataType, SampleSize}.

dataStrToNumeric_NumHandler(NumStr) -> 
  NumStrFixed =
        case NumStr of
        [$-,$.|Rest]  -> "-0."++Rest;
        [$.|Rest]     -> "0."++Rest;
        Str -> Str
      end,
      {Num, _Type} = nerl_tools:list_to_numeric(NumStrFixed),
      float(Num).    %% TODO: change to case of Type

dataStrToNumeric_lineHandler(PIPD, LineOfData, EtsTable, EtsKey) -> 
  FloatDataList = lists:map(fun dataStrToNumeric_NumHandler/1, string:split(binary_to_list(LineOfData), ",", all)),
  % io:format("FloatDataList ~p~n",[{FloatDataList,EtsKey}]),
  ets:insert(EtsTable, {EtsKey, FloatDataList}),
  PIPD ! done.

dataStrToNumeric_sync(0) -> ok;
dataStrToNumeric_sync(PF) ->
  receive 
    done -> dataStrToNumeric_sync(PF-1);
    _Other -> throw("unexpected message in source parse")
  end.


dataStrToNumericParallelLoop(_PF, _EtsTable, [], _LastKey) -> done;
dataStrToNumericParallelLoop(PF, EtsTable, ListOfLinesOfData, LastKey) when length(ListOfLinesOfData) > PF -> % PF - Parallelization Factor
  {ListOfLinesOfDataToBeProcessed, ListOfLinesOfDataRest} = lists:split(PF, ListOfLinesOfData),
  IdxList = lists:seq(LastKey,LastKey+PF-1),
  PIPD = self(),
  lists:zipwith(fun(LineOfData, Idx) -> spawn_link(?MODULE,dataStrToNumeric_lineHandler,[PIPD, LineOfData, EtsTable, Idx]) end, ListOfLinesOfDataToBeProcessed, IdxList),
  dataStrToNumeric_sync(PF),
  dataStrToNumericParallelLoop(PF, EtsTable, ListOfLinesOfDataRest, LastKey+PF);


dataStrToNumericParallelLoop(PF, EtsTable, ListOfLinesOfData, LastKey) ->
  PIPD = self(),
  CurrentKey = LastKey + 1,
  dataStrToNumeric_lineHandler(PIPD, hd(ListOfLinesOfData), EtsTable, CurrentKey),
  receive 
    done -> dataStrToNumericParallelLoop(PF, EtsTable, tl(ListOfLinesOfData), CurrentKey);
    _Other -> throw("unexpected message in source parse")
  end.
  

dataStrToNumericData(ListOfLinesOfData)->
  EtsTable = ets:new(data_str_to_numeric_data, [ordered_set, public]),
  dataStrToNumericParallelLoop(?PARALLELIZATION_FACTOR, EtsTable, ListOfLinesOfData, 0),
  [ element(?DATA_IDX, Attribute) || Attribute <- ets:tab2list(EtsTable)].

generateListOfBatches(ListOfList, BatchSize) -> generateListOfBatches(ListOfList, BatchSize, []).

generateListOfBatches([], _BatchSize, Ret) -> Ret;
generateListOfBatches(ListOfList, BatchSize, Ret) when (BatchSize >= length(ListOfList))-> Ret++[lists:flatten(ListOfList)];
generateListOfBatches(ListOfList, BatchSize, Ret) ->
  {NewBatch, Rest} = lists:split(BatchSize, ListOfList),
  generateListOfBatches(Rest, BatchSize, Ret ++ [lists:flatten(NewBatch)]).


% DEPRACATED
% decodeListOfLists(L) -> decodeListOfLists(L,[]).

% decodeListOfLists([],Ret) -> Ret;
% decodeListOfLists([[<<>>]|Tail],Ret) -> decodeListOfLists(Tail,Ret);
% decodeListOfLists([Head|Tail],Ret) ->
%   decodeListOfLists(Tail,Ret++[dataStrToNumericData(Head)]).


%DEPRECATED
% encodeListOfListsNerlTensor(L, TargetBinaryType, YDim, ZDim)->
%   {_Num, Type} = nerl_tools:list_to_numeric(hd(hd(L))),
  
%   ErlType =
%     case Type of 
%         float -> erl_float;
%         integer -> erl_int;
%         _Other -> throw("bad type in conversion")
%     end,
%   encodeListOfListsNerlTensor(L, ErlType, TargetBinaryType, [], YDim, ZDim).

% encodeListOfListsNerlTensor([], _ErlType, _TargetBinaryType, Ret, _YDim, _ZDim)-> Ret;
% encodeListOfListsNerlTensor([Head|Tail], ErlType, TargetBinaryType, Ret, YDim, ZDim)->
%   XDim = length(Head)/YDim,  %% XDim is the number of samples in batch
%   if XDim == 0 -> encodeListOfListsNerlTensor(Tail,ErlType,TargetBinaryType,Ret, YDim, ZDim);   %% skip empty tensor
%   true ->
%     NewTensor = nerlNIF:nerltensor_conversion({[XDim, YDim, ZDim | Head], ErlType}, TargetBinaryType),%% create new tensor
%     encodeListOfListsNerlTensor(Tail,ErlType,TargetBinaryType,Ret++[NewTensor], YDim, ZDim)
%   end.

batchesProcFunc(PPID, BatchesKey,BatchFunc) ->
      Batches = ets:lookup_element(encodeListOfBatchesToNerlTensorsBinBatches, BatchesKey, ?DATA_IDX),
      Result = lists:map(BatchFunc, Batches),
      ResultKeyName = list_to_atom(atom_to_list(BatchesKey)++"Res"),
      ets:insert(encodeListOfBatchesToNerlTensorsBinBatches, {ResultKeyName, Result}),
      PPID ! done.

encodeListOfBatchesToNerlTensorsBinBatches(ListOfBatches, ErlType, TargetBinaryType, SampleSize) ->
  BatchFunc =
    fun(Batch) ->
      NumOfSamples = round(length(Batch)/SampleSize),
      if NumOfSamples == 0 -> {<<>>, TargetBinaryType};
      true ->
        XDim = float(NumOfSamples),
        YDim = float(SampleSize),
        ZDim = 1.0,
        % io:format("sending conversion: ~p ~p ~n ",[{[XDim, YDim, ZDim | Batch], ErlType}, TargetBinaryType]),
        _NewTensor = nerlNIF:nerltensor_conversion({[XDim, YDim, ZDim | Batch], ErlType}, TargetBinaryType)
      end
    end,
  {UpBatches, DownBatches} = lists:split(round(length(ListOfBatches)/2)-1, ListOfBatches),

  ets:new(encodeListOfBatchesToNerlTensorsBinBatches, [named_table, public]),
  ets:insert(encodeListOfBatchesToNerlTensorsBinBatches, {upBatches, UpBatches}),
  ets:insert(encodeListOfBatchesToNerlTensorsBinBatches, {downBatches, DownBatches}),
  spawn_link(?MODULE, batchesProcFunc, [self(), upBatches, BatchFunc]),
  spawn_link(?MODULE, batchesProcFunc, [self(), downBatches, BatchFunc]),
  receive 
    done -> cont;
    _Other -> throw("unexpected message in source parse")
  end,
  receive 
    done -> cont;
    _Other2 -> throw("unexpected message in source parse")
  end,
  UpRes = ets:lookup_element(encodeListOfBatchesToNerlTensorsBinBatches, upBatchesRes, ?DATA_IDX),
  DownRes = ets:lookup_element(encodeListOfBatchesToNerlTensorsBinBatches, downBatchesRes, ?DATA_IDX),

  ets:delete(encodeListOfBatchesToNerlTensorsBinBatches),
  % [io:format("~p" ,[nerlNIF:nerltensor_conversion(Item, erl_float)]) || Item <- UpRes ++ DownRes],
  UpRes ++ DownRes.

%%return a binary representing a list of floats: List-> <<binaryofthisList>>
%%%%%%% this function is for FLOATS, converts int data to float
% decodeFloatsList(L)->
%   Splitted = re:split(binary_to_list(L), ",", [{return,list}]),
%   decodeFloatsList(Splitted,[]).
% decodeFloatsList([],Ret)->Ret;
% decodeFloatsList([[]|ListOfFloats],Ret)-> decodeFloatsList(ListOfFloats,Ret);
% decodeFloatsList([H|ListOfFloats],Ret)->
%   %% numbers sometime appear as ".7" / "-.1" 
%   Num = case H of
%     [$-,$.|Rest]  -> "-0."++Rest;
%     [$.|Rest]     -> "0."++Rest;
%     List -> List
%   end,
%   {NumToAdd, _Type} = nerl_tools:list_to_numeric(Num),
    
%   decodeFloatsList(ListOfFloats,Ret++[float(NumToAdd)]).     %% remove float() to keep mixed data type

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