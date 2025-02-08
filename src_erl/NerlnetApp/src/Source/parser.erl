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
-define(PARALLELIZATION_FACTOR, 2).

%% API
-export([parseCSV/4, batchesProcFunc/3]).
-export([dataStrToNumeric_NumHandler/1,dataStrToNumeric_lineHandler/5]).


parseCSV(SourceName, BatchSize, NerlTensorType, CSVData)->
  ErlType = nerlNIF:erl_type_conversion(list_to_atom(NerlTensorType)),
  put(erl_tensor_type, ErlType),
  try
    parse_file(SourceName, BatchSize, NerlTensorType, ErlType, CSVData) %% change so read data only when sending (currently loading all data)
  catch
    {error,Er} ->
      nerl_tools:setup_logger(?MODULE),
      SourceNameStr = atom_to_list(SourceName),
      FileName = ?TMP_DIR_RUN ++ SourceNameStr ++ ?TMP_DATA_ADDR,
      file:write_file(FileName, CSVData),
      logger:error("couldn't write file ~p, beacuse ~p",[FileName, Er])
  end.


%%this parser takes a CSV folder containing chunked data, parsing into a list of binary.
%%each record in the line is a batch of samples
parse_file(_SourceName, BatchSize, NerlTensorType, ErlType, Data) ->
  % {ok, Data} = file:read_file(File_Address),
  Lines = re:split(Data, "\r|\n|\r\n", [{return,binary}] ),
  %% TODO: create NIF that does dataStrToNumericData. In = ["1,2,3,4", "5,6,7,8", ....] => out = [[1,2,3,4],[5,6,7,8],....] (make sure to take type into account)
  ListOfLinesOfData = dataStrToNumericData(lists:droplast(Lines) , ErlType),    %% drop empty line in the end of all CSV files
  SampleSize = length(hd(ListOfLinesOfData)),
  ListOfBatches = generateListOfBatches(ListOfLinesOfData, BatchSize),
  ListOfTensors = encodeListOfBatchesToNerlTensorsBinBatches(ListOfBatches, get(erl_tensor_type), NerlTensorType, SampleSize),
  {ListOfTensors, SampleSize}.

dataStrToNumeric_NumHandler({NumStr, ErlType}) -> 
  % NumStrFixed =
  %       case NumStr of
  %       [$-,$.|Rest]  -> "-0."++Rest;
  %       [$.|Rest]     -> "0."++Rest;
  %       Str -> Str
  %     end,
  case ErlType of
    erl_float -> 
      IsFloat = lists:member($. , NumStr), % Check if there is a decimal point (usually labels don't have)
      if IsFloat -> 
        Num = list_to_float(NumStr);
        true -> Num = list_to_float(NumStr++".0")
      end;
    erl_int -> Num = list_to_integer(NumStr);
    _ -> io:format("Error: unknown erl_tensor_type: ~p~n", [ErlType]) , Num = none
  end,
  Num.
      

dataStrToNumeric_lineHandler(PIPD, LineOfData, EtsTable, EtsKey, ErlType) -> 
  Splitted = string:split(binary_to_list(LineOfData), ",", all),
  SampleStrList = [{Sample, ErlType} || Sample <- Splitted],
  SampleNumericList = lists:map(fun dataStrToNumeric_NumHandler/1, SampleStrList),
  ets:insert(EtsTable, {EtsKey, SampleNumericList}),
  PIPD ! done.

dataStrToNumeric_sync(0) -> ok;
dataStrToNumeric_sync(PF) ->
  receive 
    done -> dataStrToNumeric_sync(PF-1);
    _Other -> throw("unexpected message in source parse")
  end.


dataStrToNumericParallelLoop(_PF, _EtsTable, [], _ErlType, _LastKey) -> done;
dataStrToNumericParallelLoop(PF, EtsTable, ListOfLinesOfData, ErlType, LastKey) when length(ListOfLinesOfData) >= PF -> % PF - Parallelization Factor
  {ListOfLinesOfDataToBeProcessed, ListOfLinesOfDataRest} = lists:split(PF, ListOfLinesOfData),
  IdxList = lists:seq(LastKey,LastKey+PF-1),
  PIPD = self(),
  lists:zipwith(fun(LineOfData, Idx) -> spawn_link(?MODULE,dataStrToNumeric_lineHandler,[PIPD, LineOfData, EtsTable, Idx, ErlType]) end, ListOfLinesOfDataToBeProcessed, IdxList),
  dataStrToNumeric_sync(PF),
  dataStrToNumericParallelLoop(PF, EtsTable, ListOfLinesOfDataRest, ErlType, LastKey+PF);

dataStrToNumericParallelLoop(PF, EtsTable, ListOfLinesOfData, ErlType, LastKey) ->
  PIPD = self(),
  CurrentKey = LastKey + 1,
  dataStrToNumeric_lineHandler(PIPD, hd(ListOfLinesOfData), EtsTable, CurrentKey, ErlType),
  receive 
    done -> dataStrToNumericParallelLoop(PF, EtsTable, tl(ListOfLinesOfData), ErlType, CurrentKey);
    _Other -> throw("unexpected message in source parse")
  end.
  

dataStrToNumericData(ListOfLinesOfData , ErlType)->
  EtsTable = ets:new(data_str_to_numeric_data, [ordered_set, public]),
  dataStrToNumericParallelLoop(?PARALLELIZATION_FACTOR, EtsTable, ListOfLinesOfData, ErlType, 0),
  NumericDataList = [ element(?DATA_IDX, Attribute) || Attribute <- ets:tab2list(EtsTable)],
  ets:delete(EtsTable), NumericDataList.

generateListOfBatches(ListOfList, BatchSize) -> generateListOfBatches(ListOfList, BatchSize, []).

generateListOfBatches([], _BatchSize, Ret) -> Ret;
generateListOfBatches(ListOfList, BatchSize, Ret) when (BatchSize >= length(ListOfList))-> Ret++[lists:flatten(ListOfList)];
generateListOfBatches(ListOfList, BatchSize, Ret) ->
  {NewBatch, Rest} = lists:split(BatchSize, ListOfList),
  generateListOfBatches(Rest, BatchSize, Ret ++ [lists:flatten(NewBatch)]).


batchesProcFunc(PPID, BatchesKey,BatchFunc) ->
      Batches = ets:lookup_element(encodeListOfBatchesToNerlTensorsBinBatches, BatchesKey, ?DATA_IDX),
      Result = lists:map(BatchFunc, Batches),
      ResultKeyName = list_to_atom(atom_to_list(BatchesKey)++"Res"),
      ets:insert(encodeListOfBatchesToNerlTensorsBinBatches, {ResultKeyName, Result}),
      PPID ! done.

encodeListOfBatchesToNerlTensorsBinBatches(ListOfBatches, ErlType, TargetBinaryType, SampleSize) ->
  NerlTensorAtom = list_to_atom(TargetBinaryType),
  BatchFunc =
    fun(Batch) ->
      NumOfSamples = round(length(Batch)/SampleSize),
      if NumOfSamples == 0 -> {<<>>, TargetBinaryType};
      true ->
        XDim = float(NumOfSamples),
        YDim = float(SampleSize),
        ZDim = 1.0,
        % io:format("sending conversion: ~p ~p ~n ",[{[XDim, YDim, ZDim | Batch], ErlType}, TargetBinaryType]),
        _NewTensor = nerlNIF:nerltensor_conversion({[XDim, YDim, ZDim | Batch], ErlType}, NerlTensorAtom)
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

