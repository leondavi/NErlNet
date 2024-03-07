-module(nerlTensor).
-author("David Leon").

-include("nerlTensor.hrl").

-import(nerlNIF,[nerltensor_sum_nif/3]).

-export([nerltensor_sum_erl/2]).
-export([sum_nerltensors_lists/2, sum_nerltensors_lists_erl/2 , split_cols_erl_tensor/3]).

get_all_nerltensor_list_types() -> ?LIST_GROUP_NERLTENSOR_TYPE.

split_cols_erl_tensor(Tensor , _DataType , SplitColumnIdx) -> %% DataType should determine the variable type for the dimensions and data
      [DimX, DimY, DimZ | Data] = Tensor,
      {FeaturesSamples , LabelsSamples} = split_data(Data , round(DimY) , SplitColumnIdx , [] , []),
      {[DimX , float(SplitColumnIdx) , DimZ] ++ FeaturesSamples , [DimX , DimY - SplitColumnIdx , DimZ] ++ LabelsSamples}.

split_data(Data , NumCols , ColumnIndex , Acc1 , Acc2) when is_list(Data) ->
      %% io:format("Acc1: ~p~n", [Acc1]),
      %% io:format("Acc2: ~p~n", [Acc2]),
      if
            length(Data) == 0 ->
                  {Acc1 , Acc2};
            true ->
                  {RowPart1 , RestDataTemp} = lists:split(ColumnIndex , Data),
                  {RowPart2 , RestData} = lists:split(NumCols - ColumnIndex , RestDataTemp), 
                  Split1 = Acc1 ++ RowPart1,
                  Split2 = Acc2 ++ RowPart2,
                  split_data(RestData , NumCols , ColumnIndex , Split1 , Split2)
      end.
      

nerltensor_sum_erl({NerlTensorErlA, Type}, {NerlTensorErlB, Type}) ->
      ListGroup = lists:member(Type, get_all_nerltensor_list_types()),
      if ListGroup ->
            Dims = lists:sublist(NerlTensorErlA, 1, ?NUMOF_DIMS),
            NerlTensorErlA_NODIMS = lists:sublist(NerlTensorErlA, ?NUMOF_DIMS + 1, length(NerlTensorErlA) - ?NUMOF_DIMS),
            %io:format("nerltensorA nodims: ~p~n", [NerlTensorErlA_NODIMS]),
            NerlTensorErlB_NODIMS = lists:sublist(NerlTensorErlB, ?NUMOF_DIMS + 1, length(NerlTensorErlB) - ?NUMOF_DIMS),
           % io:format("nerltensorB nodims: ~p~n", [NerlTensorErlB_NODIMS]),
            Dims ++ lists:zipwith(fun(X,Y) -> X + Y end, NerlTensorErlA_NODIMS, NerlTensorErlB_NODIMS);
         true -> throw("Bad Type")
      end.

sum_nerltensors_lists_erl([], _ErlType) ->  throw("Zero length given to sum_nerltensors_even_lists");
sum_nerltensors_lists_erl(NerltensorList, _ErlType) when length(NerltensorList) == 1 ->  NerltensorList;
sum_nerltensors_lists_erl(NerltensorList, ErlType)  -> 
      OddLength = nerl:odd(length(NerltensorList)),
      {OddFirstElement, EvenNerltensorList} =  
      if OddLength -> {hd(NerltensorList), tl(NerltensorList)};
         true -> {[], NerltensorList}
      end,

      HalfSize = round(length(EvenNerltensorList)/2),
      % Split to high and low lists
      NerlTensorsHalfListA = lists:sublist(EvenNerltensorList, HalfSize),
      NerlTensorsHalfListB = lists:sublist(EvenNerltensorList, HalfSize + 1, HalfSize),

      % sum high and low lists
      SumResultOfTwoHalfs = lists:zipwith(fun(NerlTensorA,NerlTensorB) -> nerltensor_sum_erl({NerlTensorA, ErlType}, {NerlTensorB, ErlType}) end, NerlTensorsHalfListA, NerlTensorsHalfListB),
      % take care to the first element in case of odd length
      SumResultTwoHalfsWithOddFirst = 
      if OddLength -> [nerltensor_sum_erl({OddFirstElement, ErlType}, {hd(SumResultOfTwoHalfs), ErlType})];
      true -> SumResultOfTwoHalfs % nothing to do with first element in case of even list
      end,
      sum_nerltensors_lists_erl(SumResultTwoHalfsWithOddFirst, ErlType).

% sum_nerltensors_lists:
% sums list of nerltensors: NT1 + NT2 + NT3 + ... + NTn
% NerltensorList - list of nerltensors (NT1, NT2, NT3, ..., NTn)
% BinaryType should be the same for all NerlTesnors
sum_nerltensors_lists([], _BinaryType) ->  throw("Zero length given to sum_nerltensors_even_lists");
sum_nerltensors_lists(NerltensorList, _BinaryType) when length(NerltensorList) == 1 ->  NerltensorList;
sum_nerltensors_lists(NerltensorList, BinaryType) -> 
      OddLength = nerl:odd(length(NerltensorList)),
      {OddFirstElement, EvenNerltensorList} =  
      if OddLength -> {hd(NerltensorList), tl(NerltensorList)};
      true -> {[], NerltensorList}
      end,

      HalfSize = round(length(EvenNerltensorList)/2),
      % Split to high and low lists
      NerlTensorsHalfListA = lists:sublist(EvenNerltensorList, HalfSize),
      NerlTensorsHalfListB = lists:sublist(EvenNerltensorList, HalfSize + 1, HalfSize),

      % sum high and low lists
      SumResultOfTwoHalfs = lists:zipwith(fun(NerlTensorA,NerlTensorB) -> element(1,nerlNIF:nerltensor_sum_nif(NerlTensorA, NerlTensorB, BinaryType)) end, NerlTensorsHalfListA, NerlTensorsHalfListB),

      % take care to the first element in case of odd length
      SumResultTwoHalfsWithOddFirst = 
      if OddLength -> [element(1,nerlNIF:nerltensor_sum_nif(OddFirstElement, hd(SumResultOfTwoHalfs), BinaryType))];
      true -> SumResultOfTwoHalfs % nothing to do with first element in case of even list
      end,
      sum_nerltensors_lists(SumResultTwoHalfsWithOddFirst, BinaryType).