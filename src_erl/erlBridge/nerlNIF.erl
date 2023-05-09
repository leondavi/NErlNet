-module(nerlNIF).
-include_lib("kernel/include/logger.hrl").

-import(nerl,[tic/0, toc/1]).

-export([init/0,create_nif/6,train_nif/5,call_to_train/6,predict_nif/2,call_to_predict/5,get_weights_nif/1,printTensor/2]).
-export([call_to_get_weights/1,call_to_set_weights/2]).
-export([decode_nif/2, nerltensor_binary_decode/2]).
-export([encode_nif/2, nerltensor_encode/5, nerltensor_conversion/2, get_all_binary_types/0]).

-define(FILE_IDENTIFIER,"[NERLNIF] ").
-define(NERLNET_LIB,"libnerlnet").
-define(NERLNET_PATH,"/usr/local/lib/nerlnet-lib/NErlNet").
-define(BUILD_TYPE_DEBUG,"debug").
-define(BUILD_TYPE_RELEASE,"/build/release").

-define(THIS_FILE_PATH_RELATIVE_TO_PROJECT_ROOT,"src_erl"). % if this file moves to inner place than update this define
-on_load(init/0).

-define(PREDICT_TIMEOUT,10000). % 10 seconds limit for prediction results
-define(TRAIN_TIMEOUT,20000). % 20 seconds limit for prediction results

%nerltensor
-define(NUMOF_DIMS,3).
-include("nerlTensor.hrl").

-export([nerltensor_sum_nif/3]).
-export([nerltensor_sum_erl/2]).

init() ->
      NELNET_LIB_PATH = ?NERLNET_PATH++?BUILD_TYPE_RELEASE++"/"++?NERLNET_LIB,
      RES = erlang:load_nif(NELNET_LIB_PATH, 0),
      RES.


% ModelID - Unique ID of the neural network model 
% ModelType - E.g. Regression, Classification 
create_nif(_ModelID, _ModelType , _ScalingMethod , _LayerTypesList , _LayersSizes , _LayersActivationFunctions) ->
      exit(nif_library_not_loaded).

train_nif(_ModelID,_OptimizationMethod,_LossMethod, _LearningRate,_DataTensor) -> %TODO change to trainn_nif
      exit(nif_library_not_loaded).

call_to_train(ModelID,OptimizationMethod,LossMethod,LearningRate, DataTensor, WorkerPid)->
      % io:format("berfor train  ~n "),
       %io:format("DataTensor= ~p~n ",[DataTensor]),
      _RetVal=train_nif(ModelID,OptimizationMethod,LossMethod,LearningRate, DataTensor),
      %io:format("Train Time= ~p~n ",[RetVal]),
      receive
            Ret->
                  % io:format("Ret= ~p~n ",[Ret]),
                  %io:format("WorkerPid,{loss, Ret}: ~p , ~p ~n ",[WorkerPid,{loss, Ret}]),
                  gen_statem:cast(WorkerPid,{loss, Ret}) % TODO @Haran - please check what worker does with this Ret value 
            after ?TRAIN_TIMEOUT ->  %TODO inspect this timeout 
                  logger:error(?FILE_IDENTIFIER++"Worker train timeout reached! ~n "),
                  gen_statem:cast(WorkerPid,{loss, -1.0})
      end.

call_to_predict(ModelID, Data, WorkerPid,CSVname, BatchID)->
      _RetVal = predict_nif(ModelID, Data),
      receive
            Ret-> gen_statem:cast(WorkerPid,{predictRes,Ret,CSVname, BatchID}) % TODO @Haran - please check what worker does with this Ret value 
            after ?PREDICT_TIMEOUT -> 
                 % worker miss predict batch  TODO - inspect this code
                  logger:error(?FILE_IDENTIFIER++"Worker prediction timeout reached! ~n "),
                  gen_statem:cast(WorkerPid,{predictRes, nan, CSVname, BatchID})
      end.

call_to_get_weights(ModelID)->
      try
            _RetVal = get_weights_nif(ModelID),
            % io:format("RetVal= ~p~n ",[RetVal]),
            receive
                  Ret->Ret
                  % io:format("Ret= ~p~n ",[Ret])
            end
      catch Err:E -> logger:error(?FILE_IDENTIFIER++"Couldnt get weights from worker~n~p~n",{Err,E}),
            []
      end.

call_to_set_weights(ModelID,Weights)->
      _RetVal = set_weights_nif(ModelID, Weights).

predict_nif(_ModelID, _Data) ->
      exit(nif_library_not_loaded).

get_weights_nif(_ModelID) ->
      exit(nif_library_not_loaded).

set_weights_nif(_ModelID, _Weights) ->
      exit(nif_library_not_loaded).

printTensor(List,_Type) when is_list(List) -> 
      exit(nif_library_not_loaded).


nerltensor_encode(X,Y,Z,List,Type) when is_number(X) and is_number(Y) and
                                        is_number(Z) and is_list(List) and is_atom(Type)-> 
      case Type of
            erl_float -> {[X,Y,Z] ++ List, erl_float}; % Make sure list of float
            erl_int -> {[X,Y,Z] ++ List, erl_int}; % make sure list of integers
            _COMPRESSED_TYPE -> encode_nif([X,Y,Z] ++ List, Type) % returns {Binary, Type}
      end.

% Input: List and the type of the encoded binary (atom from the group ?BINARY_GROUP_NERLTENSOR_TYPE)
% Output: {Binary,BinaryType}
% Warning - if _XYZ_LIST_FORM type is double it can be cast to integer if binaryType is an integer
encode_nif(_XYZ_LIST_FORM, _BinaryType)  when erlang:is_list(_XYZ_LIST_FORM) and erlang:is_atom(_BinaryType) ->
      exit(nif_library_not_loaded). 

% Input: Binary and Binary Type (atom from the group ?BINARY_GROUP_NERLTENSOR_TYPE)
% Output: {List, ListType} (ListType is an atom from the group ?LIST_GROUP_NERLTENSOR_TYPE)
decode_nif(_Binary, _BinaryType) when erlang:is_binary(_Binary) and erlang:is_atom(_BinaryType) ->
      exit(nif_library_not_loaded). % returns {List,ListType}

% Only float/double types are supported
nerltensor_sum_nif(_BinaryA, _BinaryB, _Mutual_Binary_Type) -> 
      exit(nif_library_not_loaded). % returns {Binary, Type}

%---------- nerlTensor -----------%
nerltensor_binary_decode(Binary, Type) when erlang:is_binary(Binary) and erlang:is_atom(Type) ->
      NerlTensorListForm = decode_nif(Binary, Type),
      NerlTensorListForm.

% return the merged list of all supported binary types
get_all_binary_types() -> ?LIST_BINARY_FLOAT_NERLTENSOR_TYPE ++ ?LIST_BINARY_INT_NERLTENSOR_TYPE.

% nerltensor_conversion:
% Type is Binary then: Binary (Compressed Form) --> Erlang List
% Type is list then: Erlang List --> Binary
nerltensor_conversion({NerlTensor, Type}, ResType) -> 
      BinaryGroup = lists:member(Type, get_all_binary_types()), % compressed type
      ListGroup = lists:member(Type, ?LIST_GROUP_NERLTENSOR_TYPE), % non compressed, list type
      case ResType of 
            ResType when BinaryGroup ->  decode_nif(NerlTensor,Type); % returns {Binary, Type}
            ResType when ListGroup -> encode_nif(NerlTensor,Type); % returns {Binary, Type}
            _ERROR -> error % TODO add log here
      end.

nerltensor_sum_erl({NerlTensorErlA, Type}, {NerlTensorErlB, Type}) ->
      ListGroup = lists:member(Type, ?LIST_GROUP_NERLTENSOR_TYPE),
      if ListGroup ->
            DIMS = lists:sublist(NerlTensorErlA, 1, ?NUMOF_DIMS),
            NerlTensorErlA_NODIMS = lists:sublist(NerlTensorErlA, ?NUMOF_DIMS + 1, length(NerlTensorErlA) - ?NUMOF_DIMS),
            %io:format("nerltensorA nodims: ~p~n", [NerlTensorErlA_NODIMS]),
            NerlTensorErlB_NODIMS = lists:sublist(NerlTensorErlB, ?NUMOF_DIMS + 1, length(NerlTensorErlB) - ?NUMOF_DIMS),
           % io:format("nerltensorB nodims: ~p~n", [NerlTensorErlB_NODIMS]),
      
            DIMS ++ lists:zipwith(fun(X,Y) -> X + Y end, NerlTensorErlA_NODIMS, NerlTensorErlB_NODIMS);
         true -> throw("Bad Type")
      end.



