-module(nerlNIF).
-include_lib("kernel/include/logger.hrl").
-include("nerlTensor.hrl").

-export([init/0,nif_preload/0,create_nif/6, destroy_nif/1, get_active_models_ids_list/0, train_nif/6,call_to_train/6,predict_nif/3,call_to_predict/6,get_weights_nif/1,printTensor/2]).
-export([call_to_get_weights/2,call_to_set_weights/2]).
-export([decode_nif/2, nerltensor_binary_decode/2]).
-export([encode_nif/2, nerltensor_encode/5, nerltensor_conversion/2, get_all_binary_types/0, get_all_nerltensor_list_types/0]).
-export([erl_type_conversion/1]).

-import(nerl,[even/1, odd/1, string_format/2]).

-on_load(init/0).

% validation
-export([validate_nerltensor_erl/1]).

% math of nerltensors
-export([nerltensor_sum_nif/3]).
-export([nerltensor_scalar_multiplication_nif/3, nerltensor_scalar_multiplication_erl/2]).

% worker nif methods
-export([new_worker_nif/11, remove_worker_nif/0, test_worker_nif/11]).

init() ->
      NELNET_LIB_PATH = ?NERLNET_PATH++?BUILD_TYPE_RELEASE++"/"++?NERLNET_LIB,
      RES = erlang:load_nif(NELNET_LIB_PATH, 0),
      RES.

%% make sure nif can be loaded (activates on_load)
nif_preload() -> done.

% ModelID - Unique ID of the neural network model 
% ModelType - E.g. Regression, Classification 
create_nif(_ModelID, _ModelType , _ScalingMethod , _LayerTypesList , _LayersSizes , _LayersActivationFunctions) ->
      exit(nif_library_not_loaded).


%% Returns a list of active models ids
get_active_models_ids_list() ->
       exit(nif_library_not_loaded).

% Input:
% ModelId - a valid model id (of an already created model) - otherwise nif exception is raised!
% Return
% ok - if model destroyed
destroy_nif(_ModelID) ->
      exit(nif_library_not_loaded).

train_nif(_ModelID,_OptimizationMethod,_LossMethod, _LearningRate,_DataTensor,_Type) ->
      exit(nif_library_not_loaded).

call_to_train(ModelID,OptimizationMethod,LossMethod,LearningRate, {DataTensor, Type}, WorkerPid)->
      % io:format("before train  ~n "),
       %io:format("DataTensor= ~p~n ",[DataTensor]),
       %{FakeTensor, Type} = nerltensor_conversion({[2.0,4.0,1.0,1.0,2.0,3.0,4.0,5.0,6.0,7.0,8.0], erl_float}, float),
      _RetVal=train_nif(ModelID,OptimizationMethod,LossMethod,LearningRate, DataTensor, Type),
      %io:format("Train Time= ~p~n ",[RetVal]),
      receive
            Ret->
                  % io:format("Ret= ~p~n ",[Ret]),
                  %io:format("WorkerPid,{loss, Ret}: ~p , ~p ~n ",[WorkerPid,{loss, Ret}]),
                  gen_statem:cast(WorkerPid,{loss, Ret}) % TODO @Haran - please check what worker does with this Ret value 
            after ?TRAIN_TIMEOUT ->  %TODO inspect this timeout 
                  ?LOG_ERROR("Worker train timeout reached! setting loss = -1~n "),
                  gen_statem:cast(WorkerPid,{loss, -1.0})
      end.

call_to_predict(ModelID, BatchTensor, Type, WorkerPid,CSVname, BatchID)->
      % io:format("satrting pred_nif~n"),
      _RetVal = predict_nif(ModelID, BatchTensor, Type),
      receive
            
            [PredNerlTensor, NewType, TimeTook]->
                  % io:format("pred_nif done~n"),
                  % {PredTen, _NewType} = nerltensor_conversion({PredNerlTensor, NewType}, erl_float),
                  % io:format("Pred returned: ~p~n", [PredNerlTensor]),
                  gen_statem:cast(WorkerPid,{predictRes,PredNerlTensor, NewType, TimeTook,CSVname, BatchID});
            Error ->
                  ?LOG_ERROR("received wrong prediction_nif format:"++Error),
                  throw("received wrong prediction_nif format")
            after ?PREDICT_TIMEOUT -> 
                 % worker miss predict batch  TODO - inspect this code
                  ?LOG_ERROR("Worker prediction timeout reached! ~n "),
                  gen_statem:cast(WorkerPid,{predictRes, nan, CSVname, BatchID})
      end.

call_to_get_weights(ThisEts, ModelID)->
      try   
            ?LOG_INFO("Calling get weights in model ~p~n",{ModelID}),
            _RetVal = get_weights_nif(ModelID),
            recv_call_loop(ThisEts)
      catch Err:E -> ?LOG_ERROR("Couldnt get weights from worker~n~p~n",{Err,E}),
            []
      end.

%% sometimes the receive loop gets OTP calls that its not supposed to in high freq. wait for nerktensor of weights
recv_call_loop(ThisEts) ->
      receive
            {'$gen_cast', _Any} -> ?LOG_WARNING("Missed batch in call of get_weigths"),
                  ets:update_counter(ThisEts, missedBatches, 1),
                  recv_call_loop(ThisEts);
            NerlTensorWeights -> NerlTensorWeights
      end.

call_to_set_weights(ModelID,{WeightsNerlTensor, Type})->
      _RetVal = set_weights_nif(ModelID, WeightsNerlTensor, Type).

predict_nif(_ModelID, _BatchTensor, _Type) ->
      exit(nif_library_not_loaded).

get_weights_nif(_ModelID) ->
      exit(nif_library_not_loaded).

set_weights_nif(_ModelID, _Weights, _Type) ->
      exit(nif_library_not_loaded).

printTensor(List,_Type) when is_list(List) -> 
      exit(nif_library_not_loaded).


validate_nerltensor_erl(NerlTensorErl) ->
      {[X,Y,Z], NerlTensorRest} = lists:split(?NUMOF_DIMS, NerlTensorErl),
      TensorExpectedLength = trunc(X*Y*Z),
      % io:format("{X,Y,Z} = ~p, TensorLen (X*Y*Z)= ~p~n",[{X,Y,Z}, length(NerlTensorRest)]),
      if
            TensorExpectedLength == length(NerlTensorRest) -> true;
            true -> false
      end.

nerltensor_encode(X,Y,Z,List,Type) when is_number(X) and is_number(Y) and
                                        is_number(Z) and is_list(List) and is_atom(Type)->
      TensorExpectedLength = trunc(X*Y*Z),
      if
            TensorExpectedLength /= length(List) ->
                  throw(nerl:string_format("encode failure due to incorrect dimension declaring X*Y*Z not equal to tensor data length! ~p ",[{{X,Y,Z}, List}]));
            true -> ok
      end,
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

% Only float/double types are supported
nerltensor_scalar_multiplication_nif(_NerlTensorBinary, _BinaryType, _ScalarValue) -> 
      exit(nif_library_not_loaded). % returns {Binary, Type}

%---------- nerlTensor -----------%
nerltensor_binary_decode(Binary, Type) when erlang:is_binary(Binary) and erlang:is_atom(Type) ->
      NerlTensorListForm = decode_nif(Binary, Type),
      NerlTensorListForm.

% return the merged list of all supported binary types
get_all_binary_types() -> ?LIST_BINARY_FLOAT_NERLTENSOR_TYPE ++ ?LIST_BINARY_INT_NERLTENSOR_TYPE.
get_all_nerltensor_list_types() -> ?LIST_GROUP_NERLTENSOR_TYPE.
% nerltensor_conversion:
% Type is Binary then: Binary (Compressed Form) --> Erlang List
% Type is list then: Erlang List --> Binary
nerltensor_conversion({NerlTensor, Type}, ResType) ->
      TypeListGroup = lists:member(Type, get_all_nerltensor_list_types()),
      ResTypeListGroup = lists:member(ResType, get_all_nerltensor_list_types()),

      {Operation, ErlType, BinType} = 
                  case {TypeListGroup, ResTypeListGroup} of 
                  {true, false} -> {encode, Type, ResType};
                  {false, true} -> {decode, ResType, Type};
                  _ -> throw("invalid types combination")
                  end,
      
      BinTypeInteger = lists:member(BinType, ?LIST_BINARY_INT_NERLTENSOR_TYPE),
      BinTypeFloat = lists:member(BinType, ?LIST_BINARY_FLOAT_NERLTENSOR_TYPE),
      
      % Wrong combination guard
      case ErlType of 
      erl_float when BinTypeFloat-> ok;
      erl_int when BinTypeInteger -> ok;
      _ -> throw("invalid types combination")
      end,
      
      case Operation of 
            encode -> Validated = validate_nerltensor_erl(NerlTensor),
                      if
                        Validated -> encode_nif(NerlTensor, BinType);
                        true -> io:format("Wrong NerlTensor size!~n"), {<<>>, BinType}
                        % true -> throw(nerl:string_format("encode failure due to incorrect dimension declaring X*Y*Z not equal to tensor data length! ~p ",[NerlTensor]))
                      end;
            decode -> decode_nif(NerlTensor, BinType);
            _ -> throw("wrong operation")
      end.

%% get BinType (float, double...) -> ErlType (erl_float / erl_int)
erl_type_conversion(BinType) ->
      {_, ErlType} = lists:keyfind(BinType, 1, ?NERL_TYPES),
      ErlType.

nerltensor_scalar_multiplication_erl({NerlTensorErl, Type}, ScalarValue) -> 
      ListGroup = lists:member(Type, get_all_nerltensor_list_types()),
      if 
            ListGroup ->
                  Dims = lists:sublist(NerlTensorErl, 1, ?NUMOF_DIMS),
                  NerlTensorErl_NODIMS = lists:sublist(NerlTensorErl, ?NUMOF_DIMS + 1, length(NerlTensorErl) - ?NUMOF_DIMS),
                  Dims ++ lists:map(fun(X) -> X * ScalarValue end, NerlTensorErl_NODIMS);
            true -> throw("Bad Type")
      end.



%%%%%% NerlWorker NIF Methods %%%%%%

new_worker_nif(_ModelType, _LayersSizes, _LayersTypes, _LayersFunctionalityCodes, _LearningRate, _Epochs, _OptimizerType,
_OptimizerArgs, _LossMethod, _DistributedSystemType, _DistributedSystemArgs) ->
      exit(nif_library_not_loaded).

remove_worker_nif() ->
      exit(nif_library_not_loaded).

%% All of inputs must be binary strings!
test_worker_nif(_ModelType, _LayersSizes, _LayersTypes, _LayersFunctionalityCodes, _LearningRate, _Epochs, _OptimizerType,
                _OptimizerArgs, _LossMethod, _DistributedSystemType, _DistributedSystemArgs) ->
      exit(nif_library_not_loaded).