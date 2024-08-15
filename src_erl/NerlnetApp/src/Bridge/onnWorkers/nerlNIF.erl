-module(nerlNIF).
-include_lib("kernel/include/logger.hrl").
-include("../nerlTensor.hrl").

-export([init/0,nif_preload/0,get_active_models_ids_list/0, train_nif/3,update_nerlworker_train_params_nif/6,call_to_train/5,predict_nif/3,call_to_predict/5,get_weights_nif/1,printTensor/2]).
-export([call_to_get_weights/1,call_to_set_weights/2]).
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

% nerlworker nif methods
-export([new_nerlworker_nif/14, remove_nerlworker_nif/1, test_nerlworker_nif/14,get_distributed_system_train_labels_count_nif/1]).

init() ->
      NELNET_LIB_PATH = ?NERLNET_PATH++?BUILD_TYPE_RELEASE++"/"++?NERLNET_LIB,
      % io:format("PATH: ~p~n",[NELNET_LIB_PATH]),
      RES = erlang:load_nif(NELNET_LIB_PATH, 0), %% CRASHES HERE
      RES.

%% make sure nif can be loaded (activates on_load)
nif_preload() -> done.


%% Returns a list of active models ids
get_active_models_ids_list() ->
       exit(nif_library_not_loaded).

% Input:
% ModelId - a valid model id (of an already created model) - otherwise nif exception is raised!
% Return
% ok - if model destroyed

train_nif(_ModelID,_DataTensor,_Type) ->
      exit(nif_library_not_loaded).

update_nerlworker_train_params_nif(_ModelID,_LearningRate,_Epochs,_OptimizerType,_OptimizerArgs,_LossMethod) ->
      exit(nif_library_not_loaded).

call_to_train(ModelID, {DataTensor, Type}, WorkerPid , BatchID , SourceName)-> 
      ok = train_nif(ModelID, DataTensor, Type),
      receive
            {nerlnif, nan, TrainTime} -> 
                  gen_statem:cast(WorkerPid,{loss, nan , TrainTime , BatchID , SourceName}); %TODO Guy - Please the behavior when this case happens
            {nerlnif , LossTensor, LossTensorType , TrainTime}->
                  gen_statem:cast(WorkerPid,{loss, {LossTensor, LossTensorType} , TrainTime , BatchID , SourceName})
            after ?TRAIN_TIMEOUT ->  %TODO inspect this timeout 
                  ?LOG_ERROR("Worker train timeout reached! bid:~p s:~p",[BatchID , SourceName]),
                  gen_statem:cast(WorkerPid,{loss, timeout , SourceName}) %% TODO Guy Define train timeout state 
      end.

call_to_predict(ModelID, {BatchTensor, Type}, WorkerPid, BatchID , SourceName)->
      ok = predict_nif(ModelID, BatchTensor, Type),
      receive
            
            {nerlnif , PredNerlTensor, PredNerlTensorType, TimeNif}-> %% nerlnif atom means a message from the nif implementation
                  % io:format("pred_nif done~n"),
                  % {PredTen, _NewType} = nerltensor_conversion({PredNerlTensor, NewType}, erl_float),
                  % io:format("Pred returned: ~p~n", [PredNerlTensor]),
                  gen_statem:cast(WorkerPid,{predictRes,PredNerlTensor, PredNerlTensorType, TimeNif, BatchID , SourceName});
            Error ->
                  ?LOG_ERROR("received wrong prediction_nif format: ~p" ,[Error]),
                  throw("received wrong prediction_nif format")
            after ?PREDICT_TIMEOUT -> 
                 % worker miss predict batch  TODO - inspect this code
                  ?LOG_ERROR("Worker prediction timeout reached! ~n "),
                  gen_statem:cast(WorkerPid,{predictRes, nan, BatchID , SourceName})
      end.

% This function calls to get_weights_nif() and waits for the result using receive block
% Returns {NerlTensorWeights , BinaryType} 
call_to_get_weights(ModelID)->
      try   
            ?LOG_INFO("Calling get weights in model ~p~n",{ModelID}),
            _RetVal = get_weights_nif(ModelID),
            recv_call_loop()
      catch Err:E -> ?LOG_ERROR("Couldnt get weights from worker~n~p~n",{Err,E}),
            []
      end.

%% sometimes the receive loop gets OTP calls that its not supposed to in high freq. wait for nerktensor of weights
recv_call_loop() ->
      receive
            {'$gen_cast', _Any} -> ?LOG_WARNING("Missed batch in call of get_weigths"),
                  recv_call_loop();
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


validate_nerltensor_erl(NerlTensorErl) when is_list(NerlTensorErl) ->
      {[X,Y,Z], NerlTensorRest} = lists:split(?NUMOF_DIMS, NerlTensorErl),
      TensorExpectedLength = trunc(X*Y*Z),
      % io:format("{X,Y,Z} = ~p, TensorLen (X*Y*Z)= ~p~n",[{X,Y,Z}, length(NerlTensorRest)]),
      if
            TensorExpectedLength == length(NerlTensorRest) -> true;
            true -> false
      end.

%% return {Binary, BinaryType}
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
            erl_float when BinTypeFloat -> ok;
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
            decode -> 
                  if 
                        is_binary(NerlTensor) -> decode_nif(NerlTensor, BinType);
                        true -> throw("Given non-binary NerlTensor for decoding!")
                  end;
            _ -> throw("wrong operation")
      end.

%% get BinType (float, double...) -> ErlType (erl_float / erl_int)
erl_type_conversion(BinType) ->
      {_, ErlType} = lists:keyfind(BinType, 1, ?NERL_TYPES),
      % TODO Throw exception if wrong type
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

new_nerlworker_nif(_ModelId,_ModelType, _ModelArgs , _LayersSizes, _LayersTypes, _LayersFunctionalityCodes, _LearningRate, _Epochs, _OptimizerType,
_OptimizerArgs, _LossMethod, _LossArgs, _DistributedSystemType, _DistributedSystemArgs) ->
      exit(nif_library_not_loaded).

remove_nerlworker_nif(_ModelId) ->
      exit(nif_library_not_loaded).

%% All of inputs must be binary strings! except for _ModelId which is an integer
test_nerlworker_nif(_ModelId,_ModelType, _ModelArgs, _LayersSizes, _LayersTypes, _LayersFunctionalityCodes, _LearningRate, _Epochs, _OptimizerType,
                _OptimizerArgs, _LossMethod, _LossArgs, _DistributedSystemType, _DistributedSystemArgs) ->
      exit(nif_library_not_loaded).

%  input -  unsigned long modelId 
%  output - nerltensor that is the acc sum of each label in the last training data_set
% distributed system type should be FedClientWeightedAvgClassification
get_distributed_system_train_labels_count_nif(_ModelId) ->
       exit(nif_library_not_loaded).