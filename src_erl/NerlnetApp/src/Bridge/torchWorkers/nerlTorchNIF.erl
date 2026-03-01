-module(nerlTorchNIF).
-include_lib("kernel/include/logger.hrl").
-include("torchDefs.hrl").

-export([init/0,nif_preload/0,get_active_models_ids_list/0, train_nif/3,train_microbatch_nif/4,optimizer_barrier_nif/1,
         pipeline_stage0_forward_nif/5,pipeline_stage_forward_nif/7,pipeline_stage_last_forward_backward_nif/7,pipeline_stage_backward_nif/5,
         pipeline_predict_stage0_forward_nif/3,pipeline_predict_stage_forward_nif/3,
         update_nerlworker_train_params_nif/6,call_to_train/4,call_to_train_microbatch/5,call_to_optimizer_barrier/1,
         call_to_pipeline_stage0_forward/4,call_to_pipeline_stage_forward/6,call_to_pipeline_stage_last_forward_backward/6,
         call_to_pipeline_stage_backward/4,call_to_pipeline_predict_stage0_forward/3,call_to_pipeline_predict_stage_forward/3,
         predict_nif/3,call_to_predict/4,get_weights_nif/1,set_weights_nif/3,printTensor/2]).
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
-export([nerltensor_split_nif/4, nerltensor_concat_nif/3, nerltensor_reduce_sum_list_nif/2]).

% nerlworker nif methods
-export([new_nerlworker_nif/4, remove_nerlworker_nif/1, test_nerlworker_nif/4,get_distributed_system_train_labels_count_nif/1]).

% negotiators for train and predict
-export([start_train_negotiator/2, stop_train_negotiator/0, start_predict_negotiator/2, stop_predict_negotiator/0]).
-export([train_negotiator/4, predict_negotiator/4]).


init() ->
      BasePath = get_env_or_default("NERLNET_PATH", ?NERLNET_PATH),
      BuildSuffix = get_env_or_default("NERL_BUILD_TYPE", ?BUILD_TYPE_RELEASE),
      LibName = get_env_or_default("NERLTORCH_LIB", ?NERLTORCH_LIB),
      BuildPath = BasePath ++ BuildSuffix,
      NerlLibPath = filename:join(BuildPath, LibName),
      % Avoid io:format during -on_load as it crashes in release mode before IO is ready
      % Use logger which queues messages safely, or defer to application start
      case catch erlang:load_nif(NerlLibPath, 0) of
            ok -> 
                  ?LOG_INFO("[nerlTorchNIF] Successfully loaded Torch NIF from ~ts", [NerlLibPath]),
                  ok;
            {error, Reason} ->
                  ?LOG_ERROR("[nerlTorchNIF] Failed to load Torch NIF from ~ts reason: ~p", [NerlLibPath, Reason]),
                  erlang:error({failed_to_load_torch_nif, NerlLibPath, Reason});
            {'EXIT', Reason} ->
                  ?LOG_ERROR("[nerlTorchNIF] Torch NIF loader crashed for ~ts reason: ~p", [NerlLibPath, Reason]),
                  erlang:error({failed_to_load_torch_nif, NerlLibPath, Reason})
      end.

get_env_or_default(Key, Default) ->
      case os:getenv(Key) of
            false -> Default;
            Value -> Value
      end.

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

train_microbatch_nif(_ModelID, _DataTensor, _Type, _MicrobatchID) ->
      exit(nif_library_not_loaded).

optimizer_barrier_nif(_ModelID) ->
      exit(nif_library_not_loaded).

pipeline_stage0_forward_nif(_ModelID, _DataTensor, _Type, _BatchID, _MicrobatchID) ->
      exit(nif_library_not_loaded).

pipeline_stage_forward_nif(_ModelID, _ActivationTensor, _ActivationType, _LabelsTensor, _LabelsType, _BatchID, _MicrobatchID) ->
      exit(nif_library_not_loaded).

pipeline_stage_last_forward_backward_nif(_ModelID, _ActivationTensor, _ActivationType, _LabelsTensor, _LabelsType, _BatchID, _MicrobatchID) ->
      exit(nif_library_not_loaded).

pipeline_stage_backward_nif(_ModelID, _GradTensor, _GradType, _BatchID, _MicrobatchID) ->
      exit(nif_library_not_loaded).

pipeline_predict_stage0_forward_nif(_ModelID, _DataTensor, _Type) ->
      exit(nif_library_not_loaded).

pipeline_predict_stage_forward_nif(_ModelID, _ActivationTensor, _ActivationType) ->
      exit(nif_library_not_loaded).

update_nerlworker_train_params_nif(_ModelID,_LearningRate,_Epochs,_OptimizerType,_OptimizerArgs,_LossMethod) ->
      exit(nif_library_not_loaded).


% Train Negotiator process - to handle train requests without spawning for each batch
% This process is spawned once per phase by the worker statem
% ModelID - the model to use for training
% WorkerPid - the pid of the worker statem to send back the results
start_train_negotiator(ModelID, WorkerPid) ->
      put(nerlnif_train_negotiator_pid, spawn(fun() -> nerlTorchNIF:train_negotiator(ModelID, WorkerPid, none, none) end)).

stop_train_negotiator() ->
      TrainNegotiatorPID = get(nerlnif_train_negotiator_pid),
      TrainNegotiatorPID ! {nerlnif_stop_train},
      put(nerlnif_train_negotiator_pid, undefined),
      ok.

train_negotiator(ModelID, WorkerPid, BatchID, SourceName) ->
      receive
            {start_train , CurrentSourceName, CurrentBatchID, ModelID, DataTensor, Type} ->
                  ok = train_nif(ModelID, DataTensor, Type), train_negotiator(ModelID, WorkerPid, CurrentBatchID, CurrentSourceName);
            {start_train_microbatch, CurrentSourceName, CurrentBatchID, CurrentMicrobatchID, ModelID, DataTensor, Type} ->
                  ok = train_microbatch_nif(ModelID, DataTensor, Type, CurrentMicrobatchID),
                  train_negotiator(ModelID, WorkerPid, CurrentBatchID, CurrentSourceName);
            {optimizer_barrier, ModelID} ->
                  _ = optimizer_barrier_nif(ModelID),
                  train_negotiator(ModelID, WorkerPid, BatchID, SourceName);
            {nerlnif, nan, TrainTime} -> 
                  gen_statem:cast(WorkerPid,{loss, nan , TrainTime , BatchID , SourceName}), train_negotiator(ModelID, WorkerPid, BatchID, SourceName);
            {nerlnif , LossTensor, LossTensorType , TrainTime}-> % TrainTime is in microseconds
                  gen_statem:cast(WorkerPid,{loss, {LossTensor, LossTensorType} , TrainTime , BatchID , SourceName}), train_negotiator(ModelID, WorkerPid, BatchID, SourceName);
            {nerlnif , LossTensor, LossTensorType , TrainTime, MicrobatchID}-> % microbatch parallel mode
                  gen_statem:cast(WorkerPid,{loss_microbatch, {LossTensor, LossTensorType}, TrainTime, BatchID, SourceName, MicrobatchID}),
                  train_negotiator(ModelID, WorkerPid, BatchID, SourceName);
            {nerlnif_stop_train} ->
                  ok
            % after ?TRAIN_TIMEOUT ->  %TODO inspect this timeout 
            %       ?LOG_ERROR("Worker train timeout reached! bid:~p s:~p",[BatchID , SourceName]),
            %       gen_statem:cast(WorkerPid,{loss, timeout , BatchID , SourceName}),
            %       train_negotiator(ModelID, WorkerPid, BatchID, SourceName)
      end.

call_to_train(ModelID, {DataTensor, Type} , BatchID , SourceName) ->
      TrainNegotiatorPID = get(nerlnif_train_negotiator_pid),
      % send the batch to the nif for training
      TrainNegotiatorPID ! {start_train , SourceName, BatchID, ModelID, DataTensor, Type},
      ok.

call_to_train_microbatch(ModelID, {DataTensor, Type}, BatchID, SourceName, MicrobatchID) ->
      TrainNegotiatorPID = get(nerlnif_train_negotiator_pid),
      TrainNegotiatorPID ! {start_train_microbatch, SourceName, BatchID, MicrobatchID, ModelID, DataTensor, Type},
      ok.

call_to_optimizer_barrier(ModelID) ->
      optimizer_barrier_nif(ModelID).

call_to_pipeline_stage0_forward(ModelID, {DataTensor, Type}, BatchID, MicrobatchID) ->
      pipeline_stage0_forward_nif(ModelID, DataTensor, Type, BatchID, MicrobatchID).

call_to_pipeline_stage_forward(ModelID, {ActivationTensor, ActivationType}, {LabelsTensor, LabelsType}, BatchID, _SourceName, MicrobatchID) ->
      pipeline_stage_forward_nif(ModelID, ActivationTensor, ActivationType, LabelsTensor, LabelsType, BatchID, MicrobatchID).

call_to_pipeline_stage_last_forward_backward(ModelID, {ActivationTensor, ActivationType}, {LabelsTensor, LabelsType}, BatchID, _SourceName, MicrobatchID) ->
      pipeline_stage_last_forward_backward_nif(ModelID, ActivationTensor, ActivationType, LabelsTensor, LabelsType, BatchID, MicrobatchID).

call_to_pipeline_stage_backward(ModelID, {GradTensor, GradType}, BatchID, MicrobatchID) ->
      pipeline_stage_backward_nif(ModelID, GradTensor, GradType, BatchID, MicrobatchID).

call_to_pipeline_predict_stage0_forward(ModelID, {DataTensor, Type}, _BatchID) ->
      pipeline_predict_stage0_forward_nif(ModelID, DataTensor, Type).

call_to_pipeline_predict_stage_forward(ModelID, {ActivationTensor, ActivationType}, _BatchID) ->
      pipeline_predict_stage_forward_nif(ModelID, ActivationTensor, ActivationType).

% Predict Negotiator process - to handle predict requests without spawning for each batch
% This process is spawned once per phase by the worker statem
% ModelID - the model to use for prediction
% WorkerPid - the pid of the worker statem to send back the results
start_predict_negotiator(ModelID, WorkerPid) ->
      put(nerlnif_predict_negotiator_pid, spawn(fun() -> nerlTorchNIF:predict_negotiator(ModelID, WorkerPid, none, none) end)).

stop_predict_negotiator() ->
      PredictNegotiatorPID = get(nerlnif_predict_negotiator_pid),
      PredictNegotiatorPID ! {nerlnif_stop_predict},
      put(nerlnif_predict_negotiator_pid, undefined),
      ok.

predict_negotiator(ModelID, WorkerPid, BatchID, SourceName) ->
      receive
            {start_predict, CurrentSourceName, CurrentBatchID, ModelID, DataTensor, Type} ->
                  ok = predict_nif(ModelID, DataTensor, Type),
                  predict_negotiator(ModelID, WorkerPid, CurrentBatchID, CurrentSourceName); % here we update the current batch id and source name
            {nerlnif , PredNerlTensor, PredNerlTensorType, TimeNif}-> %% nerlnif atom means a message from the nif implementation
                  gen_statem:cast(WorkerPid,{predictRes,PredNerlTensor, PredNerlTensorType, TimeNif, BatchID , SourceName}), % here we use the already updated batch id and source name
                  predict_negotiator(ModelID, WorkerPid, BatchID, SourceName); 
            {nerlnif_stop_predict} ->
                  ok
            % after ?PREDICT_TIMEOUT ->
            %       % worker miss predict batch  TODO - inspect this code
            %       ?LOG_ERROR("Worker prediction timeout reached! ~n "),
            %       gen_statem:cast(WorkerPid,{predictRes, timeout, BatchID , SourceName}),
            %       predict_negotiator(ModelID, WorkerPid, BatchID, SourceName)
      end.

call_to_predict(ModelID, {BatchTensor, Type} , BatchID , SourceName)-> 
      PredictNegotiatorPID = get(nerlnif_predict_negotiator_pid),
      % send the batch to the nif for prediction
      PredictNegotiatorPID ! {start_predict, SourceName, BatchID, ModelID, BatchTensor, Type},
      ok.

save_to_file([]) -> 
      file:write_file("/tmp/nerlnet/predict_error.csv", io_lib:fwrite("~n", []), [append]);
save_to_file(List) ->
      file:write_file("/tmp/nerlnet/predict_error.csv", io_lib:fwrite("~p,", [hd(List)]), [append]),
      save_to_file(tl(List)).

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

nerltensor_split_nif(NerlTensorBinary, BinaryType, NumShards, Axis) ->
      {DecodedTensor, TensorListType} = decode_nif(NerlTensorBinary, BinaryType),
      [DimXRaw, DimYRaw, DimZRaw | Data] = DecodedTensor,
      DimX = round(DimXRaw),
      DimY = round(DimYRaw),
      DimZ = round(DimZRaw),
      case Axis of
            0 ->
                  split_axis0(Data, DimX, DimY, DimZ, NumShards, TensorListType, BinaryType);
            1 ->
                  split_axis1(Data, DimX, DimY, DimZ, NumShards, TensorListType, BinaryType);
            _ ->
                  throw({unsupported_split_axis, Axis})
      end.

nerltensor_concat_nif(ShardsList, BinaryType, Axis) when is_list(ShardsList) ->
      DecodedShards = [decode_nif(ShardBin, BinaryType) || {ShardBin, _ShardType} <- ShardsList],
      case Axis of
            0 -> concat_axis0(DecodedShards, BinaryType);
            1 -> concat_axis1(DecodedShards, BinaryType);
            _ -> throw({unsupported_concat_axis, Axis})
      end.

nerltensor_reduce_sum_list_nif([], _BinaryType) ->
      {<<>>, float};
nerltensor_reduce_sum_list_nif([{TensorBinary, BinaryType}], _RequestedBinaryType) ->
      {TensorBinary, BinaryType};
nerltensor_reduce_sum_list_nif([{TensorBinaryA, BinaryType} | Rest], RequestedBinaryType) ->
      {TensorBinary, TensorType} =
            lists:foldl(
              fun({TensorBinaryB, _TensorType}, {AccBinary, AccType}) ->
                      nerltensor_sum_nif(AccBinary, TensorBinaryB, AccType)
              end,
              {TensorBinaryA, BinaryType},
              Rest
            ),
      case RequestedBinaryType of
            BinaryType -> {TensorBinary, TensorType};
            _Other -> {TensorBinary, TensorType}
      end.

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

split_axis0(Data, DimX, DimY, DimZ, NumShards, TensorListType, BinaryType) ->
      SampleSpan = DimY * DimZ,
      split_axis0_loop(Data, DimX, DimY, DimZ, SampleSpan, NumShards, TensorListType, BinaryType, 0, []).

split_axis0_loop(_Data, _DimX, _DimY, _DimZ, _SampleSpan, NumShards, _TensorListType, _BinaryType, ShardIdx, Acc)
when ShardIdx >= NumShards ->
      lists:reverse(Acc);
split_axis0_loop(Data, DimX, DimY, DimZ, SampleSpan, NumShards, TensorListType, BinaryType, ShardIdx, Acc) ->
      RemainingShards = NumShards - ShardIdx,
      RemainingSamples = length(Data) div SampleSpan,
      SamplesForShard = case RemainingShards =< 1 of
                          true -> RemainingSamples;
                          false -> RemainingSamples div RemainingShards
                        end,
      ValuesCount = SamplesForShard * SampleSpan,
      {ShardData, RestData} = lists:split(ValuesCount, Data),
      DimPrefix = tensor_dims_prefix(TensorListType, SamplesForShard, DimY, DimZ),
      {ShardBin, _ShardType} = encode_nif(DimPrefix ++ ShardData, BinaryType),
      split_axis0_loop(
        RestData, DimX, DimY, DimZ, SampleSpan, NumShards, TensorListType, BinaryType,
        ShardIdx + 1,
        [{ShardBin, BinaryType} | Acc]
      ).

split_axis1(Data, DimX, DimY, DimZ, NumShards, TensorListType, BinaryType) ->
      SampleSpan = DimY * DimZ,
      split_axis1_loop(Data, DimX, DimY, DimZ, SampleSpan, NumShards, TensorListType, BinaryType, 0, 0, []).

split_axis1_loop(_Data, _DimX, _DimY, _DimZ, _SampleSpan, NumShards, _TensorListType, _BinaryType, ShardIdx, _StartY, Acc)
when ShardIdx >= NumShards ->
      lists:reverse(Acc);
split_axis1_loop(Data, DimX, DimY, DimZ, SampleSpan, NumShards, TensorListType, BinaryType, ShardIdx, StartY, Acc) ->
      RemainingShards = NumShards - ShardIdx,
      RemainingY = DimY - StartY,
      WidthY = case RemainingShards =< 1 of
                 true -> RemainingY;
                 false -> RemainingY div RemainingShards
               end,
      ShardData = collect_axis1_shard_data(Data, DimX, DimY, DimZ, SampleSpan, StartY, WidthY),
      DimPrefix = tensor_dims_prefix(TensorListType, DimX, WidthY, DimZ),
      {ShardBin, _ShardType} = encode_nif(DimPrefix ++ ShardData, BinaryType),
      split_axis1_loop(
        Data, DimX, DimY, DimZ, SampleSpan, NumShards, TensorListType, BinaryType,
        ShardIdx + 1, StartY + WidthY, [{ShardBin, BinaryType} | Acc]
      ).

collect_axis1_shard_data(Data, DimX, _DimY, DimZ, SampleSpan, StartY, WidthY) ->
      lists:flatten(
        [begin
            Offset = SampleIdx * SampleSpan + StartY * DimZ,
            lists:sublist(Data, Offset + 1, WidthY * DimZ)
         end || SampleIdx <- lists:seq(0, DimX - 1)]
      ).

concat_axis0(DecodedShards, BinaryType) ->
      {TotalX, DimY, DimZ, TensorListType, DataList} =
            lists:foldl(
              fun({ShardTensor, ShardTensorType}, {AccX, _AccY, _AccZ, _AccType, AccData}) ->
                      [ShardXRaw, ShardYRaw, ShardZRaw | ShardData] = ShardTensor,
                      {AccX + round(ShardXRaw), round(ShardYRaw), round(ShardZRaw), ShardTensorType, AccData ++ ShardData}
              end,
              {0, 0, 0, erl_float, []},
              DecodedShards
            ),
      DimPrefix = tensor_dims_prefix(TensorListType, TotalX, DimY, DimZ),
      encode_nif(DimPrefix ++ DataList, BinaryType).

concat_axis1(DecodedShards, BinaryType) ->
      [{FirstTensor, TensorListType} | _] = DecodedShards,
      [DimXRaw, _DimYRaw, DimZRaw | _] = FirstTensor,
      DimX = round(DimXRaw),
      DimZ = round(DimZRaw),
      YWidths = [round(YRaw) || {[_, YRaw, _ | _], _} <- DecodedShards],
      TotalY = lists:sum(YWidths),
      ReconstructedData = reconstruct_axis1_data(DecodedShards, DimX, DimZ),
      DimPrefix = tensor_dims_prefix(TensorListType, DimX, TotalY, DimZ),
      encode_nif(DimPrefix ++ ReconstructedData, BinaryType).

reconstruct_axis1_data(DecodedShards, DimX, DimZ) ->
      lists:flatten(
        [lists:flatten(
           [begin
               [_, YRaw, _ | ShardData] = ShardTensor,
               Width = round(YRaw) * DimZ,
               Offset = SampleIdx * Width,
               lists:sublist(ShardData, Offset + 1, Width)
            end || {ShardTensor, _} <- DecodedShards]
         ) || SampleIdx <- lists:seq(0, DimX - 1)]
      ).

tensor_dims_prefix(erl_float, DimX, DimY, DimZ) ->
      [float(DimX), float(DimY), float(DimZ)];
tensor_dims_prefix(_OtherType, DimX, DimY, DimZ) ->
      [DimX, DimY, DimZ].



%%%%%% NerlWorker NIF Methods %%%%%%

new_nerlworker_nif(_ModelId,_DistributedSystemType, _DistributedSystemArgs, _TrainParams) ->
      exit(nif_library_not_loaded).

remove_nerlworker_nif(_ModelId) ->
      exit(nif_library_not_loaded).

%% All of inputs must be binary strings! except for _ModelId which is an integer
test_nerlworker_nif(_ModelId,_DistributedSystemType, _DistributedSystemArgs, _TrainParams) ->
      exit(nif_library_not_loaded).

%  input -  unsigned long modelId 
%  output - nerltensor that is the acc sum of each label in the last training data_set
% distributed system type should be FedClientWeightedAvgClassification
get_distributed_system_train_labels_count_nif(_ModelId) ->
       exit(nif_library_not_loaded).
