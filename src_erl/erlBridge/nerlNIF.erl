-module(nerlNIF).
-include_lib("kernel/include/logger.hrl").
-include("nerlTensor.hrl").

-export([init/0,create_nif/6,train_nif/5,call_to_train/6,predict_nif/2,call_to_predict/5,get_weights_nif/1,printTensor/2]).
-export([call_to_get_weights/1,call_to_set_weights/2]).
-export([encode/2, encode1/2, decode/2, decode1/2]).

-define(FILE_IDENTIFIER,"[NERLNIF] ").
-define(NERLNET_LIB,"libnerlnet").
-define(NERLNET_PATH,"/usr/local/lib/nerlnet-lib/NErlNet").
-define(BUILD_TYPE_DEBUG,"debug").
-define(BUILD_TYPE_RELEASE,"/build/release").

-define(THIS_FILE_PATH_RELATIVE_TO_PROJECT_ROOT,"src_erl"). % if this file moves to inner place than update this define
-on_load(init/0).

-define(PREDICT_TIMEOUT,10000). % 10 seconds limit for prediction results
-define(TRAIN_TIMEOUT,20000). % 20 seconds limit for prediction results


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


% Num - number to convert to string
% NumType (Number of bytes): For 64-bit UNIX and Linux applications:
% char - 1, short - 2, int - 4, long - 8 (4 for Windows 64-bit and unix/linux 32 bit), float - 4, double - 8, 
% long double - 16 (8 for Windows 64-bit and 32 bit AIX® and Linux PPC), float16 - 2, float32 - 4
encode1(Num, NumOfBytesForType) ->
      Result = encode(Num, NumOfBytesForType),
      io:format("~w~n", [Result]).

encode(_Num, _NumOfBytesForType) ->
      exit(nif_library_not_loaded).

decode1(String, NumOfBytesForType) ->
      Result = decode(String, NumOfBytesForType),
      io:format("~w~n", [Result]).

decode(_String, _NumOfBytesForType) ->
      exit(nif_library_not_loaded).


%---------- nerlTensor -----------%

%-spec conversion(From :: nerlTensor, To :: nerlTensorType()) -> any().
nerltensor_conversion({NerlTensorFrom, TypeFrom}, {NerlTensorTo, TypeTo}) -> ok.
   %   case TypeFrom of:
            % if TypeFrom is [float32 | double | int32 ]-> decode
            % if TypeTo is [float32 | double | int32] -> encode

%  float32 | double | int32.
-spec nerltensor_create(X :: number(), Y :: number(), Z :: number(), List :: list(), To :: nerlTensorType()) -> list().
nerltensor_create(X,Y,Z,List,Type) -> 
      case Type of
            erl_float -> {[X,Y,Z] ++ List, erl_float}; % Make sure list of float
            erl_int -> {[X,Y,Z] ++ List, erl_int}; % make sure list of integers
            _COMPRESSED_TYPE -> encode 
      end.


