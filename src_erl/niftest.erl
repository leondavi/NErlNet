-module(niftest).

-export([init/0,create_nif/6,train_nif/4,trainn_nif/4,call_to_train/4,predict_nif/2,call_to_predict/2,get_weights_nif/1,printTensor/2]).
-export([trainNifTest/1]).
-define(DEBUG,false). % set here if it is debug or release  TODO change to read from hrl auto generated file
-if(DEBUG).
-define(BUILD_TYPE,"debug").
-else.
-define(BUILD_TYPE,"release").
-endif. 

-define(THIS_FILE_PATH_RELATIVE_TO_PROJECT_ROOT,"src_erl"). % if this file moves to inner place than update this define
-on_load(init/0).

init() ->
      {_,CWD} = file:get_cwd(), 
      CWD_UPPER_DIR = re:replace(CWD,"/"++?THIS_FILE_PATH_RELATIVE_TO_PROJECT_ROOT,"",[{return,list}]),
      FULL_PATH = CWD_UPPER_DIR++"/build/"++?BUILD_TYPE++"/libnerlnet",
      io:format("~p",[CWD_UPPER_DIR]),
      RES = erlang:load_nif(FULL_PATH, 0),
      io:format("load nif results: ~p",[RES]),
      ok.
      % io:format("hello"),
      %hello("hello").
      %erlang:hello().
      % erlang:nif_error("NIF library not loaded").

%hello(Integer ) when is_integer(Integer) ->
%      exit(nif_library_not_loaded).

%
%
%
trainNifTest(NumberOfSamples) -> ModelID = 586000901, 
                  ModelType = 1, 
                  ScalingMethod = 2,
                  LayerTypesList = 3, 
                  LayersSizes = [128,64,32,16,4,1],
                  LayersSizesFirst = hd(LayersSizes),
                  LayersSizesLast = lists:last(LayersSizes),
                  LayersActivationFunctions = [6,6,6,6,6,11],  % to play with until getting convergence    
                  create_nif(ModelID, ModelType , ScalingMethod , LayerTypesList , LayersSizes , LayersActivationFunctions),
                  RandomGeneratedData = [], % size NumberOfSamples*(LayersSizesFirst + LayersSizesLast,RandomGeneratedData)
                  DataTensor = [NumberOfSamples,LayersSizesFirst + LayersSizesLast,1,RandomGeneratedData], % ask Tal to generate random matrix - with given dimensions LayerSizes.front + LayerSizes.back (128 + 1) X number of samples 
                  OptimizationMethod = 1,
                  LossMethod = 2,
                  call_to_train(ModelID, OptimizationMethod , LossMethod , DataTensor ).


% ModelID - Unique ID of the neural network model 
% ModelType - E.g. Regression, Classification 
create_nif(_ModelID, _ModelType , _ScalingMethod , _LayerTypesList , _LayersSizes , _LayersActivationFunctions) ->
      exit(nif_library_not_loaded).

train_nif(Integer,Integer,Integer, []) ->
      exit(nif_library_not_loaded).

call_to_train(ModelID,OptimizationMethod,LossMethod, DataTensor)->
      RetVal=trainn_nif(ModelID,OptimizationMethod,LossMethod, DataTensor),
      io:format("RetVal= ~p~n ",[RetVal]),
      receive
            Ret->
            io:format("Ret= ~p~n ",[Ret])
      end.

trainn_nif(_ModelID,_OptimizationMethod,_LossMethod, _DataTensor) -> %TODO change to trainn_nif
      exit(nif_library_not_loaded).

call_to_predict(A,B)->
      RetVal = predict_nif(A, B),
      io:format("RetVal= ~p~n ",[RetVal]),
      receive
            Ret->
            io:format("Ret= ~p~n ",[Ret])
      end.

predict_nif(Integer, []) ->
      exit(nif_library_not_loaded).

get_weights_nif(Integer) ->
      exit(nif_library_not_loaded).

printTensor(List,Type) when is_list(List) -> 
      exit(nif_library_not_loaded).
