-module(niftest).

%<<<<<<< HEAD
%-export([init/0,create_nif/6,train_nif/4,trainn_nif/4,call_to_train/4,predict_nif/2,call_to_predict/2,get_weights_nif/1,call_to_get_weights/1,printTensor/2]).

-export([init/0,create_nif/6,train_nif/4,trainn_nif/4,call_to_train/4,predict_nif/2,call_to_predict/2,get_weights_nif/1,printTensor/2]).
-export([trainNifTest/1,call_to_get_weights/1]).

-define(DEBUG,false). % set here if it is debug or release  TODO change to read from hrl auto generated file
-if(DEBUG).
-define(BUILD_TYPE,"debug").
-else.
-define(BUILD_TYPE,"release").
-endif. 

-define(THIS_FILE_PATH_RELATIVE_TO_PROJECT_ROOT,"src_erl"). % if this file moves to inner place than update this define
-on_load(init/0).

init() ->
       %io:format("loading niff init()~n",[]),
       {_,CWD} = file:get_cwd(), 
       %io:format("CWD~p~n",[CWD]),
       CWD_UPPER_DIR = re:replace(CWD,"/"++?THIS_FILE_PATH_RELATIVE_TO_PROJECT_ROOT,"",[{return,list}]),
       %io:format("CWD_UPPER_DIR~p~n",[CWD_UPPER_DIR]),
      
   
       %%FULL_PATH = CWD_UPPER_DIR++"/build/"++?BUILD_TYPE++"/libnerlnet",

   %  Full path
       FULL_PATH = "/home/evgeny/work_test/NErlNet/build/release/libnerlnet",


   % TODO TODO return to relative parh brfor commit to master. 
      %FULL_PATH = "../../../build/"++?BUILD_TYPE++"/libnerlnet",
      % io:format("~p~n",[CWD_UPPER_DIR]),
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


generateNormalDistributionList(Mean,Variance,SampleLength) ->
      NewNormalDistributionList = [rand:normal(Mean,Variance) || _X <- lists:seq(1,round(SampleLength))], NewNormalDistributionList. 

generateNormalDistributionSamples(0,_Mean,_Variance,_SampleLength,_ListOfSamples) -> _ListOfSamples;
generateNormalDistributionSamples(N,Mean,Variance,SampleLength,ListOfSamples) -> generateNormalDistributionSamples(N-1,Mean,Variance,SampleLength,ListOfSamples ++ generateNormalDistributionList(Mean,Variance,SampleLength)).

trainNifTest(NumberOfSamples) -> ModelID = 586000901, 
                  ModelType = 1, 
                  ScalingMethod = 1,
                  LayerTypesList = [1,1,1,3], 
                  LayersSizes = [6,1,1,128,64,32,16,4,1],
                  % LayersSizesFirst = hd(LayersSizes),
                  LayersSizesFirst = 128.0,
                  %LayersSizesLast = lists:last(LayersSizes),
                  LayersSizesLast = 1.0,
                  LayersActivationFunctions = [5,1,1,0,0,0,0,0],  % to play with until getting convergence    
                  _S = create_nif(ModelID, ModelType , ScalingMethod , LayerTypesList , LayersSizes , LayersActivationFunctions),
                  io:format("4 ~n"),
                  RandomGeneratedData = lists:flatten([[rand:normal()||_<-lists:seq(1,128)] ++[1.0]||_<-lists:seq(1,100)]),
                  % RandomGeneratedData = [rand:normal()||_<-lists:seq(1,1290)] , % size NumberOfSamples*(LayersSizesFirst + LayersSizesLast,RandomGeneratedData)
                  %Class1Mean = 3,
                  %Class2Mean = 6,
                  %Class1Std = 1,
                  %Class2Std = 1,
                  %DataClass1 = generateNormalDistributionSamples(NumberOfSamples,Class1Mean,Class1Std,LayersSizesFirst,[]), 
                  %DataClass2 = generateNormalDistributionSamples(NumberOfSamples,Class2Mean,Class2Std,LayersSizesFirst,[]),
                  %  @@@@ Tal and Evgeni TODO: create a mutual data tensor from shuffled data of class 1 and 2 and add the label (according to each class )
                  DataTensor = [NumberOfSamples , LayersSizesFirst + LayersSizesLast , 1.0] ++ RandomGeneratedData, % ask Tal to generate random matrix - with given dimensions LayerSizes.front + LayerSizes.back (128 + 1) X number of samples 
                  %io:format("DataTensor ~p~n" , [DataTensor]),
                  OptimizationMethod = 1,
                  LossMethod = 2, 
                  call_to_train(ModelID, OptimizationMethod , LossMethod , DataTensor ),
                  RandomGeneratedDataP = [rand:normal()||_<-lists:seq(1,1280)] ,
                  DataTensorP = [10.0 , LayersSizesFirst , 1.0] ++ RandomGeneratedDataP,
                  call_to_predict(ModelID,DataTensorP).


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

call_to_get_weights(A)->
      RetVal = get_weights_nif(A),
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
