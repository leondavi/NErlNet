-module(niftest).

%<<<<<<< HEAD
%-export([init/0,create_nif/6,train_nif/4,trainn_nif/4,call_to_train/4,predict_nif/2,call_to_predict/2,get_weights_nif/1,call_to_get_weights/1,printTensor/2]).

-export([init/0,create_nif/6,train_nif/5,trainn_nif/5,call_to_train/6,predict_nif/2,call_to_predict/3,get_weights_nif/1,printTensor/2]).
-export([call_to_get_weights/1]).

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
      %  FULL_PATH = "/home/evgeny/work_test/NErlNet/build/release/libnerlnet",


   % TODO TODO return to relative parh brfor commit to master. 
      FULL_PATH = "../../../build/"++?BUILD_TYPE++"/libnerlnet",
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



% ModelID - Unique ID of the neural network model 
% ModelType - E.g. Regression, Classification 
create_nif(_ModelID, _ModelType , _ScalingMethod , _LayerTypesList , _LayersSizes , _LayersActivationFunctions) ->
      exit(nif_library_not_loaded).

train_nif(Integer,Integer,Integer,Integer, []) ->
      exit(nif_library_not_loaded).

call_to_train(ModelID,OptimizationMethod,LossMethod,LearningRate, DataTensor, WorkerPid)->
      io:format("berfor train  ~n "),
      _RetVal=trainn_nif(ModelID,OptimizationMethod,LossMethod,LearningRate, DataTensor),
      %io:format("Train Time= ~p~n ",[RetVal]),
      receive
            Ret->
            %io:format("Ret= ~p~n ",[Ret]),
            %io:format("WorkerPid,{loss, Ret}: ~p , ~p ~n ",[WorkerPid,{loss, Ret}]),
            gen_statem:cast(WorkerPid,{loss, Ret})
      end.

trainn_nif(_ModelID,_OptimizationMethod,_LossMethod, _LearningRate,_DataTensor) -> %TODO change to trainn_nif
      exit(nif_library_not_loaded).

call_to_predict(ModelID, Data, WorkerPid)->
      _RetVal = predict_nif(ModelID, Data),
      receive
            Ret->

            io:format("predict res = ~p~n Worker Pid: ~p ",[Ret,WorkerPid]),

            gen_statem:cast(WorkerPid,{predictRes,Ret}) 
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
