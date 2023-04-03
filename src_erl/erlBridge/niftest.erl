% This file will be depracated
-module(niftest).

-export([init/0,create_nif/6,train_nif/5,trainn_nif/5,call_to_train/6,predict_nif/2,call_to_predict/5,get_weights_nif/1,printTensor/2]).
-export([call_to_get_weights/1,call_to_set_weights/2]).
-export([encode/2, encode1/2, decode/2, decode1/2]).

-define(DEBUG,false). % set here if it is debug or release  TODO change to read from hrl auto generated file
-if(DEBUG).
-define(BUILD_TYPE,"debug").
-else.
-define(BUILD_TYPE,"release").
-endif. 

-define(THIS_FILE_PATH_RELATIVE_TO_PROJECT_ROOT,"src_erl"). % if this file moves to inner place than update this define
-on_load(init/0).

init() ->
       io:format("loading niff init()~n",[]),
       {_,CWD} = file:get_cwd(), 
       %io:format("CWD~p~n",[CWD]),
       _CWD_UPPER_DIR = re:replace(CWD,"/"++?THIS_FILE_PATH_RELATIVE_TO_PROJECT_ROOT,"",[{return,list}]),
       %io:format("CWD_UPPER_DIR~p~n",[CWD_UPPER_DIR]),
      
   
       %%FULL_PATH = CWD_UPPER_DIR++"/build/"++?BUILD_TYPE++"/libnerlnet",

   %  Full path
      %  FULL_PATH = "/home/evgeny/work_test/NErlNet/build/release/libnerlnet",


   % TODO TODO return to relative parh brfor commit to master. 
      FULL_PATH = "../../../NErlNet/build/"++?BUILD_TYPE++"/libnerlnet",
      io:format("Full path: ~p~n",[FULL_PATH]),
      RES = erlang:load_nif(FULL_PATH, 0),
      io:format("load nif results: ~p",[RES]),
      ok.
      % io:format("hello"),
      %hello("hello").
      %erlang:hello().
      % erlang:nif_error("NIF library not loaded").

%hello(Integer ) when is_integer(Integer) ->
%      exit(nif_library_not_loaded).


% generateNormalDistributionList(Mean,Variance,SampleLength) ->
%       NewNormalDistributionList = [rand:normal(Mean,Variance) || _X <- lists:seq(1,round(SampleLength))], NewNormalDistributionList. 

% generateNormalDistributionSamples(0,_Mean,_Variance,_SampleLength,_ListOfSamples) -> _ListOfSamples;
% generateNormalDistributionSamples(N,Mean,Variance,SampleLength,ListOfSamples) -> generateNormalDistributionSamples(N-1,Mean,Variance,SampleLength,ListOfSamples ++ generateNormalDistributionList(Mean,Variance,SampleLength)).



% ModelID - Unique ID of the neural network model 
% ModelType - E.g. Regression, Classification 
create_nif(_ModelID, _ModelType , _ScalingMethod , _LayerTypesList , _LayersSizes , _LayersActivationFunctions) ->
      exit(nif_library_not_loaded).

train_nif(Integer,Integer,Integer,Integer, []) ->
      exit(nif_library_not_loaded).

call_to_train(ModelID,OptimizationMethod,LossMethod,LearningRate, DataTensor, WorkerPid)->
      % io:format("berfor train  ~n "),
       %io:format("DataTensor= ~p~n ",[DataTensor]),
      _RetVal=trainn_nif(ModelID,OptimizationMethod,LossMethod,LearningRate, DataTensor),
      %io:format("Train Time= ~p~n ",[RetVal]),
      receive
            Ret->
                  % io:format("Ret= ~p~n ",[Ret]),
                  %io:format("WorkerPid,{loss, Ret}: ~p , ~p ~n ",[WorkerPid,{loss, Ret}]),
                  gen_statem:cast(WorkerPid,{loss, Ret})
            after 1000 ->  
                  io:format("///// woker miss train batch ~n "),
                  gen_statem:cast(WorkerPid,{loss, -1.0})
      end.

trainn_nif(_ModelID,_OptimizationMethod,_LossMethod, _LearningRate,_DataTensor) -> %TODO change to trainn_nif
      exit(nif_library_not_loaded).

call_to_predict(ModelID, Data, WorkerPid,CSVname, BatchID)->
     
      _RetVal = predict_nif(ModelID, Data),
      receive
            Ret->
           % io:format("Ret= ~p~n ",[Ret]),      
      
            gen_statem:cast(WorkerPid,{predictRes,Ret,CSVname, BatchID}) 
     
            after 1000 -> 
                 % io:format("///// woker miss predict batch ~n "), 
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
      catch Err:E -> io:format("Couldnt get weights from worker~n~p~n",{Err,E}),
            []
      end.

call_to_set_weights(ModelID,Weights)->
      _RetVal = set_weights_nif(ModelID, Weights).
     % io:format("RetVal= ~p~n ",[RetVal]).
      %receive
      %      Ret->Ret
            % io:format("Ret= ~p~n ",[Ret])
      %end.

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

