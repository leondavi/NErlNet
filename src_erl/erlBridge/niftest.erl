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

-define(NERLNET_DIR_STR,"NErlNet").
-on_load(init/0).

index_of(Item, List) -> index_of(Item, List, 1).
index_of(_, [], _)  -> not_found;
index_of(Item, [Item|_], Index) -> Index;
index_of(Item, [_|Tl], Index) -> index_of(Item, Tl, Index+1).

init() ->
       io:format("loading niff init()~n",[]),
       {_,CWD} = file:get_cwd(),
       SeparatedList = re:split(CWD,"/"),
       SeparatedSubList = lists:sublist(SeparatedList,index_of(?NERLNET_DIR_STR,SeparatedList)),
       NerlnetDir = lists:flatten(SeparatedSubList),
       io:format("Nerlnet absolute Path~p~n",[NerlnetDir]),
   
      FULL_PATH = NerlnetDir++"/build/"++?BUILD_TYPE++"/libnerlnet",

      RES = erlang:load_nif(FULL_PATH, 0),
      io:format("load nif results: ~p",[RES]),
      ok.


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
            io:format("Ret= ~p~n ",[Ret]),
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
