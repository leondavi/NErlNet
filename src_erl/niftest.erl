-module(niftest).

-export([init/0,create_nif/6,train_nif/4,trainn_nif/4,call_to_train/4,predict_nif/2,call_to_predict/2,get_weights_nif/1,printTensor/2]).

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

create_nif(Integer, Integer , Integer , [] , [] , []) ->
      exit(nif_library_not_loaded).

train_nif(Integer,Integer,Integer, []) ->
      exit(nif_library_not_loaded).

call_to_train(A,B,C,D)->
      RetVal=trainn_nif(A,B,C,D),
      io:format("RetVal= ~p~n ",[RetVal]),
      receive
            Ret->
            io:format("Ret= ~p~n ",[Ret])
      end.


trainn_nif(Integer,Integer,Integer, []) ->
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
