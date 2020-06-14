-module(erlModule).

%% API
-export([foo/1,testMatrix/2,singleton/0,singletonGetData/0,singletonSetData/1,testSingleton/2]).

%%  on_load directive is used get function init called automatically when the module is loaded
-on_load(init/0).


%%  Function init in turn calls erlang:load_nif/2 which loads the NIF library and replaces the foo,bar,square functions with its native implementation in C
init() ->
  ok = erlang:load_nif("/home/ziv/workspace/NErlNet/src_cpp/./nifModule_nif", 0).

%% Add 1 - using int
foo(_X) ->
  %% Each NIF must have an implementation in Erlang to be invoked if the function is called before the NIF library is successfully loaded. A typical such stub implementation is to call erlang:nif_error which will raise an exception. The Erlang function can also be used as a fallback implementation if the NIF library lacks implementation for some OS or hardware architecture for example.
  exit(nif_library_not_loaded).



%%----------------------------------------------------
%% Matrix RxW : List of lists (each list is a row)
matrix(R, W) ->
  createMatrix(R, W, []).


createMatrix(0, _W, M) ->
  io:fwrite("Matrix: ~p ~n",[M]),
  M;
createMatrix(R, W, M) ->
  Row = createRow(W, []),
  createMatrix(R-1, W, [Row | M]).

createRow(0, NewR) -> NewR;
createRow(W, NewR) -> createRow(W-1, [rand:normal() | NewR]).

matrixToList(M) ->
  List = createMList(M, []),
  io:fwrite("Matrix list: ~p ~n",[List]),
  List.

createMList([],NewList) -> NewList;
createMList([H|T], NewList) ->
  createMList(T, NewList ++ H).

testMatrix(R, W) ->
  dList(matrixToList(matrix(R, W))).


%testSquareM1(R, W) ->
%  M=matrixToList(matrix(R, W)),
%  M.

%% Make double list
dList(List) -> dList(List,[]).
dList([],NewList) -> NewList;
dList([H|T],NewList) -> dList(T,NewList++[H+0.0]).

%dListSquare(List)->List.
	
%%---------------------------------------------------
singleton()->
	exit(nif_library_not_loaded).
	
singletonGetData() ->
	exit(nif_library_not_loaded).

singletonSetData(_Int) ->
	exit(nif_library_not_loaded).

testSingleton(Data1,Data2) ->
	A = singletonGetData(),
	io:fwrite("Singleton initial data: ~p ~n",[A]),
	_Pid1 = spawn(fun()->start(Data1) end),
	_Pid2 = spawn(fun()->start(Data2) end),
	timer:sleep(100),
	B = singletonGetData(),
	io:fwrite("Singleton finish data: ~p ~n",[B]).

start(Data) ->
	FirstData = singletonGetData(),
	io:fwrite("Singleton from pid ~p first data: ~p ~n",[self(),FirstData]),
	singletonSetData(Data),
	UpdatedData = singletonGetData(),
	io:fwrite("Singleton from pid ~p updated data: ~p ~n",[self(),UpdatedData]).

