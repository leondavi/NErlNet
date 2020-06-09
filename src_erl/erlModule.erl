-module(erlModule).

%% API
-export([foo/1, bar/1, square/1, square/2, push/1, squareTest/1, matrix/2, testSquareM1/2, dListSquare/1,singleton/0]).

%%  on_load directive is used get function init called automatically when the module is loaded
-on_load(init/0).


%%  Function init in turn calls erlang:load_nif/2 which loads the NIF library and replaces the foo,bar,square functions with its native implementation in C
init() ->
  ok = erlang:load_nif("./nifModule_nif", 0).

%% Add 1 - using int
foo(_X) ->
  %% Each NIF must have an implementation in Erlang to be invoked if the function is called before the NIF library is successfully loaded. A typical such stub implementation is to call erlang:nif_error which will raise an exception. The Erlang function can also be used as a fallback implementation if the NIF library lacks implementation for some OS or hardware architecture for example.
  exit(nif_library_not_loaded).

%% Multiply by 2 - using int
bar(_Y) ->
  exit(nif_library_not_loaded).

%% Square all the elements in List - using std::vector
square(_List) ->
  exit(nif_library_not_loaded).

%% Add List2 arguments to List arguments if they are in the same size. Returns List if not. - using std::vector
square(_List, _List2) ->
  exit(nif_library_not_loaded).

squareTest(_List) -> square(_List)++[9].

%% Add 10 to back and 20 to front - using std::deque
push(_List) ->
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
  matrixToList(matrix(R, W)).


testSquareM1(R, W) ->
  M=matrixToList(matrix(R, W)),
  square(M).

%% Make double list
dList(List) -> dList(List,[]).
dList([],NewList) -> NewList;
dList([H|T],NewList) -> dList(T,NewList++[H+0.0]).

dListSquare(List)->square(dList(List)).
	
%%------------
singleton()->
	exit(nif_library_not_loaded).
	




