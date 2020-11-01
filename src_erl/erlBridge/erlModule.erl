-module(erlModule).

%% API
-export([testMatrix/2,cppBridgeControler/0,cppBridgeControlerGetMid/0,cppBridgeControlerGetModelPtr/1 ,cppBridgeControlerSetData/1,testcppBridgeControler/2,
	 create_module/6, train2double/5, predict2double/4, niftest/1, thread_create_test/0,
	 predict/0, module_create/5, train_predict_create/5, train_predict_create/6, cppBridgeControlerSetModelPtrDat/2,
	cppBridgeControlerDeleteModel/1, startTest/1]).

%%  on_load directive is used get function init called automatically when the module is loaded
-on_load(init/0).


%%  Function init in turn calls erlang:load_nif/2 which loads the NIF library and replaces the erlang functions with its
%%  native implementation in C
init() ->
	RelativeDirPath = filename:dirname(filename:absname("")),
	Nif_Module_Cpp_Path = string:concat(RelativeDirPath,"/src_cpp/./nifModule_nif"), % Relative path for nifModule_nif
	%Nif_Module_Cpp_Path = string:concat(RelativeDirPath,"/src_py/lib/./libnifModule_nif"), % Relative path for nifModule_nif
	%% load_info is the second argument to erlang:load_nif/2
  ok = erlang:load_nif(Nif_Module_Cpp_Path, 0).

% todo: delete function foo
%% Add 1 - using int
%%foo(_X) ->
  %% Each NIF must have an implementation in Erlang to be invoked if the function is called before the NIF library is successfully loaded.
  %% A typical such stub implementation is to call erlang:nif_error which will raise an exception.
  %% The Erlang function can also be used as a fallback implementation if the NIF library lacks implementation for some OS
  %% or hardware architecture for example.
  % exit(nif_library_not_loaded).


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

% Convert "int" to "double"
dInt(Int) -> Int+0.0.

%dListSquare(List)->List.

%%---------------------------------------------------
cppBridgeControler()->
	exit(nif_library_not_loaded).

cppBridgeControlerGetMid() ->
	exit(nif_library_not_loaded).

cppBridgeControlerGetModelPtr(_Mid) ->
	exit(nif_library_not_loaded).

cppBridgeControlerSetModelPtrDat(_Dat, _Mid) ->
	exit(nif_library_not_loaded).

cppBridgeControlerSetData(_Int) ->
	exit(nif_library_not_loaded).

cppBridgeControlerDeleteModel(_Mid) ->
	exit(nif_library_not_loaded).

testcppBridgeControler(Data1,Data2) ->
	A = cppBridgeControlerGetMid(),
	io:fwrite("cppBridgeControler initial data: ~p ~n",[A]),
	_Pid1 = spawn(fun()->start(Data1) end),
	_Pid2 = spawn(fun()->start(Data2) end),
	timer:sleep(100),
	B = cppBridgeControlerGetMid(),
	io:fwrite("cppBridgeControler finish data: ~p ~n",[B]).

start(Data) ->
	FirstData = cppBridgeControlerGetMid(),
	io:fwrite("cppBridgeControler from pid ~p first data: ~p ~n",[self(),FirstData]),
	cppBridgeControlerSetData(Data),
	UpdatedData = cppBridgeControlerGetMid(),
	io:fwrite("cppBridgeControler from pid ~p updated data: ~p ~n",[self(),UpdatedData]).

%%---------------------------------------------------

%% ---- Create module ----
%% layers_sizes - list
%% Learning_rate - number (0-1). Usually 1/number_of_samples
%% Train_set_size - percentage number. Usually 70%-80%
%% Activation_list (optional) - list
%% Optimizer (optional) - default ADAM
module_create(Layers_sizes, Learning_rate, Train_set_size, Activation_list, Optimizer)->
	create_module(0, Layers_sizes, Learning_rate, dInt(Train_set_size), Activation_list, Optimizer).

%% Create module
create_module(0, _Layers_sizes, _Learning_rate, _Train_set_size, _Activation_list, _Optimizer) ->
	exit(nif_library_not_loaded).

%% ---- Train  ----

%% train_predict_create - mode: 0 - model creation, 1 - train, 2 - predict
%% Rows, Col, Labels - "int"
%% Data_Label_mat - list
train2double(Rows, Cols, Labels, Data_Label_mat, ModelId) -> % TODO
	%% make double list and send to train_predict_create
	_Return = train_predict_create(1, Rows, Cols, Labels, dList(Data_Label_mat), ModelId),
	receive
		LOSS_FUNC->
			io:fwrite("Loss func: ~p\n",[LOSS_FUNC]),
			LOSS_FUNC
	end.

%% Second version - optional for the future
%%train_predict2double(Create_train_predict_mode, Data_mat, Label_mat) ->
	%% make double list and send to train_predict_create
	%%train_predict_create(Create_train_predict_mode, dList(Data_mat), dList(Label_mat)).

%% Train module
%% _Rows, _Col, _Labels - "ints"
%% _Data_Label_mat - list
train_predict_create(1, _Rows, _Cols, _Labels, _Data_Label_mat, _ModelId) ->
  exit(nif_library_not_loaded).


%% ---- Predict ----

%% _Rows, _Col, _Labels - "ints"
%% _Data_Label_mat - list
predict2double(_Data_mat, _rows, _cols, _ModelId) ->
	%% make double list and send to train_predict_create
	_Return = train_predict_create(2, dList(_Data_mat), _rows, _cols, _ModelId),
	receive
		RESULTS->
		io:fwrite("Results: ~p\n",[RESULTS])
	end.

%% Predict module
%% _Rows, _Col, _Labels - "ints"
%% _Data_Label_mat - list
train_predict_create(2, _Data_mat, _rows, _cols, _ModelId) ->
	exit(nif_library_not_loaded).


startTest(ProcNumTrain)->
	Start_Time = os:system_time(microsecond),
	io:fwrite("start module_create ~n"),
	_Pid1 = spawn(fun()->module_create([8,4,3,2], 0.01, 80, [2,1,1,2], 1) end),
	niftest(ProcNumTrain),
	%timer:sleep(5),
	io:fwrite("start predict2double ~n"),
	%_Pid3 = spawn(fun()->erlModule:predict2double([1,2,3,2,3,2,1,0,1,2,3,2,3,2,1,0,1,2,3,2,3,2,1,0,1,2,3,2,3,2,1,0],4,8,0) end),
	Finish_Time = os:system_time(microsecond),
	Time_elapsed=Finish_Time-Start_Time,
	io:fwrite("Time took for nif: ~p ms , Number of processes = ~p ~n",[Time_elapsed, ProcNumTrain]).

niftest(0) ->
	io:fwrite("finish all ~n");
niftest(Num) ->
	%timer:sleep(1),
	io:fwrite("start train2double ~n"),
	_Pid2 = spawn(fun()->erlModule:train2double(4, 8, 2, [1,2,3,2,3,2,1,0,1,2,3,2,3,2,1,0,1,2,3,2,3,2,1,0,1,2,3,2,3,2,1,0,0,1,0,1,0,1,0,1],0) end),
	timer:sleep(1),
	niftest(Num-1).

thread_create_test() ->
	exit(nif_library_not_loaded).

predict() ->
	exit(nif_library_not_loaded).