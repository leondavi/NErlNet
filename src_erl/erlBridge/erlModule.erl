-module(erlModule).

%% API
-export([testMatrix/2,cppBridgeController/0,cppBridgeControllerGetModelPtr/1 ,cppBridgeControllerSetData/1,
	 create_module/6, train2double/6, predict2double/7, niftest/6, thread_create_test/0,
	 predict/0, module_create/5, train_predict_create/5, train_predict_create/6, cppBridgeControllerSetModelPtrDat/2,
	cppBridgeControllerDeleteModel/1, startTest/11,train/8, set_weights/5, get_weights/1, average_weights/4]).

%%  on_load directive is used get function init called automatically when the module is loaded
-on_load(init/0).


%%  Function init in turn calls erlang:load_nif/2 which loads the NIF library and replaces the erlang functions with its
%%  native implementation in C
init() ->
	RelativeDirPath = filename:dirname(filename:dirname(filename:absname(""))), % NErlNet directory path
	RelativeDirPathNew = string:sub_string(RelativeDirPath, 1, length(RelativeDirPath)-7),
	%Nif_Module_Cpp_Path = string:concat(RelativeDirPathNew,"NErlNet/src_cpp/cppBridge/libNerlNIF"), % Relative path for nifModule_nif
	Nif_Module_Cpp_Path = string:concat(RelativeDirPathNew,"src_cpp/cppBridge/libNerlNIF"), % Relative path for nifModule_nif

	io:fwrite("Nif_Module_Cpp_Path: ~p ~n",[Nif_Module_Cpp_Path]),
	%% load_info is the second argument to erlang:load_nif/2
  ok = erlang:load_nif(Nif_Module_Cpp_Path, 0).
	%ok = erlang:load_nif("/home/ziv/workspace/NErlNet/src_cpp/cppBridge/libNerlNIF", 0).


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
%dInt(Int) -> Int+0.0.

%dListSquare(List)->List.

%%---------------------------------------------------
cppBridgeController()->
	exit(nif_library_not_loaded).

cppBridgeControllerGetModelPtr(_Mid) ->
	exit(nif_library_not_loaded).

cppBridgeControllerSetModelPtrDat(_Dat, _Mid) ->
	exit(nif_library_not_loaded).

cppBridgeControllerSetData(_Int) ->
	exit(nif_library_not_loaded).

cppBridgeControllerDeleteModel(_Mid) ->
	exit(nif_library_not_loaded).

%%---------------------------------------------------

%% ---- Create module ----
%% layers_sizes - list
%% Learning_rate - number (0-1). Usually 1/number_of_samples
%% Train_set_size - percentage number. Usually 70%-80%
%% Activation_list - list of activation functions for the layers:
%% enum: ACT_NONE - 0, ACT_IDENTITY - 1, ACT_SIGMOID - 2, ACT_RELU - 3, ACT_LEAKY_RELU - 4, ACT_SWISH - 5, ACT_ELU - 6,
%% ACT_TANH - 7, ACT_GAUSSIAN - 8, ACT_TOTAL - 9
%% Optimizer (optional) - default ADAM :
%% enum: OPT_NONE - 0, OPT_SGD - 1, OPT_MINI_BATCH_SGD - 2, OPT_MOMENTUM - 3, OPT_NAG -4, OPT_ADAGRAD - 5, OPT_ADAM -6
module_create(Layers_sizes, Learning_rate, Activation_list, Optimizer, ModelId)->
	Create_Result = create_module(0, Layers_sizes, Learning_rate, ModelId, Activation_list, Optimizer),
	Create_Result.

%% Create module
create_module(0, _Layers_sizes, _Learning_rate, _ModelId, _Activation_list, _Optimizer) ->
	exit(nif_library_not_loaded).

%% ---- Train  ----

%% train_predict_create - mode: 0 - model creation, 1 - train, 2 - predict
%% Rows, Col, Labels - "int"
%% Data_Label_mat - list
train2double(Rows, Cols, Labels, Data_Label_mat, ModelId, ClientPid) ->
	Start_Time = os:system_time(microsecond),
	%% make double list and send to train_predict_create
	_Return = train_predict_create(1, Rows, Cols, Labels, dList(Data_Label_mat), ModelId),
	receive
		LOSS_And_Time->
			io:fwrite("Loss and time func in erlModule: ~p\n",[LOSS_And_Time]),
			Finish_Time = os:system_time(microsecond),
			Time_elapsedNIF=Finish_Time-Start_Time,
			gen_statem:cast(ClientPid,{loss, LOSS_And_Time,Time_elapsedNIF}) % TODO Change the cast in the client
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

%% Rows, Cols - "ints"
%% Data_mat - list
predict2double(Data_mat, Rows, Cols, ModelId, ClientPid,CSVname,BatchID) ->
	Start_Time = os:system_time(microsecond),
	%% make double list and send to train_predict_create
	_Return = train_predict_create(2, dList(Data_mat), Rows, Cols, ModelId),
	receive
		RESULTS_And_Time->
			Finish_Time = os:system_time(microsecond),
			Time_elapsedNIF=Finish_Time-Start_Time,
			%io:fwrite("Results: ~p\n",[RESULTS_And_Time]),
			gen_statem:cast(ClientPid,{predictRes,CSVname,BatchID, RESULTS_And_Time,Time_elapsedNIF}) % TODO Change the cast in the client
	end.

%% Predict module
%% _Rows, _Col, _Labels - "ints"
%% _Data_Label_mat - list
train_predict_create(2, _Data_mat, _rows, _cols, _ModelId) ->
	exit(nif_library_not_loaded).

%% Set the new weights
%% _Matrix, _Bias - lists
set_weights(_Matrix, _Bias, _Biases_sizes_list, _Wheights_sizes_list, _ModelId) ->
	exit(nif_library_not_loaded).

%% Get the weights
get_weights(_Mid) ->
	exit(nif_library_not_loaded).

average_weights(_Matrix, _Biases, _Size, _ModelId) ->
	exit(nif_library_not_loaded).

%% ------------------------ TEST ----------------------------

train(0,_ChunkSize, _Cols, _Labels, _SampleListTrain,_ModelId,_Pid,LOSS)->LOSS;
train(ProcNumTrain,ChunkSize, Cols, Labels, SampleListTrain,ModelId,Pid,_LOSS)->
	Curr_pid=self(),
	%Pid2=spawn(fun()->erlModule:train2double(ChunkSize, Cols, Labels, SampleListTrain,ModelId,Curr_pid) end),
	erlModule:train2double(ChunkSize, Cols, Labels, SampleListTrain,ModelId,Curr_pid),
	receive
		LOSS_FUNC->
		  %LOSS_new = LOSS+LOSS_FUNC,
			train(ProcNumTrain-1,ChunkSize, Cols, Labels, SampleListTrain,ModelId,Pid,LOSS_FUNC)
		%	io:fwrite("PID: ~p Loss func: ~p\n",[Pid2, LOSS_FUNC])
	end.


startTest(File, Train_predict_ratio,ChunkSize, Cols, Labels, ModelId, ActivationList, Learning_rate, Layers_sizes, Optimizer, ProcNumTrain)->

	{_FileLinesNumber,_Train_Lines,_PredictLines,SampleListTrain,_SampleListPredict}=
		parse:readfile(File, Train_predict_ratio,ChunkSize, Cols, Labels, ModelId),

	%io:fwrite("Start module_create in erlModule ~n"),
	%Pid1 = spawn(fun()->module_create([8,4,3,2], 0.01, 80, [2,1,1,2], 1) end),
	module_create(Layers_sizes, Learning_rate, ActivationList, Optimizer, ModelId),
	%io:fwrite("Create PID ~p ~n",[Pid1]),
	Start_Time = os:system_time(microsecond),
	get_weights(0),

	%io:fwrite("TrainList: ~p\n",[SampleListTrain]),
	io:fwrite("ChunkSize: ~p Cols: ~p, Labels: ~p, ModelId: ~p, pid: ~p \n",[ChunkSize,Cols,Labels, ModelId,self()]),
%	Loss=train(ProcNumTrain,ChunkSize, Cols, Labels, SampleListTrain,ModelId,self(),0.0),

	%niftest(ProcNumTrain,SampleListTrain,Train_Lines,Cols,Labels,ModelId),
	%io:fwrite("start predict2double ~n"),
	%Curr_PID = self(),
	%Pid3 = spawn(fun()->erlModule:predict2double([80,92,132],1,3,ModelId,Curr_PID) end),
	%receive
	%	RESULTS->
	%		io:fwrite("PID: ~p Results: ~p\n",[Pid3, RESULTS])
	%end,

	Finish_Time = os:system_time(microsecond),
%	io:fwrite("Loss value: ~p\n",[Loss]),
	Time_elapsed=(Finish_Time-Start_Time)/ProcNumTrain,
	io:fwrite("Time took for all the nif: ~p micro sec , Number of processes = ~p ~n",[Time_elapsed, ProcNumTrain]).

niftest(0,_SampleListTrain,_Train_Lines, _Cols, _Labels, _ModelId) ->
	io:fwrite("finished all ~n");
niftest(Num,SampleListTrain,Train_Lines,Cols,Labels, ModelId) ->
	Curr_PID = self(),
	io:fwrite("start train2double ~n"),
	Pid2 = erlModule:train2double(Train_Lines, Cols, Labels, SampleListTrain,ModelId,Curr_PID),
	%Pid2 = spawn(fun()->erlModule:train2double(Train_Lines, Cols, Labels, SampleListTrain,ModelId,Curr_PID) end),
	receive
		LOSS_FUNC->
			io:fwrite("PID: ~p Loss func: ~p\n",[Pid2, LOSS_FUNC])
	end,

	niftest(Num-1,SampleListTrain,Train_Lines,Cols,Labels, ModelId).

thread_create_test() ->
	exit(nif_library_not_loaded).

predict() ->
	exit(nif_library_not_loaded).