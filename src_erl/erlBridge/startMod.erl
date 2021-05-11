%%%-------------------------------------------------------------------
%%% @author ziv
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 20. Oct 2020 21:56
%%%-------------------------------------------------------------------
-module(startMod).
-author("ziv").

%% API
-export([start/9,startTrain/6,startPredict/5,init/10,initNew/11, startHandler/11, initp/11]).

start(File, _LayerSizes, _Learning_rate_List, Train_predict_ratio, ChunkSize, NumOfChunks, Cols, Labels, ModelId)->
  %Mid=erlModule:module_create(LayerSizes, Learning_rate_List, 80, [2,1,1,2], 1),
  %io:fwrite("Mid: ~p\n",[Mid]),
  {Fi,_FileLinesNumber,Train_Lines,PredictLines}=parse:readfile(File, Train_predict_ratio),

  %% Send samples to train
  FinishReadTrain=parse:sendToTrainPredict(Fi,Train_Lines,ChunkSize,NumOfChunks,train, Cols, Labels, ModelId),
  io:fwrite("FinishReadTrain: ~p\n",[FinishReadTrain]),

  %% Send samples to predict
  FinishReadPred=parse:sendToTrainPredict(Fi,PredictLines,ChunkSize,NumOfChunks,predict, Cols, Labels, ModelId),
  io:fwrite("FinishReadPred: ~p\n",[FinishReadPred]).

%% Start Read the dataset and train predict
%% ChunkSize - Number of samples (lines)
init(File, LayerSizes, Learning_rate_List, Train_predict_ratio, ChunkSize, NumOfChunks, Cols, Labels, ModelId,ProcNum)->
  Start_Time = os:system_time(microsecond),

  %% Start the state machine
  _Pid=nerlNetStatem:start_link(),

  %% Create the module
  nerlNetStatem:create(Learning_rate_List,LayerSizes),
  %gen_statem:cast(Pid,create(Learning_rate_List,LayerSizes)),

  %% Read the file
  %% File - File name and path
  %% Train_predict_ratio - For example 80 means 80 percent for train and 20 for predict
  %% ChunkSize - How many cols (samples) to process for each thread
  %% Cols - Number of columns in the CSV file excluding label columns(features)
  %% Labels - Number of label columns in the CSV file
  %% ModelId - Model id number
  %% _FileLinesNumber -
  %%
  {_FileLinesNumber,_Train_Lines,_PredictLines,SampleListTrain,_SampleListPredict}=
    parse:readfile(File, Train_predict_ratio,ChunkSize, Cols, Labels, ModelId),

  %io:fwrite("TrainList: ~p\n",[SampleListTrain]),
  %io:fwrite("Start sleep\n"),
  %timer:sleep(1000000),
  %io:fwrite("Finish sleep\n"),

  startTrain(ChunkSize, Cols, Labels, SampleListTrain, ModelId,ProcNum),
  %io:format("Train chunk list: ~w~n", [SampleListTrain]),


  %startPredict(ChunkSize, Cols, SampleListTrain, ModelId,ProcNum),
  %io:format("Predict chunk list: ~w~n", [SampleListPredict]),

  Finish_Time = os:system_time(microsecond),
  Time_elapsed=Finish_Time-Start_Time,
  io:fwrite("Time took for nif: ~p ms , Number of processes = ~p ChunkSize= ~p Num of chunks: ~p learning rate: ~p~n",
    [Time_elapsed, ProcNum, ChunkSize, NumOfChunks, Learning_rate_List]).

initp(_File, _Train_predict_ratio,_ChunkSize, _Cols, _Labels, _ModelId, _ActivationList, _Learning_rate, _Layers_sizes, _Optimizer, 0) ->
  finished_initp;

initp(File, Train_predict_ratio,ChunkSize, Cols, Labels, ModelId, ActivationList, Learning_rate, Layers_sizes, Optimizer, ProcNumTrain) ->
  spawn(fun()-> initNew(File, Train_predict_ratio,ChunkSize, Cols, Labels, ModelId, ActivationList, Learning_rate, Layers_sizes, Optimizer, ProcNumTrain) end),
  initp(File, Train_predict_ratio,ChunkSize, Cols, Labels, ModelId+1, ActivationList, Learning_rate, Layers_sizes, Optimizer, ProcNumTrain-1).

%% Start Read the dataset and train predict
%% ChunkSize - Number of samples (lines)
initNew(File, Train_predict_ratio,ChunkSize, Cols, Labels, ModelId, ActivationList, Learning_rate, Layers_sizes, Optimizer, ProcNumTrain)->


  %% Read the file
  %% File - File name and path
  %% Train_predict_ratio - For example 80 means 80 percent for train and 20 for predict
  %% ChunkSize - How many cols (samples) to process for each thread
  %% Cols - Number of columns in the CSV file excluding label columns(features)
  %% Labels - Number of label columns in the CSV file
  %% ModelId - Model id number
  %% _FileLinesNumber -
  %%
  {_FileLinesNumber,_Train_Lines,_PredictLines,SampleListTrain,_SampleListPredict}=
    parse:readfile(File, Train_predict_ratio,ChunkSize, Cols, Labels, ModelId),

  ClientPid = self(),

  %% Start the state machine
  NerlNetStatemPid=nerlNetStatem:start_link({ClientPid,clientName,{Layers_sizes, Learning_rate, ActivationList, Optimizer, ModelId, Cols, Labels, ChunkSize}}),

  %% Create the module
  %gen_statem:cast(NerlNetStatemPid,{create,{Layers_sizes, Learning_rate, ActivationList, Optimizer, ModelId,Curr_PID}}),

  Start_Time = os:system_time(microsecond),

  %% Start train
  gen_statem:cast(NerlNetStatemPid, {training}),
  gen_statem:cast(NerlNetStatemPid,{sample, SampleListTrain}),

  receive
    LOSS_FUNC->
      %io:fwrite("Loss func: ~p\n",[LOSS_FUNC]),
      LOSS_FUNC
  end,
  %receive
  %  LOSS_FUNC->
  %    io:fwrite("PID: ~p Loss func: ~p\n",[Curr_PID, LOSS_FUNC])
  %end,

  %timer:sleep(5000),

  % Start predict
  %gen_statem:cast(NerlNetStatemPid,predict),
  %gen_statem:cast(NerlNetStatemPid,{sample, [80,92,132]}), % TODO change arguments chunkSize is different
 % receive
  %  Result->
  %    io:fwrite("PID: ~p Result: ~p\n",[Curr_PID, Result])
  %end,

  %startPredict(ChunkSize, Cols, SampleListTrain, ModelId,ProcNum),
  %io:format("Predict chunk list: ~w~n", [SampleListPredict]),

  Finish_Time = os:system_time(microsecond),
  Time_elapsed=Finish_Time-Start_Time,
  io:fwrite("Time took for nif: ~p ms , Number of processes = ~p ChunkSize= ~p Num of chunks: ~p learning rate: ~p~n",
    [Time_elapsed, ProcNumTrain, ChunkSize, 1, Learning_rate]).

startHandler(_File, _Train_predict_ratio,_ChunkSize, _Cols, _Labels, ModelId, ActivationList, Learning_rate, Layers_sizes, Optimizer, _ProcNumTrain)->
  %% Start the state machine
  HandlerPid=handler:start_link(),
  io:fwrite("HandlerPid: ~p\n",[HandlerPid]),
  HandlerPid2=handler:start_link(),
  io:fwrite("HandlerPid: ~p\n",[HandlerPid2]),

  %% Create the module TODO put it the the init
  gen_statem:cast(HandlerPid,{create,{Layers_sizes, Learning_rate, ActivationList, Optimizer, ModelId,self()}}),

  receive
    NerlNetStatemPid->
      io:fwrite("NerlNetStatemPid: ~p\n",[NerlNetStatemPid])
  end.

  %gen_statem:cast(ClientStatemPid,{finishInit,NerlNetStatemPid,ModelId}). TODO



startTrain(_ChunkSize, _Cols, _Labels, _SampleList, _ModelId,0)-> finish;
startTrain(ChunkSize, Cols, Labels, SampleList, ModelId,ProcNum)->
  io:fwrite("TrainList: ~p\n",[SampleList]),
  Pid = spawn(fun()-> nerlNetStatem:train(ChunkSize, Cols, Labels, SampleList, ModelId) end),
  io:fwrite("Pid: ~p\n",[Pid]),
  %timer:sleep(10),
  startTrain(ChunkSize, Cols, Labels, SampleList, ModelId,ProcNum-1).


startPredict(_ChunkSize, _Cols, _SampleList, _ModelId,0)-> finish;
startPredict(ChunkSize, Cols, SampleList, ModelId,ProcNum)->
  io:fwrite("PredictList: ~p\n",[SampleList]),
  Pid = spawn(fun()-> nerlNetStatem:predict(SampleList, ChunkSize, Cols, ModelId) end),
  io:fwrite("Pid: ~p\n",[Pid]),
  timer:sleep(10),
  startPredict(ChunkSize, Cols, SampleList, ModelId,ProcNum-1).


%startFSM(LearningRate, Data_Label, Data, Mid)->
%  nerlNetStatem:start_link(),
%  nerlNetStatem:create(0,LearningRate),
%  nerlNetStatem:train(Mid,Data_Label),
%  timer:sleep(10),
%  nerlNetStatem:predict(Mid,Data).

