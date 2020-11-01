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
-export([start/9,startTrain/6,startPredict/5, startFSM/4,init/10]).

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


init(File, LayerSizes, Learning_rate_List, Train_predict_ratio, ChunkSize, NumOfChunks, Cols, Labels, ModelId,ProcNum)->
  Start_Time = os:system_time(microsecond),

  nerlNetStatem:start_link(),
  nerlNetStatem:create(Learning_rate_List,LayerSizes),

  {_FileLinesNumber,_Train_Lines,_PredictLines,SampleListTrain,SampleListPredict}=
    parse:readfile(File, Train_predict_ratio,ChunkSize, Cols, Labels, ModelId),

  startTrain(ChunkSize, Cols, Labels, SampleListTrain, ModelId,ProcNum),
  %io:format("Train chunk list: ~w~n", [SampleListTrain]),

  %startPredict(ChunkSize, Cols, SampleListTrain, ModelId,ProcNum),
  %io:format("Predict chunk list: ~w~n", [SampleListPredict]),

  Finish_Time = os:system_time(microsecond),
  Time_elapsed=Finish_Time-Start_Time,
  io:fwrite("Time took for nif: ~p ms , Number of processes = ~p ChunkSize= ~p Num of chunks: ~p learning rate: ~p~n",
    [Time_elapsed, ProcNum, ChunkSize, NumOfChunks, Learning_rate_List]).


startTrain(_ChunkSize, _Cols, _Labels, _SampleList, _ModelId,0)-> finish;
startTrain(ChunkSize, Cols, Labels, SampleList, ModelId,ProcNum)->
  io:fwrite("TrainList: ~p\n",[SampleList]),
  Pid = spawn(fun()-> nerlNetStatem:train(ChunkSize, Cols, Labels, SampleList, ModelId) end),
  io:fwrite("Pid: ~p\n",[Pid]),
  timer:sleep(10),
  startTrain(ChunkSize, Cols, Labels, SampleList, ModelId,ProcNum-1).


startPredict(_ChunkSize, _Cols, _SampleList, _ModelId,0)-> finish;
startPredict(ChunkSize, Cols, SampleList, ModelId,ProcNum)->
  io:fwrite("PredictList: ~p\n",[SampleList]),
  Pid = spawn(fun()-> nerlNetStatem:predict(SampleList, ChunkSize, Cols, ModelId) end),
  io:fwrite("Pid: ~p\n",[Pid]),
  timer:sleep(10),
  startPredict(ChunkSize, Cols, SampleList, ModelId,ProcNum-1).


startFSM(LearningRate, Data_Label, Data, Mid)->
  nerlNetStatem:start_link(),
  nerlNetStatem:create(0,LearningRate),
  nerlNetStatem:train(Mid,Data_Label),
  timer:sleep(10),
  nerlNetStatem:predict(Mid,Data).

