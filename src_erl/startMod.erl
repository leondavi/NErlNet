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
-export([start/9,start/10, startFSM/4,init/10]).

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
  nerlNetStatem:start_link(),
  io:fwrite("start module_create ~n"),
  nerlNetStatem:create(0,LearningRate),
  Mid=erlModule:module_create(LayerSizes, Learning_rate_List, 80, [2,1,1,2], 1),
  io:fwrite("Mid: ~p\n",[Mid]),
  start(File, LayerSizes, Learning_rate_List, Train_predict_ratio, ChunkSize, NumOfChunks, Cols, Labels, ModelId,ProcNum).


start(_File, _LayerSizes, _Learning_rate_List, _Train_predict_ratio, _ChunkSize, _NumOfChunks, _Cols, _Labels, _ModelId,0)-> finish;
start(File, LayerSizes, Learning_rate_List, Train_predict_ratio, ChunkSize, NumOfChunks, Cols, Labels, ModelId,ProcNum)->
  Pid = spawn(fun()->start(File, LayerSizes, Learning_rate_List, Train_predict_ratio, ChunkSize, NumOfChunks, Cols, Labels, ModelId) end),
  io:fwrite("Pid: ~p\n",[Pid]),
  start(File, LayerSizes, Learning_rate_List, Train_predict_ratio, ChunkSize, NumOfChunks, Cols, Labels, ModelId,ProcNum-1).


startFSM(LearningRate, Data_Label, Data, Mid)->
  nerlNetStatem:start_link(),
  nerlNetStatem:create(0,LearningRate),
  nerlNetStatem:train(Mid,Data_Label),
  timer:sleep(10),
  nerlNetStatem:predict(Mid,Data).

