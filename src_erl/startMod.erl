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
-export([start/7,start/1, startFSM/4]).

start(File, Learning_rate_List, Train_predict_ratio, ChunkSize, Cols, Labels, ModelId)->
  erlModule:module_create([8,4,3,2], Learning_rate_List, 80, [2,1,1,2], 1),
  parse:readfile(File, Train_predict_ratio, ChunkSize, Cols, Labels, ModelId).



start(0)-> finish;
start(ProcNum)->
  io:fwrite("start module_create ~n"),
  timer:sleep(100),
  _Pid1 = spawn(fun()->startFSM(0.01,[1,2,3,2,3,2,1,0,1,2,3,2,3,2,1,0,1,2,3,2,3,2,1,0,1,2,3,2,3,2,1,0,0,1,0,1,0,1,0,1],
    [1,2,3,2,3,2,1,0,1,2,3,2,3,2,1,0,1,2,3,2,3,2,1,0,1,2,3,2,3,2,1,0],0) end),
  start(ProcNum-1).


startFSM(_LearningRate, Data_Label, Data, Mid)->
  nerlNetStatem:start_link(),
  nerlNetStatem:create(0,_LearningRate),
  nerlNetStatem:train(Mid,Data_Label),
  timer:sleep(10),
  nerlNetStatem:predict(Mid,Data).

