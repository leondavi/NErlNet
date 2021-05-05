%%%-------------------------------------------------------------------
%%% @author ziv
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 22. Oct 2020 20:57
%%%-------------------------------------------------------------------
-module(parse).
-author("ziv").

%% API
-export([readfile/6, readLines/6, sendToTrainPredict/8]).

%% Read csv file that represents a data set
%% File - File name with quotes and destination (if it's not in the current folder). For example: "skin_nonskin.csv"
%% Train_predict_ratio - A number between 0-1 represents the percentage of train samples to predict samples. The total
%% amount of train lines is trunced: 5.5->5.
%% For example: 0.8 means: 80% of trainning samples and 20% of prediction samples
%% ChunkSize - Number of samples per chunk that we send to train/predict module. If the chunk is larger than the amount
%% of samples, than we send all the samples in one chunk.
readfile(File, Train_predict_ratio,ChunkSize, Cols, _Labels, _ModelId) ->
  {ok, F} = file:open(File, [read, {encoding, utf8}]),
  FileLinesNumber=readNumOfLines(F,0),
  io:format("Total lines number: ~p~n", [FileLinesNumber]),
  Train_Lines = trunc(FileLinesNumber*Train_predict_ratio),
  io:format("Total train lines number: ~p~n", [Train_Lines]),
  PredictLines = FileLinesNumber-Train_Lines,
  io:format("Total predict lines number: ~p~n", [PredictLines]),
  {ok, Fi} = file:open(File, [read, {encoding, utf8}]),

  %SampleListTrain=sendToTrainPredict(Fi,Train_Lines,ChunkSize,train, Cols, Labels, ModelId),
  %SampleListPredict=sendToTrainPredict(Fi,Train_Lines,ChunkSize,predict, Cols, Labels, ModelId),
  SampleListTrain=readLines(Fi,Train_Lines,ChunkSize,[],train,Cols),
  SampleListPredict=readLines(Fi,Train_Lines,ChunkSize,[],predict,Cols),
  {FileLinesNumber,Train_Lines,PredictLines,SampleListTrain,SampleListPredict}.

  %% Send samples to train
  %sendToTrainPredict(Fi,Train_Lines,ChunkSize,train, Cols, Labels, ModelId).

  %% Send samples to predict
  %sendToTrainPredict(Fi,PredictLines,ChunkSize,predict, Cols, Labels, ModelId).


%% Send samples to train/predict depends on the Mode
sendToTrainPredict(F,Train_predict_Lines,ChunkSize, NumOfChunks, Mode, Cols, Labels, ModelId)->
  SampleList = readLines(F,Train_predict_Lines,ChunkSize,[],Mode,Cols),
  case Mode of
    train ->
      %io:format("Train chunk list: ~w~n", [SampleList]),
      LossVal=erlModule:train2double(ChunkSize, Cols, Labels, SampleList, ModelId), % Send to train
      io:fwrite("LossVal: ~p\n",[LossVal]);
      %timer:sleep(1000);
    predict ->
      %io:format("Predict chunk list: ~w~n", [SampleList]),
      Result=erlModule:predict2double(SampleList, ChunkSize, Cols, ModelId), % Send to predict
      io:fwrite("Result: ~p\n",[Result])
  end,
  if
    Train_predict_Lines >= ChunkSize ->
      if
        NumOfChunks > 1 ->
          %% Continue to another chunk
          sendToTrainPredict(F,Train_predict_Lines-ChunkSize,ChunkSize,NumOfChunks-1, Mode, Cols, Labels, ModelId);
        true -> finishRead
      end;
    true -> finishRead
  end,
  finishRead.


%% Read total lines amount in the file
readNumOfLines(F,LinesNumber)->
  case file:read_line(F) of
    eof ->
      LinesNumber;
    {ok, _Line} ->
      readNumOfLines(F,LinesNumber+1)
  end.

%% Read the samples line by line and return list that contains all the samples in the current chunk
%% End of all samples in the current train/predict
readLines(_F,0,_ChunkSize,ListOfSamples,_Mode,_Features) -> ListOfSamples;
%% End of chunk
readLines(_F,_LinesNumber,0,ListOfSamples,_Mode,_Features) -> ListOfSamples;
readLines(F,LinesNumber,ChunkSize,ListOfSamples,Mode,Features)->
case file:read_line(F) of
  eof ->
    ListOfSamples;
    {ok, Line} ->
    Trim = string:tokens(Line, ",\n"),
    SampleList=[begin {Integer,_}=string:to_integer(T), Integer end|| T<-Trim],
    %io:format("~w~n", [SampleList]),
    case Mode of
      predict ->
        {Data,_Labels}=lists:split(Features, SampleList),
        readLines(F,LinesNumber-1,ChunkSize-1,ListOfSamples ++ Data,Mode,Features);
      train ->
        readLines(F,LinesNumber-1,ChunkSize-1,ListOfSamples ++ SampleList,Mode,Features)
    end
end.