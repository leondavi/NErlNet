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
-export([readfile/6, readLines/4]).

%% Read csv file that represents a data set
%% File - File name with quotes and destination (if it's not in the current folder). For example: "skin_nonskin.csv"
%% Train_predict_ratio - A number between 0-1 represents the percentage of train samples to predict samples. The total
%% amount of train lines is trunced: 5.5->5.
%% For example: 0.8 means: 80% of trainning samples and 20% of prediction samples
%% ChunkSize - Number of samples per chunk that we send to train/predict module. If the chunk is larger than the amount
%% of samples, than we send all the samples in one chunk.
readfile(File, Train_predict_ratio, ChunkSize, Cols, Labels, ModelId) ->
  {ok, F} = file:open(File, [read, {encoding, utf8}]),
  FileLinesNumber=readNumOfLines(F,0),
  io:format("Total lines number: ~p~n", [FileLinesNumber]),
  Train_Lines = trunc(FileLinesNumber*Train_predict_ratio),
  io:format("Total train lines number: ~p~n", [Train_Lines]),
  PredictLines = FileLinesNumber-Train_Lines,
  io:format("Total predict lines number: ~p~n", [PredictLines]),

  {ok, Fi} = file:open(File, [read, {encoding, utf8}]),

  %% Send samples to train
  sendToTrainPredict(Fi,Train_Lines,ChunkSize,train, Train_Lines, Cols, Labels, ModelId),

  %% Send samples to predict
  sendToTrainPredict(Fi,PredictLines,ChunkSize,predict, Train_Lines, Cols, Labels, ModelId).


%% Send samples to train/predict depends on the Mode
sendToTrainPredict(F,Train_Lines,ChunkSize, Mode, Rows, Cols, Labels, ModelId)->
  SampleList = readLines(F,Train_Lines,ChunkSize,[]),
  case Mode of
    train ->
      io:format("Train chunk list: ~p~n", [SampleList]),
      erlModule:train2double(Rows, Cols, Labels, SampleList, ModelId); % TODO: Send to train
    predict ->
      io:format("Predict chunk list: ~p~n", [SampleList])
     % erlModule:predict2double(Data_mat, Rows, Cols, ModelId) % TODO: Send to predict
  end,
  if
    Train_Lines >= ChunkSize ->
      %% Continue to another chunk
      sendToTrainPredict(F,Train_Lines-ChunkSize,ChunkSize, Mode, Rows, Cols, Labels, ModelId)
  end.


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
readLines(_F,0,_ChunkSize,ListOfSamples) -> ListOfSamples;
%% End of chunk
readLines(_F,_LinesNumber,0,ListOfSamples) -> ListOfSamples;
readLines(F,LinesNumber,ChunkSize,ListOfSamples)->
case file:read_line(F) of
  eof ->
    ListOfSamples;
  {ok, Line} ->
    Trim = string:tokens(Line, ",\n"),
    SampleList=[begin {Integer,_}=string:to_integer(T), Integer end|| T<-Trim],
    io:format("~p~n", [SampleList]),

    readLines(F,LinesNumber-1,ChunkSize-1,ListOfSamples ++ SampleList)
end.