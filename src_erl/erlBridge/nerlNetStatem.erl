%%%-------------------------------------------------------------------
%%% @author ziv
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 07. Oct 2020 21:58
%%%-------------------------------------------------------------------
-module(nerlNetStatem).
-author("ziv").
-include("cppSANNStatemModes.hrl").

-behaviour(gen_statem).

%% API
-export([start_link/1]).

%% gen_statem callbacks
-export([init/1, format_status/2, state_name/3, handle_event/4, terminate/3,
  code_change/4, callback_mode/0]).
%% States functions
-export([idle/3, train/3, predict/3, wait/3]).

-define(SERVER, ?MODULE).

%% federatedMode = 0 - Not federated, 1 - Federated get and send weights, 2 - Federated set weights
%% countLimit - Number of samples to count before sending the weights for averaging. Predifined in the json file.
%% count - Number of samples recieved for training after the last weights sended.
-record(nerlNetStatem_state, {clientPid, features, labels, myName, modelId, nextState, currentBatchID=0,  missedSamplesCount = 0, missedTrainSamples= [], federatedMode, count = 1, countLimit,optimizer, lossMethod, learningRate}).

%%%===================================================================
%%% API
%%%===================================================================

%% @doc Creates a gen_statem process which calls Module:init/1 to
%% initialize. To ensure a synchronized start-up procedure, this
%% function does not return until Module:init/1 has returned.
start_link(ARGS) ->
  %{ok,Pid} = gen_statem:start_link({local, ?SERVER}, ?MODULE, [], []),
  {ok,Pid} = gen_statem:start_link(?MODULE, ARGS, []),
  Pid.

%%%===================================================================
%%% gen_statem callbacks
%%%===================================================================

%% @private
%% @doc Whenever a gen_statem is started using gen_statem:start/[3,4] or
%% gen_statem:start_link/[3,4], this function is called by the new
%% process to initialize.

init({WorkerName,ModelId, ModelType, ScalingMethod,LayerTypesList,LayersSizes,LayersActivationFunctions,FederatedMode,CountLimit,Optimizer, Features, Labels, LossMethod, LearningRate, ClientPID}) ->
  io:fwrite("start module_create ~n"),
  io:fwrite("WorkerName: ~p ,ModelId: ~p , ModelType: ~p , ScalingMethod: ~p ,LayerTypesList: ~p ,LayersSizes: ~p ,LayersActivationFunctions: ~p ,FederatedMode: ~p ,CountLimit: ~p ,Optimizer: ~p , Features: ~p , Labels: ~p , ClientPID: ~p~n"
    ,[WorkerName,ModelId, ModelType, ScalingMethod,LayerTypesList,LayersSizes,LayersActivationFunctions,FederatedMode,CountLimit,Optimizer, Features, Labels, ClientPID]),
%%^^^^^^^^^^^^^^^^^^^^^^^^^^
        Res=niftest:create_nif(ModelId, ModelType , ScalingMethod , LayerTypesList , LayersSizes , LayersActivationFunctions),
    io:fwrite("Res = ~p ~n",[Res]),

  {ok, idle, #nerlNetStatem_state{clientPid = ClientPID, features = Features, labels = Labels, myName = WorkerName,
                                 modelId = ModelId, federatedMode = FederatedMode,optimizer = Optimizer,  countLimit = CountLimit,learningRate = LearningRate, lossMethod = LossMethod}}.

%% @private
%% @doc This function is called by a gen_statem when it needs to find out
%% the callback mode of the callback module.
callback_mode() ->
  state_functions.

%% @private
%% @doc Called (1) whenever sys:get_status/1,2 is called by gen_statem or
%% (2) when gen_statem terminates abnormally.
%% This callback is optional.
format_status(_Opt, [_PDict, _StateName, _State]) ->
  Status = some_term,
  Status.

%% @private
%% @doc There should be one instance of this function for each possible
%% state name.  If callback_mode is state_functions, one of these
%% functions is called when gen_statem receives and event from
%% call/2, cast/2, or as a normal process message.
state_name(_EventType, _EventContent, State = #nerlNetStatem_state{}) ->
  NextStateName = next_state,
  {next_state, NextStateName, State}.

%% @private
%% @doc If callback_mode is handle_event_function, then whenever a
%% gen_statem receives an event from call/2, cast/2, or as a normal
%% process message, this function is called.
handle_event(_EventType, _EventContent, _StateName, State = #nerlNetStatem_state{}) ->
  NextStateName = the_next_state_name,
  {next_state, NextStateName, State}.

%% @private
%% @doc This function is called by a gen_statem when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_statem terminates with
%% Reason. The return value is ignored.
terminate(_Reason, _StateName, _State) ->
  ok.

%% @private
%% @doc Convert process state when code is changed
code_change(_OldVsn, StateName, State = #nerlNetStatem_state{}, _Extra) ->
  {ok, StateName, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================


%% Define states

%% State idle
idle(cast, {training}, State = #nerlNetStatem_state{}) ->
  io:fwrite("Go from idle to train\n"),
  {next_state, train, State};

idle(cast, {predict}, State = #nerlNetStatem_state{currentBatchID = CurrentBatchID}) ->
  io:fwrite("Go from idle to predict\n"),
  {next_state, predict, State#nerlNetStatem_state{currentBatchID = CurrentBatchID + 1}};

idle(cast, {set_weights,Ret_weights_list}, State = #nerlNetStatem_state{nextState = NextState, modelId=ModelId}) ->

  io:fwrite("Set weights in wait state: \n"),
  
  %% Set weights TODO maybe send the results of the update
  [WeightsList, BiasList, Biases_sizes_list, Wheights_sizes_list] = Ret_weights_list,

  %% Make bias sizes and weights sizes as integer 
  NewBiases_sizes_list = [round(X)||X<-Biases_sizes_list],
  NewWheights_sizes_list = [round(X)||X<-Wheights_sizes_list],
  _Result_set_weights = erlModule:set_weights(WeightsList, BiasList, NewBiases_sizes_list, NewWheights_sizes_list, ModelId),
  _Result_set_weights = erlModule:set_weights(WeightsList, BiasList, Biases_sizes_list, Wheights_sizes_list, ModelId),

  {next_state, NextState, State};

idle(cast, Param, State) ->
  io:fwrite("Same state idle, command: ~p\n",[Param]),
  {next_state, idle, State}.

%% Regular mode (Not federated)
wait(cast, {loss, LossFunc}, State = #nerlNetStatem_state{clientPid = ClientPid, myName = MyName, nextState = NextState}) ->
  % Federated mode = 0 (not federated)
  %io:fwrite("loss LossFunc at worker: ~p~n",[{loss, LossFunc}]),
  gen_statem:cast(ClientPid,{loss, MyName, LossFunc}), %% TODO Add Time and Time_NIF to the cast
  {next_state, NextState, State};


%% Waiting for receiving results or loss function
%% Got nan or inf from loss function - Error, loss function too big for double
wait(cast, {loss,nan,_Time_NIF}, State = #nerlNetStatem_state{clientPid = ClientPid, myName = MyName, nextState = NextState}) ->
  io:fwrite("ERROR: Loss func in wait: nan (Loss function too big for double)\n"),
  gen_statem:cast(ClientPid,{loss, MyName, nan}), %% TODO send to tal stop casting request with error desc
  {next_state, NextState, State};

%% Federated mode
wait(cast, {loss, LossAndTime,_Time_NIF}, State = #nerlNetStatem_state{clientPid = ClientPid, myName = MyName, nextState = NextState, count = Count, countLimit = CountLimit, modelId = Mid, federatedMode = ?MODE_FEDERATED}) ->
  {LOSS_FUNC,_TimeCpp} = LossAndTime,
  if Count == CountLimit ->
      % Get weights
      Ret_weights_tuple = erlModule:get_weights(Mid),
      {Wheights,Bias,Biases_sizes_list,Wheights_sizes_list} = Ret_weights_tuple,

      ListToSend = [Wheights,Bias,Biases_sizes_list,Wheights_sizes_list],

      % Send weights and loss value
      gen_statem:cast(ClientPid,{loss, federated_weights, MyName, LOSS_FUNC, ListToSend}), %% TODO Add Time and Time_NIF to the cast
      
      % Reset count and go to state train
      {next_state, NextState, State#nerlNetStatem_state{count = 1}};

    true ->
      %% Send back the loss value
      gen_statem:cast(ClientPid,{loss, MyName, LOSS_FUNC}), %% TODO Add Time and Time_NIF to the cast
      {next_state, NextState, State#nerlNetStatem_state{count = Count + 1}}
  end;


%% Regular mode (Not federated)
wait(cast, {loss, {LossVal,Time}}, State = #nerlNetStatem_state{clientPid = ClientPid, myName = MyName, nextState = NextState, federatedMode = ?MODE_REGULAR}) ->

io:fwrite("loss, {LossVal,Time}: ~p~n",[{loss, {LossVal,Time}}]),

  gen_statem:cast(ClientPid,{loss, MyName, LossVal}), %% TODO Add Time and Time_NIF to the cast
  {next_state, NextState, State};

%% Regular mode (Not federated)
wait(cast, {loss, LossAndTime,_Time_NIF}, State = #nerlNetStatem_state{clientPid = ClientPid, myName = MyName, nextState = NextState, federatedMode = ?MODE_REGULAR}) ->
  {LOSS_FUNC,_TimeCpp} = LossAndTime,
  % Federated mode = 0 (not federated)
  gen_statem:cast(ClientPid,{loss, MyName, LOSS_FUNC}), %% TODO Add Time and Time_NIF to the cast
  {next_state, NextState, State};

% Not supposed to be here - for future additions
wait(cast, {loss, LossAndTime,_Time_NIF}, State = #nerlNetStatem_state{nextState = NextState}) ->
  io:fwrite("Error, Not supposed to be here. nerlNetStatem. Got LossAndTime: ~p~n",[LossAndTime]),
  {next_state, NextState, State};

wait(cast, {set_weights,Ret_weights_list}, State = #nerlNetStatem_state{nextState = NextState, modelId=ModelId}) ->
  io:fwrite("Set weights in wait state: \n"),

  %% Set weights
  [WeightsList, BiasList, Biases_sizes_list, Wheights_sizes_list] = Ret_weights_list,
  %% Make bias sizes and weights sizes as integer 
  NewBiases_sizes_list = [round(X)||X<-Biases_sizes_list],
  NewWeights_sizes_list = [round(X)||X<-Wheights_sizes_list],
  _Result_set_weights = erlModule:set_weights(WeightsList, BiasList, NewBiases_sizes_list, NewWeights_sizes_list, ModelId),

  {next_state, NextState, State};

wait(cast, {predictRes,Res}, State = #nerlNetStatem_state{currentBatchID = CurrentBatchID, myName = MyName, clientPid = ClientPid, nextState = NextState}) ->
    io:fwrite("~ngot predict result ~p~n",[Res]),

  gen_statem:cast(ClientPid,{predictRes,MyName, "CSVname",CurrentBatchID, Res}), %% TODO TODO change csv name and batch id(1)
  {next_state, NextState, State};

wait(cast, {predictRes,CSVname, BatchID, {RESULTS,_TimeCpp},_Time_NIF}, State = #nerlNetStatem_state{myName = MyName, clientPid = ClientPid, nextState = NextState}) ->
  gen_statem:cast(ClientPid,{predictRes,MyName, CSVname, BatchID, RESULTS}), %% TODO Add Time and Time_NIF to the cast
  {next_state, NextState, State};

wait(cast, {idle}, State) ->
  io:fwrite("Waiting, next state - idle: \n"),
  {next_state, wait, State#nerlNetStatem_state{nextState = idle}};

wait(cast, {training}, State) ->
  io:fwrite("Waiting, next state - train: \n"),
  {next_state, wait, State#nerlNetStatem_state{nextState = train}};

wait(cast, {predict}, State) ->
  io:fwrite("Waiting, next state - predict: \n"),
  {next_state, wait, State#nerlNetStatem_state{nextState = predict}};

wait(cast, {sample, SampleListTrain}, State = #nerlNetStatem_state{missedSamplesCount = MissedSamplesCount, missedTrainSamples = MissedTrainSamples}) ->
  io:fwrite("Missed in pid: ~p, Missed batches count: ~p\n",[self(), MissedSamplesCount]),
  Miss = MissedTrainSamples++SampleListTrain,
  {next_state, wait, State#nerlNetStatem_state{missedSamplesCount = MissedSamplesCount+1, missedTrainSamples = Miss}};

wait(cast, Param, State) ->
  io:fwrite("Not supposed to be. Got: ~p\n",[{Param}]),
  {next_state, wait, State}.


%% State train
train(cast, {sample, SampleListTrain}, State = #nerlNetStatem_state{modelId = ModelId, features = Features, labels = Labels, optimizer = Optimizer, lossMethod = LossMethod, learningRate = LearningRate}) ->
  % CurrPid = self(),
  % ChunkSizeTrain = round(length(SampleListTrain)/(Features + Labels)),
  % ^^^^^^^^^^^^^^^^^^
  %ModelID = 586000901,
  % OptimizationMethod = 1,
  %io:format("~p~n",[SampleListTrain]),
  %RandomGeneratedData1 = [[rand:normal(0,0.5)||_<-lists:seq(1,128)] ++[0.0]||_<-lists:seq(1,5)],
  %RandomGeneratedData2 = [[rand:normal(1,0.5)||_<-lists:seq(1,128)] ++[1.0]||_<-lists:seq(1,5)],
  RandomGeneratedData1 = [[rand:normal(0,1)||_<-lists:seq(1,128)] ||_<-lists:seq(1,5)],
  RandomGeneratedData2 = [[rand:normal(1,1)||_<-lists:seq(1,128)] ||_<-lists:seq(1,5)],
  Shuffled = lists:flatten([X||{_,X} <- lists:sort([ {random:uniform(), N} || N <- RandomGeneratedData1++RandomGeneratedData2])]),
  %RandomGeneratedData = lists:flatten([[rand:normal()||_<-lists:seq(1,128)] ++[0.0]||_<-lists:seq(1,10)]),
  DataTensor = [10.0 , 128.0 , 1.0] ++ Shuffled,
  MyPid=self(),
  _Pid = spawn(fun()-> niftest:call_to_train(ModelId, Optimizer , LossMethod , LearningRate , SampleListTrain ,MyPid) end),
  {next_state, wait, State#nerlNetStatem_state{nextState = train}};


  train(cast, {set_weights,Ret_weights_list}, State = #nerlNetStatem_state{modelId = ModelId, nextState = NextState}) ->
  %% Set weights
  [WeightsList, BiasList, Biases_sizes_list, Wheights_sizes_list] = Ret_weights_list,
  
  %% Make bias sizes and weights sizes as integer 
  NewBiases_sizes_list = [round(X)||X<-Biases_sizes_list],
  NewWheights_sizes_list = [round(X)||X<-Wheights_sizes_list],
  _Result_set_weights = erlModule:set_weights(WeightsList, BiasList, NewBiases_sizes_list, NewWheights_sizes_list, ModelId),

  {next_state, NextState, State};


train(cast, {idle}, State) ->
  io:fwrite("Go from train to idle\n"),
  {next_state, idle, State};

train(cast, {predict}, State) ->
  io:fwrite("Go from train to predict\n"),
  {next_state, predict, State};

train(cast, Param, State) ->
  io:fwrite("Same state train, not supposed to be, command: ~p\n",[Param]),
  {next_state, train, State}.

%% State predict
predict(cast, {sample,CSVname, BatchID, SampleListPredict}, State = #nerlNetStatem_state{currentBatchID = CurrentBatchID, features = Features, modelId = ModelId}) ->
  ChunkSizePred = round(length(SampleListPredict)/Features),
  CurrPID = self(),
    % ^^^^^^^^^^^^^^^^^^
  %  ModelID = 586000901,
  %  RandomGeneratedDataP = [rand:normal()||_<-lists:seq(1,1280)] ,
    RandomGeneratedData1 = [[rand:normal(0,0.5)||_<-lists:seq(1,128)]||_<-lists:seq(1,5)],
  RandomGeneratedData2 = [[rand:normal(1,0.5)||_<-lists:seq(1,128)]||_<-lists:seq(1,5)],
  Shuffled = lists:flatten([X||{_,X} <- lists:sort([ {random:uniform(), N} || N <- RandomGeneratedData1++RandomGeneratedData2])]),
  %RandomGeneratedData = lists:flatten([[rand:normal()||_<-lists:seq(1,128)] ++[0.0]||_<-lists:seq(1,10)]),
  DataTensor = [10.0 , 128.0 , 1.0] ++ Shuffled,

  %  DataTensorP = [10.0 , 128.0 , 1.0] ++ RandomGeneratedDataP,


  _Pid = spawn(fun()-> niftest:call_to_predict(ModelId,SampleListPredict,CurrPID) end),


  {next_state, wait, State#nerlNetStatem_state{currentBatchID = CurrentBatchID + 1, nextState = predict}};

predict(cast, {idle}, State) ->
  io:fwrite("Go from predict to idle\n"),
  {next_state, idle, State};

predict(cast, {training}, State) ->
  io:fwrite("Go from predict to train\n"),
  {next_state, train, State};

predict(cast, Param, State) ->
  io:fwrite("Same state Predict, command: ~p\n",[Param]),
  {next_state, predict, State}.
