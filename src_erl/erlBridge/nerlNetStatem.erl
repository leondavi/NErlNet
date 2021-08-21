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
-record(nerlNetStatem_state, {clientPid, features, labels, myName, modelId, nextState, missedSamplesCount = 0, missedTrainSamples= [], federatedMode, count = 0, countLimit}).

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
init({ClientPID, MyName, {Layers_sizes, Learning_rate, ActivationList, Optimizer, ModelId, Features, Labels, FederatedMode, CountLimit}}) ->
%init({ClientPID, MyName, {Layers_sizes, Learning_rate, ActivationList, Optimizer, ModelId, Features, Labels}}) ->
 % FederatedMode = 0, % TODO delete
%  CountLimit = 1, % TODO delete
  io:fwrite("start module_create ~n"),

  _Res=erlModule:module_create(Layers_sizes, Learning_rate, ActivationList, Optimizer, ModelId),
  io:fwrite("Layers_sizes: ~p, Learning_rate: ~p, ActivationList: ~p, Optimizer: ~p, ModelId ~p\n",[Layers_sizes, Learning_rate, ActivationList, Optimizer, ModelId]),
  %io:fwrite("Mid: ~p\n",[Mid]),
  %{ok, idle, []}.
  %timer:sleep(20000),
  %Ret_weights_tuple = erlModule:get_weights(0),
  %{Wheights,Bias,Size} = Ret_weights_tuple,
  %file:write_file("NerlStatemOut.txt", [lists:flatten(io_lib:format("~p~p~p",[Size,Bias,Wheights]))]),
  {ok, idle, #nerlNetStatem_state{clientPid = ClientPID, features = Features, labels = Labels, myName = MyName, modelId = ModelId, federatedMode = FederatedMode, countLimit = CountLimit}}.

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

idle(cast, {predict}, State) ->
  io:fwrite("Go from idle to predict\n"),
  {next_state, predict, State};

idle(cast, {set_weights,Ret_weights_tuple}, State = #nerlNetStatem_state{nextState = NextState, modelId=ModelId}) ->
  {Wheights,Bias,Size} = Ret_weights_tuple,
  io:fwrite("Set weights in wait state: \n"),

  %% Set weights TODO maybe send the results of the update
  _Result_set_weights = erlModule:set_weights(Wheights,Bias,Size,ModelId),
  {next_state, NextState, State};

idle(cast, Param, State) ->
  io:fwrite("Same state idle, command: ~p\n",[Param]),
  {next_state, idle, State}.

%% Waiting for receiving results or loss function
%% Got nan or inf from loss function - Error, loss function too big for double
wait(cast, {loss,nan,_Time_NIF}, State = #nerlNetStatem_state{clientPid = ClientPid, myName = MyName, nextState = NextState}) ->
  io:fwrite("ERROR: Loss func in wait: nan (Loss function too big for double)\n"),
  gen_statem:cast(ClientPid,{loss, MyName, nan}), %% TODO send to tal stop casting request with error desc
  {next_state, NextState, State};

wait(cast, {loss, LossAndTime,Time_NIF}, State = #nerlNetStatem_state{clientPid = ClientPid, myName = MyName, nextState = NextState, count = Count, countLimit = CountLimit, modelId = Mid, federatedMode = FederatedMode}) ->
  {LOSS_FUNC,TimeCpp} = LossAndTime,
  io:fwrite("Loss func in wait: ~p\nTime for train execution in cppSANN (micro sec): ~p\nTime for train execution in NIF+cppSANN (micro sec): ~p\n",[LOSS_FUNC, TimeCpp, Time_NIF]),
  if
    FederatedMode == 1 and (Count == CountLimit)-> 
      % Get weights
      io:fwrite("Get weights: \n"),
      Ret_weights_tuple = erlModule:get_weights(Mid),
      {Wheights,Bias,Size} = Ret_weights_tuple,
      file:write_file("NerlStatemOut.txt", [lists:flatten(io_lib:format("~p~p~p",[Size,Bias,Wheights]))]), %  TODO delete, for debugging

      % Send weights and loss value TODO
      gen_statem:cast(ClientPid,{loss, federated_weights, MyName, LOSS_FUNC, Ret_weights_tuple}), %% TODO Add Time and Time_NIF to the cast
      
      % Reset count and go to state train
      State#nerlNetStatem_state{count = 1};

    FederatedMode == 1 ->
      %% Send back the loss value
      gen_statem:cast(ClientPid,{loss, federated, MyName, LOSS_FUNC}); %% TODO Add Time and Time_NIF to the cast
      %State#nerlNetStatem_state{count = Count + 1};

    true -> % Federated mode = 0 (not federated)
    io:fwrite("NOT Federated wait: \n"),
      gen_statem:cast(ClientPid,{loss, MyName, LOSS_FUNC}) %% TODO Add Time and Time_NIF to the cast
  end,

  {next_state, NextState, State};

wait(cast, {set_weights,Ret_weights_tuple}, State = #nerlNetStatem_state{nextState = NextState, modelId=ModelId}) ->
  {Wheights,Bias,Size} = Ret_weights_tuple,
  io:fwrite("Set weights in wait state: \n"),

  %% Set weights TODO
  _Result_set_weights = erlModule:set_weights(Wheights,Bias,Size,ModelId),

  {next_state, NextState, State};

%wait(cast, {loss,LossAndTime,Time_NIF}, State = #nerlNetStatem_state{clientPid = ClientPid, myName = MyName, nextState = NextState}) ->

  %{LOSS_FUNC,TimeCpp} = LossAndTime,
  %io:fwrite("Loss func in wait: ~p\nTime for train execution in cppSANN (micro sec): ~p\nTime for train execution in NIF+cppSANN (micro sec): ~p\n",[LOSS_FUNC, TimeCpp, Time_NIF]),
 % gen_statem:cast(ClientPid,{loss, MyName, LOSS_FUNC}), %% TODO Add Time and Time_NIF to the cast
%  {next_state, NextState, State};

wait(cast, {predictRes,CSVname, BatchID, {RESULTS,TimeCpp},Time_NIF}, State = #nerlNetStatem_state{clientPid = ClientPid, nextState = NextState}) ->
  io:fwrite("Predict results: ~p\nTime for predict execution in cppSANN (micro sec): ~p\nTime for predict execution in NIF+cppSANN (micro sec): ~p\n",[RESULTS, TimeCpp, Time_NIF]),
  gen_statem:cast(ClientPid,{predictRes, CSVname, BatchID, RESULTS}), %% TODO Add Time and Time_NIF to the cast
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
  io:fwrite("Missed, got sample. Got: ~p \n Missed batches count: ~p\n",[{SampleListTrain}, MissedSamplesCount]),
  io:fwrite("Missed in pid: ~p, Missed batches count: ~p\n",[self(), MissedSamplesCount]),
  Miss = MissedTrainSamples++SampleListTrain,
  {next_state, wait, State#nerlNetStatem_state{missedSamplesCount = MissedSamplesCount+1, missedTrainSamples = Miss}};

wait(cast, Param, State) ->
  io:fwrite("Not supposed to be. Got: ~p\n",[{Param}]),
  {next_state, wait, State}.


%% State train
train(cast, {sample, SampleListTrain}, State = #nerlNetStatem_state{modelId = ModelId, features = Features, labels = Labels, count = Count}) ->
  CurrPid = self(),
  ChunkSizeTrain = round(length(SampleListTrain)/(Features + Labels)),
  %io:fwrite("length(SampleListTrain)/(Features + Labels): ~p\n",[length(SampleListTrain)/(Features + Labels)]),
  %io:fwrite("Send sample to train: ~p\n",[SampleListTrain]),
  %io:fwrite("ChunkSizeTrain: ~p, Features: ~p Labels: ~p ModelId ~p\n",[ChunkSizeTrain, Features, Labels, ModelId]),
  _Pid = spawn(fun()-> erlModule:train2double(ChunkSizeTrain, Features, Labels, SampleListTrain,ModelId,CurrPid) end),
  {next_state, wait, State#nerlNetStatem_state{nextState = train, count = Count + 1}};


train(cast, {set_weights,Ret_weights_tuple}, State = #nerlNetStatem_state{modelId = ModelId, nextState = NextState}) ->

  {Wheights,Bias,Size} = Ret_weights_tuple,
  io:fwrite("Set weights in train state: \n"),

  %% Set weights TODO
  _Result_set_weights = erlModule:set_weights(Wheights,Bias,Size,ModelId),

  {next_state, NextState, State};


%train(cast, {idle}, State = #nerlNetStatem_state{missedTrainSamples = MissedTrainSamples,modelId = ModelId, features = Features, labels = Labels}) ->
%  io:fwrite("Go from train to idle after finishing Missed train samples: ~p\n",[MissedTrainSamples]),
  %trainMissed(MissedTrainSamples,ModelId,Features,Labels),
%  io:fwrite("Go from train to idle\n"),
%  {next_state, idle, State};

train(cast, {idle}, State) ->
  %io:fwrite("Go from train to idle after finishing Missed train samples: ~p\n",[MissedTrainSamples]),
  %trainMissed(MissedTrainSamples,ModelId,Features,Labels),
  io:fwrite("Go from train to idle\n"),
  {next_state, idle, State};

train(cast, {predict}, State) ->
  io:fwrite("Go from train to predict\n"),
  {next_state, predict, State};

train(cast, Param, State) ->
  io:fwrite("Same state train, not supposed to be, command: ~p\n",[Param]),
  {next_state, train, State}.

trainMissed([],ModelId,Features,Labels)->
  io:fwrite("Finished train samples\n");

trainMissed([FirstTrainChunk|MissedTrainSamples],ModelId,Features,Labels)->
  CurrPid = self(),
  ChunkSizeTrain = round(length(FirstTrainChunk)/(Features + Labels)),
  _Pid = spawn(fun()-> erlModule:train2double(ChunkSizeTrain, Features, Labels, FirstTrainChunk,ModelId,CurrPid) end),
  trainMissed([FirstTrainChunk|MissedTrainSamples],ModelId,Features,Labels).

%% State predict
predict(cast, {sample,CSVname, BatchID, SampleListPredict}, State = #nerlNetStatem_state{features = Features, modelId = ModelId}) ->
  ChunkSizePred = round(length(SampleListPredict)/Features),
  CurrPID = self(),
  %io:fwrite("length(SampleListPredict)/Features): ~p\n",[length(SampleListPredict)/Features]),
  io:fwrite("Send sample to predict\n"),
  _Pid = spawn(fun()-> erlModule:predict2double(SampleListPredict,ChunkSizePred,Features,ModelId,CurrPID, CSVname, BatchID) end),
  {next_state, wait, State#nerlNetStatem_state{nextState = predict}};

predict(cast, {idle}, State) ->
  io:fwrite("Go from predict to idle\n"),
  {next_state, idle, State};

predict(cast, {training}, State) ->
  io:fwrite("Go from predict to train\n"),
  {next_state, train, State};

predict(cast, Param, State) ->
  io:fwrite("Same state Predict, command: ~p\n",[Param]),
  {next_state, predict, State}.