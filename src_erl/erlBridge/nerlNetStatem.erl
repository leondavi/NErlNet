%%%-------------------------------------------------------------------
%%% @author ziv
%%% @copyright (C) 2020, Nerlnet
%%% @doc
%%%
%%% @end
%%% Created : 07. Oct 2020 21:58
%%%-------------------------------------------------------------------
-module(nerlNetStatem).

-import(nerlNIF,[decode_nif/2, nerltensor_binary_decode/2]).
-import(nerlNIF,[encode_nif/2, nerltensor_encode/5, nerltensor_conversion/2, get_all_binary_types/0]).
-import(nerlNIF,[erl_type_conversion/1]).

-behaviour(gen_statem).

%% API
-export([start_link/1]).
-include("../Communication_Layer/http_Nerlserver/src/nerl_tools.hrl").
%% gen_statem callbacks
-export([init/1, format_status/2, state_name/3, handle_event/4, terminate/3,
  code_change/4, callback_mode/0]).
%% States functions
-export([idle/3, train/3, predict/3, wait/3]).


%% federatedMode = 0 - Not federated, 1 - Federated get and send weights, 2 - Federated set weights
%% countLimit - Number of samples to count before sending the weights for averaging. Predifined in the json file.
%% count - Number of samples recieved for training after the last weights sended.
-record(nerlNetStatem_state, {clientPid, features, labels, myName, modelId, nextState, currentBatchID=0,ackClient=0,  missedSamplesCount = 0, missedTrainSamples= [], federatedMode, count = 1, countLimit,optimizer, lossMethod, learningRate}).

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
  ?LOG_NOTICE("Creating: WorkerName: ~p ,ModelId: ~p , ModelType: ~p , ScalingMethod: ~p ,LayerTypesList: ~p ,LayersSizes: ~p ,LayersActivationFunctions: ~p ,FederatedMode: ~p ,CountLimit: ~p ,Optimizer: ~p , Features: ~p , Labels: ~p , ClientPID: ~p~n"
    ,[WorkerName,ModelId, ModelType, ScalingMethod,LayerTypesList,LayersSizes,LayersActivationFunctions,FederatedMode,CountLimit,Optimizer, Features, Labels, ClientPID]),
%%^^^^^^^^^^^^^^^^^^^^^^^^^^
        Res=nerlNIF:create_nif(ModelId, ModelType , ScalingMethod , LayerTypesList , LayersSizes , LayersActivationFunctions),
        ?LOG_NOTICE("Res = ~p ~n",[Res]),

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
idle(cast, {training}, State = #nerlNetStatem_state{myName = MyName,clientPid = ClientPid}) ->
  ?LOG_NOTICE("Go from idle to train!\n"),
  gen_statem:cast(ClientPid,{stateChange,MyName}),
  {next_state, train, State};

idle(cast, {predict}, State = #nerlNetStatem_state{myName = MyName,clientPid = ClientPid}) ->
  ?LOG_NOTICE("Go from idle to predict\n"),
  gen_statem:cast(ClientPid,{stateChange,MyName}),
  {next_state, predict, State#nerlNetStatem_state{nextState = predict}};

idle(cast, {set_weights,_Ret_weights_list}, State = #nerlNetStatem_state{modelId=_ModelId}) ->

  ?LOG_NOTICE("Set weights in wait state: \n"),
  
  % %% Set weights TODO maybe send the results of the update
  % [WeightsList, BiasList, Biases_sizes_list, Wheights_sizes_list] = Ret_weights_list,

  % %% Make bias sizes and weights sizes as integer 
  % NewBiases_sizes_list = [round(X)||X<-Biases_sizes_list],
  % NewWheights_sizes_list = [round(X)||X<-Wheights_sizes_list],
  % _Result_set_weights = niftest:set_weights_nif(WeightsList, BiasList, NewBiases_sizes_list, NewWheights_sizes_list, ModelId),
  % _Result_set_weights2 = niftest:set_weights_nif(WeightsList, BiasList, Biases_sizes_list, Wheights_sizes_list, ModelId),
 %io:format("####sending new weights to workers####~n"),
  %niftest:call_to_set_weights(ModelId, Ret_weights_list), niftest is depracated - use nerlNIF instead
  ?LOG_NOTICE("####end set weights idle####~n"),

  {next_state, idle, State};

idle(cast, _Param, State) ->
  % io:fwrite("Same state idle, command: ~p\n",[Param]),
  {next_state, idle, State}.

%% Regular mode (Not federated)
% wait(cast, {loss, LossFunc}, State = #nerlNetStatem_state{clientPid = ClientPid, myName = MyName, nextState = NextState,ackClient = AckClient}) ->
%   % Federated mode = 0 (not federated)
%   %io:fwrite("loss LossFunc at worker: ~p~n",[{loss, LossFunc}]),
%   gen_statem:cast(ClientPid,{loss, MyName, LossFunc}), %% TODO Add Time and Time_NIF to the cast
%   checkAndAck(MyName,ClientPid,AckClient),
%   {next_state, NextState, State#nerlNetStatem_state{ackClient = 0}};


%% Waiting for receiving results or loss function
%% Got nan or inf from loss function - Error, loss function too big for double
wait(cast, {loss,nan,Time_NIF}, State = #nerlNetStatem_state{clientPid = ClientPid, myName = MyName, nextState = NextState,ackClient = AckClient}) ->
  ?LOG_NOTICE("Loss func in wait: nan (Loss function too big for double)\n"),
  gen_statem:cast(ClientPid,{loss, MyName, nan,Time_NIF}), %% TODO send to tal stop casting request with error desc
  checkAndAck(MyName,ClientPid,AckClient),

  {next_state, NextState, State#nerlNetStatem_state{ackClient = 0}};


%% Regular mode (Not federated)
wait(cast, {loss, {LossVal,Time}}, State = #nerlNetStatem_state{clientPid = ClientPid, myName = MyName, nextState = NextState, federatedMode = ?MODE_REGULAR,ackClient = AckClient}) ->

% io:fwrite("loss, {LossVal,Time}: ~p~n",[{loss, {LossVal,Time}}]),

  gen_statem:cast(ClientPid,{loss, MyName, LossVal,Time/1000}), %% TODO Add Time and Time_NIF to the cast
  checkAndAck(MyName,ClientPid,AckClient),

  {next_state, NextState, State#nerlNetStatem_state{ackClient = 0}};

%% Regular mode (Not federated)
% wait(cast, {loss, LossAndTime,Time_NIF}, State = #nerlNetStatem_state{clientPid = ClientPid,ackClient = AckClient, myName = MyName, nextState = NextState, federatedMode = ?MODE_REGULAR}) ->
%   {LOSS_FUNC,_TimeCpp} = LossAndTime,
%   % Federated mode = 0 (not federated)
%   gen_statem:cast(ClientPid,{loss, MyName, LOSS_FUNC,Time_NIF/1000}), %% TODO Add Time and Time_NIF to the cast
%   checkAndAck(MyName,ClientPid,AckClient),

%   {next_state, NextState, State#nerlNetStatem_state{ackClient = 0}};

%% Federated mode
wait(cast, {loss, {LOSS_FUNC,Time_NIF}}, State = #nerlNetStatem_state{clientPid = ClientPid,ackClient = AckClient, myName = MyName, nextState = NextState, count = Count, countLimit = CountLimit, modelId = Mid, federatedMode = ?MODE_FEDERATED}) ->
  % {LOSS_FUNC,_TimeCpp} = LossAndTime,
  if Count == CountLimit ->
      % Get weights
      Ret_weights = nerlNIF:call_to_get_weights(Mid),
      % Ret_weights_tuple = niftest:call_to_get_weights(Mid),
      % {Wheights,Bias,Biases_sizes_list,Wheights_sizes_list} = Ret_weights_tuple,

      % ListToSend = [Wheights,Bias,Biases_sizes_list,Wheights_sizes_list],

      % Send weights and loss value
      gen_statem:cast(ClientPid,{loss, federated_weights, MyName, LOSS_FUNC, Ret_weights}), %% TODO Add Time and Time_NIF to the cast
      checkAndAck(MyName,ClientPid,AckClient),
      % Reset count and go to state train
      {next_state, NextState, State#nerlNetStatem_state{ackClient = 0, count = 0}};

    true ->
      %% Send back the loss value
      gen_statem:cast(ClientPid,{loss, MyName, LOSS_FUNC,Time_NIF/1000}), %% TODO Add Time and Time_NIF to the cast
      checkAndAck(MyName,ClientPid,AckClient),
      

      {next_state, NextState, State#nerlNetStatem_state{ackClient = 0, count = Count + 1}}
  end;


% Not supposed to be here - for future additions
% wait(cast, {loss, LossAndTime,_Time_NIF}, State = #nerlNetStatem_state{nextState = NextState,myName = MyName,clientPid = ClientPid,ackClient = AckClient}) ->
%   io:fwrite("Error, Not supposed to be here. nerlNetStatem. Got LossAndTime: ~p~n",[LossAndTime]),
%   checkAndAck(MyName,ClientPid,AckClient),

%   {next_state, NextState, State#nerlNetStatem_state{ackClient = 0}};

% Not supposed to be here - for future additions
wait(cast, {loss, LossAndTime}, State = #nerlNetStatem_state{ federatedMode = _FederatedMode, nextState = NextState,myName = MyName,clientPid = ClientPid,ackClient = AckClient}) ->
  ?LOG_NOTICE("Error at worker, loss fun-: ~p~n",[LossAndTime]),
  % io:fwrite("FederatedMode-: ~p~n",[FederatedMode]),
  
  checkAndAck(MyName,ClientPid,AckClient),

  {next_state, NextState, State#nerlNetStatem_state{ackClient = 0}};

wait(cast, {set_weights,_Ret_weights_list}, State = #nerlNetStatem_state{nextState = _NextState, modelId=_ModelId,clientPid = _ClientPid,ackClient = _AckClient}) ->
  ?LOG_NOTICE("Set weights in wait state"),

  % %% Set weights
  % [WeightsList, BiasList, Biases_sizes_list, Wheights_sizes_list] = Ret_weights_list,
  % %% Make bias sizes and weights sizes as integer 
  % NewBiases_sizes_list = [round(X)||X<-Biases_sizes_list],
  % NewWeights_sizes_list = [round(X)||X<-Wheights_sizes_list],
  % _Result_set_weights = niftest:set_weights_nif(WeightsList, BiasList, NewBiases_sizes_list, NewWeights_sizes_list, ModelId),
   %io:format("####sending new weights to workers####~n"),
  %niftest:call_to_set_weights(ModelId, Ret_weights_list),
  io:format("####end set weights wait####~n"),

  % checkAndAck(MyName,ClientPid,AckClient),

  {next_state, wait, State#nerlNetStatem_state{}};

wait(cast, {predictRes,Res,CSVname,BatchID}, State = #nerlNetStatem_state{myName = MyName, clientPid = ClientPid, nextState = NextState,ackClient = AckClient}) ->
   % io:fwrite("~nworker got predict result ~p~n",[{predictRes,Res,CSVname,BatchID}]),

  gen_statem:cast(ClientPid,{predictRes,MyName, CSVname,BatchID, Res}), %% TODO TODO change csv name and batch id(1)
  checkAndAck(MyName,ClientPid,AckClient),

  {next_state, NextState, State#nerlNetStatem_state{ackClient = 0}};

wait(cast, {predictRes,CSVname, BatchID, {RESULTS,_TimeCpp},_Time_NIF}, State = #nerlNetStatem_state{myName = MyName,ackClient = AckClient, clientPid = ClientPid, nextState = NextState}) ->
  gen_statem:cast(ClientPid,{predictRes,MyName, CSVname, BatchID, RESULTS}), %% TODO Add Time and Time_NIF to the cast
  checkAndAck(MyName,ClientPid,AckClient),

  {next_state, NextState, State#nerlNetStatem_state{ackClient = 0}};

wait(cast, {idle}, State) ->
  %logger:notice(?FILE_IDENTIFIER++"Waiting, next state - idle"),
  {next_state, wait, State#nerlNetStatem_state{nextState = idle,ackClient=1}};

wait(cast, {training}, State) ->
  %logger:notice(?FILE_IDENTIFIER++"Waiting, next state - train"),
  % gen_statem:cast(ClientPid,{stateChange,WorkerName}),
  {next_state, wait, State#nerlNetStatem_state{nextState = train,ackClient=1}};

wait(cast, {predict}, State) ->
  %logger:notice(?FILE_IDENTIFIER++"Waiting, next state - predict"),
  {next_state, wait, State#nerlNetStatem_state{nextState = predict,ackClient=1}};

wait(cast, {sample, _SampleListTrain}, State = #nerlNetStatem_state{missedSamplesCount = MissedSamplesCount, missedTrainSamples = _MissedTrainSamples}) ->
  ?LOG_NOTICE(?LOG_HEADER++"Missed in pid: ~p, Missed batches count: ~p\n",[self(), MissedSamplesCount]),
  % Miss = MissedTrainSamples++SampleListTrain,
  {next_state, wait, State#nerlNetStatem_state{missedSamplesCount = MissedSamplesCount+1}};

wait(cast, {sample,_CSVname, _BatchID, _SampleListPredict}, State = #nerlNetStatem_state{missedSamplesCount = MissedSamplesCount, missedTrainSamples = _MissedTrainSamples}) ->
  ?LOG_NOTICE(?LOG_HEADER++"Missed in pid: ~p, Missed batches count: ~p\n",[self(), MissedSamplesCount]),
  % Miss = MissedTrainSamples++SampleListTrain,
  {next_state, wait, State#nerlNetStatem_state{missedSamplesCount = MissedSamplesCount+1}};

wait(cast, _Param, State) ->
  %logger:notice(?FILE_IDENTIFIER++"worker Not supposed to be. Got: ~p\n",[Param]),
  {next_state, wait, State}.


%% State train
train(cast, {sample, []}, State ) ->
  {next_state, train, State#nerlNetStatem_state{nextState = train}};
  
%% Change SampleListTrain to NerlTensor
train(cast, {sample, {SampleListTrain, Type}}, State = #nerlNetStatem_state{modelId = ModelId, optimizer = Optimizer, lossMethod = LossMethod, learningRate = LearningRate}) ->
    ErlTensor = nerlNIF:nerltensor_conversion({SampleListTrain, Type}, erl_float),  %% for debug
    io:format("Got ErlTensor: ~p~n",[ErlTensor]),
    MyPid = self(),
    _Pid = spawn(fun()-> nerlNIF:call_to_train(ModelId, Optimizer , LossMethod , LearningRate , SampleListTrain ,MyPid) end),
    {next_state, wait, State#nerlNetStatem_state{nextState = train}};
  

train(cast, {set_weights,Ret_weights_list}, State = #nerlNetStatem_state{modelId = ModelId}) ->
  %% Set weights
  %io:format("####sending new weights to workers####~n"),
  nerlNIF:call_to_set_weights(ModelId, Ret_weights_list),
  %logger:notice(?FILE_IDENTIFIER++"####end set weights train####~n"),
  {next_state, train, State};


train(cast, {idle}, State = #nerlNetStatem_state{myName = MyName, clientPid = ClientPid}) ->
  %logger:notice(?FILE_IDENTIFIER++"Go from train to idle\n"),
  gen_statem:cast(ClientPid,{stateChange,MyName}),
  {next_state, idle, State};

train(cast, {predict}, State = #nerlNetStatem_state{myName = MyName, clientPid = ClientPid}) ->
  %logger:notice(?FILE_IDENTIFIER++"Go from train to predict\n"),
  gen_statem:cast(ClientPid,{stateChange,MyName}),

  {next_state, predict, State};

train(cast, _Param, State) ->
  % io:fwrite("Same state train, not supposed to be, command: ~p\n",[Param]),
  {next_state, train, State}.

%% State predict
predict(cast, {sample,_CSVname, _BatchID, []}, State = #nerlNetStatem_state{}) ->
  
  {next_state, predict, State#nerlNetStatem_state{nextState = predict}};

% send predict sample to worker
predict(cast, {sample,CSVname, BatchID, SampleListPredict}, State = #nerlNetStatem_state{ modelId = ModelId}) ->
    CurrPID = self(),
    _Pid = spawn(fun()-> nerlNIF:call_to_predict(ModelId,SampleListPredict,CurrPID,CSVname, BatchID) end),
    {next_state, wait, State#nerlNetStatem_state{nextState = predict}};
  
predict(cast, {idle}, State = #nerlNetStatem_state{myName = MyName, clientPid = ClientPid}) ->
  %logger:notice(?FILE_IDENTIFIER++"Go from predict to idle\n"),
  gen_statem:cast(ClientPid,{stateChange,MyName}),

  {next_state, idle, State};

predict(cast, {training}, State = #nerlNetStatem_state{myName = MyName, clientPid = ClientPid}) ->
  %logger:notice(?FILE_IDENTIFIER++"Go from predict to train\n"),
  gen_statem:cast(ClientPid,{stateChange,MyName}),

  {next_state, train, State};

predict(cast, _Param, State) ->
  %logger:notice(?FILE_IDENTIFIER++"Same state Predict, command: ~p\n",[Param]),
  {next_state, predict, State}.

  checkAndAck(_MyName,_ClientPid,0) ->ok_no_need;
  checkAndAck(MyName, ClientPid, 1) ->  gen_statem:cast(ClientPid,{stateChange,MyName}).