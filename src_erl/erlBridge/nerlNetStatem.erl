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
-export([idle/3, train/3, predict/3]).

-define(SERVER, ?MODULE).

-record(nerlNetStatem_state, {clientPid, features, labels}).

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
init({Layers_sizes, Learning_rate, ActivationList, Optimizer, ModelId, ClientPid, Features, Labels}) ->
  io:fwrite("start module_create ~n"),
  _Res=erlModule:module_create(Layers_sizes, Learning_rate, ActivationList, Optimizer, ModelId),
  %io:fwrite("Mid: ~p\n",[Mid]),
  %{ok, idle, []}.
  {ok, idle, #nerlNetStatem_state{clientPid = ClientPid, features = Features, labels = Labels}}.

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
idle(cast, start_train, State = #nerlNetStatem_state{}) ->
  io:fwrite("Go from idle to train\n"),
  {next_state, train, State};

idle(cast, start_predict, State) ->
  io:fwrite("Go from idle to predict\n"),
  {next_state, predict, State};

idle(cast, Param, State) ->
  io:fwrite("Same state idle, command: ~p\n",[Param]),
  {next_state, idle, State}.

%% State train
train(cast, {ChunkSize, SampleListTrain,ModelId}, State = #nerlNetStatem_state{clientPid = ClientPid,
  features = Features, labels = Labels}) ->
  erlModule:train2double(ChunkSize, Features, Labels, SampleListTrain,ModelId,self()),
  receive
    LOSS_FUNC->
  	  io:fwrite("Loss func: ~p\n",[LOSS_FUNC]),
      gen_statem:cast(ClientPid,LOSS_FUNC)%%  TODO check the send back the loss function
  end,
  {next_state, train, State};

train(cast, stop, State) ->
  io:fwrite("Go from train to idle\n"),
  {next_state, idle, State};

train(cast, start_predict, State) ->
  io:fwrite("Go from train to predict\n"),
  {next_state, predict, State};

train(cast, Param, State) ->
  io:fwrite("Same state train, command: ~p\n",[Param]),
  {next_state, train, State}.


%% State predict
predict(cast, {SampleListPredict, ChunkSize, ModelId}, State = #nerlNetStatem_state{clientPid = ClientPid, features = Features}) ->
  erlModule:predict2double(SampleListPredict,ChunkSize,Features,ModelId,self()),
  receive
    RESULTS->
      io:fwrite("Predict results: ~p\n",[RESULTS]),
      gen_statem:cast(ClientPid,RESULTS)%%  TODO check the send back the results
  end,

  {next_state, predict, State};

predict(cast, stop, State) ->
  io:fwrite("Go from predict to idle\n"),
  {next_state, idle, State};

predict(cast, start_train, State) ->
  io:fwrite("Go from predict to train\n"),
  {next_state, train, State};

predict(cast, Param, State) ->
  io:fwrite("Same state Predict, command: ~p\n",[Param]),
  {next_state, predict, State}.