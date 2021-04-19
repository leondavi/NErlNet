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
-export([start_link/0]).

%% gen_statem callbacks
-export([init/1, format_status/2, state_name/3, handle_event/4, terminate/3,
  code_change/4, callback_mode/0]).
%% States functions
-export([idle/3, train/3, predict/3, create/3, init/3]).
%% Client functions
-export([train/5, predict/4, create/2,init/5]).

-define(SERVER, ?MODULE).

-record(nerlNetStatem_state, {dig1=1,dig2=2,dig3=4,dig4=5}).

%%%===================================================================
%%% API
%%%===================================================================

%% @doc Creates a gen_statem process which calls Module:init/1 to
%% initialize. To ensure a synchronized start-up procedure, this
%% function does not return until Module:init/1 has returned.
start_link() ->
  {ok,Pid} = gen_statem:start_link({local, ?SERVER}, ?MODULE, [], []),
  Pid.

%%%===================================================================
%%% gen_statem callbacks
%%%===================================================================

%% @private
%% @doc Whenever a gen_statem is started using gen_statem:start/[3,4] or
%% gen_statem:start_link/[3,4], this function is called by the new
%% process to initialize.
init([]) ->
  {ok, init, []}.
  %{ok, idle, #nerlNetStatem_state{}}.

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

%% Client functions
train(ChunkSize, Cols, Labels, SampleList, ModelId) -> gen_statem:cast(?MODULE,{train,{ChunkSize, Cols, Labels, SampleList, ModelId}}).

predict(SampleList, ChunkSize, Cols, ModelId) -> gen_statem:cast(?MODULE,{predict,{SampleList, ChunkSize, Cols, ModelId}}).

create(Learning_rate,LayerSizes) -> gen_statem:cast(?MODULE,{create,{Learning_rate,LayerSizes}}).

init(Layers_sizes, Learning_rate, ActivationList, Optimizer, ModelId) -> gen_statem:cast(?MODULE,{create,{Layers_sizes, Learning_rate, ActivationList, Optimizer, ModelId}}).

%------------
%% Define states

%% State init
init(cast, Command, State) ->
  io:fwrite("Start module_create in nerlNetStatem ~n"),
  {_Mod,Param} = Command,
  if
    Command == {create,Param} ->
      {_Mod,{Layers_sizes, Learning_rate, ActivationList, Optimizer, ModelId}} = Command,
      io:fwrite("start module_create ~n"),
      Mid=erlModule:module_create(Layers_sizes, Learning_rate, ActivationList, Optimizer, ModelId),
      io:fwrite("Mid: ~p\n",[Mid]),
      {next_state, idle, State};
    true -> {next_state, init, State}
  end.

%% State idle
idle(cast, Train_predict, State) ->
  {_Mod,Param} = Train_predict,
  if
    Train_predict == {predict,Param} ->
      {_Mod,{Data_mat, Rows, Cols, ModelId}} = Train_predict,
      io:fwrite("start predict2double ~n"),
      Curr_PID = self(),
      erlModule:predict2double(Data_mat,Rows,Cols,ModelId,Curr_PID),
      receive
        Result->
          io:fwrite("PID: ~p Result: ~p\n",[Curr_PID, Result])
      end,

      {next_state, idle, State};

    Train_predict == {train,Param} ->
      {_Mod,{ChunkSize, Cols, Labels, SampleListTrain, ModelId}} = Train_predict,
      Curr_PID = self(),
      io:fwrite("start train2double ~n"),
      io:fwrite("TrainList: ~p\n",[SampleListTrain]),
      io:fwrite("ChunkSize: ~p Cols: ~p, Labels: ~p, ModelId: ~p, pid: ~p \n",[ChunkSize,Cols,Labels, ModelId,self()]),

      _LossVal=erlModule:train2double(ChunkSize, Cols, Labels, SampleListTrain, ModelId,Curr_PID), % Send to train
      receive
        LOSS_FUNC->
          io:fwrite("PID: ~p Loss func: ~p\n",[Curr_PID, LOSS_FUNC])
      end,
      {next_state, idle, State};

    true -> {next_state, idle, State}
  end.


%% State train TODO
train(cast, _Idle_predict, State) ->
  {next_state, idle, State}.

%% State predict TODO
predict(cast, _Idle_train, State) ->
{next_state, idle, State}.

%--------------------temp

%% State create
create(cast, Command, State) ->
  {_Mod,Param} = Command,
  if
    Command == {create,Param} ->
      {_Mod,{Learning_rate,LayerSizes}} = Command,
      io:fwrite("start module_create ~n"),
      Mid=erlModule:module_create(LayerSizes, Learning_rate, 80, [2,1,1,2], 1),
      %Result = erlModule:module_create([8,4,3,2], Learning_rate_List, 80, [2,1,1,2], 1),
      io:fwrite("Mid: ~p\n",[Mid]),
      {next_state, idle, State};
    true -> {next_state, create, State}
  end.