%%%-------------------------------------------------------------------
%%% @author ziv
%%% @copyright (C) 2021, <COMPANY>
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(cppSANNFedServStateM).
-author("ziv").

-behaviour(gen_statem).

%% API
-export([start_link/1]).

%% gen_statem callbacks
-export([init/1, format_status/2, state_name/3, handle_event/4, terminate/3,code_change/4, callback_mode/0]).
%% Extra functions
-export([averageFun/2]).
%% States functions
-export([average/3, receives/3]).

-define(SERVER, ?MODULE).

%% federatedMode = 0 - Not federated, 1 - Federated get and send weights, 2 - Federated set weights
%% countLimit - Number of samples to count before sending the weights for averaging. Predifined in the json file.
%% count - Number of samples recieved for training after the last weights sended.
-record(fedServ_state, {counter, counterLimit, buffer, fedServPID, myName}).

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
init({FedServPID, MyName,CounterLimit}) ->

  io:fwrite("start federated server stateM ~n"),

  {ok, receives, #fedServ_state{counter = 0, counterLimit =  CounterLimit, buffer = [], fedServPID = FedServPID , myName = MyName}}.

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
state_name(_EventType, _EventContent, State = #fedServ_state{}) ->
  NextStateName = next_state,
  {next_state, NextStateName, State}.

%% @private
%% @doc If callback_mode is handle_event_function, then whenever a
%% gen_statem receives an event from call/2, cast/2, or as a normal
%% process message, this function is called.
handle_event(_EventType, _EventContent, _StateName, State = #fedServ_state{}) ->
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
code_change(_OldVsn, StateName, State = #fedServ_state{}, _Extra) ->
  {ok, StateName, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================


%% Define states

%% State receives
%% Receives the weights from the client
receives(cast, {weights,WeightsTuple}, State = #fedServ_state{buffer = Buffer, counter = Counter, counterLimit =  CounterLimit}) ->
  io:fwrite("receives state\n"),
  NewCount = Counter + 1,
  if
    NewCount < CounterLimit ->
      %% Add to the buffer, increment the counter and continue to recieve weights
      {next_state, receives, State#fedServ_state{counter = NewCount, buffer = Buffer ++ [WeightsTuple]}};

    NewCount >= CounterLimit ->
      %% Reset the buffer, decrease the counter by CounterLimit, start averaging in a different process and go to average state
      _Pid = spawn(fun()-> averageFun(Buffer ++ [WeightsTuple],self()) end),
      {next_state, average, State#fedServ_state{counter = NewCount - CounterLimit, buffer = []}};
    
    true ->
      io:fwrite("Error: cppSANNEdServStateM not supposed to be here.\n"),
      {next_state, receives, State#fedServ_state{counter = NewCount}}
  end;


%% Not supposed to be here
receives(cast, Else, State) ->
  io:fwrite("Error: State receives in cppSANNFedServStateM.erl. Got: ~p\n",[Else]),
  {next_state, receives, State}.



%% State average

%% Got weights results
average(cast, {weights,WeightsTuple}, State = #fedServ_state{buffer = Buffer, counter = Counter}) ->
  io:fwrite("Got weights at average state.\n"),
  {next_state, average, State#fedServ_state{counter = Counter + 1, buffer = Buffer ++ [WeightsTuple]}};

%% Got average results
average(cast, {average, WeightsTuple}, State= #fedServ_state{fedServPID = FedServPID, myName = MyName}) ->
  io:fwrite("Average state: Got average results.\n"),
  io:fwrite("The average results are: ~p\n",[WeightsTuple]),

  %% Send the results to the clients through the main server
  gen_statem:cast(FedServPID,{averageResult, MyName, WeightsTuple}),
  {next_state, receives, State};


%% Not supposed to be here
average(cast, Else, State) ->
  io:fwrite("Error: State average in cppSANNFedServStateM.erl. Got: ~p\n",[Else]),
  {next_state, average, State}.


% Functions

%% Do the averaging
averageFun(WeightsBuffer,FedServPid) ->
  % TODO: call average in the nif
  %R = erlModule:average_weights(_Matrix, _Biases, _Size, _ModelId),
  [Weights|_T] = WeightsBuffer,
  gen_statem:cast(FedServPid,{average, Weights}).