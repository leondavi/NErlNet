%%%-------------------------------------------------------------------
%%% @author ziv
%%% @copyright (C) 2021, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 19. Apr 2021 06:35
%%%-------------------------------------------------------------------
-module(handler).
-author("ziv").

-behaviour(gen_statem).

%% API
-export([start_link/0]).

%% gen_statem callbacks
-export([init/1, format_status/2, state_name/3, handle_event/4, terminate/3,
  code_change/4, callback_mode/0]).
-export([receives/3, createInst/3, sends/3]).

-define(SERVER, ?MODULE).

-record(handler_state, {}).

%%%===================================================================
%%% API
%%%===================================================================

%% @doc Creates a gen_statem process which calls Module:init/1 to
%% initialize. To ensure a synchronized start-up procedure, this
%% function does not return until Module:init/1 has returned.
start_link() ->
  %gen_statem:start_link({local, ?SERVER}, ?MODULE, [], []).
  gen_statem:start_link(?MODULE, [], []).

%%%===================================================================
%%% gen_statem callbacks
%%%===================================================================

%% @private
%% @doc Whenever a gen_statem is started using gen_statem:start/[3,4] or
%% gen_statem:start_link/[3,4], this function is called by the new
%% process to initialize.
init([]) ->
  {ok, receives, #handler_state{}}.

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
state_name(_EventType, _EventContent, State = #handler_state{}) ->
  NextStateName = next_state,
  {next_state, NextStateName, State}.

%% Receives arguments for new instance creating
receives(cast, EventContent, _State = #handler_state{}) ->

  io:fwrite("EventContent: ~p\n",[EventContent]),
  {create,{Layers_sizes_list, Learning_rate, ActivationList, Optimizer, ModelId,CallerPid}} = EventContent,

  io:fwrite("nerlNetStatem start link:\n"),
  %% Create instance
  NerlNetStatemPid=nerlNetStatem:start_link(),
  io:fwrite("NerlNetStatemPid: ~p\n",[NerlNetStatemPid]),
  %% Create the module
  gen_statem:cast(NerlNetStatemPid, EventContent),
  CallerPid!NerlNetStatemPid,

{next_state, receives, _State}.

%% Create the instance with the received arguments
createInst(cast, EventContent, _State) ->

  %{create,{Layers_sizes_list, Learning_rate, ActivationList, Optimizer, ModelId}} = InstanceInfo,

  % TODO Create instance
  NerlNetStatemPid=nerlNetStatem:start_link(),
  %% Create the module
  gen_statem:cast(NerlNetStatemPid, EventContent),

  {next_state, sends, {finishCreatingInst,NerlNetStatemPid}}.

%% Send back the Pid of the created instance TODO also add mid?
sends(cast, _EventContent, State) ->

  %TODO send back: {finishCreatingInst,Pid}
  {next_state, receives, State}.


%% @private
%% @doc If callback_mode is handle_event_function, then whenever a
%% gen_statem receives an event from call/2, cast/2, or as a normal
%% process message, this function is called.
handle_event(_EventType, _EventContent, _StateName, State = #handler_state{}) ->
  NextStateName = the_next_state_name,
  {next_state, NextStateName, State}.

%% @private
%% @doc This function is called by a gen_statem when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_statem terminates with
%% Reason. The return value is ignored.
terminate(_Reason, _StateName, _State = #handler_state{}) ->
  ok.

%% @private
%% @doc Convert process state when code is changed
code_change(_OldVsn, StateName, State = #handler_state{}, _Extra) ->
  {ok, StateName, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
