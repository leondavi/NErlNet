%%%-------------------------------------------------------------------
%%% @author kapelnik
%%% @copyright (C) 2021, Nerlnet
%%% @doc
%%%
%%% @end
%%% Created : 02. Jan 2021 4:05 AM
%%%-------------------------------------------------------------------
-module(routingHandler).
-author("kapelnik").
-export([init/2]).
-include("../nerl_tools.hrl").

%%handler for routing all messages in the network.
%%Action contains the information about the action performed, and Body contains the information needed for the action

%%State = [Action, RouterPID, Graph, RouterName]
init(Req0, State) ->
  Action = lists:nth(1, State),
  Router_genserver_Pid = lists:nth(2, State),

  %Bindings also can be accesed as once, giving a map of all bindings of Req0:
  {_,Body,_} = cowboy_req:read_body(Req0, #{length => ?DATA_LEN}),
  %Decoded_body = binary_to_list(Body),
%  io:format("router got action ~p body:~p~n",[Action,Body]),
  case Action of
%%     router was meant to rout no?
    custom_worker_message->
      {To, PassingAction, Data} = binary_to_term(Body),
      Graph = lists:nth(3, State),
      MyName = lists:nth(4, State),
      {Host,Port} = nerl_tools:getShortPath(MyName,To,Graph),
      nerl_tools:http_request(Host,Port, "custom_worker_message", Body);
    pass ->
      {To, PassingAction, Data} = binary_to_term(Body),
      Graph = lists:nth(3, State),
      MyName = lists:nth(4, State),
      {Host,Port} = nerl_tools:getShortPath(MyName,To,Graph),
      nerl_tools:http_request(Host,Port, "custom_worker_message", Body);

    rout ->
      gen_server:cast(Router_genserver_Pid, {rout,Body});

    %%sends an cast for genserver to make an http request for updating CSV lists at all sensors found in Body.
    updateCSV ->
      [Source|_] = re:split(binary_to_list(Body), "#", [{return, list}]),
      gen_server:cast(Router_genserver_Pid, {updateCSV,Source,Body});

    %%sends an cast for genserver to make an http request for updating CSV lists at all sensors found in Body.
    csvReady ->   gen_server:cast(Router_genserver_Pid, {csvReady, Body});

    sourceDone ->  gen_server:cast(Router_genserver_Pid, {sourceDone, Body});

    %%sends an cast for genserver to make an http request for updating CSV lists at all sensors found in Body.clientIdle
    clientReady ->  gen_server:cast(Router_genserver_Pid, {clientReady, Body});

    clientTraining ->gen_server:cast(Router_genserver_Pid, {clientTraining, Body});
    lossFunction ->gen_server:cast(Router_genserver_Pid, {lossFunction, Body});
    predictRes ->gen_server:cast(Router_genserver_Pid, {predictRes, Body});
    clientPredict ->gen_server:cast(Router_genserver_Pid, {clientPredict, Body});
    
    clientIdle ->gen_server:cast(Router_genserver_Pid, {clientIdle, Body});

    %%sends an cast for genserver to make an http request for start feeding data.
    startCasting ->  gen_server:cast(Router_genserver_Pid, {startCasting,Body});

      %%sends an cast for genserver to make an http request for updating CSV lists at all sensors found in Body.
    stopCasting ->  gen_server:cast(Router_genserver_Pid, {stopCasting,Body});
    statistics  -> gen_server:cast(Router_genserver_Pid, {statistics,Body});

    % federatedWeightsVector  -> gen_server:cast(Router_genserver_Pid, {federatedWeightsVector,Body});

    % federatedWeights -> % io:format("router got action ~p body:~p~n",[Action,Body]),
    %   gen_server:cast(Router_genserver_Pid, {federatedWeights,Body});

    %%%%%%%%%%%%%%GUI actions
    getStats -> gen_server:cast(Router_genserver_Pid, {getStats,Body})

  end,
  Reply = io_lib:format(" ", []),
%%  Reply = io_lib:format("Body Received: ~p, Decoded Body = ~p ~n Client_StateM_Pid:~p, Handler's Pid: ~p~n ", [Body,Decoded_body,  Router_genserver_Pid,self()]),
  Req = cowboy_req:reply(200,
    #{<<"content-type">> => <<"text/plain">>},
    Reply,
    Req0),
  {ok, Req, State}.