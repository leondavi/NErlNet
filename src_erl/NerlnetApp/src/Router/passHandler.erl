-module(passHandler).

-export([init/2]).

%%% Ideally, router will only pass the message, with spawn(nerl_tools, http_request, [Host,Port, PassingAction, Body])
%%% But there is no way to know the type of next entity (only named is saved)
%%% possible solutions:
%%%     1. add entity type identifier in graph
%%%     2. standardize communications in all handlers, so each only has one path (/pass) with conssitent format:
%%%         {To, FinalAction, Data}

%%State = [RouterPID, Graph, RouterName]
init(Req0, State) ->
    Router_genserver_Pid = lists:nth(1, State),
  
    %Bindings also can be accesed as once, giving a map of all bindings of Req0:
    {_,Body,_} = cowboy_req:read_body(Req0),
    %Decoded_body = binary_to_list(Body),
  %  io:format("router got action ~p body:~p~n",[Action,Body]),
    {To, PassingAction, Data} = binary_to_term(Body),
    Graph = lists:nth(2, State),
    MyName = lists:nth(3, State),
    {Host,Port} = nerl_tools:getShortPath(MyName,To,Graph),
    spawn(nerl_tools, http_request, [Host,Port, PassingAction, Body]),

    Reply = io_lib:format(" ", []),
    %%  Reply = io_lib:format("Body Received: ~p, Decoded Body = ~p ~n Client_StateM_Pid:~p, Handler's Pid: ~p~n ", [Body,Decoded_body,  Router_genserver_Pid,self()]),
      Req = cowboy_req:reply(200,
        #{<<"content-type">> => <<"text/plain">>},
        Reply,
        Req0),
    {ok, Req, State}.