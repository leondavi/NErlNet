%%%-------------------------------------------------------------------
%%% @author kapelnik
%%% @copyright (C) 2021, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 19. Apr 2021 4:27 AM
%%%-------------------------------------------------------------------
-module(parser).
-author("kapelnik").

%% API
-export([parse_file/2]).


%%parsing a given CSV file
parse_file(File_Address,ChunkSize) ->

  {ok, Data} = file:read_file(File_Address),%%TODO change to File_Address
  CSVlist = parse(Data),
%%  io:format("~p~n",[CSVlist]),
  Chunked= makeChunks(CSVlist,ChunkSize,ChunkSize,[],[]),
%%  io:format("~p~n~n~n~n~n~n~n~n~n~n~n~n~n~n~n~n~n~n~n~n~n~n",[Chunked]),
%%
%%    A = divide(CSVlist,ChunkSize),
  [append2(X)||X<-Chunked].


parse(Data) ->
  Lines = re:split(Data, "\r|\n|\r\n", [] ),
  [ [begin
       case  re:split(Token, "\"", [] ) of
         [_,T,_] -> T;
         [T] ->binary_to_list(T);  %%if token is not surrounded by ""
%%                                  io:format("~p~n",[A]),A;
         [] -> <<"">>
       end
     end || Token <- re:split(Line, ",", [] ) ] || Line <- Lines, Line =/= <<"">>].


makeChunks([],_Left,_ChunkSize,Acc,Ret) ->
  Ret++[Acc];

makeChunks([Head|Tail],1,ChunkSize,Acc,Ret) ->
  makeChunks(Tail,ChunkSize,ChunkSize,[],Ret++[Acc++[Head]]);

makeChunks([Head|Tail],Left,ChunkSize,Acc,Ret) ->
  makeChunks(Tail,Left-1,ChunkSize,Acc++[Head],Ret).

append2(List) -> append2(List,[]).
append2([], Acc) ->
  string:join([[X] || X <- Acc], ",");
%%  makeString(Acc);
%%                    io:format("~p~n",[A]),A;
append2([H|T],Acc) -> append2(T, H ++ Acc).

%%makeString([Head|Tail]) -> makeString(Tail,Head).
%%makeString([Head],Ret) ->
%%  A = string:concat(Ret,Head),
%%  io:format("~p~n",[A]),A;
%%makeString([Head|Tail],Acc) ->
%%  Acc2=string:concat(Acc,","),
%%%%  io:format("~p~n",[Acc2]),
%%%%  io:format("~p~n",[binary_to_list(Head)]),
%%
%%  makeString(Tail,string:concat(Acc2,Head)).