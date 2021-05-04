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
-export([parse_file/1, parse_line/1]).


%%parsing a given CSV file
parse_file(File_Address) ->

  {ok, Data} = file:read_file(File_Address),%%TODO change to File_Address
  DataListtemp = binary_to_list(Data),
  DataList = lists:sublist(DataListtemp,1,length(DataListtemp)),
  Linestemp = [re:replace(X, "\r\n", "", [global,{return,list}])||X<-re:split(DataListtemp, "\r\n", [{return, list}])],
  Lines = lists:sublist(Linestemp,1,length(Linestemp)-1).
%%  io:format("~p~n",[Lines]),
%%  [parse_line(Line)||Line<-Lines].

parse_line(Line)->

  [begin
     case string:to_float(X) of
       {error,no_float} -> list_to_integer(X);
       {F,_Rest} -> F
     end
%%    {Int,_}=string:to_integer(X), Int

    end|| X<-string:tokens(Line,",")].
