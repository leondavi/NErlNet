%%
%% %CopyrightBegin%
%% 
%% Copyright Omayer Gharra 2020. All Rights Reserved.
%% 
%% %CopyrightEnd%

%% This application allow us to read data set from a csv file, send it “for learning mode” to the neural network, predict and show the results with statistics.

-module(csv_parser).

-export([parse_file/1]).

parse_file(Fn) ->
{ok, Data} = file:read_file(Fn),
parse(binary_to_list(Data)).

parse(Data) -> lists:reverse(parse(Data, [])).

parse([], Acc) -> Acc;
parse(Data, Acc) ->
{Line, Tail} = parse_line(Data),
parse(Tail, [Line|Acc]).

parse_line(Data) ->
{Line, Tail} = parse_line(Data, []),
{lists:reverse(Line), Tail}.

parse_line([13,10|Data], Acc) -> {Acc, Data};
parse_line([10|Data], Acc) -> {Acc, Data};
parse_line([13|Data], Acc) -> {Acc, Data};
parse_line([], Acc) -> {Acc, []};
parse_line([$,,$,|Data], Acc) -> parse_line(Data, [""|Acc]);
parse_line([$,|Data], Acc) -> parse_line(Data, Acc);
parse_line(Data, Acc) ->
{Fld, Tail} = parse_field(Data),
parse_line(Tail, [Fld|Acc]).

parse_field([34|Data]) ->
{Fld, Tail} = parse_fieldq(Data, ""),
{lists:reverse(Fld), Tail};
parse_field(Data) ->
{Fld, Tail} = parse_field(Data, ""),
{lists:reverse(Fld), Tail}.

parse_field([$,|Tail], Acc) -> {Acc, [$,|Tail]};
parse_field([13|Tail], Acc) -> {Acc, [13|Tail]};
parse_field([10|Tail], Acc) -> {Acc, [10|Tail]};
parse_field([], Acc) -> {Acc, []};
parse_field([Ch|Tail], Acc) -> parse_field(Tail, [Ch|Acc]).

parse_fieldq([34,34|Tail], Acc) -> parse_fieldq(Tail, [34|Acc]);
parse_fieldq([34|Tail], Acc) -> {Acc, Tail};
parse_fieldq([Ch|Tail], Acc) -> parse_fieldq(Tail, [Ch|Acc]).