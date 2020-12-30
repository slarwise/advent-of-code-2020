-module(day_6).

-export([
         first/0,
         second/0
        ]).

first() ->
    % Input = read_input("input-simple"),
    Input = read_input("input"),
    Answers = parse_input(Input),
    UniqueAnswers = lists:map(fun(A) -> lists:usort(A) end, Answers),
    SumsPerGroup = lists:map(fun(A) -> length(A) end, UniqueAnswers),
    lists:sum(SumsPerGroup).

second() ->
    % Input = read_input("input-simple"),
    Input = read_input("input"),
    Answers = parse_input_per_person(Input),
    SumsPerGroup = lists:map(fun(A) -> sets:size(A) end, Answers),
    lists:sum(SumsPerGroup).

read_input(Filename) ->
    {ok, IoDevice} = file:open(Filename, [read]),
    read_lines(IoDevice, []).

read_lines(IoDevice, Acc) ->
    case file:read_line(IoDevice) of
        eof ->
            lists:reverse(Acc);
        {ok, Line} ->
            read_lines(IoDevice, [string:trim(Line)|Acc])
    end.

parse_input(Input) ->
    {CurrentGroup, PreviousGroups} = lists:foldl(
                            fun(Line, {C, P}) ->
                                    parse_line(Line, {C, P})
                            end,
                            {[], []},
                            Input
                           ),
    Answers = [CurrentGroup|PreviousGroups],
    lists:reverse(Answers).

parse_line(Line, {CurrentGroup, PreviousGroups}) ->
    case Line of
        "" ->
            {[], [CurrentGroup|PreviousGroups]};
        _ ->
            {unicode:characters_to_list([Line,CurrentGroup]), PreviousGroups}
    end.

parse_input_per_person(Input) ->
    {CurrentGroup, PreviousGroups} = lists:foldl(
                                       fun(Line, Acc) ->
                                               parse_line_per_person(Line, Acc)
                                       end,
                                       {[], []},
                                       Input),
    lists:reverse([sets:intersection(CurrentGroup)|PreviousGroups]).

parse_line_per_person(Line, {CurrentGroup, PreviousGroups}) ->
    case Line of
        "" ->
            {[], [sets:intersection(CurrentGroup)|PreviousGroups]};
        _ ->
            {[sets:from_list(Line)|CurrentGroup], PreviousGroups}
    end.

print_set_as_list(Set) ->
    io:format("~p~n", [sets:to_list(Set)]).
