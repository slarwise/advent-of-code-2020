-module(day_10).

-export([main/0, main/2]).

main() ->
    main(1, input).

main(Part, Filename) ->
    Ratings = read_input(Filename),
    SortedRatings = create_sorted_ratings(Ratings),
    case Part of
        1 -> part1(SortedRatings);
        2 -> part2(SortedRatings)
    end.

part1(SortedRatings) ->
    Counts = lists:foldl(
        fun({Next, Current}, AccCounts) ->
            Diff = Next - Current,
            OldCount = maps:get(Diff, AccCounts, 0),
            maps:put(Diff, OldCount + 1, AccCounts)
        end,
        #{},
        lists:zip(tl(SortedRatings), lists:droplast(SortedRatings))
    ),
    io:format("Counts: ~p~n", [Counts]),
    Answer = maps:get(1, Counts) * maps:get(3, Counts),
    io:format("#1 counts * #3 counts: ~p~n", [Answer]).

part2(SortedRatings) ->
    NPaths = lists:foldl(
        fun(X, AccNPaths) ->
            Count = lists:sum(
                [maps:get(X - Diff, AccNPaths, 0) || Diff <- [1, 2, 3]]
            ),
            maps:put(X, Count, AccNPaths)
        end,
        #{0 => 1},
        tl(SortedRatings)
    ),
    maps:get(lists:last(SortedRatings), NPaths).

read_input(Filename) ->
    {ok, Binary} = file:read_file(Filename),
    StringList = string:lexemes(binary_to_list(Binary), "\n"),
    lists:map(fun list_to_integer/1, StringList).

create_sorted_ratings(Ratings) ->
    SortedRatings = lists:sort(Ratings),
    [0] ++ SortedRatings ++ [lists:last(SortedRatings) + 3].
