-module(day_3).

-export([
         first/0,
         second/1
        ]).

first() ->
    Input = read_input("simple_input"),
    % Input = read_input("input"),

    ColLength = length(lists:nth(1, Input)),
    Coordinate = {1, 1},
    Slope = {1, 3},

    count_trees(Input, Coordinate, Slope, ColLength).

second(Slope) ->
    % Input = read_input("simple_input"),
    Input = read_input("input"),

    ColLength = length(lists:nth(1, Input)),
    Coordinate = {1, 1},

    count_trees(Input, Coordinate, Slope, ColLength).

read_input(FileName) ->
    {ok, IoDevice} = file:open(FileName, [read]),
    read_lines(IoDevice).

read_lines(IoDevice) ->
    read_lines(IoDevice, []).

read_lines(IoDevice, Acc) ->
    case file:read_line(IoDevice) of
        eof ->
            lists:reverse(Acc);
        {ok, Line} ->
            read_lines(IoDevice, [string:trim(Line)|Acc])
    end.

count_trees(Map, Coordinate, Slope, ColLength) ->
    count_trees(Map, Coordinate, Slope, ColLength, 0).

count_trees(Map, {Row, _Col}, _Slope, _ColLength, TreesFound)
  when Row > length(Map) ->
    TreesFound;

count_trees(Map, {Row, Col}, {DeltaRow, DeltaCol}, ColLength, TreesFound) ->
    Line = lists:nth(Row, Map),
    Value = lists:nth(Col, Line),
    NewTreesfound = case is_tree(Value) of
                        true -> TreesFound + 1;
                        false -> TreesFound
                    end,
    NextCoordinate = {Row + DeltaRow,
                      next_col(Col, DeltaCol, ColLength)},
    count_trees(Map, NextCoordinate, {DeltaRow, DeltaCol}, ColLength,
                NewTreesfound).

is_tree(String) ->
    String == hd("#").

next_col(Current, Increment, ColLength) ->
    case Current + Increment =< ColLength of
        true -> Current + Increment;
        false -> (Current + Increment) - ColLength
    end.
