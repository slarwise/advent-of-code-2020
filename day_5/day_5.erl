-module(day_5).

-export([
         first/0,
         second/0
        ]).

first() ->
    % Input = read_input("simple-input"),
    Input = read_input("input"),
    SeatNumbers = lists:map(fun(S) -> string_to_seat_number(S) end, Input),
    SeatIds = lists:map(fun(S) -> seat_number_to_seat_id(S) end, SeatNumbers),
    lists:max(SeatIds).

second() ->
    % Input = read_input("simple-input"),
    Input = read_input("input"),
    SeatNumbers = lists:map(fun(S) -> string_to_seat_number(S) end, Input),
    SortedSeatNumbers = lists:sort(SeatNumbers),
    MissingSeatNumber = find_missing_seat_number(SortedSeatNumbers),
    MissingSeatId = seat_number_to_seat_id(MissingSeatNumber),
    io:format("Missing seat number: ~p~n", [MissingSeatNumber]),
    io:format("Missing seat id: ~p~n", [MissingSeatId]).

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

string_to_seat_number(String) ->
    RowString = string:slice(String, 0, 7),
    Row = string_to_row(RowString),
    ColumnString = string:slice(String, 7, 3),
    Column = string_to_column(ColumnString),
    {Row, Column}.

string_to_row(String) ->
    String1 = re:replace(String, "F", "0", [{return, list}, global]),
    String2 = re:replace(String1, "B", "1", [{return, list}, global]),
    list_to_integer(String2, 2).

string_to_column(String) ->
    String1 = re:replace(String, "L", "0", [{return, list}, global]),
    String2 = re:replace(String1, "R", "1", [{return, list}, global]),
    list_to_integer(String2, 2).

seat_number_to_seat_id({Row, Column}) ->
    Row * 8 + Column.

find_missing_seat_number([]) ->
    not_found;
find_missing_seat_number([CurrentSeatNumber|RemainingSeatNumbers]) ->
    NextSeatNumber = hd(RemainingSeatNumbers),
    case NextSeatNumber =:= next_expected_seat_number(CurrentSeatNumber) of
        true -> find_missing_seat_number(RemainingSeatNumbers);
        false -> next_expected_seat_number(CurrentSeatNumber)
    end.

next_expected_seat_number({Row, Column}) ->
    NextColumn = case Column of
                     7 -> 0;
                     _ -> Column + 1
                 end,
    NextRow = case NextColumn of
                  0 -> Row + 1;
                  _ -> Row
              end,
    {NextRow, NextColumn}.
