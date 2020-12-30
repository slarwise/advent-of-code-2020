% input.txt holds numbers separated by newlines
% The sum of two of these numbers sum to 2020
% Return the product of these two numbers

-module(day_1).

-export([
         first_half/0,
         second_half/0
        ]).

first_half() ->
    % Read the input file
    {ok, IoDevice} = file:open("input.txt", [read]),
    Input = read_integers(IoDevice),

    % Find the two numbers that sum to 2020
    [Int1, Int2] = sums_to_2020(Input),
    
    % Return their product  
    Int1 * Int2.

read_integers(IoDevice) ->
    read_integers(IoDevice, []).

read_integers(IoDevice, Acc) ->
    case file:read_line(IoDevice) of
        eof ->
            lists:reverse(Acc);
        {ok, Line} ->
            {Integer, _NewLine} = string:to_integer(Line),
            read_integers(IoDevice, [Integer|Acc])
    end.

sums_to_2020([_Head|[]]) ->
    not_found;
sums_to_2020([Head|Tail]) ->
    case lists:filter(fun(X) -> Head + X =:= 2020 end, Tail) of
        [] ->
            sums_to_2020(Tail);
        FoundInt ->
            [Head|FoundInt]
    end.

second_half() ->
    % Read the input file
    {ok, IoDevice} = file:open("input.txt", [read]),
    Input = read_integers(IoDevice),
    
    % Find the three numbers that sum to 2020
    Permutations = [{X, Y, Z} || X <- Input, Y <- Input, Z <- Input],
    Matches = lists:filter(
                fun({X, Y, Z}) -> X + Y + Z =:= 2020 end,
                Permutations),
    
    % Return their product
    [{X, Y, Z}|_] = Matches,
    X * Y * Z.
