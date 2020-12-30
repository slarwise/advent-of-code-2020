-module(day_2).

-export([
         first/0,
         second/0
        ]).

first() ->
    % Read the input into [{Times, Char, Password}, ...]
    Input = get_input(),

    % Return how many passwords are valid
    ValidPasswords = lists:filter(fun(P) -> is_valid_password(P) end, Input),
    length(ValidPasswords).

get_input() ->
    {ok, IoDevice} = file:open("input", [read]),
    read_passwords(IoDevice).

read_passwords(IoDevice) ->
    read_passwords(IoDevice, []).

read_passwords(IoDevice, Acc) ->
    case file:read_line(IoDevice) of
        eof ->
            lists:reverse(Acc);
        {ok, Line} ->
            [Times, Char, Password, _NewLine] = re:split(Line, " |: |\n", [{return,list}]),
            [LowerStr, UpperStr] = string:split(Times, "-"),
            {Lower, Rest} = string:to_integer(LowerStr),
            {Upper, Rest} = string:to_integer(UpperStr),
            read_passwords(IoDevice, [{{Lower, Upper}, Char, Password}|Acc])
    end.

is_valid_password({{Lower, Upper}, Char, Password}) ->
    case re:run(Password, Char, [global]) of
        {match, Matches} ->
            % io:format("~w~n", [Matches]),
            % io:format("~w~n", [length(Matches)]),
            Result = Lower =< length(Matches) andalso length(Matches) =< Upper,
            % io:format("~w~n", [Result]),
            Result;
        nomatch ->
            false
    end.

second() ->
    Input = get_input(),
    % Input = [{{1, 3}, "a", "abcde"},
    %          {{1, 3}, "b", "cdefg"},
    %          {{2, 9}, "c", "ccccccccc"},
    %          {{2, 9}, "c", "cvc"}],

    ValidPasswords = lists:filter(fun(P) -> is_valid_password_2(P) end, Input),
    length(ValidPasswords).

is_valid_password_2({{Pos1, Pos2}, Char, Password}) ->
    % Need to check that Pos =< length
    Pos1Check = case Pos1 =< length(Password) of
                    true ->
                        lists:nth(Pos1, Password) == hd(Char);
                    false ->
                        false
                end,
    Pos2Check = case Pos2 =< length(Password) of
                    true ->
                        lists:nth(Pos2, Password) == hd(Char);
                    false ->
                        false
                end,
    Pos1Check xor Pos2Check.
