-module(day_4).

-export([
         first/0,
         second/0,
         is_valid_value/2
        ]).

first() ->
    % Input = read_input("simple-input"),
    Input = read_input("input"),

    Passports = parse_passports(Input),

    ValidPassports = get_valid_passports(Passports),
    length(ValidPassports).

second() ->
    % Input = read_input("simple-input-2"),
    Input = read_input("input"),

    Passports = parse_passports(Input),

    ValidPassports = get_valid_passports_2(Passports),
    io:format("~p~n", [ValidPassports]),
    length(ValidPassports).


read_input(Filename) ->
    {ok, Device} = file:open(Filename, [read]),
    read_input(Device, []).

read_input(Device, Acc) ->
    case file:read_line(Device) of
        eof ->
            lists:reverse(Acc);
        {ok, Line} ->
            read_input(Device, [string:trim(Line)|Acc])
    end.

% Make a list of all passports of the form
% [
%   [{Key1, Val1}, {Key2, Val2}], ...,
%   [{Key1, Val1}, {Key2, Val2}], ...,
%   ...,
% ]
parse_passports(Input) ->
    {CurrentData, Data} = lists:foldl(
                            fun(Line, Acc) ->
                                    parse_passports(Line, Acc)
                            end,
                            {[], []}, Input),
    [CurrentData|Data].

parse_passports(Line, {CurrentData, Data}) ->
    case Line of
        "" ->
            {[], [CurrentData|Data]};
        _ ->
            {lists:flatten([get_key_value_pairs(Line), CurrentData]), Data}
    end.

get_key_value_pairs(Line) ->
    KeyValuePairs = string:split(Line, " ", all),
    lists:map(
      fun(KeyValuePair) ->
              case length(string:split(KeyValuePair, ":")) of
                  2 ->
                      [Key, Value] = string:split(KeyValuePair, ":"),
                      {Key, Value};
                  1 -> [Key] = string:split(KeyValuePair, ":"),
                       {Key, ""}
              end
      end,
      KeyValuePairs).

get_valid_passports(Passports) ->
    lists:filter(fun(P) -> has_required_fields(P) end, Passports).

has_required_fields(Passport) ->
    {Keys, _Values} = lists:unzip(Passport),
    MandatoryKeys = ["byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid"],
    case lists_are_equal(Keys, MandatoryKeys) of
        true ->
            true;
        false ->
            MandatoryKeysWithCid = ["cid"|MandatoryKeys],
            lists_are_equal(Keys, MandatoryKeysWithCid)
    end.

lists_are_equal(List1, List2) ->
    lists:sort(List1) =:= lists:sort(List2).

get_valid_passports_2(Passports) ->
    lists:filter(
      fun(P) ->
              has_required_fields(P) and has_valid_values(P)
      end,
      Passports).

has_valid_values(Passport) ->
    lists:all(fun({Key, Value}) -> is_valid_value(Key, Value) end, Passport).

is_valid_value(Key, Value) ->
    case Key of
        "byr" ->
            {BirthYear, _} = string:to_integer(Value),
            (1920 =< BirthYear) and (BirthYear =< 2002);
        "iyr" ->
            {IssueYear, _} = string:to_integer(Value),
            (2010 =< IssueYear) and (IssueYear =< 2020);
        "eyr" ->
            {ExpirationYear, _} = string:to_integer(Value),
            (2020 =< ExpirationYear) and (ExpirationYear =< 2030);
        "hgt" ->
            {Height, Unit} = string:to_integer(Value),
            case Unit of
                "cm" ->
                    (150 =< Height) and (Height =< 193);
                "in" ->
                    (59 =< Height) and (Height =< 76);
                _ ->
                    false
            end;
        "hcl" ->
            case re:run(Value, "^#[0-9a-f]{6,6}$", [{capture, none}]) of
                match -> true;
                nomatch -> false
            end;
        "ecl" ->
            case re:run(Value, "amb|blu|brn|gry|grn|hzl|oth", [{capture, none}]) of
                match -> true;
                nomatch -> false
            end;
        "pid" ->
            case re:run(Value, "^[0-9]{9,9}$", [{capture, none}]) of
                match -> true;
                nomatch -> false
            end;
        "cid" ->
           true
    end.
