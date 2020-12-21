-module(day_4).

-export([
         first/0
        ]).

first() ->
    % Input = read_input("simple-input"),
    Input = read_input("input"),
    parse_passwords(Input).

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

parse_passwords(Data) ->
    lists:foldl(
      fun(Line, Acc) ->
              parse_passwords(Line, Acc)
      end,
      {sets:new(), 0}, Data).

parse_passwords(Line, {CurrentKeys, ValidPasswords}) ->
    case Line of
        "" ->
            MandatoryKeys = sets:from_list(["byr", "iyr", "eyr", "hgt", "hcl",
                                            "ecl", "pid"]),
            MandatoryKeysWithCid = sets:add_element("cid", MandatoryKeys),
            io:format("MandatoryKeys: ~p~n", [MandatoryKeys]),
            io:format("MandatoryKeysWithCid: ~p~n", [MandatoryKeysWithCid]),
            HasValidKeys = (
              (
               sets:is_subset(CurrentKeys, MandatoryKeys) and
               sets:is_subset(MandatoryKeys, CurrentKeys)
              ) or
              (
               sets:is_subset(CurrentKeys, MandatoryKeysWithCid) and
               sets:is_subset(MandatoryKeysWithCid, CurrentKeys)
              )
             ),
            NewValidPasswords = case HasValidKeys of
                                    true -> ValidPasswords + 1;
                                    false -> ValidPasswords
                                end,
            % io:format("Current: ~p~n", [CurrentKeys]),
            % io:format("NewValidPasswords: ~p~n", [NewValidPasswords]),
            {sets:new(), NewValidPasswords};
        _ ->
            Keys = get_keys_in_line(Line),
            {sets:union(CurrentKeys, Keys), ValidPasswords}
    end.

get_keys_in_line(Line) ->
    % Make sure there are no duplicates, actually better to use a list here
    % since we need to check that a key is not present twice.
    KVPairs = string:split(Line, " ", all),
    Keys = lists:map(
             fun(S) ->
                     Split = string:split(S, ":", all),
                     hd(Split)
             end,
             KVPairs
            ),
    sets:from_list(Keys).
