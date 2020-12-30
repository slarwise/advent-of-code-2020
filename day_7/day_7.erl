-module(day_7).

-include_lib("eunit/include/eunit.hrl").

-export([
         main/0,
         main/2,
         parse_rule/1
        ]).

main() ->
    main(1, "input-simple").

-spec main(Part :: 1 | 2, Filename :: string()) -> ok.
main(Part, Filename) ->
    Input = read_file_into_list(Filename),
    RuleList = lists:map(fun parse_rule/1, Input),
    Rules = maps:from_list(RuleList),

    case Part of
        1 ->
            BagOnlyRules = maps:map(fun(_Key, Value) ->
                                            [element(1, X) || X <- Value]
                                    end,
                                    Rules),
            BagsToTest = maps:keys(BagOnlyRules),
            BagsThatCanHoldShinyGold = lists:filter(
                                         fun(Start) ->
                                                 can_hold_bag("shiny gold", Start,
                                                              BagOnlyRules)
                                         end,
                                         BagsToTest
                                        ),
            io:format("~p~n", [BagsThatCanHoldShinyGold]),
            io:format("~p~n", [length(BagsThatCanHoldShinyGold)]);
        2 ->
            bags_contained("shiny gold", Rules)
    end.

read_file_into_list(Filename) ->
    {ok, Binary} = file:read_file(Filename),
    String = binary_to_list(Binary),
    string:lexemes(String, "\n").

parse_rule(Rule) ->
    [ContainerHalf, ContainedHalf] = string:split(Rule, " contain "),
    {parse_container(ContainerHalf), parse_contained(ContainedHalf)}.

parse_container(ContainerHalf) ->
    {match, [Container]} = re:run(
                              ContainerHalf, "^[a-z]\+\s[a-z]\+",
                              [{capture, first, list}]
                             ),
    Container.

parse_contained(ContainedHalf) ->
    ContainedBags = string:split(ContainedHalf, ", ", all),
    case string:slice(ContainedBags, 0, 2) of
        "no" ->
            [];
        _ ->
            lists:map(
              fun(Bag) ->
                      {match, [Quantity]} = re:run(
                                              Bag, "^[1-9]\+",
                                              [{capture, first, list}]
                                             ),
                      {match, [Color]} = re:run(
                                           Bag, "[a-z]\+\s[a-z]\+",
                                           [{capture, first, list}]
                                          ),
                      {Color, list_to_integer(Quantity)}
              end,
              ContainedBags
             )
    end.

parse_rule_test_() ->
    [?_assertEqual(
        parse_rule("light red bags contain 1 bright white bag, 2 muted yellow
                   bags."),
        {"light red", [{"bright white", 1}, {"muted yellow", 2}]}
       ),
     ?_assertEqual(
        parse_rule("bright white bags contain 1 shiny gold bag."),
        {"bright white", [{"shiny gold", 1}]}
       ),
     ?_assertEqual(
        parse_rule("faded blue bags contain no other bags."),
        {"faded blue", []}
       )
    ].

% Returns true if bag color Start can hold bag color DestColor.
-spec can_hold_bag(DestColor, Start, Map) -> Result when
      DestColor :: string(),
      Start :: string(),
      Map :: map(),
      Result :: boolean().
can_hold_bag(DestColor, DestColor, _Map) ->
    false;
can_hold_bag(DestColor, Start, Map) ->
    try can_hold_bag(DestColor, {Start, maps:get(Start, Map)}, Map, []) of
        _Visited -> false
    catch
        true -> true
    end.

% Returns the visited bags if the destination color is not found. If it is
% found, found is thrown.
-spec can_hold_bag(DestColor, {Color, CanHold}, Map, Visited) -> Result when
      DestColor :: string(),
      Color     :: string(),
      CanHold   :: [string()],
      Map       :: map(),
      Visited   :: [string()],
      Result    :: no_return()  |  [string()].
can_hold_bag(DestColor, {DestColor, _}, _Map, _Visited) ->
    throw(true);
can_hold_bag(_DestColor, {Color, []}, _Map, Visited) ->
    [Color|Visited];
can_hold_bag(DestColor, {Color, CanHold}, Map, Visited) ->
    case lists:member(Color, Visited) of
        true ->
            Visited;
        false ->
            lists:flatmap(
              fun(C) ->
                      can_hold_bag(DestColor, {C, maps:get(C, Map)}, Map,
                                   [Color|Visited])
              end,
              CanHold
             )
    end.

can_hold_bag_test_() ->
    [?_assert(
        can_hold_bag("shiny gold", "bright white",
                       #{"light red"    => ["bright white", "muted yellow"],
                         "dark orange"  => ["bright white", "muted yellow"],
                         "bright white" => ["shiny gold"],
                         "muted yellow" => ["shiny gold", "faded blue"],
                         "shiny gold"   => ["dark olive", "vibrant plum"],
                         "dark olive"   => ["faded blue", "dotted black"],
                         "vibrant plum" => ["faded blue", "dotted black"],
                         "faded blue"   => [],
                         "dotted black" => []
                        })
       ),
     ?_assert(
        can_hold_bag("shiny gold", "muted yellow",
                       #{"light red"    => ["bright white", "muted yellow"],
                         "dark orange"  => ["bright white", "muted yellow"],
                         "bright white" => ["shiny gold"],
                         "muted yellow" => ["shiny gold", "faded blue"],
                         "shiny gold"   => ["dark olive", "vibrant plum"],
                         "dark olive"   => ["faded blue", "dotted black"],
                         "vibrant plum" => ["faded blue", "dotted black"],
                         "faded blue"   => [],
                         "dotted black" => []
                        })
       ),
     ?_assert(
        can_hold_bag("shiny gold", "dark orange",
                       #{"light red"    => ["bright white", "muted yellow"],
                         "dark orange"  => ["bright white", "muted yellow"],
                         "bright white" => ["shiny gold"],
                         "muted yellow" => ["shiny gold", "faded blue"],
                         "shiny gold"   => ["dark olive", "vibrant plum"],
                         "dark olive"   => ["faded blue", "dotted black"],
                         "vibrant plum" => ["faded blue", "dotted black"],
                         "faded blue"   => [],
                         "dotted black" => []
                        })
       ),
     ?_assert(
        can_hold_bag("shiny gold", "light red",
                       #{"light red"    => ["bright white", "muted yellow"],
                         "dark orange"  => ["bright white", "muted yellow"],
                         "bright white" => ["shiny gold"],
                         "muted yellow" => ["shiny gold", "faded blue"],
                         "shiny gold"   => ["dark olive", "vibrant plum"],
                         "dark olive"   => ["faded blue", "dotted black"],
                         "vibrant plum" => ["faded blue", "dotted black"],
                         "faded blue"   => [],
                         "dotted black" => []
                        })
       ),
     ?_assertNot(
        can_hold_bag("shiny gold", "shiny gold",
                       #{"light red"    => ["bright white", "muted yellow"],
                         "dark orange"  => ["bright white", "muted yellow"],
                         "bright white" => ["shiny gold"],
                         "muted yellow" => ["shiny gold", "faded blue"],
                         "shiny gold"   => ["dark olive", "vibrant plum"],
                         "dark olive"   => ["faded blue", "dotted black"],
                         "vibrant plum" => ["faded blue", "dotted black"],
                         "faded blue"   => [],
                         "dotted black" => []
                        })
       ),
     ?_assertNot(
        can_hold_bag("shiny gold", "dark olive",
                       #{"light red"    => ["bright white", "muted yellow"],
                         "dark orange"  => ["bright white", "muted yellow"],
                         "bright white" => ["shiny gold"],
                         "muted yellow" => ["shiny gold", "faded blue"],
                         "shiny gold"   => ["dark olive", "vibrant plum"],
                         "dark olive"   => ["faded blue", "dotted black"],
                         "vibrant plum" => ["faded blue", "dotted black"],
                         "faded blue"   => [],
                         "dotted black" => []
                        })
       ),
     ?_assertNot(
        can_hold_bag("shiny gold", "vibrant plum",
                       #{"light red"    => ["bright white", "muted yellow"],
                         "dark orange"  => ["bright white", "muted yellow"],
                         "bright white" => ["shiny gold"],
                         "muted yellow" => ["shiny gold", "faded blue"],
                         "shiny gold"   => ["dark olive", "vibrant plum"],
                         "dark olive"   => ["faded blue", "dotted black"],
                         "vibrant plum" => ["faded blue", "dotted black"],
                         "faded blue"   => [],
                         "dotted black" => []
                        })
       ),
     ?_assertNot(
        can_hold_bag("shiny gold", "faded blue",
                       #{"light red"    => ["bright white", "muted yellow"],
                         "dark orange"  => ["bright white", "muted yellow"],
                         "bright white" => ["shiny gold"],
                         "muted yellow" => ["shiny gold", "faded blue"],
                         "shiny gold"   => ["dark olive", "vibrant plum"],
                         "dark olive"   => ["faded blue", "dotted black"],
                         "vibrant plum" => ["faded blue", "dotted black"],
                         "faded blue"   => [],
                         "dotted black" => []
                        })
       ),
     ?_assertNot(
        can_hold_bag("shiny gold", "dotted black",
                       #{"light red"    => ["bright white", "muted yellow"],
                         "dark orange"  => ["bright white", "muted yellow"],
                         "bright white" => ["shiny gold"],
                         "muted yellow" => ["shiny gold", "faded blue"],
                         "shiny gold"   => ["dark olive", "vibrant plum"],
                         "dark olive"   => ["faded blue", "dotted black"],
                         "vibrant plum" => ["faded blue", "dotted black"],
                         "faded blue"   => [],
                         "dotted black" => []
                        })
       )
    ].

bags_contained(Color, Rules) ->
    % Base case: Color can't hold any other bags -> 1
    % Else: Count the number of bags this color can hold
    case maps:get(Color, Rules) of
        [] ->
            0;
        Colors ->
            lists:foldl(
              fun({Col, Quantity}, Sum) ->
                      Sum + Quantity + Quantity * bags_contained(Col, Rules)
              end,
              0,
              Colors
             )
    end.

bags_contained_test_() ->
    [
     ?_assertEqual(
        32,
        bags_contained("shiny gold",
                       #{"light red"    => [{"bright white",1}, {"muted yellow",2}],
                         "dark orange"  => [{"bright white",3}, {"muted yellow",4}],
                         "bright white" => [{"shiny gold",1}],
                         "muted yellow" => [{"shiny gold",2}, {"faded blue,",9}],
                         "shiny gold"   => [{"dark olive",1}, {"vibrant plum",2}],
                         "dark olive"   => [{"faded blue",3}, {"dotted black",4}],
                         "vibrant plum" => [{"faded blue",5}, {"dotted black",6}],
                         "faded blue"   => [],
                         "dotted black" => []
                        })
       )
    ].
