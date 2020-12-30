-module(day_7_graph).

-include_lib("eunit/include/eunit.hrl").

-export([
         main/0,
         main/2
        ]).

main() ->
    main(1, "input-simple").

-spec main(Part :: 1 | 2, Filename :: string()) -> ok.
main(Part, Filename) ->
    Input = read_file_into_list(Filename),
    RuleList = lists:map(fun parse_rule/1, Input),
    Graph = create_graph(RuleList),
    case Part of
        1 -> part1(Graph);
        2 -> part2(Graph)
    end.

part1(Graph) ->
    Bags = lists:filter(
             fun(Start) ->
                     Path = digraph:get_path(Graph, Start, "shiny gold"),
                     case Path of
                         false -> false;
                         _ -> true
                     end
             end,
             digraph:vertices(Graph)
            ),
    io:format("~p~n", [Bags]),
    io:format("~p~n", [length(Bags)]).

part2(Graph) ->
    Sum = sum_weights(Graph, "shiny gold"),
    Edges = digraph:out_edges(Graph, "shiny gold"),
    Sum + lists:foldl(
            fun(Edge, S) ->
                    {_, _, _, Weight} = digraph:edge(Graph, Edge),
                    S + Weight
            end,
            0,
            Edges
           ).

% Base case: no neighbors
sum_weights(Graph, Vertex) ->
    case digraph:out_edges(Graph, Vertex) of
        [] ->
            1;
        Edges ->
            lists:foldl(
              fun(Edge, Sum) ->
                      {_, _, Neighbour, Weight} = digraph:edge(Graph, Edge),
                      Sum + (Weight * sum_weights(Graph, Neighbour))
              end,
              0,
              Edges
             )
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

create_graph(RuleList) ->
    Graph = digraph:new(),
    Vertices = [element(1, Rule) || Rule <- RuleList],
    lists:foreach(
      fun(V) ->
              digraph:add_vertex(Graph, V)
      end,
      Vertices
     ),
    lists:foreach(
      fun({V, Out}) ->
              lists:foreach(
                fun({OutV, Weight}) ->
                        digraph:add_edge(Graph, V, OutV, Weight)
                end,
                Out
               )
      end,
      RuleList
     ),
    Graph.
