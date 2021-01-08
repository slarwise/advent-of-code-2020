-module(day_9).

-export([main/2, get_next_indices/2]).

main(Part, Filename) ->
    Input = read_numbers_into_list(Filename),
    PreambleLength = case Filename of
                         "input" -> 25;
                         "input-simple" -> 5
                     end,
    {value, {_, IncorrectNumber}} =
     lists:search(
       fun({Index, Number}) ->
               Preamble = lists:sublist(Input, Index-PreambleLength,
                                        PreambleLength),
               not is_sum_of_two_elements(Preamble, Number)
       end,
       lists:zip(
         _Indices = lists:seq(PreambleLength+1, length(Input)),
         _Numbers = lists:nthtail(PreambleLength, Input)
        )
      ),
     case Part of
         1 ->
             IncorrectNumber;
         2 ->
             Set = find_set_summing_to_number(Input, IncorrectNumber),
             lists:min(Set) + lists:max(Set)
     end.

read_numbers_into_list(Filename) ->
    {ok, Binary} = file:read_file(Filename),
    String = binary_to_list(Binary),
    StringWithoutNewlines = string:lexemes(String, "\n"),
    lists:map(fun(Line) -> list_to_integer(Line) end, StringWithoutNewlines).

is_sum_of_two_elements(List, X) ->
    is_sum_of_two_elements(List, X, _StartIndices={1, 2}).

is_sum_of_two_elements(List, X, _Indices={I, J}) ->
    case lists:nth(I, List) + lists:nth(J, List) == X of
        true ->
            true;
        false ->
            case get_next_indices({I, J}, length(List)) of
                no_more_indices ->
                    false;
                {NextI, NextJ} ->
                    is_sum_of_two_elements(List, X, {NextI, NextJ})
            end
    end.

get_next_indices({I, J}, N) when I == (N-1), J == N ->
    no_more_indices;
get_next_indices({I, J}, N) when I < N, J < N ->
    {I, J+1};
get_next_indices({I, J}, N) when I < N, J == N ->
    {I+1, I+2}.

find_set_summing_to_number([], _Number) ->
    false;
find_set_summing_to_number(Candidates, Number) ->
    case first_elements_sum_to_number(Candidates, Number) of
        {true, Terms} -> Terms;
        false -> find_set_summing_to_number(tl(Candidates), Number)
    end.

first_elements_sum_to_number(List, Number) ->
    first_elements_sum_to_number(List, Number, 0, []).

first_elements_sum_to_number(_List, Number, Sum, Terms) when Sum == Number ->
    {true, Terms};
first_elements_sum_to_number(_List, Number, Sum, _Terms) when Sum >= Number ->
    false;
first_elements_sum_to_number([], _Number, _Sum, _Terms) ->
    false;
first_elements_sum_to_number([Head|Tail], Number, Sum, Terms) ->
    first_elements_sum_to_number(Tail, Number, Sum+Head, [Head|Terms]).
