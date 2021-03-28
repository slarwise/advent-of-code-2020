-module(day_11_2).

-export([
    main/1,
    get_positions_in_direction/3
]).

main(Filename) ->
    Input = read_input(Filename),
    Layout = get_layout(Input),
    ConvergedLayout = simulate_until_convergence(Layout, []),
    count_occupied_seats(ConvergedLayout).

read_input(Filename) ->
    {ok, Binary} = file:read_file(Filename),
    string:lexemes(binary_to_list(Binary), "\n").

get_layout(Input) ->
    lists:map(
      fun(Elements) ->
              get_states(Elements)
      end,
      Input
     ).

simulate_until_convergence(Layout, Layout) ->
    Layout;
simulate_until_convergence(Layout, _OldLayout) ->
    simulate_until_convergence(simulate(Layout), Layout).

simulate(Layout) ->
    simulate(1, Layout, []).

simulate(Row, Layout, NewLayout) when Row > length(Layout) ->
    lists:reverse(NewLayout);
simulate(Row, Layout, NewLayout) ->
    Elements = lists:nth(Row, Layout),
    NewElements = lists:map(
        fun(Col) ->
            CurrentState = lists:nth(Col, Elements)
            % get_new_state(CurrentState, NeighbourStates)
        end,
        lists:seq(1, length(Elements))
    ),
    simulate(Row + 1, Layout, [NewElements | NewLayout]).

get_element({Row, Col}, Layout) ->
    lists:nth(Col, lists:nth(Row, Layout)).

get_elements(Indices, Layout) ->
    lists:map(
        fun(Index) ->
            get_element(Index, Layout)
        end,
        Indices
    ).

get_row(Row, Layout) ->
    lists:nth(Row, Layout).

get_col(Col, Layout) ->
    lists:map(
        fun(Row) ->
            lists:nth(Col, Row)
        end,
        Layout
    ).

get_state(46) -> floor;    % "."
get_state(76) -> empty;    % "L"
get_state(35) -> occupied. % "#"

get_states(Elements) ->
    lists:map(
        fun(E) ->
            get_state(E)
        end,
        Elements
    ).

% Returns {value, State} of the first found seat in the given Direction or
% false if no seat was found.
find_first_seat({Row, Col}, Layout, e) ->
    lists:search(
        fun
            (empty) -> false;
            (_) -> true
        end,
        lists:nthtail(Col, lists:nth(Row, Layout))
    ).

get_positions_in_direction({Row, Col}, Layout, e) ->
    lists:nthtail(Col, get_row(Row, Layout));
get_positions_in_direction({Row, Col}, Layout, w) ->
    lists:sublist(get_row(Row, Layout), Col-1);
get_positions_in_direction({Row, Col}, Layout, s) ->
    lists:nthtail(Row, get_col(Col, Layout));
get_positions_in_direction({Row, Col}, Layout, n) ->
    lists:sublist(get_col(Col, Layout), Row-1);
% TODO
get_positions_in_direction({Row, Col}, Layout, ne) ->
    ok.

get_new_state(floor, _NeighbourStates) ->
    floor;
get_new_state(empty, NeighbourStates) ->
    AllNeighboursAreEmpty = lists:all(
        fun
            (occupied) -> false;
            (_) -> true
        end,
        NeighbourStates
    ),
    case AllNeighboursAreEmpty of
        true -> occupied;
        false -> empty
    end;
get_new_state(occupied, NeighbourStates) ->
    NNeighboursOccupied = lists:foldl(
        fun
            (occupied, Sum) -> Sum + 1;
            (_, Sum) -> Sum
        end,
        0,
        NeighbourStates
    ),
    case NNeighboursOccupied >= 4 of
        true -> empty;
        false -> occupied
    end.

count_occupied_seats(Layout) ->
    count_occupied_seats(Layout, 0).

count_occupied_seats([], Count) ->
    Count;
count_occupied_seats([Head | Tail], Count) ->
    NewCount = lists:foldl(
        fun
            (occupied, AccSum) -> AccSum + 1;
            (_, AccSum) -> AccSum
        end,
        Count,
        Head
    ),
    count_occupied_seats(Tail, NewCount).
