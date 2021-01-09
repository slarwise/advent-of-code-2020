-module(day_11).

-export([
    main/2,
    get_neighbour_indices/2
]).

main(Part, Filename) ->
    Input = read_input(Filename),
    Layout = get_layout(Input),
    case Part of
        1 -> part1(Layout);
        2 -> ok
    end.

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

part1(Layout) ->
    ConvergedLayout = simulate_until_convergence(Layout, []),
    count_occupied_seats(ConvergedLayout).

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
            CurrentState = lists:nth(Col, Elements),
            NeighbourIndices = get_neighbour_indices({Row, Col}, Layout),
            NeighbourStates = get_elements(
                NeighbourIndices,
                Layout
            ),
            get_new_state(CurrentState, NeighbourStates)
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

get_neighbour_indices({Row, Col}, Layout) ->
    NRows = length(Layout),
    NCols = length(hd(Layout)),
    Rows =
        case Row of
            1 -> [1, 2];
            NRows -> [Row - 1, Row];
            _ -> [Row - 1, Row, Row + 1]
        end,
    Cols =
        case Col of
            1 -> [1, 2];
            NCols -> [Col - 1, Col];
            _ -> [Col - 1, Col, Col + 1]
        end,
    Indices = [{R, C} || R <- Rows, C <- Cols],
    lists:filter(
        fun(Index) ->
            Index =/= {Row, Col}
        end,
        Indices
    ).

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
