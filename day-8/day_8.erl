-module(day_8).

-export([
         main/2
        ]).

main(Part, Filename) ->
    BootCode = read_file_into_list(Filename),
    case Part of
        1 ->
            run_boot_code(BootCode);
        2 ->
            % Change either one jmp to nop or one nop to jmp such that the
            % program terminates. The program terminates when the line number
            % just after the last is supposed to be executed.
            test_until_termination(BootCode)
    end.

read_file_into_list(Filename) ->
    {ok, Binary} = file:read_file(Filename),
    String = binary_to_list(Binary),
    string:lexemes(String, "\n").

run_boot_code(BootCode) ->
    run_boot_code(BootCode, _LineNumber=1, sets:new(), _Accumulator=0).

run_boot_code(BootCode, LineNumber, VisitedLines, Accumulator) ->
    case sets:is_element(LineNumber, VisitedLines) of
        true ->
            Accumulator;
        false ->
            NewVisitedLines = sets:add_element(LineNumber, VisitedLines),
            case string:lexemes(lists:nth(LineNumber, BootCode), " ") of
                ["acc", Value] ->
                    run_boot_code(BootCode, LineNumber+1, NewVisitedLines,
                                  Accumulator+list_to_integer(Value));
                ["jmp", Value] ->
                    run_boot_code(BootCode, LineNumber+list_to_integer(Value),
                                  NewVisitedLines, Accumulator);
                ["nop", _] ->
                    run_boot_code(BootCode, LineNumber+1, NewVisitedLines,
                                  Accumulator)
            end
    end.

test_until_termination(BootCode) ->
    test_until_termination(BootCode, 1).

test_until_termination(BootCode, LineToChange) ->
    {ChangedLine, NewBootCode} = generate_new_boot_code(BootCode, LineToChange),
    case terminates(NewBootCode) of
        {true, Accumulator} -> {true, Accumulator};
        false -> test_until_termination(BootCode, ChangedLine+1)
    end.

terminates(BootCode) ->
    terminates(BootCode, 1, sets:new(), 0).

terminates(BootCode, LineNumber, VisitedLines, Accumulator) ->
    case LineNumber == length(BootCode)+1 of
        true ->
            {true, Accumulator};
        false ->
            case sets:is_element(LineNumber, VisitedLines) of
                true -> false;
                false ->
                    NewVisitedLines = sets:add_element(LineNumber, VisitedLines),
                    case string:lexemes(lists:nth(LineNumber, BootCode), " ") of
                        ["acc", Value] ->
                            terminates(BootCode, LineNumber+1, NewVisitedLines,
                                          Accumulator+list_to_integer(Value));
                        ["jmp", Value] ->
                            terminates(BootCode, LineNumber+list_to_integer(Value),
                                          NewVisitedLines, Accumulator);
                        ["nop", _] ->
                            terminates(BootCode, LineNumber+1, NewVisitedLines,
                                          Accumulator)
                    end
            end
    end.

% Create the next version of boot code by starting from the given line number
% and change the next nop or jmp found to jmp or nop. Return the new code and
% the line that was changed.
generate_new_boot_code(BootCode, LineNumber) ->
    CurrentLine = string:lexemes(lists:nth(LineNumber, BootCode), " "),
    generate_new_boot_code(BootCode, LineNumber, CurrentLine).

generate_new_boot_code(BootCode, LineNumber, ["acc", _]) ->
    NextLine = string:lexemes(lists:nth(LineNumber+1, BootCode), " "),
    generate_new_boot_code(BootCode, LineNumber+1, NextLine);
generate_new_boot_code(BootCode, LineNumber, ["nop", Val]) ->
    {LineNumber, lists:sublist(BootCode, LineNumber-1) ++ ["jmp " ++ Val] ++
     lists:nthtail(LineNumber, BootCode)};
generate_new_boot_code(BootCode, LineNumber, ["jmp", Val]) ->
    {LineNumber, lists:sublist(BootCode, LineNumber-1) ++ ["nop " ++ Val] ++
     lists:nthtail(LineNumber, BootCode)}.
