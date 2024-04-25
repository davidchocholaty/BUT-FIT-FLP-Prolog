/** FLP 2024
File: main.pl
Project: Logic Project - Turing Machine
Author: David Chocholatý <xchoch09>
Year: 2024
*/

% Read line from the standard input, ending at LF or EOF.
% read_line(-L,-C)
read_line(L,C) :-
	get_char(C),
	(isEOFEOL(C), L = [], !;
		read_line(LL,_),% atom_codes(C,[Cd]),
		[C|LL] = L).

% Tests a character for EOF or LF.
% isEOFEOL(?C)
isEOFEOL(C) :-
	C == end_of_file;
	(char_code(C,Code), Code==10).

% Read lines from the standard input.
% read_lines(-Ls)
read_lines(Ls) :-
	read_line(L,C),
	( C == end_of_file, Ls = [] ;
	  read_lines(LLs), Ls = [L|LLs]
	).

% Remove spaces from the list.
%
% The predicate removes the spaces from the list which are between symbols 
% representing states and tape symbols. With that it removes only the even 
% whitespace because the whitespace can represent the empty symbol, so they are
% kept in list.
%
% cut_whitespaces_even(+List, ?Result)
cut_whitespaces_even([], []).
cut_whitespaces_even([X], [X]).
cut_whitespaces_even([H,_|T], [H|Res]) :-
    cut_whitespaces_even(T, Res).

% Remove spaces from the sublists.
%
% The predicate takes the sublists and by using the "cut_whitespaces_even"
% the extra spaces are removed.
%
% cut_whitespaces_ll(+List, ?Result)
cut_whitespaces_ll([], []).
cut_whitespaces_ll([H|T], [HRes|TRes]) :-
    cut_whitespaces_even(H, HRes),
    cut_whitespaces_ll(T, TRes).

% The dynamic predicate for the program exit codes.
%
% exitCode/2
:- dynamic exitCode/2.
% The dynamic predicate for the Turing Machine rules.
%
% rule/4
:- dynamic rule/4.

% The predicate checks if the state is accepting.
%
% The accepting state is only the 'F' state. Otherwise, the state is not 
% accepting.
%
% accepts(+InnerState)
accepts(InnerState) :-
    InnerState == 'F'.

% The predicate inits the exit codes.
%
% The predicate inserts the used exit codes using the "exitCode" dynamic 
% predicate.
%
% init_exit_codes/0
init_exit_codes :-
    assertz(exitCode('abnormal_looping', 1)),
    assertz(exitCode('invalid_input', 2)).

% The predicate adds the rule by using the "rule" dynamic predicate.
%
% add_rule(+InnerState, +TapeSymbol, +NextState, +NewTapeSymbol)
add_rule([InnerState, TapeSymbol, NextState, NewTapeSymbol]) :-
    assertz(rule(InnerState, TapeSymbol, NextState, NewTapeSymbol)).

% The predicate inserts all rules from the provided list.
%
% add_rules_from_list(+List)
/** prida vsechna pravidla ze seznamu pravidel */
add_rules_from_list([]).
add_rules_from_list([H|T]) :-
    add_rule(H),
    add_rules_from_list(T).

% The predicate retracts all values for all dynamic predicates.
%
% retract_all_dynamic/0
retract_all_dynamic :-
    retractall(exitCode(_,_)),
    retractall(rule(_,_,_,_)).

% The function replaces the symbol on tape under the head.
%
% Nothing is done for the left move. For the right move, if the head is over 
% the tape length, the whitespace is added. The same is true for inserting any 
% different symbol and for this. For this symbol, it is additionally written 
% on the tape.
%
% replace_symbol(?Tape, +NewTapeSymbol, +HeadPosition, -UpdatedTape)
replace_symbol([], _, _, []).
replace_symbol(Tape, 'L', _, UpdatedTape) :-
    UpdatedTape = Tape.
replace_symbol(Tape, NewTapeSymbol, HeadPosition, UpdatedTape) :-
    NewTapeSymbol \= 'L',
    length(Tape, TapeLength),
    ( HeadPosition >= TapeLength ->      
      append(Tape, [' '], ActTape)
    ; ActTape = Tape
    ),
    ( NewTapeSymbol == 'R' ->
      UpdatedTape = ActTape
    ; nth0(HeadPosition, ActTape, _, Rest),
      nth0(HeadPosition, UpdatedTape, NewTapeSymbol, Rest)
    ).

% Update the head position if it should be moved ('L' or 'R').
%
% update_head_position(+Current, +Symb, -New)
update_head_position(Current, 'L', New) :-
    Current > 0,
    New is Current - 1.
update_head_position(Current, 'R', New) :-    
    New is Current + 1.
update_head_position(Current, Symb, New) :-
    Symb \= 'L',
    New is Current.

% The predicate adds the State into the tape on the Head Position for print 
% purposes.
%
% add_state_to_tape(+InnerState, +Tape, +HeadPosition, -TapeWState)
add_state_to_tape(InnerState, Tape, HeadPosition, TapeWState) :-
    length(Pref, HeadPosition),
    append(Pref, Suf, Tape),
    append(Pref, [InnerState|Suf], TapeWState).

% The predicate converts list to a string for a print purposes.
%
% list_2_str(+List, -Res)
list_2_str([], '').
list_2_str([H|T], Res) :-
    atom_string(H, HStr),
    list_2_str(T, TStr),
    string_concat(HStr, TStr, Res).

% The predicate which prints the configurations to stdout.
%
% The configuration is written as tape on which is added the inner symbol on 
% the head position.
%
% write_confs(+List)
write_confs([]).
write_confs([InnerState-Tape-HeadPosition|T]) :-
    add_state_to_tape(InnerState, Tape, HeadPosition, TapeWState),
    list_2_str(TapeWState, TapeWStateStr),
    writeln(TapeWStateStr),
    write_confs(T).

% The predicate returns the list without the last element.
%
% The behaviour is the same as in the Haskell language library. The predicate 
% is inspired from the following source: https://stackoverflow.com/a/16175064
%
% init(+List, -Result)
init([_], []).
init([H|T], [H|Res]) :-
    init(T, Res).

% The predicate returns the last element from the list.
%
% The behaviour is the same as in the Haskell language library.
%
% last(+List, -Res)
last([X], X).
last([_|T], Res) :-
    last(T, Res).

% The predicate states if the symbol for the state is valid.
%
% The symbol has to be uppercase character.
%
% is_valid_state(+Symb)
is_valid_state(X) :-
    atom_length(X, 1),
    char_type(X, upper).

% The predicate checks if the symbol is valid tape symbol.
%
% The symbol has to be lowercase character or a whitespace.
%
% is_valid_tape_symbol(+Symb)
is_valid_tape_symbol(X) :-
    atom_length(X, 1),
    (char_type(X, lower) ; char_type(X, space)).

% The predicate checks if the symbol is a valid tape symbol (lowercase 
% or whitespace) or an 'L' or 'R' character.
%
% is_valid_new_tape_symbol(+Symb)
is_valid_new_tape_symbol(X) :-
    atom_length(X, 1),
    (X = 'L' ; X = 'R' ; char_type(X, lower) ; char_type(X, space)).

% The predicate checks if the tape is valid.
%
% The valid tape contains only valid tape symbols. If not the program prints 
% an error message and halts with a specific exit code.
%
% is_valid_tape(+List)
is_valid_tape([]).
is_valid_tape([H|T]) :-
    ( is_valid_tape_symbol(H) ->
      is_valid_tape(T)
    ; writeln('Error: invalid input Turing Machine tape.'),
      exitCode(invalid_input, Code),
      retract_all_dynamic,
      halt(Code)
    ).

% The predicate checks if the rule is in valid format.
%
% It checks sub-parts. If all sub-parts are valid, the whole rule is valid.
%
% is_valid_rule(+List)
is_valid_rule([InnerState, TapeSymbol, NextState, NewTapeSymbol]) :-
    is_valid_state(InnerState),
    is_valid_state(NextState),
    is_valid_tape_symbol(TapeSymbol),
    is_valid_new_tape_symbol(NewTapeSymbol).

% The predicate checks if all rules are valid.
%
% The rules are valid only if each rule is valid. If not the program prints 
% an error message and halts with a specific exit code.
%
% valid_rules(+List)
valid_rules([]).
valid_rules([H|T]) :-
    ( is_valid_rule(H) ->
      valid_rules(T)
    ; writeln('Error: invalid input Turing Machine rule.'),
      exitCode(invalid_input, Code),
      retract_all_dynamic,
      halt(Code)
    ).

% The predicate runs the Turing Machine simulation.
%
% run(?InnerState, +Tape, ?HeadPosition, +Depth, +MaxDepth, ?History)
run(InnerState, Tape, HeadPosition, _, _, History) :-
    accepts(InnerState),
    append(History, [InnerState-Tape-HeadPosition], ExtendedHistory),
    write_confs(ExtendedHistory),
    !.
run(InnerState, Tape, HeadPosition, Depth, MaxDepth, History) :-    
    Depth < MaxDepth,
    not(member(InnerState-Tape-HeadPosition, History)),
    length(Tape, TapeLength),
    ( HeadPosition >= TapeLength ->
      TapeSymbol = ' '
    ; nth0(HeadPosition, Tape, TapeSymbol)
    ),
    rule(InnerState, TapeSymbol, NextState, NewTapeSymbol),    
    replace_symbol(Tape, NewTapeSymbol, HeadPosition, UpdatedTape),
    update_head_position(HeadPosition, NewTapeSymbol, NewHeadPosition),
    NewDepth is Depth + 1,
    append(History, [InnerState-Tape-HeadPosition], ExtendedHistory),
    run(NextState, UpdatedTape, NewHeadPosition, NewDepth, MaxDepth, ExtendedHistory).
run(InnerState, Tape, HeadPosition, _, _, History) :-    
    member(InnerState-Tape-HeadPosition, History),
    !,
    fail.

% The predicate sets the maximum depth for searching (cycle prevention).
%
% If the value is provided using the user argument, use it. Otherwise, use 
% the default value (10000).
%
% set_max_depth(+Argv, -MaxDepth)
set_max_depth(Argv, MaxDepth) :-
    ( append(_, [Arg], Argv),
      ( atom_number(Arg, MaxDepth),
        number(MaxDepth)
      )
    ; MaxDepth = 10000
    ).

% Main predicate.
%
% The main predicate of the program reads the standard input, the user argument
% and runs the simulation.
%
% start/0
start :-
        init_exit_codes,

        prompt(_, ''),
        read_lines(LL),

        % Use the default or user-defined maximum depth.
        current_prolog_flag(argv, Argv),
        set_max_depth(Argv, MaxDepth),

        % Preprocces rules and tape.

        % Rules
        init(LL, Rules),
        cut_whitespaces_ll(Rules, RulesNoWhitespace),
        add_rules_from_list(RulesNoWhitespace),

        valid_rules(RulesNoWhitespace),

        % Tape
        last(LL, Tape),
        is_valid_tape(Tape),

        % The configuration of the machine is determined by the state of the 
        % control and the configuration of the tape - this is a formal matter 
        % of an element of the set Q × {γ∆ω | γ ∈ Γ∗} × N.
        ( !, run('S', Tape, 0, 0, MaxDepth, []) ->
            true
        ; writeln('Error: Turing Machine stopped abnormally or looped.'),
          exitCode(abnormal_looping, Code),
          retract_all_dynamic,
          halt(Code)
        ),

        retract_all_dynamic,

        halt.
