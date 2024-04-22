/** FLP 2024
File: main.pl
Project: Logic Project - Turing Machine
Author: David Chocholatý <xchoch09>
Year: 2024
*/

/** Read lines from standard input, ending at LF or EOF. */
read_line(L,C) :-
	get_char(C),
	(isEOFEOL(C), L = [], !;
		read_line(LL,_),% atom_codes(C,[Cd]),
		[C|LL] = L).

/** Tests a character for EOF or LF. */
isEOFEOL(C) :-
	C == end_of_file;
	(char_code(C,Code), Code==10).

read_lines(Ls) :-
	read_line(L,C),
	( C == end_of_file, Ls = [] ;
	  read_lines(LLs), Ls = [L|LLs]
	).

/** odstrani ze seznamu mezery */
cut_whitespaces_even([], []).
cut_whitespaces_even([X], [X]).
cut_whitespaces_even([H,_|T], [H|Res]) :-
    cut_whitespaces_even(T, Res).

/** odstrani ze list of lists mezery */
cut_whitespaces_ll([], []).
cut_whitespaces_ll([H|T], [HRes|TRes]) :-
    cut_whitespaces_even(H, HRes),
    cut_whitespaces_ll(T, TRes).

/** Dynamic rule for the Turing Machine rules */
:- dynamic exitCode/2.
:- dynamic rule/4.

accepts(InnerState) :-
    InnerState == 'F'.

init_exit_codes :-
    assertz(exitCode('abnormal_looping', 1)),
    assertz(exitCode('invalid_input', 2)).

add_rule([InnerState, TapeSymbol, NextState, NewTapeSymbol]) :-
    assertz(rule(InnerState, TapeSymbol, NextState, NewTapeSymbol)).

/** prida vsechna pravidla ze seznamu pravidel */
add_rules_from_list([]).
add_rules_from_list([H|T]) :-
    add_rule(H),
    add_rules_from_list(T).

retract_all_dynamic :-
    retractall(exitCode(_,_)),
    retractall(rule(_,_,_,_)),
    retractall(visitedConf(_,_,_)).

% TODO jestli nespojit tuto a funkci pro posun
replace_symbol([], _, _, []).
replace_symbol(Tape, 'L', _, UpdatedTape) :-
    UpdatedTape = Tape.
replace_symbol(Tape, 'R', _, UpdatedTape) :-
    UpdatedTape = Tape. 
replace_symbol(Tape, NewTapeSymbol, HeadPosition, UpdatedTape) :-
    NewTapeSymbol \= 'L',
    NewTapeSymbol \= 'R',
    nth0(HeadPosition, Tape, _, Rest),
    nth0(HeadPosition, UpdatedTape, NewTapeSymbol, Rest).

% TODO mozna nekde kontrola, zda nejsme mimo pasku uz
update_head_position(Current, 'L', New) :-
    Current > 0,
    New is Current - 1.
update_head_position(Current, 'R', New) :-    
    New is Current + 1.
update_head_position(Current, Symb, New) :-
    Symb \= 'L',
    New is Current.

add_state_to_tape(InnerState, Tape, HeadPosition, TapeWState) :-
    length(Pref, HeadPosition),
    append(Pref, Suf, Tape),
    append(Pref, [InnerState|Suf], TapeWState).

list_2_str([], '').
list_2_str([H|T], Res) :-
    atom_string(H, HStr),
    list_2_str(T, TStr),
    string_concat(HStr, TStr, Res).

write_confs([]).
write_confs([InnerState-Tape-HeadPosition|T]) :-
    add_state_to_tape(InnerState, Tape, HeadPosition, TapeWState),
    list_2_str(TapeWState, TapeWStateStr),
    writeln(TapeWStateStr),
    write_confs(T).

/** vrati seznam bez posledniho prvku (jako init funkce z Haskellu) */
% https://stackoverflow.com/a/16175064
init([_], []).
init([H|T], [H|Res]) :-
    init(T, Res).

last([X], X).
last([_|T], Res) :-
    last(T, Res).

is_valid_state(X) :-
    atom_length(X, 1),
    char_type(X, upper).

is_valid_tape_symbol(X) :-
    atom_length(X, 1),
    (char_type(X, lower) ; char_type(X, space)).

is_valid_new_tape_symbol(X) :-
    atom_length(X, 1),
    (X = 'L' ; X = 'R' ; char_type(X, lower) ; char_type(X, space)).

is_valid_tape([]).
is_valid_tape([H|T]) :-
    ( is_valid_tape_symbol(H) ->
      is_valid_tape(T)
    ; writeln('Error: invalid input Turing Machine tape.'),
      exitCode(invalid_input, Code),
      retract_all_dynamic,
      halt(Code)
    ).

is_valid_rule([InnerState, TapeSymbol, NextState, NewTapeSymbol]) :-
    is_valid_state(InnerState),
    is_valid_state(NextState),
    is_valid_tape_symbol(TapeSymbol),
    is_valid_new_tape_symbol(NewTapeSymbol).

valid_rules([]).
valid_rules([H|T]) :-
    ( is_valid_rule(H) ->
      valid_rules(T)
    ; writeln('Error: invalid input Turing Machine rule.'),
      exitCode(invalid_input, Code),
      retract_all_dynamic,
      halt(Code)
    ).

% TODO jeslti nekde pouzit !
run(InnerState, Tape, HeadPosition, _, _, History) :-    
    accepts(InnerState),
    append(History, [InnerState-Tape-HeadPosition], ExtendedHistory),
    write_confs(ExtendedHistory).
run(InnerState, Tape, HeadPosition, Depth, MaxDepth, History) :-    
    Depth < MaxDepth,
    not(member(InnerState-Tape-HeadPosition, History)),
    length(Tape, TapeLength),
    ( HeadPosition >= TapeLength ->
      append(Tape, [' '], ActTape)
    ; ActTape = Tape
    ),
    nth0(HeadPosition, ActTape, TapeSymbol),
    rule(InnerState, TapeSymbol, NextState, NewTapeSymbol),
    replace_symbol(ActTape, NewTapeSymbol, HeadPosition, UpdatedTape),
    update_head_position(HeadPosition, NewTapeSymbol, NewHeadPosition),
    NewDepth is Depth + 1,
    append(History, [InnerState-Tape-HeadPosition], ExtendedHistory),
    run(NextState, UpdatedTape, NewHeadPosition, NewDepth, MaxDepth, ExtendedHistory).
run(InnerState, Tape, HeadPosition, _, _, History) :-
    member(InnerState-Tape-HeadPosition, History),
    fail.

start :-
        init_exit_codes,

        prompt(_, ''),
        read_lines(LL),

        % Preprocces rules and tape.

        % Rules
        init(LL, Rules),
        cut_whitespaces_ll(Rules, RulesNoWhitespace),
        add_rules_from_list(RulesNoWhitespace),

        %writeln(RulesNoWhitespace),

        valid_rules(RulesNoWhitespace),

        % Tape
        last(LL, Tape),
        is_valid_tape(Tape),

        % The configuration of the machine is determined by the state of the 
        % control and the configuration of the tape - this is a formal matter 
        % of an element of the set Q × {γ∆ω | γ ∈ Γ∗} × N.
        ( !, run('S', Tape, 0, 0, 10000, []) ->
            true
        ; writeln('Error: Turing Machine stopped abnormally or looped.'),
          exitCode(abnormal_looping, Code),
          retract_all_dynamic,
          halt(Code)
        ),

        retract_all_dynamic,

		halt.
