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
cut_whitespaces([], []).
cut_whitespaces([' '|T], Res) :-
    cut_whitespaces(T, Res).
cut_whitespaces([H|T], [H|Res]) :-
    cut_whitespaces(T, Res).

/** odstrani ze list of lists mezery */
cut_whitespaces_ll([], []).
cut_whitespaces_ll([H|T], [HRes|TRes]) :-
    cut_whitespaces(H, HRes),
    cut_whitespaces_ll(T, TRes).

/** Dynamic rule for the Turing Machine rules */
:- dynamic rule/4.
:- dynamic visitedConf/3.

is_visited_conf(InnerState, Tape, HeadPosition) :-
    visitedConf(InnerState, Tape, HeadPosition).

add_conf(InnerState, Tape, HeadPosition) :-
    assertz(visitedConf(InnerState, Tape, HeadPosition)).

accepts(InnerState) :-
    InnerState == 'F'.

init_head_position(0).

add_rule([InnerState, TapeSymbol, NextState, NewTapeSymbol]) :-
    assertz(rule(InnerState, TapeSymbol, NextState, NewTapeSymbol)).

/** prida vsechna pravidla ze seznamu pravidel */
add_rules_from_list([]).
add_rules_from_list([H|T]) :-
    add_rule(H),
    add_rules_from_list(T).

retract_all_dynamic :-
    retractall(rule(_,_,_,_)),
    retractall(visitedConf(_,_,_)).

replace_symbol([], _, _, []).
replace_symbol(Tape, 'L', _, UpdatedTape) :-
    UpdatedTape = Tape.
replace_symbol(Tape, 'R', _, UpdatedTape) :-
    UpdatedTape = Tape. 
replace_symbol(Tape, NewTapeSymbol, HeadPosition, UpdatedTape) :-
    nth0(HeadPosition, Tape, _, Rest),
    nth0(HeadPosition, UpdatedTape, NewTapeSymbol, Rest).

% TODO mozna nekde kontrola, zda nejsme mimo pasku uz
update_head_position(Current, 'L', New) :-
    New is Current - 1.
update_head_position(Current, 'R', New) :-    
    New is Current + 1.
update_head_position(Current, _, New) :-
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
write_confs([H|T]) :-
    list_2_str(H, HStr),
    writeln(HStr),
    write_confs(T).

write_all_confs :-
    findall(TapeWState, (
        visitedConf(InnerState, Tape, HeadPosition),
        add_state_to_tape(InnerState, Tape, HeadPosition, TapeWState)
    ), Confs),
    write_confs(Confs).

/** vrati seznam bez posledniho prvku (jako init funkce z Haskellu) */
% https://stackoverflow.com/a/16175064
init([_], []).
init([H|T], [H|Res]) :-
    init(T, Res).

last([X], X).
last([_|T], Res) :-
    last(T, Res).

% TODO jeslti nekde pouzit !
run(InnerState, Tape, HeadPosition, _, _) :-
    accepts(InnerState),
    add_conf(InnerState, Tape, HeadPosition).
run(InnerState, Tape, HeadPosition, Depth, MaxDepth) :-
    Depth < MaxDepth,
    not(is_visited_conf(InnerState, Tape, HeadPosition)),
    add_conf(InnerState, Tape, HeadPosition),
    nth0(HeadPosition, Tape, TapeSymbol),
    %once(rule(InnerState, TapeSymbol, NextState, NewTapeSymbol)),
    rule(InnerState, TapeSymbol, NextState, NewTapeSymbol),
    replace_symbol(Tape, NewTapeSymbol, HeadPosition, UpdatedTape),
    update_head_position(HeadPosition, NewTapeSymbol, NewHeadPosition),
    NewDepth is Depth + 1,
    run(NextState, UpdatedTape, NewHeadPosition, NewDepth, MaxDepth).
run(InnerState, Tape, HeadPosition, _, _) :-
    not(accepts(InnerState)),
    is_visited_conf(InnerState, Tape, HeadPosition),
    retract(visitedConf(InnerState, Tape, HeadPosition)),
    fail.

start :-
        prompt(_, ''),
        read_lines(LL),

        % Preprocces rules and tape.

        % Rules
        init(LL, Rules),
        cut_whitespaces_ll(Rules, RulesNoWhitespace),
        add_rules_from_list(RulesNoWhitespace),

        % Tape
        last(LL, Tape),

        %write(RulesNoWhitespace),
        %write(Tape),

        % The configuration of the machine is determined by the state of the 
        % control and the configuration of the tape - this is a formal matter 
        % of an element of the set Q × {γ∆ω | γ ∈ Γ∗} × N.
        
        % TODO mozna mohou i nektere z toho byt reprezentovany jako dynamic (v podstate by se to dalo aplikovat na vsechny tri, 
        % ale asi je hloupost to mit, protoze vzdy u kazdyho muze byt pouze jeden predikat - jeden stav, ...)

        % Mozna ale by nebylo spatny neco jako uchovavat vsechny tri dohromady, abych pak byl schopny detekovat zacykleni.

        run('S', Tape, 0, 0, 1000),

        write_all_confs,

        retract_all_dynamic,

		halt.


/** rozdeli radek na podseznamy */
%split_line([],[[]]) :- !.
%split_line([' '|T], [[]|S1]) :- !, split_line(T,S1).
%split_line([32|T], [[]|S1]) :- !, split_line(T,S1).    % aby to fungovalo i s retezcem na miste seznamu
%split_line([H|T], [[H|G]|S1]) :- split_line(T,[G|S1]). % G je prvni seznam ze seznamu seznamu G|S1


/** vstupem je seznam radku (kazdy radek je seznam znaku) */
%split_lines([],[]).
%split_lines([L|Ls],[H|T]) :- split_lines(Ls,T), split_line(L,H).

/** nacte zadany pocet radku */
%read_lines2([],0).
%read_lines2(Ls,N) :-
%	N > 0,
%	read_line(L,_),
%	N1 is N-1,
%	read_lines2(LLs, N1),
%	Ls = [L|LLs].


/** vypise seznam radku (kazdy radek samostatne) */
%write_lines2([]).
%write_lines2([H|T]) :- writeln(H), write_lines2(T). %(writeln je "knihovni funkce")


/** rozdeli radek na podseznamy -- pracuje od konce radku */
%zalozit prvni (tzn. posledni) seznam:
%split_line2([],[[]]) :- !.
%pridat novy seznam:
%split_line2([' '|T], [[]|S1]) :- !, split_line2(T,S1).
%pridat novy seznam, uchovat oddelujici znak:
%split_line2([H|T], [[],[H]|S1]) :- (H=','; H=')'; H='('), !, split_line2(T,S1).
%pridat znak do existujiciho seznamu:
%split_line2([H|T], [[H|G]|S1]) :- split_line2(T,[G|S1]).


/** pro vsechny radky vstupu udela split_line2 */
% vstupem je seznam radku (kazdy radek je seznam znaku)
%split_lines2([],[]).
%ssplit_lines2([L|Ls],[H|T]) :- split_lines2(Ls,T), split_line2(L,H).


/** nacte N radku vstupu, zpracuje, vypise */
%start2(N) :-
%		prompt(_, ''),
%		read_lines2(LL, N),
%		split_lines2(LL,S),
%		write_lines2(S).


/** prevede retezec na seznam atomu */
% pr.: string("12.35",S). S = ['1', '2', '.', '3', '5'].
%retezec([],[]).
%retezec([H|T],[C|CT]) :- atom_codes(C,[H]), retezec(T,CT).


/** prevede seznam cislic na cislo */
% pr.: cislo([1,2,'.',3,5],X). X = 12.35
%cislo(N,X) :- cislo(N,0,X).
%cislo([],F,F).
%cislo(['.'|T],F,X) :- !, cislo(T,F,X,10).
%cislo([H|T],F,X) :- FT is 10*F+H, cislo(T,FT,X).
%cislo([],F,F,_).
%cislo([H|T],F,X,P) :- FT is F+H/P, PT is P*10, cislo(T,FT,X,PT).


/** existuje knihovni predikat number_chars(?Number, ?CharList) */
% pr.: number_chars(12.35, ['1', '2', '.', '3', '5']).
