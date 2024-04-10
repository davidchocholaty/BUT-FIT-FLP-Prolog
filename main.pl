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

add_rule([InnerState, TapeSymbol, NextState, NewTapeSymbol]) :-
    assertz(rule(InnerState, TapeSymbol, NextState, NewTapeSymbol)).

/** prida vsechna pravidla ze seznamu pravidel */
add_rules_from_list([]).
add_rules_from_list([H|T]) :-
    add_rule(H), add_rules_from_list(T).

retract_all_dynamic :-
    retractall(rule(_,_,_,_)).

/** vrati seznam bez posledniho prvku (jako init funkce z Haskellu) */
% https://stackoverflow.com/a/16175064
init([_], []).
init([H|T], [H|Res]) :-
    init(T, Res).

last([X], X).
last([_|T], Res) :-
    last(T, Res).

start :-
		prompt(_, ''),
		read_lines(LL),

        % Preprocces rules and tape.

        % Rules
        init(LL, Rules),
        cut_whitespaces_ll(Rules, RulesNoWhitespace),

        % Tape
        last(LL, Tape),

        write(RulesNoWhitespace),
        write(Tape),

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
