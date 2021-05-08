% This program plays the minesweeper game. In the interactive version, the
% human supplies information about the mines and the computer tries to deduce
% the positions of the mines. In the non-interactive version the computer
% deduces the position of mines from the information given.

% | ?- mine(m0, B).
% 
% B = [[1,0,0,1],[0,0,1,1]] ? ;
% 
% no
% | ?- mine(m1, B).
% 
% B = [[1,0,1,0],[1,0,0,0],[0,0,0,0],[0,1,0,1]] ? ;
% 
% no
% | ?- 

:- use_module(library(clpb)).
:- use_module(library(lists)).


% sample problems:
% problem(Id, task(Rows,Cols,Mines,[i(R,C,NoMines),...])).
problem(m0, task(2,4,4,[i(1,3,3),i(2,1,1)])).
problem(m1, task(4,4,5,
	      [i(1,2,3),i(2,3,1),i(3,1,2),i(4,3,2)])).
problem(m9, task(8,8,13,
	      [i(1,3,1),i(1,5,2),i(2,4,1),i(2,7,1),
	       i(3,2,1),i(3,6,1),i(4,6,3),i(5,4,6),
	       i(6,2,3),i(6,8,4),i(7,4,1),i(7,6,1),
	       i(8,3,1),i(8,8,1)])).

% Interactive version:
mine(Rows, Cols, Mines, Bd) :-
	length(Bd, Rows), all_length(Bd, Cols), append(Bd, All),
	sat(card([Mines], All)), play_mine(Bd, []).

all_length([], _).
all_length([L|Ls], Len) :- length(L, Len), all_length(Ls, Len).

play_mine(Bd, Asked) :- select_field(Bd, Asked, R, C, E), !,
	format('Row ~w, col ~w (m for mine)? ', [R,C]), read(Ans), 
	process_ans(Ans, E, R, C, Bd), play_mine(Bd, [R-C|Asked]).
play_mine(_Bd, _Asked).

select_field(Bd, Asked, R, C, E) :-
	nth1(R, Bd, L), nth1(C, L, E), nonmember(R-C, Asked), taut(E, 0), !.
select_field(Bd, _Asked, R, C, E) :-
	nth1(R, Bd, L), nth1(C, L, E), var(E), !.

process_ans(m, 1, _, _, _) :- format('Mine!~n', []), !, fail.
process_ans(Ans, 0, R, C, Bd) :-
	integer(Ans), neighbs(n(R, C, Bd), Ns), sat(card([Ans], Ns)).

neighbs(RCB, N7) :-
	neighbour(-1,-1, RCB, [], N0), neighbour(-1, 0, RCB, N0, N1),
	neighbour(-1, 1, RCB, N1, N2), neighbour( 0,-1, RCB, N2, N3),
	neighbour( 0, 1, RCB, N3, N4), neighbour( 1,-1, RCB, N4, N5),
	neighbour( 1, 0, RCB, N5, N6), neighbour( 1, 1, RCB, N6, N7).

neighbour(ROf, COf, n(R0, C0, Bd), Nbs, [E|Nbs]) :-
	R is R0+ROf, C is C0+COf, nth1(R, Bd, Row), nth1(C, Row, E), !.
neighbour(_, _, _, Nbs, Nbs).

% Non-interactive version:

mine(task(Rows,Cols,Mines,Infos), Bd) :- !,
	length(Bd, Rows), all_length(Bd, Cols), append(Bd, All),
	sat(card([Mines], All)), 
	mine_infos(Infos, Bd),
	labeling(All).
mine(PId, Bd) :-
	problem(PId, Task), mine(Task, Bd).

mine_infos([], _).
mine_infos([i(R,C,M)|Infos], Bd) :-
	nth1(R, Bd, Row), nth1(C, Row, 0),
	neighbs(n(R, C, Bd), Ns), sat(card([M], Ns)),
	mine_infos(Infos, Bd).

time(Task, _T) :-
	statistics(runtime, _),
	mine(Task, B),
	(   member(R, B), write(R), nl, fail
	;   nl, fail
	).
time(_Task, T) :-
	statistics(runtime, [_,T]).
