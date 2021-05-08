%---------------------------------------------------------------------------

:- use_module(library(clpb)).

% | ?- sat(X + Y).                            sat(X=\=_A*Y#Y) ? 
% 
% | ?- sat(x + Y).                            sat(Y=\=_A*x#x) ? 
% 
% | ?- taut(_A ^ (X=\=_A*Y#Y) =:= X+Y, T).    T = 1 ? 
% 
% | ?- sat(A # B =:= 0).                      B = A ? 
% 
% | ?- sat(A # B =:= C), A = B.               B = A, C = 0 ? 
% 
% | ?- taut(A =< C, T).                       no
% 
% | ?- sat(A =< B), sat(B =< C), taut(A =< C, T).
%                                             T = 1,
%                                             sat(A=:=_A*_B*C),
%                                             sat(B=:=_B*C) ? 
% 
% | ?- taut(X, T).                            no
% 
% | ?- taut(x, T).                            T = 0 ? 
% 
% | ?- taut(~x, T).                           T = 0 ? 

%---------------------------------------------------------------------------

adder(X, Y, Sum, Cin, Cout) :-
     sat(Sum =:= card([1,3],[X,Y,Cin])),
     sat(Cout =:= card([2-3],[X,Y,Cin])).


% | ?- adder(x, y, Sum, cin, Cout).
% 
% sat(Sum=:=cin#x#y),
% sat(Cout=:=x*cin#x*y#y*cin) ?
% 
% yes
% | ?- adder(x, y, Sum, 0, Cout).
% 
% sat(Sum=:=x#y),
% sat(Cout=:=x*y) ?
% 
% yes
% | ?- adder(X, Y, 0, Cin, 1), labeling([X,Y,Cin]).
% 
% Cin = 0, X = 1, Y = 1 ? ; 
% 
% Cin = 1, X = 0, Y = 1 ? ;
% 
% Cin = 1, X = 1, Y = 0 ? ;
% 
% no

%---------------------------------------------------------------------------
fault([F1,F2,F3,F4,F5], [X,Y,Cin], [Sum,Cout]) :-
        sat(
                    card([0-1],[F1,F2,F3,F4,F5]) *
                    (F1 + (U1 =:= X * Cin)) *
                    (F2 + (U2 =:= Y * U3)) *
                    (F3 + (Cout =:= U1 + U2)) *
                    (F4 + (U3 =:= X # Cin)) *
                    (F5 + (Sum =:= Y # U3))
                ).

fault_ex(1, Faults) :- fault(Faults, [1,1,0], [1,0]).
fault_ex(2, Faults) :- fault(Faults, [1,0,1], [0,0]).

% | ?- fault(L, [1,1,0], [1,0]).
% L = [0,0,0,1,0] ? ;
% no
% | ?- fault(L, [1,0,1], [0,0]).
% L = [_A,0,_B,0,0],
% sat(_A=\=_B) ? ;
% no
% | ?- fault(L, [1,0,1], [0,0]), labeling(L).
% L = [1,0,0,0,0] ? ;
% L = [0,0,1,0,0] ? ;
% no
% | ?- fault_ex(I,L), labeling(L).
% I = 1, L = [0,0,0,1,0] ? ;
% I = 2, L = [1,0,0,0,0] ? ;
% I = 2, L = [0,0,1,0,0] ? ;
% no
% | ?- fault([0,0,0,0,0], [x,y,cin], [Sum,Cout]).
% sat(Cout=:=x*cin#x*y#y*cin),
% sat(Sum=:=cin#x#y)


%---------------------------------------------------------------------------

n(D, G, S) :-    % Gate => Drain = Source
        sat( G*D =:= G*S).

p(D, G, S) :-    % ~ Gate => Drain = Source
        sat( ~G*D =:= ~G*S).

xor(A, B, Out) :-
        p(1, A, X),
        n(0, A, X),
        p(B, A, Out),
        n(B, X, Out),
        p(A, B, Out),
        n(X, B, Out).

% | ?- n(D, 1, S).                 S = D ? 
% 
% | ?- n(D, 0, S).                 true ? 
% 
% | ?- p(D, 0, S).                 S = D ? 
% 
% | ?- p(D, 1, S).                 true ? 
% 
% | ?- xor(a, b, X).               sat(X=:=a#b) ? 


%---------------------------------------------------------------------------

:- use_module(library(clpb)).    :- use_module(library(lists)).

mine(Rows, Cols, Mines, Bd) :-
  length(Bd, Rows), all_length(Bd, Cols), append_lists(Bd, All),
  sat(card([Mines], All)), play_mine(Bd, []).

all_length([], _).
all_length([L|Ls], Len) :- length(L, Len), all_length(Ls, Len).

append_lists([], []).
append_lists([L|Ls], Es) :- 
  append_lists(Ls, Es0), append(L, Es0, Es).

play_mine(Bd, Asked) :- select_field(Bd, Asked, R, C, E), !,
  format('Row ~w, col ~w (m for mine)? ', [R,C]), read(Ans), get_code(_),
  process_ans(Ans, E, R, C, Bd), play_mine(Bd, [R-C|Asked]).
play_mine(_Bd, _Asked).

select_field(Bd, Asked, R, C, E) :-
  nth1(R, Bd, L), nth1(C, L, E), nonmember(R-C, Asked), taut(E, 0), !.
select_field(Bd, Asked, R, C, E) :-
  nth1(R, Bd, L), nth1(C, L, E), nonmember(R-C, Asked), \+ taut(E,1), !.

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

% | ?- mine(4, 5, 5, Bd).
% Row 1, col 1 (m for mine)? 1.
% Row 1, col 2 (m for mine)? 1.
% Row 1, col 3 (m for mine)? 1.
% Row 2, col 3 (m for mine)? 3.
% Row 1, col 4 (m for mine)? 1.
% Row 1, col 5 (m for mine)? 1.
% Row 2, col 1 (m for mine)? 1.
% Row 2, col 4 (m for mine)? 3.
% Row 3, col 1 (m for mine)? 2.
% Row 3, col 2 (m for mine)? 3.
% Row 3, col 5 (m for mine)? 2.
% Row 4, col 3 (m for mine)? 2.
% Row 4, col 2 (m for mine)? 2.
% Row 4, col 4 (m for mine)? 2.
% Row 4, col 5 (m for mine)? 1.
% 
% Bd = [[0,0,0,0,0],[0,1,0,0,1],[0,0,1,1,0],[1,0,0,0,0]] ? ;
% 
% no
% | ?- 

%---------------------------------------------------------------------------
