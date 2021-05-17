-module(b2).
-export([prodp/1 ]).
:- use_module(library(clpfd)), use_module(library(lists)).


prodp([H|T]):-
%    maplist(positive_multip(H),T),
    count_neg([H|T],0,C),
    C mod 2 #= 0.

count_neg([],C_in, C_out):-
    C_out #= C_in.
count_neg([H|T], C_in, C_out):-
    H #\= 0,
    H #< 0 #<=> B,
    C_in1 #= C_in + B,
    count_neg(T, C_in1, C_out).

%positive_multip(A,B):-
%    A #\=0, B #\=0,
%    A #> 0 #<=> B #> 0.

%egyneg(L) :-
%minmax_susps(L, S),
%fd_global(egyneg(L), L, S).
%:- multifile clpfd:dispatch_global/4.
%clpfd:dispatch_global(egyneg(_), L0, L, Actions) :-
%egyneg_megoldo(L0, L, Actions).
%egyneg_megoldo(L0, L, Actions) :-
%szuro(L0, L, 0, Neg),
%( Neg = 1 -> Actions = [exit|Acts0], allnonneg(L, Acts0)
%; Neg = 0 ->
%( L = [] -> Actions = [fail]
%; L = [X] -> Actions = [exit,X in inf.. -1]
%; Actions = []
%)
%; Actions = [fail]
%).




main :-
%    domain([A,B],-2,2),
%    positive_multip(A,B), labeling([ff], [A,B]), write(A - B),nl,fail,
%    count_neg([A,B],0,C),labeling([ff], [A,B]), write(A : B : C),nl,fail,
%    prodp([A,B]),
    prodp([A,B,C]), A in 1..10, B in 5..7,
    labeling([ff], [A,B,C]), write(A : B),nl,fail,
    nl.