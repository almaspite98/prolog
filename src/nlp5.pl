-module(khf5).
-export().
:- use_module(library(clpfd)), use_module(library(lists)).

%dist_to_be(E1, E, N) +:
%    E1 in ({E + N} \/ {E - N}) ,
%    E in ({E1 + N} \/ {E1 - N}).
%dist_not_be(E1, E, N) +:
%    E1 in \({E + N} \/ {E - N}) ,
%    E in \({E1 + N} \/ {E1 - N}).


'z>max(x,y)'(X,Y,Z) +:
    Z in (((min(X)+1)..sup) /\ ((min(Y)+1)..sup)),
    Y in ( (( min(X)..sup) /\  (inf..(max(Z)-1))) ? (inf..(max(Z)-1)) ),
    X in ( (( min(Y)..sup) /\  (inf..(max(Z)-1))) ? (inf..(max(Z)-1)) ).
'z>max(x,y)'(X,Y,Z) -:
    Z in (inf..max(X)) \/ (inf..max(Y)),
    Y in ((min(Z)..sup)) \/ ( ((inf..max(X)) /\  (min(Z)..sup)) ? (inf..sup) ),
    X in ((min(Z)..sup)) \/ ( ((inf..max(Y)) /\  (min(Z)..sup)) ? (inf..sup) ).
'z>max(x,y)'(X,Y,Z) +?
    Z in (((max(X)+1)..sup) /\ ((max(Y)+1)..sup)).
'z>max(x,y)'(X,Y,Z) -?
   Z in (inf..min(X)) \/ (inf..min(Y)).

%pred(X,Y,Z) +:
%    Z in max(max(X) \/ max(Y))..max(Z).
%
%pred1(X, Y) +:
%    Y in (inf .. max(X)) \/ (inf .. (-min(X)),
%
%    X in (inf .. Y) \/ (inf .. -Y).
%
%pred1(X,Y) -:
%    Y in
%
%pred(X, Y) +:
%    Y in (inf .. X) \/ (inf .. -X),
%    X in

nullsoros([]):-
    print('Done').
nullsoros([H|T]):-
    H #> 0,
    rule(H, T),
    nullsoros(T).


rule0(0,_).
rule1(1,[H1,H2|T]):-
    H1 #= 0,
    H2 #\=0.
%rule1(1, [H1]):- fail.
rule(A, [H|T]):-
%    print(A),
    A #> 1 #<=> H #= 0 #/\ A1 #= A - 1,
    A #= 1 #<=> A1 #=1,
    A #= 0 #<=> A1 #= 0,
    (
    A1 == 1,
    rule1(1,T)
    ;
    A1 == 0,
    rule0(0,T)
    ;
    A1 > 1,
    rule(A1, T)
    ).


main :-

%    domain([X,Y,Z], 1,10),
%    X #< 6,
%    Y #< 8,
%    pred(X, Y, Z),
%    fd_mon(Z, Min),
%    print(Min),nl,
    L = [0,_,0,1,B],
    domain(L,0,3),
    rule(3,L),labeling([ff], L), print(L),nl,fail,

%    length(L,3), nullsoros(L), labeling([ff], L), print(L),nl,fail,
    nl.