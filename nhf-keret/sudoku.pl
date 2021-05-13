-module(sudoku).
-export([ sudoku/2]).
:- use_module(library(clpfd)), use_module(library(lists)).


apply_dist(M,_,_,_,_,C):-
        length(M, K),
        C is K*K.
apply_dist(M,N,D,I,J,C):-
        length(M,L),
        nth0_mtx(M,I,J,E1),
        L0 is L - 1,
        I0 is I + 1,
        nth0_mtx(D,I,J,D1),
        (
            J > 0 ->
            J0 is J - 1,
            nth0_mtx(M,I,J0,E3),
            (
                I < L0 ->
                nth0_mtx(M,I0,J,E2),
                dist(D1,E1,E2,E3,N)
            ;
                dist(D1,E1,E1,E3,N)
            )
        ;
            (
                I < L0 ->
                nth0_mtx(M,I0,J,E2),
                dist(D1,E1,E2,E1,N)
            ;
                dist(D1,E1,E1,E1,N)
            )
        ),
        length(M, K),
        C1 is C + 1,
        I1 is truncate(C1/K),
        J1 is C1 mod K,
        apply_dist(M,N,D,I1,J1,C1).


nth0_mtx(M,I,J,E):-
    nth0(I,M,L),
    nth0(J,L,E).

dist_to_be(E1, E, N) +:
    E1 in ({E + N} \/ {E - N}),
    E in ({E1 + N} \/ {E1 - N}).
dist_not_be(E1, E, N) +:
    E1 in \({E + N} \/ {E - N}),
    E in \({E1 + N} \/ {E1 - N}).

%dist_equals(E1, A)+:
%    E1 in {A}.

dist([],E1,E2,E3,N):-
    dist_not_be(E1, E2, N),
    dist_not_be(E2, E1, N),
    dist_not_be(E1, E3, N),
    dist_not_be(E3, E1, N).
dist([s],E1,E2,E3,N):-
    dist_to_be(E1, E2, N),
    dist_to_be(E2, E1, N),
    dist_not_be(E1, E3, N),
    dist_not_be(E3, E1, N).
dist([s,w],E1,E2,E3,N):-
    dist_to_be(E1, E2, N),
    dist_to_be(E2, E1, N),
    dist_to_be(E1, E3, N),
    dist_to_be(E3, E1, N).
dist([w],E1,E2,E3,N):-
    dist_not_be(E1, E2, N),
    dist_not_be(E2, E1, N),
    dist_to_be(E1, E3, N),
    dist_to_be(E3, E1, N).
dist([A|Tail],E1,E2,E3,N):-
    integer(A),
    E1 #= A,
    dist(Tail, E1, E2, E3, N).


sudoku(s(N,T), Rows) :-
%    print(T),nl,
    length(T, L),
    length(Rows, L),
    maplist(same_length(Rows), Rows),
    append(Rows, Vs),
    domain(Vs, 1, L),

    maplist(all_distinct, Rows),

    transpose(Rows, Columns),
    maplist(all_distinct, Columns),

    cella(Rows, 1),

    apply_dist(Rows,N,T,0,0,0),

    labeling([ff, enum], Vs).
%    maplist(portray_clause, FlatList),nl.


get_element([H|_],0, H).
get_element([_|T], I, L) :-
	I1 is I-1,
	get_element(T, I1, L).

get_result_list(_,_,_,K,L,IC,_,L):-
	IC is K*K.
get_result_list(M,I,J,K,L,IC,JC,LR):-
	I1 is I + IC mod K,
	get_element(M,I1,L1),
	get_element(L1,JC,E1),
	append(L,[E1],L2),

	IC1 is IC + 1,
	JC1 is J + div(IC1,K),
	get_result_list(M,I,J,K,L2,IC1,JC1,LR).

cella(S,I):-
    length(S, L),
    I > L.
cella(S,I):-
	length(S,K),
	K1 is truncate(sqrt(K)),
	X is truncate((I-1)/K1)*K1,
	Y is ((I-1-(truncate((I-1)/K1))*K1)*K1),
	get_result_list(S,X,Y,K1,[],0,Y, C),
	all_distinct(C),
	I1 is I + 1,
	cella(S, I1).
