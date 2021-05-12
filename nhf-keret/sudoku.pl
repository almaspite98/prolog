/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   Sudoku CLP(Z) formulation.
   Written Feb. 2008 by Markus Triska  (triska@metalevel.at)
   Public domain code.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
-module(sudoku).
-export([ sudoku/2]).
:- use_module(library(clpfd)), use_module(library(lists)).



apply_dist(M,_,_,_,_,C):-
    length(M, K),
    C is K*K.
apply_dist(M,N,D,I,0,C):-
    length(M,L),
    L0 is L - 1,
    I == L0,
    nth0_mtx(M,I,0,E1),
%    E2 is E1 + N,
%    E3 is E1 + N,
    nth0_mtx(D,I,0,D1),

    dist(D1,E1,E1,E1,N),

    length(M, K),
    C1 is C + 1,
    I1 is truncate(C1/K),
    J1 is C1 mod K,
    apply_dist(M,N,D,I1,J1,C1).
apply_dist(M,N,D,I,J,C):-

    length(M,L),
    L0 is L - 1,
    I == L0,
    nth0_mtx(M,I,J,E1),
%    E3 is E1 + N,
    J0 is J-1,
    nth0_mtx(M,I,J0,E3),
    nth0_mtx(D,I,J,D1),

    dist(D1,E1,E1,E3,N),

    length(M, K),
    C1 is C + 1,
    I1 is truncate(C1/K),
    J1 is C1 mod K,
    apply_dist(M,N,D,I1,J1,C1).
apply_dist(M,N,D,I,0,C):-

    nth0_mtx(M,I,0,E1),
%    E3 is E1 + N,
    I0 is I + 1,
    nth0_mtx(M,I0,0,E2),
    nth0_mtx(D,I,0,D1),
    dist(D1,E1,E2,E1,N),


    length(M, K),
    C1 is C + 1,
    I1 is truncate(C1/K),
    J1 is C1 mod K,
    apply_dist(M,N,D,I1,J1,C1).
apply_dist(M,N,D,I,J,C):-
    J > 0,
    length(M,L),
    L0 is L - 1,
    I < L0,

nth0_mtx(M,I,J,E1),
    I0 is I + 1,
    nth0_mtx(M,I0,J,E2),
    J0 is J - 1,
    nth0_mtx(M,I,J0,E3),
    nth0_mtx(D,I,J,D1),
    dist(D1,E1,E2,E3,N),

%
    length(M, K),
    C1 is C + 1,
    I1 is truncate(C1/K),
    J1 is C1 mod K,
    apply_dist(M,N,D,I1,J1,C1).



nth0_mtx(M,I,J,E):-
    nth0(I,M,L),
    nth0(J,L,E).

%dist_contr(E1,E2,N):-
%    abs(E1-E2) #= N.
%

dist([],E1,E2,E3,N):-
    abs(E1-E2) #\= N,
    abs(E1-E3) #\= N.
dist([s],E1,E2,E3,N):-
    abs(E1-E2) #= N,
    abs(E1-E3) #\= N.
dist([s,w],E1,E2,E3,N):-
    abs(E1-E2) #= N,
    abs(E1-E3) #= N.
dist([w],E1,E2,E3,N):-
    abs(E1-E3) #= N,
    abs(E1-E2) #\= N.
dist([A],E1,E2,E3,N):-
    integer(A),
    E1 is A,
    abs(A-E2) #\= N,
    abs(A-E3) #\= N.
dist([A,s],E1,E2,E3,N):-
    integer(A),
    E1 is A,
    abs(A-E2) #= N,
    abs(A-E3) #\= N.
dist([A,w],E1,E2,E3,N):-
    integer(A),
    E1 is A,
    abs(A-E3) #= N,
    abs(A-E2) #\= N.
dist([A,s,w],E1,E2,E3,N):-
    integer(A),
    E1 is A,
    abs(A-E2) #= N,
    abs(A-E3) #= N.


sudoku(s(N,T), Rows) :-
    print(T),
    length(T, L),
    length(Rows, L), maplist(same_length(Rows), Rows),
    append(Rows, Vs),
    domain(Vs, 1, L),
%        Vs in 1..9,
    maplist(all_distinct, Rows),
    transpose(Rows, Columns),
    maplist(all_distinct, Columns),
    cella(Rows, 1),
    apply_dist(Rows,N,T,0,0,0),
    maplist(labeling([ff]), Rows),
    maplist(portray_clause, Rows).


%problem(M,L):-
%    length(M,L),

blocks([], []).
blocks([N1,N2|Ns1], [N3,N4|Ns2]) :-
        all_distinct([N1,N2,N3,N4]),
        blocks(Ns1, Ns2).



goal(X,Z,Y):-
      Y #= X+Z.
%      Y is X + 2.
%    abs(X-Y) #= 3.


get_element([H|_],0, H).
get_element([_|T], I, L) :-
	I1 is I-1,
	get_element(T, I1, L).

get_result_list(M,I,J,K,L,IC,JC,L):-
	IC is K*K.
get_result_list(M,I,J,K,L,IC,JC,LR):-
	I1 is I + IC mod K,
	get_element(M,I1,L1),
	get_element(L1,JC,E1),
	append(L,[E1],L2),

	IC1 is IC + 1,
	JC1 is J + truncate(IC1/K),
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
