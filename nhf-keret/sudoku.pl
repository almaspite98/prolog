-module(sudoku).
-export([ sudoku/2]).
:- use_module(library(clpfd)), use_module(library(lists)).

%:-asserta(clpfd:full_answer).  % only necessary for SICStus

%apply_dist(M,_,_,_,_,C):-
%    length(M, K),
%    C is K*K,
%    print('DONE'),nl.
%apply_dist(M,N,D,I,J,C):-
%    length(M,L),
%    L0 is L - 1,
%    nth0_mtx(M,I,J,E1),
%    I0 is I + 1,
%    nth0_mtx(M,I0,J,E2),
%    J0 is J - 1,
%    nth0_mtx(M,I,J0,E3),
%    nth0_mtx(D,I,J,D1),
%    (
%        J>0 ->
%        (
%            I < L0 ->
%            print('00: '),nl,
%            print(I),print(' '),print(J),print(' '),print(C),nl,
%            dist(D1,E1,E2,E3,N)
%        ;
%%            I == L0,
%            print('10: '),nl,
%            print(I),print(' '),print(J),print(' '),print(C),nl,
%            dist(D1,E1,E1,E3,N)
%        )
%    ;
%%        J==0,
%        (
%            I < L0 ->
%            print('01: '),nl,
%            print(I),print(' '),print(0),print(' '),print(C),nl,
%            dist(D1,E1,E2,E1,N)
%        ;
%%            I == L0,
%            print('11: '),nl,
%            print(I),print(' '),print(0),print(' '),print(C),nl,
%            dist(D1,E1,E1,E1,N)
%        )
%    ),
%    length(M, K),
%    C1 is C + 1,
%    I1 is truncate(C1/K),
%    J1 is C1 mod K,
%    apply_dist(M,N,D,I1,J1,C1).

apply_dist(M,_,_,_,_,C):-
    length(M, K),
    C is K*K.
apply_dist(M,N,D,I,0,C):-
    length(M,L),
    L0 is L - 1,
    I == L0,
    nth0_mtx(M,I,0,E1),
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
    length(M, K),
    C1 is C + 1,
    I1 is truncate(C1/K),
    J1 is C1 mod K,
    apply_dist(M,N,D,I1,J1,C1).

nth0_mtx(M,I,J,E):-
    nth0(I,M,L),
    nth0(J,L,E).

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

%    statistics(walltime, [TimeSinceStart2 | [TimeSinceLastCall2]]),
    maplist(all_distinct, Rows),
%    statistics(walltime, [NewTimeSinceStart2 | [ExecutionTime2]]),
%    write('Execution took '), write(ExecutionTime2), write(' ms.'), nl,

%    statistics(walltime, [TimeSinceStart3 | [TimeSinceLastCall3]]),
    transpose(Rows, Columns),
    maplist(all_distinct, Columns),
%    statistics(walltime, [NewTimeSinceStart3 | [ExecutionTime3]]),
%    write('Execution took '), write(ExecutionTime3), write(' ms.'), nl,


%    statistics(walltime, [TimeSinceStart | [TimeSinceLastCall]]),
    cella(Rows, 1),
%    statistics(walltime, [NewTimeSinceStart | [ExecutionTime]]),
%    write('Execution took '), write(ExecutionTime), write(' ms.'), nl,

%    statistics(walltime, [TimeSinceStart4 | [TimeSinceLastCall4]]),
    apply_dist(Rows,N,T,0,0,0),
%    statistics(walltime, [NewTimeSinceStart4 | [ExecutionTime4]]),
%    write('Execution took '), write(ExecutionTime4), write(' ms.'), nl,

%    fd_statistics(constraints, Result),
%    print('Result: '),print(Result),nl,
    labeling([ff], Vs).
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
