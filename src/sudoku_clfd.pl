/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   Sudoku CLP(Z) formulation.
   Written Feb. 2008 by Markus Triska  (triska@metalevel.at)
   Public domain code.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

:- use_module(library(clpfd)), use_module(library(lists)).

apply_dist(M,_,_,_,_,C):-
    length(M, K),
    C is K*K,
    print('DONE'),nl.
apply_dist(M,N,D,I,0,C):-
    length(M,L),
    L1 is L*L,
    C < L1,
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
    L1 is L*L,
    C < L1,
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
    length(M,L),
    L1 is L*L,
    C < L1,
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
    L1 is L*L,
    C < L1,
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
    abs(E1-E2) #\= N,
    abs(E1-E3) #= N.
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
    abs(A-E2) #\= N,
    abs(A-E3) #= N.
dist([A,s,w],E1,E2,E3,N):-
    integer(A),
    E1 is A,
    abs(A-E2) #= N,
    abs(A-E3) #= N.


sudoku(s(N,T), Rows) :-
%    print(T),nl,
    statistics(walltime, [TimeSinceStart | [TimeSinceLastCall]]),
    length(T, L),
    length(Rows, L),
    apply_dist(Rows,N,T,0,0,0),
    statistics(walltime, [NewTimeSinceStart | [ExecutionTime]]),
    write('Execution took '), write(ExecutionTime), write(' ms.'), nl,
    statistics(walltime, [TimeSinceStart2 | [TimeSinceLastCall2]]),
    maplist(same_length(Rows), Rows),
    append(Rows, Vs),
    domain(Vs, 1, L),
    maplist(all_distinct, Rows),
    transpose(Rows, Columns),
    maplist(all_distinct, Columns),
    statistics(walltime, [NewTimeSinceStart2 | [ExecutionTime2]]),
    write('Execution took '), write(ExecutionTime2), write(' ms.'), nl,
    statistics(walltime, [TimeSinceStart3 | [TimeSinceLastCall3]]),
%    apply_dist(Rows,N,T,0,0,0),
    cella(Rows, 1),
%    K is truncate(sqrt(L)),
%    chunks(K, Rows, RowChunks),
%    maplist(blocks((K, K)), RowChunks),
    statistics(walltime, [NewTimeSinceStart3 | [ExecutionTime3]]),
    write('Execution took '), write(ExecutionTime3), write(' ms.'), nl,

    maplist(labeling([ff]), Rows),
    maplist(portray_clause, Rows),nl.

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


main :-
%	print('cella([[1,2,3,4],[5,6,7,8],[9,10,11,12],[13,14,15,16]], 1, C2): '),nl,
%	cella([[2,5,4,6,1,7,3,8,9],
%	       [3,9,6,5,4,8,7,1,2],
%	       [7,1,8,2,3,9,4,5,6],
%	       [5,8,7,1,2,4,9,6,3],
%	       [1,6,2,7,9,3,8,4,5],
%	       [4,3,9,8,6,5,1,2,7],
%	       [9,4,1,3,5,2,6,7,8],
%	       [8,2,3,4,7,6,5,9,1],
%	       [6,7,5,9,8,1,2,3,4]]
%, 1),nl,

%    maplist(goal,[1,2,4],[2,0,1],Ys),
%    print(Ys),nl,
%    problem(1, Rows),
%    sudoku(s(N,T), Rows),
% maplist(labeling([ff]), Rows),
%    maplist(portray_clause, Rows),nl,
%   apply_dist(M,N,D,I,J,C):-

%    apply_dist([[ 2, 4, 1, 3],
%                [ 3, 1, 4, 2],
%                [ 1, 3, 2, 4],
%                [ 4, 2, 3, 1]],1,                            [[  [s],    [],     [],    [s]],
%                                                              [   [],    [],     [],     []],
%                                                              [   [],   [s],  [s,w],     []],
%                                                              [   [4],    [],    [w],     []]],0,0,0),

%   problem(Table),
%    sudoku(s(1,  [[[s] ,[s], [s,w],  [s]],
%                 [_ , _,[s,w],  _],
%                 [ [s] , [s], [s,w], [s,w]],
%                 [_ ,[3,w],  [w],  _]]
%                        ), Table),
%sudoku(s(3, [[[4,s],[2],[],[s]],[[1],[],[2],[]],[[3],[4,s],[s,w],[]],[[],[],[w],[]]]
%                        ), Table1),
%sudoku(s(1, [[[],[w],[],[]],[[s],[],[s],[s,w]],[[],[1,w],[],[w]],[[],[w],[],[w]]]
%                        ), Table2),
%sudoku(s(1, [[[s],[],[w],[s],[],[w],[],[7],[]],
%            [[],[],[s],[2,w],[w],[],[],[w],[]],
%            [[s],[s,w],[s],[],[],[w],[],[w],[s]],
%            [[],[w],[],[s],[],[s],[],[s],[]],
%            [[s],[],[],[w],[w],[],[w],[],[]],
%            [[s],[],[w],[],[],[w],[9],[w],[w]],
%            [[s],[s],[s],[],[],[],[],[s],[s]],
%            [[],[3],[s],[s],[],[],[s],[],[]],
%            [[],[],[w],[],[],[w],[],[w],[w]]]
%                        ), Table),
%    statistics(runtime,[Start|_]),
%    M = [
%        [_,_,_,_,_,_,_,_,_],
%        [_,_,_,_,_,_,_,_,_],
%        [_,_,_,_,_,_,_,_,_],
%        [_,_,_,_,_,_,_,_,_],
%        [_,_,_,_,_,_,_,_,_],
%        [_,_,_,_,_,_,_,_,_],
%        [_,_,_,_,_,_,_,_,_],
%        [_,_,_,_,_,_,_,_,_],
%        [_,_,_,_,_,_,_,_,_]
%    ],
%    apply_dist(M, 1, [[[],[],[],[],[],[],[],[],[]],
%                         [[],[],[],[],[],[],[],[],[]],
%                         [[],[],[],[],[],[8],[],[],[]],
%                         [[],[],[],[],[9],[],[],[],[6]],
%                         [[],[],[],[],[],[],[],[],[]],
%                         [[],[],[],[],[],[],[5],[],[]],
%                         [[],[9],[],[],[],[],[],[],[]],[[],[],[],[],[],[],[],[],[]],[[],[3],[],[],[5],[],[4],[],[]]],
%                         0,0,0),
%    length(M, 9),
%    maplist(same_length(M), M),
%    append(M, Vs),
%    domain(Vs, 1, 9),
%    maplist(all_distinct, M),
%    transpose(M, Columns),
%    maplist(all_distinct, Columns),

%    cella(Rows, 1),
%    statistics(runtime,[Stop|_]),
%    Runtime is Stop - Start,
%    print('Time: '), print(Runtime),nl,
%
%    statistics(walltime, [TimeSinceStart | [TimeSinceLastCall]]),
%    apply_dist(M, 1, [[[],[],[],[],[],[],[],[],[]],
%                             [[],[],[],[],[],[],[],[],[]],
%                             [[],[],[],[],[],[8],[],[],[]],
%                             [[],[],[],[],[9],[],[],[],[6]],
%                             [[],[],[],[],[],[],[],[],[]],
%                             [[],[],[],[],[],[],[5],[],[]],
%                             [[],[9],[],[],[],[],[],[],[]],[[],[],[],[],[],[],[],[],[]],[[],[3],[],[],[5],[],[4],[],[]]],
%                             0,0,0),
%    cella(M, 1),
%%    maplist(labeling([ff]), M),
%    statistics(walltime, [NewTimeSinceStart | [ExecutionTime]]),
%    write('Execution took '), write(ExecutionTime), write(' ms.'), nl,
    sudoku(s(1, [[[],[],[],[],[],[],[],[],[]],
    [[],[],[],[],[],[],[],[],[]],
    [[],[],[],[],[],[8],[],[],[]],
    [[],[],[],[],[9],[],[],[],[6]],
    [[],[],[],[],[],[],[],[],[]],
    [[],[],[],[],[],[],[5],[],[]],
    [[],[9],[],[],[],[],[],[],[]],[[],[],[],[],[],[],[],[],[]],[[],[3],[],[],[5],[],[4],[],[]]]
                            ), Table),





%    domain([X,Y], 0, 10), dist([s],X,Y,_,2),
%    Y is 5,
%    print('X: '), print(X),nl,


    %%    E2 #= 1.
    %    dist([2,s,w],E1, E2, E3, 1).
%    domain([E2, E1], 1, 4),
%%    print('E2: '), print(E2),nl,
%    dist([s],E1, E2, 3, 1),
%%    E1 #= ,
%    print('E2: '), print(E2),nl,
%    X is 2,
%    print(X),nl,
%    print(Y),nl,
%
%    nth0_mtx([[ 2, 4, 1, 3],
%                      [ 3, 1, 4, 2],
%                      [ 1, 3, 5, 6],
%                      [ 4, 2, 3, 1]], 2,3,E),
%    print('E: '), print(E),
    nl.
