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
%        nth0(0,D1,A),
%        (
%            integer(A) ->
%            nth0(I, M, Sor),
%            global_cardinality(Sor, [A-0,_-_,])
%%            nth0(J, M)
%        ;
%
%        )
        length(M, K),
        C1 is C + 1,
        I1 is truncate(C1/K),
        J1 is C1 mod K,
        apply_dist(M,N,D,I1,J1,C1).


nth0_mtx(M,I,J,E):-
    nth0(I,M,L),
    nth0(J,L,E).

dist_to_be(E1, E, N) +:
    E1 in (dom(E) + N) \/ (dom(E) - N),
    E in (dom(E1) + N) \/ (dom(E1) - N).
%    E1 in ({E + N} \/ {E - N}) ,
%    E in ({E1 + N} \/ {E1 - N}).
dist_not_be(E1, E, N) +:
    E1 in \((dom(E) + N) \/ (dom(E)- N)) ,
    E in \((dom(E1) + N) \/ (dom(E1) - N)).


dist([],E1,E2,E3,N):-
    dist_not_be(E1, E2, N),
    dist_not_be(E1, E3, N).
dist([s],E1,E2,E3,N):-
    dist_to_be(E1, E2, N),
    dist_not_be(E1, E3, N).
dist([s,w],E1,E2,E3,N):-
    dist_to_be(E1, E2, N),
    dist_to_be(E1, E3, N).
dist([w],E1,E2,E3,N):-
    dist_not_be(E1, E2, N),
    dist_to_be(E1, E3, N).
dist([A|Tail],E1,E2,E3,N):-
    integer(A),
    E1 #= A,
    dist(Tail, E1, E2, E3, N).


sudoku(s(N,T), Rows) :-
    print(T),nl,
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

    labeling([ff, value(x)], Vs).
%    maplist(portray_clause, FlatList),nl.
%labelling([value(x)],list),
x(X, Rest, BB0, BB):-
	BB0 = BB,
%	fd_dom(X, X'),
%	fdset_to_list(X', X'')
	%fdset_member
%	[X']
	shave_var(X),
	labelling([], [X]).
%	x(X, _,_,_)

shave_var(V):-
	fd_dom(V, Dom),
	fdset_member(C, Dom),
	( \+ V = C -> V #\= C % meghiusul
	; fail
	), !.
shave_var(_).

%filter_var([], [], _).
%filter_var([V|Vs], Fs, N) :-
%    integer(V), !, filter_var(Vs, Fs, N).
%filter_var([V|Vs], [V|Fs], N) :-
%    (  \+ (V = N) -> V #\= N
%    ;
%    ), filter_var(Vs, Fs, N).




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
sudoku(s(3, [[[4,s],[2],[],[s]],[[1],[],[2],[]],[[3],[4,s],[s,w],[]],[[],[],[w],[]]]
                        ), Table2),
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
