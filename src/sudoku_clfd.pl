/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   Sudoku CLP(Z) formulation.
   Written Feb. 2008 by Markus Triska  (triska@metalevel.at)
   Public domain code.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

:- use_module(library(clpfd)), use_module(library(lists)).
%:- use_module(clpz).
%TODO
%sudoku(s(N,T), Table) :-
%    print(N),nl,
%    print(T),nl,
%    print(Table),nl.


%sudoku(Rows) :-
%        length(Rows, 9), maplist(same_length(Rows), Rows),
%        append(Rows, Vs),
%        domain(Vs, 1, 9),
%%        Vs in 1..9,
%        maplist(all_distinct, Rows),
%        transpose(Rows, Columns),
%        maplist(all_distinct, Columns),
%
%        Rows = [As,Bs,Cs,Ds,Es,Fs,Gs,Hs,Is],
%        blocks(As, Bs, Cs),
%        blocks(Ds, Es, Fs),
%        blocks(Gs, Hs, Is).

%
%blocks([], [], []).
%blocks([N1,N2,N3|Ns1], [N4,N5,N6|Ns2], [N7,N8,N9|Ns3]) :-
%        all_distinct([N1,N2,N3,N4,N5,N6,N7,N8,N9]),
%        blocks(Ns1, Ns2, Ns3).




%problem(1, [[_,_,_,_,_,_,_,_,_],
%            [_,_,_,_,_,3,_,8,5],
%            [_,_,1,_,2,_,_,_,_],
%            [_,_,_,5,_,7,_,_,_],
%            [_,_,4,_,_,_,1,_,_],
%            [_,9,_,_,_,_,_,_,_],
%            [5,_,_,_,_,_,_,7,3],
%            [_,_,2,_,1,_,_,_,_],
%            [_,_,_,_,4,_,_,_,_]]).





apply_dist(M,_,_,_,_,C):-
    length(M, K),
    C is K*K,
    print('DONE'),nl.
apply_dist(M,N,D,I,0,C):-
    length(M,L),
    L0 is L - 1,
    I == L0,
    print('11: '),nl,
    print(I),print(' '),print(0),print(' '),print(C),nl,
    nth0_mtx(M,I,0,E1),
%    E2 is E1 + N,
%    E3 is E1 + N,
    nth0_mtx(D,I,0,D1),
    print(D1),nl,

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
    print('10: '),nl,
    print(I),print(' '),print(J),print(' '),print(C),nl,



    nth0_mtx(M,I,J,E1),
%    E3 is E1 + N,
    J0 is J-1,
    nth0_mtx(M,I,J0,E3),
    nth0_mtx(D,I,J,D1),
    print(D1),nl,

    dist(D1,E1,E1,E3,N),
%    print('E1: '),print(E1),nl,
%    print('E2: '),print(E2),nl,
%    print('E3: '),print(E3),nl,

    length(M, K),
    C1 is C + 1,
    I1 is truncate(C1/K),
    J1 is C1 mod K,
    apply_dist(M,N,D,I1,J1,C1).
apply_dist(M,N,D,I,0,C):-
    print('01: '),nl,
    print(I),print(' '),print(0),print(' '),print(C),nl,


    nth0_mtx(M,I,0,E1),
%    E3 is E1 + N,
    I0 is I + 1,
    nth0_mtx(M,I0,0,E2),
    nth0_mtx(D,I,0,D1),
    print(D1),nl,
    print('p1:'),nl,
    dist(D1,E1,E2,E1,N),
    print('p2:'),nl,

%    print('E1: '),print(E1),nl,
%    print('E2: '),print(E2),nl,
%    print('E3: '),print(E3),nl,

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
    print('00: '),nl,
    print(I),print(' '),print(J),print(' '),print(C),nl,


    nth0_mtx(M,I,J,E1),
    I0 is I + 1,
    nth0_mtx(M,I0,J,E2),
    J0 is J - 1,
    nth0_mtx(M,I,J0,E3),
    nth0_mtx(D,I,J,D1),
    print(D1),nl,
        print('E1: '),print(E1),nl,
        print('E2: '),print(E2),nl,
        print('E3: '),print(E3),nl,
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
    print('A'),nl,
    abs(E1-E2) #\= N,
    abs(E1-E3) #\= N.
dist([s],E1,E2,E3,N):-
    print('S'),nl,
    abs(E1-E2) #= N,
    abs(E1-E3) #\= N.
dist([s,w],E1,E2,E3,N):-
    print('F'),nl,
    abs(E1-E2) #= N,
    abs(E1-E3) #= N.
dist([w],E1,E2,E3,N):-
    print('G'),nl,
    abs(E1-E3) #= N,
    abs(E1-E2) #\= N.
dist([A],A,E2,E3,N):-
    print('B'),nl,
    abs(A-E2) #\= N,
    abs(A-E3) #\= N.
dist([A,s],A,E2,E3,N):-
    print('C'),nl,
    abs(A-E2) #= N,
    abs(A-E3) #= N.
dist([A,w],A,E2,E3,N):-
    print('D'),nl,
    abs(A-E3) #= N,
    abs(A-E2) #\= N.
dist([A,s,w],A,E2,E3,N):-
    print('E'),nl,
    abs(A-E2) #= N,
    abs(A-E3) #= N.


sudoku(s(N,T), Rows) :-

    length(Rows, 4), maplist(same_length(Rows), Rows),
    append(Rows, Vs),
    domain(Vs, 1, 4),
%        Vs in 1..9,
    maplist(all_distinct, Rows),
    transpose(Rows, Columns),
    maplist(all_distinct, Columns),

    Rows = [As,Bs,Cs,Ds],
    blocks(As, Bs),
    blocks(Cs, Ds),
%    nth0_mtx(Rows, 0,1,E1),
%    nth0_mtx(Rows, 1,1,E2),
%    nth0_mtx(Rows, 0,0,E3),
%%    E2 #= 1.
%    dist([2,s,w],E1, E2, E3, 1).
%    dist([s],E1, E2, 0, 2).
%%    dist([s],E1, E2, 0, 3),
%%    dist([s],E1, E2, 0, 3),
%%    dist([s],E1, E2, 0, 3).
    apply_dist(Rows,N,T,0,0,0).

problem(1, [[ _, _, _, _],
            [ _, _, _, _],
            [ _, _, _, _],
            [ _, _, _, _]]).

blocks([], []).
blocks([N1,N2|Ns1], [N3,N4|Ns2]) :-
        all_distinct([N1,N2,N3,N4]),
        blocks(Ns1, Ns2).

goal(X,Z,Y):-
      Y #= X+Z.
%      Y is X + 2.
%    abs(X-Y) #= 3.

main :-
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

   problem(1, Table),
%    sudoku(s(1,  [[[s] ,[s], [s,w],  [s]],
%                 [_ , _,[s,w],  _],
%                 [ [s] , [s], [s,w], [s,w]],
%                 [_ ,[3,w],  [w],  _]]
%                        ), Table),
sudoku(s(1, [[[s], [],    [],[s]],
                         [ [], [],    [], []],

                         [ [],[s], [s,w], []],
                         [ [4], [],   [w], []]
                        ]
                        ), Table),
    maplist(labeling([ff]), Table),
    maplist(portray_clause, Table),nl,



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
