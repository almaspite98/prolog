-module(khf2).
-export([szamker/2 ]).
:- use_module(library(clpfd)), use_module(library(lists)).

% szamker(SzK, Max): SzK az 1..Max számokkal
% helyesen kitöltött számkeresztrejtvény.
% Megjegyzés: egyes sorban/oszlopban középen
% is lehet

szamker(SzK, Max).



rule(A\x, I, J, M):-
    print('A'),
    integer(A),
    transpose(M, Columns),
    print(Columns),nl,
    nth0(J, Columns, L),
    print(L),nl,
    I1 is I + 1,
    length(L, Len),
    Len1 is Len - 1,
    write(I1 - Len1),
    my_sublist(L, I1, Len1, SL),
    print(SL),nl,
    print('A'),
    find_next_black(SL,0, I_out),
    print(I_out),nl,
    print('A'),
    I_end is I_out - 1,
    print('I_end: '),print(I_end),nl,
    my_sublist(L, I+1, I_out, SL2),
    print('SL2: '),print(SL2),nl,
    sum(SL2, #=, A).

    %TODO fekete elemek

%find_next_black([x\B|T], I_in, I_in):-
%    !.
%find_next_black([A\x|T], I_in, I_in):-
%    !.
%find_next_black([x\x|T], I_in, I_in):-
%    !.
find_next_black([H|T], I_in, I_out):-
    print('H: '),print(H),nl,
    (
        num_or_var(H)  ->
        I2 is I_in + 1,
        find_next_black(T, I2, I_out)
        ;
%        integer(H) ->
%        I2 is I_in + 1,
%        find_next_black(T, I2, I_out)
%        ;
        I_out is I_in
    )
    .
find_next_black([], I_in, I_in).

num_or_var(A) :-
    integer(A);var(A);fail.

%find_next_black([A|T], I_in, I_out):-
%     \+integer(A),
%     I_out = I_in.



my_sum(Sum, List):-
    sum(List, #=, Sum).

my_sublist(_,0,-1,[]).
my_sublist([_|B],M,N,S):- M>0, M=<N, my_sublist(B,M-1,N-1,S).
my_sublist([A|B],M,N,S):- 0 is M, M=<N, N2 is N-1, S=[A|D], my_sublist(B,0,N2,D).

main :-
    my_sublist([1,2,3,4,5],2,4,L),
%    domain([D,E],1,9),
%    print(integer(E)),nl,
%    find_next_black([1,E,D,3,2,1\x,3],0,I_out),
    print('I_out: '), print(I_out),
    domain([A,B,C], 1, 9),
    rule(15\x, 0, 1, [[x\ x,11\x,21\x, 8\x],
                     [x\24, A, _, _],
                     [x\10, B, _, _],
                     [x\10, 1, _, _],
                     [x\10, A, _, _],
                     [x\10, x\5, _, _],
                     [x\6, C, _,x\x]]),

    nl.