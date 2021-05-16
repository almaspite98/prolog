-module(khf2).
-export([szamker/2 ]).
:- use_module(library(clpfd)), use_module(library(lists)).

% szamker(SzK, Max): SzK az 1..Max számokkal
% helyesen kitöltött számkeresztrejtvény.
% Megjegyzés: egyes sorban/oszlopban középen
% is lehet
nth0_mtx(M,I,J,E):-
    nth0(I,M,L),
    nth0(J,L,E).

szamker(SzK, Max):-
    apply_rules(SzK,Max,0,0,0,[]).
%    labeling([ff], Max).


apply_rules(M,_,_,_,C,L):-
        length(M, K),
        C is K*K,
        labeling([ff],L).
apply_rules(M,Max,I,J,C,L):-
        nth0_mtx(M,I,J,E),
        (
            num_or_var(E) ->
%            print('E: '), print(E),nl,
            append(L,[E],L1)
            ;
%            var(E) ->
%             append(L,[E],L1)
%            ;
%            print('E: '), print(E),nl,
%            E = A\B,
            L1 = L,
            rule_divider(E,I,J,M,Max)

%            rule(E,I,J,M,Max)
        ),
        length(M, K),
        C1 is C + 1,
        I1 is truncate(C1/K),
        J1 is C1 mod K,
        apply_rules(M,Max,I1,J1,C1,L1).


rule0(A\B, I, J , M, Max):-
    print('rule0'),nl,
    rule1(A\x, I, J, M, Max),
    rule2(x\B, I, J, M, Max).
rule1(A\x, I, J, M, Max):-
    print('rule1'),nl,
    integer(A),
    transpose(M, Columns),
    nth0(J, Columns, L),
    I1 is I + 1,
    length(L, Len),
    Len1 is Len - 1,
%    write(I1 - Len1),nl,
    my_sublist(L, I1, Len1, SL),
    find_next_black(SL,0, I_out),
    I_end is I_out - I1,
%    print('I_end: '),print(I_end),nl,
%    my_sublist(L, I+1, I_out, SL2),
    my_sublist(SL, 0, I_end, SL2),
    domain(SL2,1,Max),
    all_distinct(SL2),
%    print('SL2: '),print(SL2),nl,
    sum(SL2, #=, A).

rule2(x\A, I, J, M, Max):-
    print('rule2'),nl,
    integer(A),
%    transpose(M, Columns),
    nth0(I, M, L),
    J1 is J + 1,
    length(L, Len),
    Len1 is Len - 1,
%    write(I1 - Len1),nl,
    my_sublist(L, J1, Len1, SL),
    find_next_black(SL,0, I_out),
    I_end is I_out - J1,
%    print('I_end: '),print(I_end),nl,
    my_sublist(SL, 0, I_end, SL2),
    domain(SL2,1,Max),
    all_distinct(SL2),
%    print('SL2: '),print(SL2),nl,
    sum(SL2, #=, A).

rule3(x\x,_,_,_,_):-
    print('rule3'),nl.

    %TODO fekete elemek


rule_divider(A\B,I,J,M,Max):-
    (
        integer(A),
        integer(B),
        rule0(A\B,I,J,M,Max)
        ;
        integer(A),
        B == x,
        rule1(A\x,I,J,M,Max),!
        ;
        A == x,
        integer(B),
        rule2(x\B,I,J,M,Max),!
        ;
        rule3(x\x,I,J,M,Max)
    ).

find_next_black([H|T], I_in, I_out):-
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
%    my_sublist([1,2,3,4,5],2,4,L),
%    domain([D,E],1,9),
%    print(integer(E)),nl,
%    find_next_black([1,E,D,3,2,1\x,3],0,I_out),
%    domain([A,B,C], 1, 9),
%    rule(x\10, 2, 0, [[x\x,11\x,21\x, 8\x],
%                     [x\24, A, _, _],
%                     [x\10, B, 1, A,1,A,3,8\x,5],
%                     [x\10, 1, _, _],
%                     [x\10, A, _, _],
%                     [x\10, x\5, _, _],
%                     [x\6, C, _,x\x]]),
    M =             [[x\x,11\x,21\x, 8\x],
                    [x\24, _, _, _],
                    [x\10, _, _, _],
                    [x\6, _, _,x\x]],
    M1 = [[x\x, 11\x,21\x,8\x],
        [x\24,8, 9, 7 ],
        [x\10,2, 7, 1 ],
        [x\6, 1, 5, x\x]],
    szamker(M,9),
    print(M),nl,
%    A = B\C,
%    C = x,
%    C == x,
%    print(C),
    nl.