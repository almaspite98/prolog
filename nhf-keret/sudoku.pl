-module(sudoku).
-export([ sudoku/2]).
:- use_module(library(clpfd)), use_module(library(lists)).

% Ez a feladatban elvárt eljárás fejléce, maga a sudoku solver, mely egy s(N,T) struktúrát vár és
% eredményét a Rows-ba tölti.
sudoku(s(N,T), Rows) :-
    length(T, L),
    length(Rows, L),
    maplist(same_length(Rows), Rows),
    append(Rows, Vs),
    domain(Vs, 1, L),

    maplist(all_distinct, Rows),

    transpose(Rows, Columns),
    maplist(all_distinct, Columns),

    make_blocks_distinct(Rows, 1),

    apply_dist(Rows,N,T,0,0,0),

    labeling([ff, value(shave)], Vs).


% Az apply_dist eljárás feladata, hogy a távolság szabályokat felvegye minden elemre.
% Input: M – mátrix, N – távolság érték, D – infómátrix, I – aktuális sor indexe, J – aktuális oszlop
% indexe, C – index/counter
% output: - (felvett szabályok)
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

%Egy M mátrix I. sorának J. elemét teszi E-be (0-tól indexelünk)
nth0_mtx(M,I,J,E):-
    nth0(I,M,L),
    nth0(J,L,E).

%Ez az eljárás azt köti ki, hogy X és Y különbségének abszolút értéke N legyen.
dist_to_be(X, Y, N) :-
    scalar_product([1,-1],[X,Y],#=,D,[consistency(domain)]),
    abs(D) #= N.

%Ez az eljárás azt köti ki, hogy X és Y különbségének abszolút értéke ne legyen N.
dist_not_be(X, Y, N) :-
    scalar_product([1,-1],[X,Y],#=,D,[consistency(domain)]),
    abs(D) #\= N.

%A dist eljárás egy adott E1 cellára köti ki a távolság korlátokat D lista alapján.
%input: E – aktuális cella, D – infómátrix E cellájához tartozó mezője,
% S – déli szomszéd, W – nyugati szomszéd, N – távolság érték
%output: - (megkötések)
dist([],E,S,W,N):-
    dist_not_be(E, S, N),
    dist_not_be(E, W, N).
dist([s],E,S,W,N):-
    dist_to_be(E, S, N),
    dist_not_be(E, W, N).
dist([s,w],E,S,W,N):-
    dist_to_be(E, S, N),
    dist_to_be(E, W, N).
dist([w],E,S,W,N):-
    dist_not_be(E, S, N),
    dist_to_be(E, W, N).
dist([A|Tail],E,S,W,N):-
    integer(A),
    E #= A,
    dist(Tail, E, S, W, N).

% A borotválásért felelős eljárás, mely az X változó és a többi változó értékét is borotválja.
shave(X, Rest, BB0, BB):-
	BB0 = BB,
    shave_vars(Rest),
	shave_var(X),
	indomain(X).

%L változó lista elemein iterál végig rekurzívan és mindegyikre meghívja a shave_var eljárást
shave_vars([]).
shave_vars([H|T]):-
    shave_var(H),
    shave_vars(T).

% A V változó tartományát szűkíti borotválás segítségével, tehát azon értékeket,
% melyek amúgyis meghiúsuláshoz vezetnének, kiveszi.
shave_var(V):-
	fd_set(V, Dom),
	fdset_member(C, Dom),
	( \+ V = C -> V #\= C % meghiusul
	; fail
	), !.
shave_var(_).

%Ez az eljárás azért felel, hogy az adott koordináták alapján visszaadja oszlop folytonosan az adott blokkot.
get_block(_,_,_,K,L,IC,_,L):-
	IC is K*K.
get_block(M,I,J,K,L,IC,JC,LR):-
	I1 is I + IC mod K,
    nth0_mtx(M, I1, JC, E1),
	append(L,[E1],L2),
	IC1 is IC + 1,
	JC1 is J + div(IC1,K),
	get_block(M,I,J,K,L2,IC1,JC1,LR).

%Ez az eljárás azért felel, hogy az összes blokkra kikösse az elemek egyediségének a szabályát.
%Ehhez először a blokkokat le kell kérni a get_block segítségével.
% S - mátrix
% I - cella index
make_blocks_distinct(S,I):-
    length(S, L),
    I > L.
make_blocks_distinct(S,I):-
	length(S,K),
	K1 is truncate(sqrt(K)),
	X is truncate((I-1)/K1)*K1,
	Y is ((I-1-(truncate((I-1)/K1))*K1)*K1),
	get_block(S,X,Y,K1,[],0,Y, C),
	all_distinct(C),
	I1 is I + 1,
	make_blocks_distinct(S, I1).
