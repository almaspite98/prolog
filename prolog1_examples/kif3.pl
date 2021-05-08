:- use_module(library(lists), [permutation/2]).

% Kif a n�gy alapm�velettel az L list�ban megadott sz�mokb�l
% fel�p�tett kifejez�s, amelynek �rt�ke Ertek.
levelek_ertek_kif(L, Ertek, Kif) :-
	permutation(L, PL),
	levelek_kif(PL, Kif),
	Kif =:= Ertek.

% kif_levelek(+Kif, ?L): A Kif kifejez�s leveleiben lev�
% sz�mok list�ja  L.
kif_levelek(Kif, L) :-
	number(Kif), L = [Kif].
kif_levelek(Kif, L) :-
	alap4(K1, K2, Kif),
	kif_levelek(K1, L1),
	kif_levelek(K2, L2),
	append(L1, L2, L).

% levelek_kif(+L, ?Kif): A Kif kifejez�s leveleiben lev�
% sz�mok list�ja  L.
levelek_kif(L, Kif) :-
	L = [Kif], number(Kif).
levelek_kif(L, Kif) :-
	append(L1, L2, L),
	L1 \= [], 
	L2 \= [], 
	levelek_kif(L1, K1),
	levelek_kif(L2, K2),
	alap4_0(K1, K2, Kif).

% alap4_0(X, Y, Kif):
% Kif az X �s Y kifejez�sekb�l a n�gy alapm�velet egyik�nek
% �RTELMES alkalmaz�s�val �ll el�.
alap4_0(X, Y, X+Y).
alap4_0(X, Y, X-Y).
alap4_0(X, Y, X*Y).
alap4_0(X, Y, X/Y) :- Y =\= 0.

end_of_file.

| ?- use_module(library(between)).

| ?- between(1, 1000, E), \+ ( levelek_ertek_kif([1,3,4,6], E, K), write(E=K), nl ).

| ?- between(1, 1000, E), \+ ( levelek_ertek_kif([1,2,3,4,5], E, K), write(E=K), nl ).
