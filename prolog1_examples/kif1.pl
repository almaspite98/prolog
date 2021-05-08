% alap4(X, Y, Kif):
% Kif az X �s Y kifejez�sekb�l a n�gy alapm�velet egyik�vel �ll el�.
alap4(X, Y, X+Y).
alap4(X, Y, X-Y).
alap4(X, Y, X*Y).
alap4(X, Y, X/Y).

% kif(K): K a n�gy alapm�velettel sz�mokb�l k�pzett kifejez�s.
kif(K) :- number(K).
kif(K) :-
	alap4(X, Y, K),
	kif(X),
	kif(Y).


