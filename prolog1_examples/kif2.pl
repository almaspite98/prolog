% alap4(KX, KY, Kif):
% Kif az KX �s KY kifejez�sekb�l a n�gy alapm�velet egyik�vel �ll el�.
alap4(KX, KY, KX+KY).
alap4(KX, KY, KX-KY).
alap4(KX, KY, KX*KY).
alap4(KX, KY, KX/KY).


% kif(K): K a n�gy alapm�velettel sz�mokb�l k�pzett kifejez�s.
kif(K) :- number(K).
kif(K) :-
	alap4(X, Y, K),
	kif(X),
	kif(Y).


% kif_levelek(+Kif, ?L): A Kif kifejez�s leveleiben lev�
% sz�mok list�ja  L.
kif_levelek(Kif, L) :-
	L = [Kif], number(Kif).
kif_levelek(Kif, L) :-
	alap4(K1, K2, Kif),
	kif_levelek(K1, L1),
	kif_levelek(K2, L2),
	append(L1, L2, L).

