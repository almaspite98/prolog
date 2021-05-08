% alap4(KX, KY, Kif):
% Kif az KX és KY kifejezésekbõl a négy alapmûvelet egyikével áll elõ.
alap4(KX, KY, KX+KY).
alap4(KX, KY, KX-KY).
alap4(KX, KY, KX*KY).
alap4(KX, KY, KX/KY).


% kif(K): K a négy alapmûvelettel számokból képzett kifejezés.
kif(K) :- number(K).
kif(K) :-
	alap4(X, Y, K),
	kif(X),
	kif(Y).


% kif_levelek(+Kif, ?L): A Kif kifejezés leveleiben levõ
% számok listája  L.
kif_levelek(Kif, L) :-
	L = [Kif], number(Kif).
kif_levelek(Kif, L) :-
	alap4(K1, K2, Kif),
	kif_levelek(K1, L1),
	kif_levelek(K2, L2),
	append(L1, L2, L).

