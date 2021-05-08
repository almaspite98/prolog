% alap4(X, Y, Kif):
% Kif az X és Y kifejezésekbõl a négy alapmûvelet egyikével áll elõ.
alap4(X, Y, X+Y).
alap4(X, Y, X-Y).
alap4(X, Y, X*Y).
alap4(X, Y, X/Y).

% kif(K): K a négy alapmûvelettel számokból képzett kifejezés.
kif(K) :- number(K).
kif(K) :-
	alap4(X, Y, K),
	kif(X),
	kif(Y).


