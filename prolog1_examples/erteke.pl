% Formula: sz�mokb�l �s az `x' n�vkonstansb�l `+' �s `*' oper�torokkal
% fel�p�l� kifejez�s
% :- type kif == {x} \/ number \/ {kif+kif} \/ {kif*kif}

% erteke(Kif, X, E): A Kif formula �rt�ka  E, az x=X behelyettes�t�s mellett.
erteke(x, X, E) :-
	E = X.
erteke(Kif, _, E) :-
	number(Kif), E = Kif.
erteke(K1+K2, X, E) :-
	erteke(K1, X, E1),
	erteke(K2, X, E2),
	E is E1+E2.
erteke(K1*K2, X, E) :-
	erteke(K1, X, E1),
	erteke(K2, X, E2),
	E is E1*E2.

% | ?- erteke(((x+1)*3)+x+2*(x+x+3), 0, E).
