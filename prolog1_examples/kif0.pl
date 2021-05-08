% kif(K): K a n�gy alapm�velettel sz�mokb�l k�pzett kifejez�s.
kif(K) :-                % K a n�gy alapm�velettel sz�mokb�l k�pzett kifejez�s HA
	number(K).       %         K egy sz�m.
kif(X+Y) :-              % X+Y a n�gy alapm�velettel sz�mokb�l k�pzett kifejez�s HA
	kif(X),          %         X a n�gy alapm�velettel sz�mokb�l k�pzett kifejez�s �S
	kif(Y).          %         Y a n�gy alapm�velettel sz�mokb�l k�pzett kifejez�s.
kif(X-Y) :-
	kif(X),          % (...)
	kif(Y).
kif(X*Y) :-
	kif(X),
	kif(Y).
kif(X/Y) :-
	kif(X),
	kif(Y).
