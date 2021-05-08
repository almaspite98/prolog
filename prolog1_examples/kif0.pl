% kif(K): K a négy alapmûvelettel számokból képzett kifejezés.
kif(K) :-                % K a négy alapmûvelettel számokból képzett kifejezés HA
	number(K).       %         K egy szám.
kif(X+Y) :-              % X+Y a négy alapmûvelettel számokból képzett kifejezés HA
	kif(X),          %         X a négy alapmûvelettel számokból képzett kifejezés ÉS
	kif(Y).          %         Y a négy alapmûvelettel számokból képzett kifejezés.
kif(X-Y) :-
	kif(X),          % (...)
	kif(Y).
kif(X*Y) :-
	kif(X),
	kif(Y).
kif(X/Y) :-
	kif(X),
	kif(Y).
