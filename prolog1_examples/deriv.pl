% deriv(Kif, D):  Kif-nek az x szerinti deriváltja D.
deriv(x, 1).
deriv(C, 0) :-
	number(C).
deriv(U+V, DU+DV) :-
	deriv(U, DU),
	deriv(V, DV).
deriv(U-V, DU-DV) :-
	deriv(U, DU),
	deriv(V, DV).
deriv(U*V, DU*V + U*DV) :-
	deriv(U, DU),
	deriv(V, DV).

pelda1(D) :-
	deriv(x*x+x, D).

pelda2(D) :-
	deriv((x+1)*(x+1), D).

pelda3(I) :-
	deriv(I, 1*x+x*1+1).
