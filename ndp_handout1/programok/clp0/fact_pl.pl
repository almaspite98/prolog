fact_p(0, 1).
fact_p(N, F) :-
	N > 0,
	N1 is N-1,
	fact_p(N1, F1),
	F is N*F1.

% fact(+N, ?F): F = N!.
fact_tr(N, F) :-
	fact_tr(N, 1, F).

% fact(+N, +F0, ?F): F = F0*N!.
fact_tr(0, F0, F0).
fact_tr(N, F0, F) :-
	N > 0,
	N1 is N-1,
	F1 is N*F0,
	fact_tr(N1, F1, F).

