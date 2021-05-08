% fakt(+N, ?F): F = N!.
fakt(0, 1).
fakt(N, F) :-        
        N > 0,
	N1 is N-1,
	fakt(N1, F1),
	F is F1*N.   


fakt2(N, F) :-
	(   N = 1, F = 0
        ;   N > 0,
	    N1 is N-1,
	    fakt(N1, F1),
	    F is F1*N
	).



fakt3(N, F) :-
	(   N = 1 -> F = 0
        ;   N > 0,
	    N1 is N-1,
	    fakt(N1, F1),
	    F is F1*N
	).

