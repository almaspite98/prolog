	     
% between(M, N, I): M =< I =< N, I eg�sz.
% El�felt�tel: M =< N.
between(M, _N, M).
between(M, N, I) :-
        M < N, 
        M1 is M+1,
        between(M1, N, I).
