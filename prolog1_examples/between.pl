	     
% between(M, N, I): M =< I =< N, I egész.
% Elõfeltétel: M =< N.
between(M, _N, M).
between(M, N, I) :-
        M < N, 
        M1 is M+1,
        between(M1, N, I).
