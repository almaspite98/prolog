% value_of0(P, X, V): A P polinom x=X helyen vett értéke V.
value_of0(x, X, V) :- 
    V = X.
value_of0(N, _, V) :-      
    number(N), V = N.
value_of0(P1+P2, X, V) :-     
    value_of0(P1, X, V1),     
    value_of0(P2, X, V2),     
    V is V1+V2.
value_of0(P1*P2, X, V) :-     
    value_of0(P1, X, V1),     
    value_of0(P2, X, V2),     
    V is V1*V2.

% value_of0(P, X, V): A P polinom x=X helyen vett értéke V.
value_of(x, X, V) :- V = X.
value_of(N, _, V) :-      
    number(N), V = N.  
value_of(Poly, X, V) :-      
    Poly =.. [Op,P1,P2],            
    value_of(P1, X, V1),     
    value_of(P2, X, V2),     
    PolyV =.. [Op,V1,V2],           
    V is PolyV.              


end_of_file.

| ?- value_of0((x+1)*x+x+2*(x+x+3), 2, V).

| ?- value_of((x+1)*x+x+2*(x+x+3), 2, V).

| ?- value_of(exp(100,min(x,1/x)), 2, V).



