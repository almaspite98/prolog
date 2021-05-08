:- use_module(library(between)).

% app(L1, L2, L3): L1 �s L2 �sszef�z�ttje L3.
% ahol L1, L2 �s L3 1-es sz�mokb�l �ll� list�k.
app([], L, L).
app([1|L1], L2, [1|L3]) :-
    when((nonvar(L1);nonvar(L3)),
         app(L1, L2, L3)).

len(L, Len) :-
        when(ground(L), length(L, Len)),
        when(nonvar(Len), findall(1, between(1, Len, _), L)).

% X+Y=Z, ahol X, Y �s Z term�szetes sz�mok.
% B�rmelyik argumentum lehet behelyettes�tetlen.
plusz(X, Y, Z) :-
        app(A, B, C),
        len(A, X),
        len(B, Y),
        len(C, Z).
