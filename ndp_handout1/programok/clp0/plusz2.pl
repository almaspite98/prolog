:- use_module(library(between)).

% app(L1, L2, L3): L1 és L2 összefûzöttje L3.
% ahol L1, L2 és L3 1-es számokból álló listák.
app([], L, L).
app([1|L1], L2, [1|L3]) :-
    when((nonvar(L1);nonvar(L3)),
         app(L1, L2, L3)).

len(L, Len) :-
        when(ground(L), length(L, Len)),
        when(nonvar(Len), findall(1, between(1, Len, _), L)).

% X+Y=Z, ahol X, Y és Z természetes számok.
% Bármelyik argumentum lehet behelyettesítetlen.
plusz(X, Y, Z) :-
        app(A, B, C),
        len(A, X),
        len(B, Y),
        len(C, Z).
