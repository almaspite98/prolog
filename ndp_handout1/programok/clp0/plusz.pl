:- block app(-, ?, -).
% blokkol, ha az elsõ és a harmadik argumentum 
% egyaránt behelyettesítetlen
app([], L, L).
app([X|L1], L2, [X|L3]) :-
    app(L1, L2, L3).

% X+Y=Z, ahol X, Y és Z természetes számok.
% Bármelyik argumentum lehet behelyettesítetlen.
plusz(X, Y, Z) :-
        app(A, B, C),
        len(A, X),
        len(B, Y),
        len(C, Z).

% L hossza Len.
len(L, Len) :-
        len(L, 0, Len).

:- block len(-, ?, -).
% L lista hossza Len-Len0. Len0 mindig ismert.
len(L, Len0, Len) :-
        nonvar(Len), !, Len1 is Len-Len0, 
        length(L, Len1).
len(L, Len0, Len) :- 
        % nonvar(L), % a blokkolási feltétel miatt!
        (   L == [] -> Len = Len0
        ;   L = [_|L1],
            Len1 is Len0+1, len(L1, Len1, Len)
        ).

