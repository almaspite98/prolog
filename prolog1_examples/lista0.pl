% v_ossz(+A, +B, ?C): C az A �s B vektorok �sszege
v_ossz([], [], []).
v_ossz([A|AL], [B|BL], [C|CL]) :-
        C is A+B,
        v_ossz(AL, BL, CL).

% v_ossz_s(+A, +B, ?C): C az A �s B vektorok szimbolikus �sszege
v_ossz_s([], [], []).
v_ossz_s([A|AL], [B|BL], [C|CL]) :-
        C = A+B,
        v_ossz_s(AL, BL, CL).

% vs_szorz(+A, +S, ?B): B az A vektor S skal�rral val� szorzata 
vs_szorz([], _, []).
vs_szorz([A|AL], S, [B|BL]) :-
        B is A*S,
	vs_szorz(AL, S, BL).

% skszorz(+A, +B, ?S): S az A �s B vektorok skal�rszorzata 
skszorz([], [], 0).
skszorz([A|AL], [B|BL], S) :-
        skszorz(AL, BL, S0),
	S is S0+A*B.
