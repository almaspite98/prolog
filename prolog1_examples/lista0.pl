% v_ossz(+A, +B, ?C): C az A és B vektorok összege
v_ossz([], [], []).
v_ossz([A|AL], [B|BL], [C|CL]) :-
        C is A+B,
        v_ossz(AL, BL, CL).

% v_ossz_s(+A, +B, ?C): C az A és B vektorok szimbolikus összege
v_ossz_s([], [], []).
v_ossz_s([A|AL], [B|BL], [C|CL]) :-
        C = A+B,
        v_ossz_s(AL, BL, CL).

% vs_szorz(+A, +S, ?B): B az A vektor S skalárral való szorzata 
vs_szorz([], _, []).
vs_szorz([A|AL], S, [B|BL]) :-
        B is A*S,
	vs_szorz(AL, S, BL).

% skszorz(+A, +B, ?S): S az A és B vektorok skalárszorzata 
skszorz([], [], 0).
skszorz([A|AL], [B|BL], S) :-
        skszorz(AL, BL, S0),
	S is S0+A*B.
