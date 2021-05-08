:- module(mininat, [{}/1,int_to_peano/2]).

% A bb_put(allow_inf, true) h�v�ssal enged�lyezhet�k a v�gtelen sz�mok.
% A bb_put(allow_inf, false) h�v�ssal tilthatjuk le ezeket.

allow_inf :-
	bb_get(allow_inf, true).


% {Korlat}: Korlat fenn�ll.
{Korlat} :-
        korlat_cel(Korlat, Cel), 
	call(Cel).

		     
% plusz(X, Y, Z): X+Y=Z (Peano sz�mokkal).
:- block plusz(-, ?, -).
plusz(0, Y, Y).
plusz(s(X), Y, s(Z)) :-
        plusz(X, Y, Z).

% +(X, Y, Z): X+Y=Z (Peano sz�mokkal). Hat�konyabb mint plusz/3, mert az
% �sszead�s kommutat�vit�s�t felhaszn�lva tov�bbl�p, ha b�rmelyik
% argumentum behelyettes�tett.
:- block +(-, -, -).
+(X, Y, Z) :- 
        nonvar(Y), !, plusz(Y, X, Z).  
+(X, Y, Z) :-
        /* var(Y), */ plusz(X, Y, Z). % \+((var(X),var(Z)))

% X-Y=Z, ahol X, Y �s Z Peano sz�mok.
-(X, Y, Z) :-
        +(Y, Z, X).

% X*Y=Z, ahol X, Y �s Z Peano sz�mok. Blokkol, ha nincs t�m�r argumentuma.
*(X, Y, Z) :-
        when( (ground(X);ground(Y);ground(Z)),
              szorzat(X, Y, Z)
            ).

% X*Y=Z, ahol X, Y �s Z Peano sz�mok. Legal�bb egyik argumentuma t�m�r.
szorzat(X, Y, Z) :-
        (   ground(X) -> szor(X, Y, Z)
        ;   ground(Y) -> szor(Y, X, Z)
        ;   /* Z t�m�r! */
            Z == 0 -> szorzatuk_nulla(X, Y)
        ;   plusz(X, _, Z), % X =< Z
            szor(X, Y, Z)
        ).

% X*Y=0, ahol X �s Y Peano sz�mok.
szorzatuk_nulla(X, Y) :-
        (   X = 0 
%       ;   Y = 0                  % K�tszeres megold�st adhat!
        ;   dif(X, 0), Y = 0
        ).

% A fenti elj�r�s kor�bbi, nem t�k�letes v�ltozatai:
% szorzatuk_nulla(X, Y) :-
%        ( X = 0 ; Y = 0 ).
% szorzatuk_nulla(X, Y) :-
%        ( X = 0 ; X \== Y, Y = 0 ).



% szor(X, Y, Z): X*Y=Z, ahol X, Y �s Z peano sz�mok. X t�m�r.
szor(0, _X, 0).
szor(s(X), Y, Z) :-
        +(Y, Z1, Z),
        szor(X, Y, Z1).

% korlat_cel(Korlat, Cel): Korlat leford�tott alakja Cel.
korlat_cel((K1,K2), C12) :-
        korlat_cel(K1, C1), korlat_cel(K2, C2),
	osszetett_cel(C1, C2, C12).
korlat_cel(Kif1=Kif2, Cel) :-
        kiertekel(Kif1, E1, C1), kiertekel(Kif2, E2, C2),
	osszetett_cel(C1, C2, C12),
	(   true ->             % ha a "v�gtelen" sz�m is megengedett
	    E1 = E2, Cel = C12
	;   osszetett_cel(C12, unify_with_occurs_check(E1, E2), Cel)
                                     % ha a "v�gtelen" nem megengedett
	).
korlat_cel(Kif1 =< Kif2, Cel) :-
        korlat_cel(Kif1+_ = Kif2, Cel).
korlat_cel(Kif1 < Kif2, Cel) :-
        korlat_cel(Kif1+1 =< Kif2, Cel).
korlat_cel(Kif1 >= Kif2, Cel) :-
        korlat_cel(Kif2 =< Kif1, Cel).
korlat_cel(Kif1 > Kif2, Cel) :-
        korlat_cel(Kif2 < Kif1, Cel).

% kiertekel(Kif, E, Cel): A Kif aritmetikai kifejez�s �rt�k�t E-ben
% el��ll�t� c�l Cel. 
% Kif eg�szekb�l �s v�ltoz�kb�l a +, -, �s * oper�torokkal �p�l fel.
kiertekel(Kif, E, Cel) :-
	(   compound(Kif), Kif =.. [Op,Kif1,Kif2]
	->  Rel =.. [Op,E1,E2,E],
	    kiertekel(Kif1, E1, C1), 
            kiertekel(Kif2, E2, C2),
	    osszetett_cel(C1, C2, C12),   % C12 = (C1,C2)
	    osszetett_cel(C12, Rel, Cel)
	;   integer(Kif)
        ->  Cel = true, int_to_peano(Kif, E)
	;   Cel = true, E = Kif
	).

% int_to_peano(N, P): N term�szetes sz�m Peano alakja P.
int_to_peano(N, P) :-
        (   N > 0 -> N1 is N-1, P = s(P1),
            int_to_peano(N1, P1)
        ;   N = 0, P = 0
        ).

:- multifile user:portray/1.

% Peano sz�mok ki�r�s�nak form�z�sa
user:portray(Peano) :-
        (   acyclic_term(Peano) ->
	    peano_to_int(Peano, 0, N), write(N)
	;   write(inf)
        ).
% C�lok ki�rat�s�nak form�z�sa
user:portray(mininat:Rel) :-
        Rel =.. [Pred,A,B,C],
        predikatum_operator(Pred, Op),
        Fun =.. [Op,A,B],
        print({Fun=C}).

predikatum_operator(plusz, +).
predikatum_operator(+, +).
predikatum_operator(-, -).
predikatum_operator(*, *).

% A Peano Peano-sz�m �rt�ke N-N0.
peano_to_int(Peano, N0, N) :-
        nonvar(Peano),
        (   Peano == 0 -> N = N0
        ;   Peano = s(P), 
            N1 is N0+1,
            peano_to_int(P, N1, N)
        ).

% osszetett_cel(C1, C2, C12): A C12 c�l a C1 �s C2 c�lok konjunkci�ja.
osszetett_cel(true, Cel0, Cel) :- !,
	Cel = Cel0.
osszetett_cel(Cel0, true, Cel) :- !,
	Cel = Cel0.
osszetett_cel(Cel1, Cel2, (Cel1,Cel2)).

goal_expansion({Korlat}, _, _, mininat:Cel, []) :-
	korlat_cel(Korlat, Cel).


