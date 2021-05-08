%% coding: utf-8
% pzh/Prolog: feladatlap

% Az alábbi kérdésre igennel válaszoljon, ha az előző zárthelyin kapott Erlang-
% pontszámát szeretné megtartani, azaz most nem ad be Erlang programot.

% Korábbi Erlang-pontszámomat megtartom (igen/nem): ????????

% Pontozás: 17 + 34 + 34 = 85 pont

% Segédeljárások a teszteléshez:
do_test :-
	true.   % ha kérünk tesztelést.
%       fail.   % ha nem kérünk tesztelést.


check_test(Name, Got, Expected) :-
	do_test, !,
	(   Got == Expected -> format('Test ~w~13+ OK\n', [Name])
	;   format('***Test ~w wrong, expected: ~q, got ~q\n',
		   [Name,Expected,Got])
	).
check_test(_, _, _).

check_failure(Name, Test) :-
	do_test, !,
	(   call(Test) -> format('***Test ~w wrong, expected failure, got success\n',
				 [Name])
	;   format('Test ~w~13+ OK\n', [Name])
	).
check_failure(_, _).

%% ============================================================================
% I. Egyklózos feladatok (3+6+8, összesen 17 pont)


% Az alábbi 3 feladat megoldására egyetlen *nem rekurzív* klózból álló
% predikátumot kell írnia.

% Ha a feladatban nem említjük a felsorolás sorrendjét, vagy az eredményül
% várt lista elemeinek sorrendjét, akkor ez a sorrend tetszőleges lehet.

% Az alábbi beépített ill. könyvtári eljárások használhatók:

% append/3, append/2, member/2, memberchk/2, select/3, reverse/2, 
% nth0/3, nth1/3, sumlist/2, last/2, sort/2, length/2,
% between/3, findall/3, bagof/3, setof/3, = /2, \= / 2,
% valamint az aritmetikai beépített eljárások.

% A Prolog vezérlési szerkezetei (diszjunkció, negáció, feltételes kifejezés)
% szabadon használhatók.

% Törekedjék minél tömörebb megoldásra!

% A felhasználható könyvtárak betöltése, SWI-SICStus egységes interfész
% kialakítása
:- use_module(library(lists)).
:- (   current_prolog_flag(dialect, sicstus)
   ->  use_module(library(between)),
       use_module(library(samsort))
   ;   assert((samsort(L, S) :- sort(0, @=<, L, S)))
   ).


% f1(+Xs, ?R): R az Xs egészlista átlaga.           (3 pont)

% | ?- f1([2,5,11,7,8], R).              --->  R = 6.6 ? ; no

f1(Xs, R) :-
    sumlist(Xs, Sum),
    length(Xs, Len),
    R is Sum/Len
	.

:- f1([2,5,11,7,8], R), check_test(f1, R, 6.6).
 
% f2(+Xs, ?Rs): Az Xs egészlistában balról jobbra haladva előforduló,
% kétjegyű pozitív egészek I sorszámából és XX értékéből álló I-XX párok
% listája Rs. A listaelemek számozása 1-től kezdődik. Xs lehet üres, és az
% is lehet, hogy nincsenek benne kétjegyű pozitív egészek. (6 pont)

% | ?- f2([10,1,-12,99,0,23,100,9], Rs). --->  Rs = [1-10,4-99,6-23] ? ; no

f2(Xs, Rs) :-
	findall(I-X, (member(X, Xs),nth1(I, Xs, X), X>9,X<100), Rs).

:- f2([10,1,-12,99,0,23,100,9], Rs), check_test(f2, Rs, [1-10,4-99,6-23]).

% f3(+Xs, ?X, ?N): X az Xs számlistának az N-ik eleme (1-től számozva), és
% X megegyezik az őt követő N elem összegével. (8 pont)

% | ?- f3([5,5,2,2,-1,1,0,2], X, N).     --->
% X = 5, N = 1 ? ; 
% X = 2, N = 3 ? ; 
% X = 2, N = 4 ? ; no

f3(Xs, X, N) :-
	nth1(N, Xs, X),
	append(P, [X|Suff], Xs),
	N1 is N-1,
	length(P, N1),
	append(Pre, _, Suff),
	length(Pre, N),
	sumlist(Pre, X)

	.

:- findall(N-X, f3([5,5,2,2,-1,1,0,2], X, N), Rs),
	samsort(Rs, SRs), check_test(f3, SRs, [1-5,3-2,4-2]).


%% ============================================================================
% II. Programozási feladat (34 pont):

% Írjon egy tenger(+Xs, -Ts) eljárást, amelynek első, bemenő argumentuma az
% Xs egészlista, második, kimenő argumentuma pedig a Ts lista. Ts az Xs-nek
% balról az első olyan folytonos részlistája, amely csupa negatív számból
% áll, legalább három elemű, és maximális, azaz nem terjeszthető ki további
% negatív számokkal. Ha nincs ilyen részlista, akkor az eljárás hiúsuljon
% meg.

% tenger(+Xs, -Ts): A Ts lista az Xs egészlistának az első olyan folytonos
% maximális részlistája, amely csupa negatív számból áll és legalább három
% elemű.


% Példák:

% | ?- tenger([1,2,3,-3,-2,1,2,-4,-5,-6,1,2,-7,-8,-9,-4,-3,5], Ts). 
%                                      --->      Ts = [-4,-5,-6] ? ; no
% | ?- tenger([-3,-2,1,2,-4,-5,-6,1,2,-7,-8,-9,-4,-3,5], Ts).
%                                      --->      Ts = [-4,-5,-6] ? ; no
% | ?- tenger([-5,-4,-3,-2,-1], Ts).   --->      Ts = [-5,-4,-3,-2,-1] ? ; no
% | ?- tenger([-5,-4,-3], Ts).         --->      Ts = [-5,-4,-3] ? ; no
% | ?- tenger([1,2,3,-3,-2], Ts).      --->      no
% | ?- tenger([-1,-2], Ts).            --->      no
% | ?- tenger([-1], Ts).               --->      no
% | ?- tenger([], Ts).                 --->      no


aux_tenger([], Ts, Ts).
aux_tenger([H|T], Rs, Ts) :-
    (
        H<0,
        append(Rs,[H], Rs1),
        aux_tenger(T, Rs1, Ts)
    ;
        H > -1,
        Ts = Rs
    ).
tenger([H1,H2,H3|T], Ts) :-

	(
	    H1<0,H2<0,H3<0,
	    Rs = [H1,H2, H3],
	    aux_tenger(T, Rs, Ts)
	;
	    H1 > -1,
	    tenger([H2,H3|T], Ts)
    ;
	    H2 > -1,
        tenger([H2,H3|T], Ts)
    ;
       H3 > -1,
       tenger([H2,H3|T], Ts)
	)
	.

:- tenger([1,2,3,-3,-2,1,2,-4,-5,-6,1,2,-7,-8,-9,-4,-3,5], Ts),
	check_test(tgr_a, Ts, [-4,-5,-6]).
:- tenger([-3,-2,1,2,-4,-5,-6,1,2,-7,-8,-9,-4,-3,5], Ts),
	check_test(tgr_b, Ts, [-4,-5,-6]).
:- tenger([-5,-4,-3,-2,-1], Ts),
	check_test(tgr_c, Ts, [-5,-4,-3,-2,-1]).
:- tenger([-5,-4,-3], Ts),
	check_test(tgr_d, Ts, [-5,-4,-3]).
:- check_failure(tgr_e, tenger([1,2,3,-3,-2], _Ts)).
:- check_failure(tgr_f, tenger([-1,-2], _Ts)).
:- check_failure(tgr_g, tenger([-1], _Ts)).
:- check_failure(tgr_h, tenger([], _Ts)).


%% ============================================================================
% III. Programozási feladat (34 pont):

% Egy irányított gráfot úgy ábrázolunk, hogy minden Kezdo ponthoz megadjuk
% azoknak a Veg pontoknak a Vegpontok listáját, amelyekre Kezdo-ból Veg-be
% vezet él. A gráfot Kezdo-Vegpontok alakú párok listájaként adjuk meg, ahol
% Vegpontok nem üres lista. A gráf pontjait atomok jelölik.  Például az
% a-->b, a-->c, b-->c élekből álló gráfot az [a-[b,c],b-[c]] Prolog lista
% írja le.

% A gráfábrázolásban a listák elemeinek sorrendje érdektelen, tehát a fenti
% gráfot írja le a [b-[c],a-[c,b]] lista is. Feltételezheti, hogy a
% gráfban nincs többszörös él.

% Írjon Prolog nyelven egy eljárást, amely egy, a fenti módon ábrázolt gráfhoz
% hozzáad egy élet, előállítva az új gráfnak megfelelő Prolog adatstruktúrát!
% Adjon hatékony (jobbrekurzív) megoldást!
% Nem jobbrekurzív megoldásra legfeljebb 17 pontot kaphat.


% bovitett(+G0, +Kezdo, +Veg, -G): A G gráf úgy áll elő, hogy a G0 gráfhoz
% hozzávesszük a Kezdo -> Veg irányított élet. Feltételezheti, hogy a 
% Kezdo -> Veg él még nem szerepel a G0 gráfban. A G listában levő 
% kezdőpontok sorrendje egyezzék meg a G0-beli sorrenddel, új kezdőpont a 
% G lista végére kerüljön.

% Példák:

% | ?- bovitett([], b, a, G).              ---> G = [b-[a]] ? ; no
% | ?- bovitett([b-[c],a-[b,d]], b, e, G). ---> G = [b-[e,c],a-[b,d]] ? ; no
% | ?- bovitett([b-[c],a-[b,d]], a, c, G). ---> G = [b-[c],a-[c,b,d]] ? ; no
% | ?- bovitett([b-[c],a-[b,d]], e, c, G). ---> G = [b-[c],a-[b,d],e-[c]] ? ; no

% Segítség: Az első két példa által mutatott esetek kezelésére egy-egy
% nem-rekurzív klózt érdemes írni. A harmadik és negyedik példa együttes
% kezelésére egy harmadik, rekurzív klóz használható.

bovitett([], K, V, [K-[V]]).
bovitett([H-C | T], K, V, [ H-C1 | T1]) :-
    ( H = K ->
        C1 = [V| C],
        T1 = T
    ; otherwise ->
        C1 = C,
        bovitett(T, K,V, T1)
    )
.

rendez(K-VL, K-SVL) :- samsort(VL, SVL).
	
:- bovitett([], b, a, G0),
	maplist(rendez, G0, G), check_test(bov_a, G, [b-[a]]).		
:- bovitett([b-[c],a-[b,d]], b, e, G0), 
	maplist(rendez, G0, G), check_test(bov_b, G, [b-[c,e],a-[b,d]]).	
:- bovitett([b-[c],a-[b,d]], a, c, G0), 
	maplist(rendez, G0, G), check_test(bov_c, G, [b-[c],a-[b,c,d]]).	
:- bovitett([b-[c],a-[b,d]], e, c, G0), 
	maplist(rendez, G0, G), check_test(bov_d, G, [b-[c],a-[b,d],e-[c]]).


