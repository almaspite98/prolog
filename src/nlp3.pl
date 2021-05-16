-module(khf3).
-export([ sudoku/2, magic/2, p/3,p2/3, szeszam/2, latszam/2, panorama/3]).
:- use_module(library(clpfd)), use_module(library(lists)).

% 1. Feladat
sudoku_simple(Rows, N):-
    length(Rows, N),
    maplist(same_length(Rows), Rows),


    append(Rows, List),
    domain(List, 1, N),
    maplist(all_distinct, Rows),

    transpose(Rows, Columns),
    maplist(all_distinct, Columns),
    labeling([ff, enum], List).

% 2. Feladat
% magic(+N, ?Mx): Mx egy N oldalhosszú bűvös négyzet, vagyis az 1..N*N számokat pontosan egyszer
% tartalmazza, és minden sor, oszlop és átló összege ugyanaz. Az eljárás címkézzen!

magic(N, Rows):-
    length(Rows, N),
    maplist(same_length(Rows), Rows),
    append(Rows, List),
    N2 is N*N,
    domain(List, 1, N2),
    all_distinct(List),
    maplist(my_sum(Sum), Rows),
    transpose(Rows, Columns),
    maplist(my_sum(Sum), Columns),
    diagonal1(Rows, Diag1),
    diagonal2(Rows, Diag2),
    maplist(my_sum(Sum), [Diag1,Diag2|Rows]),
    labeling([ff], List).

my_sum(Sum, List):-
    sum(List, #=, Sum).

diagonal1([], []).
diagonal1([[E|_]|Ess], [E|Ds]) :-
    maplist(list_tail, Ess, Ess0),
    diagonal1(Ess0, Ds).

list_tail([_|Es], Es).

diagonal2(Ess,Ds) :-
    maplist(reverse, Ess, Fss),
    diagonal1(Fss, Ds).



% 3. Feladat
% p(A,B,C): ha az A-B szám 3-mal osztva 1 maradékot ad, akkor C páros, különben páratlan.
% A p/3 eljárás ne címkézzen!

p(A, B, C):-
    Diff #= A - B,
    Diff mod 3 #=1 #<=> C mod 2 #= 0.

p2(A, B, C):-
    Diff #= A - B,
    Diff mod 3 #=1 #=> C mod 2 #= 0,
    Diff mod 3 #\=1 #=> C mod 2 #= 1.



% 4. Feladat
% szeszam(+L, ?K): az L, csupa különböző elemből álló listában levő lokális szélsőértékek száma K.
% Az eljárás ne hozzon létre választási pontot (ne címkézzen)!

szeszam(L, K):-
    all_distinct(L),
    min_max_counter(L,K).

min_max_counter([_],0).
min_max_counter([_,_],0).
min_max_counter([H1,H2,H3|T], K):-
    domain([B],0,1),
    ((H1 #> H2 #/\ H2 #< H3) #\/ (H1 #< H2 #/\ H2 #> H3)) #<=> B,

    K1 #= K - B,
    min_max_counter([H2,H3|T], K1).


% 5. Feladat
% latszam(+L, ?K): az L listában levő balról látható elemek száma K.
% Az eljárás ne hozzon létre választási pontot (ne címkézzen)!

latszam(L, K):-
    reverse(L, RL),
    latszam1(RL, K).

latszam1([],0).
latszam1([H],1):-
    K1 #= K-1,
    latszam1([],0).
latszam1([H1, H2|T], K):-
    domain([B],0,1),
    balrol_lathato([H2|T],H1,1,B),
    K1 #= K - B,
    latszam1([H2|T], K1).


balrol_lathato([],_,B,B).

balrol_lathato([H|T],E, B1, Bout):-
    domain([B, B2],0,1),
    (E #> H) #<=> B,
    B #/\ B1 #<=> B2,
    balrol_lathato(T, E, B2, Bout).


% 6. Feladat
% panorama(+N, +Latvanyok, ?Lakotelep): Lakotelep egy N*N-es mátrix, amely egy lakótelep alaprajzát adja
% ki. A mátrix elemei az egyes épületek magasságát mutatják. Minden sorban es minden oszlopban különböző
% magasságúak az épületek és ezek a magasságok az 1..N tartományból kerülnek ki. Latvanyok egy olyan
% lista, amelynek elemei bal(I,K), felul(J,K), jobb(I,K), alul(J,K) alakú Prolog kifejezések, ahol I, J
% és K egyaránt az 1..N intervallumba esik. A Latvanyok listában előforduló bal(I,K) elem azt a
% korlátozást fejezi ki, hogy a lakótelep I-edik sorát balról nézve K ház látszik, a felul(J,K) azt,
% hogy a lakótelep J-edik oszlopát felülről nézve K ház látszik, stb.
% Az eljárás sorolja fel az összes megoldást. Csak a labeling/2 könyvtári eljárás hívása hozzon
% létre választási pontot!

panorama(N, Latvanyok, Rows):-
    length(Rows, N),
    maplist(same_length(Rows), Rows),
    append(Rows, List),
    domain(List, 1, N),
    maplist(all_distinct, Rows),
    transpose(Rows, Columns),
    maplist(all_distinct, Columns),
    latvany_szamlalo(Latvanyok, Rows),
    labeling([ff], List).


latvany_szamlalo([],_).
latvany_szamlalo([H|T], M):-
    latvany(M, H),
    latvany_szamlalo(T, M).

latvany(M, bal(I,K)):-
    nth1(I,M,L),
    latszam(L, K).
latvany(M, felul(J,K)):-
    transpose(M, Columns),
    nth1(J,Columns,L),
    latszam(L, K).
latvany(M, jobb(I,K)):-
    nth1(I,M,L),
    reverse(L, RL),
    latszam(RL, K).
latvany(M, alul(J,K)):-
    transpose(M, Columns),
    nth1(J,Columns,L),
    reverse(L, RL),
    latszam(RL, K).


megoldasok(s(A,B, C), Megoldasok) :-
	findall(C,
		p(A,B, C),
		Megoldasok0),
	sort(Megoldasok0, Megoldasok).



main :-

%    A in 1..5,
%    B in 1..10,
%    C in 1..10,
%    megoldasok(s(A,B, 0),L),
%    print(L),
%    domain([A,B], 1, 3), C in 0..1, p(A, B, C), labeling([], [A,B,C]), write(A-B-C), nl,fail,
%    domain([A,B],1,4),p(A,B,0),labeling([],[A,B]), write(A-B-0), nl,fail,


%    H1 #> H2 #/\ H2 #< H3 #\ H1 #< H2 #/\ H2 #> H3 #=> B #= 1,
%    H1 #> H2 #/\ H2 #> H3 #\ H1 #< H2 #/\ H2 #< H3 #=> B #= 0,
%    domain([H1,H2,H3],1,2),
%    domain([B],0,1),
%    H1 #= 2, H2 #= 2, H3 #= 5,
%    print('B: '),print(B),nl,
%     labeling([], [H1,H2,H3,B]), write(H1-H2-H3-B),nl, fail,
%    L=[1,_,_,_], domain(L, 1, 4), szeszam(L, 2), labeling([], L),print(L),nl,fail,
%    length(L,4), szeszam(L,_N),
%    length(L,3),domain(L,1,3),szeszam(L,0),labeling([], L),print(L),nl,fail,
%    L1 = [1,3,4,0], max_member(M, L1), print(M),
%    L=[_,_,2,_], domain(L, 1, 4), all_distinct(L), latszam(L, 3), labeling([], L),print(L),nl,fail,
%    L=[_,_,_,3,_], domain(L, 1, 5), all_distinct(L), latszam(L, 3), labeling([], L),print(L),nl,fail,
%    L=[_,1,_,_], domain(L, 1, 5), latszam(L, 1), labeling([], L),print(L),nl, fail,
%    domain([B],0,1),
%    balrol_lathato([5,2,1],5,1,B),
%    print('Bout: '), print(B),nl,

%    print('Asd: '),nl,
%    balrol_lathato([1,2,6,2], 10,0,4),
%    panorama(4, [bal(1,2),bal(3,2),felul(2,3),felul(4,3),jobb(2,3),jobb(4,1),alul(1,3),alul(3,3)], Mx),print(Mx), nl,fail,
    nl.