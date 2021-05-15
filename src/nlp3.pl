-module(khf3).
-export([ sudoku/2, magic/2, p/3, szeszam/2, latszam/2, panorama/3]).
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

magic(0, []).
magic(1, [1]).
magic(N, Rows):-
    length(Rows, N),
    maplist(same_length(Rows), Rows),
    append(Rows, List),
    N2 is N*N,
    domain(List, 1, N2),
    all_distinct(List),
%    nth0(0, Rows, L0),
%    sum_list(L0, Sum),
    maplist(my_sum(Sum), Rows),
%    maplist()
%    all_same_sum(Rows),
    transpose(Rows, Columns),
    maplist(my_sum(Sum), Columns),
%    all_same_sum(Columns),

    diagonal1(Rows, Diag1),
    diagonal2(Rows, Diag2),
%    same_sum_lists(Diag1, Diag2),
    maplist(my_sum(Sum), [Diag1,Diag2|Rows]),


%    maplist(same_sum_list(Sum), Columns),
    labeling([ff], List).

my_sum(Sum, List):-
    sum(List, #=, Sum).

same_sum_list(0, []).
same_sum_list(Sum, [H|T]):-
    Sum2 #= Sum - H,
    same_sum_list(Sum2, T).

same_sum_lists([],[],Sum, Sum).
same_sum_lists([H1|T1], [H2|T2], Sum1, Sum2):-
    Sum12 #= Sum1 + H1,
    Sum22 #= Sum2 + H2,
    same_sum_lists(T1, T2, Sum12, Sum22).

all_same_sum([H1,H2|[]]):
    same_sum_lists(H1,H2, 0,0).
all_same_sum([H1,H2, H3|T]):-
    same_sum_lists(H1, H2,0,0),
    all_same_sum([H2, H3|T]).

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

p(A, B, C).


% 4. Feladat
% szeszam(+L, ?K): az L, csupa különböző elemből álló listában levő lokális szélsőértékek száma K.
% Az eljárás ne hozzon létre választási pontot (ne címkézzen)!

szeszam(L, K).


% 5. Feladat
% latszam(+L, ?K): az L listában levő balról látható elemek száma K.
% Az eljárás ne hozzon létre választási pontot (ne címkézzen)!

latszam(L, K).


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

panorama(N, Latvanyok, Lakotelep).


megoldasok(Feladvany, Megoldasok) :-
	findall(Megoldas,
		magic(Feladvany, Megoldas),
		Megoldasok0),
	sort(Megoldasok0, Megoldasok).



main :-
%    magic(3, L),

      M = [[2,7,6],
         [9,5,1],
         [4,3,8]],
%     all_same_sum(M),
     maplist(my_sum(_), Rows),


%     diagonal1(M, D1),
%     diagonal2(M, D2),
%     print(D1),nl,
%     print(D2),nl,

    megoldasok(3, L),
%    same_sum_lists([1,2,3], [3,2,2],0,0),
    print(L),
    nl.