% app(L1, L2, L3): Az L3 lista az L1 �s L2 list�k elemeinek 
% egym�s ut�n f�z�s�vel �ll el�.
app0([], L, R) :- R = L.
app0([X|L1], L2, R) :-
        app0(L1, L2, L3),
	R = [X|L3].









% app(L1, L2, L3): Az L3 lista az L1 �s L2 list�k elemeinek 
% egym�s ut�n f�z�s�vel �ll el�.
app([], L, L).
app([X|L1], L2, [X|L3]) :-
        app(L1, L2, L3).












%member(E, L):  E az L lista eleme.
memb(Elem, [Elem|_]).
memb(Elem, [_|Farok]) :- 
        memb(Elem, Farok).









% select0(Elem, Lista, Marad):  Elemet a Lista-b�l elhagyva marad Marad.
select0(Elem, [Elem|Marad], Marad).   % Elhagyjuk a fejet, marad a farok.
select0(Elem, [X|Farok], Marad) :-   
        select0(Elem, Farok, Marad0), % A farokb�l hagyunk el elemet,
        Marad = [X|Marad0].           % a marad�k el� tessz�k a fejet.




	
% select0(Elem, Lista, Marad):  Elemet a Lista-b�l elhagyva marad Marad.	
select(Elem, [Elem|Marad], Marad).
select(Elem, [X|Farok], [X|Marad0]) :- 
        select(Elem, Farok, Marad0).

% nrev(L, R): Az R lista az L megford�t�sa --- na�v megold�s.
nrev([], []).
nrev([X|L], R) :-
    nrev(L, RL),
    append(RL, [X], R).

% reverse(L, R): Az R lista az L megford�t�sa.
% Line�ris l�p�ssz�m� megold�s.
reverse(L, R) :-  revapp(L, [], R).

% revapp(L1, L2, R): L1 megford�t�s�t L2 el� f�zve kapjuk R-t.
revapp([], R, R).
revapp([X|L1], L2, R) :-
    revapp(L1, [X|L2], R).

% permutation(Lista, Perm): Lista permut�ci�ja a Perm lista.
permutation([], []).
permutation(Lista, [Elso|Perm]) :- 
	select(Elso, Lista, Maradek),
	permutation(Maradek, Perm).

















%memberchk(E, L):  E az L lista eleme c�l els� megold�sa.
membchk(Elem, [Elem|_]) :- !.
membchk(Elem, [_|Farok]) :- 
        membchk(Elem, Farok).
