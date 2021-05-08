:- use_module(library(lists), [permutation/2]).

kif(SzamL, MuvL, E, Kif) :-   % Az adott SzamL sz�mlist�b�l
                              % az adott MuvL listabeli m�veletekkel
                              % fel�p�thet� a K kifejez�s
                              % amelynek �rt�ke az adott Ertek, ha
    permutation(SzamL, PL),   %     SzamL egy permut�ci�ja PL �s
    kif1(PL, MuvL, Kif),      %     a PL lev�llist�b�l a MuvL m�veletekkel
                              %       fel�p�thet� a Kif kifejez�s �s
    catch(Kif =:= E, _, fail).%     Kif sz�m�rt�ke Ertek.

% kif1(L, MuvL, Kif):         
kif1([Kif], _, Kif).          % Az egyelemu [Kif] list�b�l Kif �p�thet� fel.
kif1(L, MuvL, Kif) :-         % L-b�l a MuvL m�veletekkel fel�p�thet� Kif ha
    append(L1, L2, L),        % L el��ll mint L1 �s L2 �sszef�z�se ahol
    L1 \= [], L2 \= [],       % L1 nem �res �s L2 nem �res, �s
    kif1(L1, MuvL, K1),       % L1-b�l a MuvL m�veletekkel fel�p�thet� K1, �s
    kif1(L2, MuvL, K2),       % L2-b�l a MuvL m�veletekkel fel�p�thet� K2, �s
    member(M, MuvL),          % M egy MuvL list�beli m�velet, �s
    Kif =.. [M,K1,K2].        % Kif egy az M m�velettel a K1-b�l �s K2-b�l 
                              % fel�p�l� k�targumentum� kifejez�s.
