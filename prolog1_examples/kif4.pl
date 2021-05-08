:- use_module(library(lists), [permutation/2]).

kif(SzamL, MuvL, E, Kif) :-   % Az adott SzamL számlistából
                              % az adott MuvL listabeli mûveletekkel
                              % felépíthetõ a K kifejezés
                              % amelynek értéke az adott Ertek, ha
    permutation(SzamL, PL),   %     SzamL egy permutációja PL és
    kif1(PL, MuvL, Kif),      %     a PL levéllistából a MuvL mûveletekkel
                              %       felépíthetõ a Kif kifejezés és
    catch(Kif =:= E, _, fail).%     Kif számértéke Ertek.

% kif1(L, MuvL, Kif):         
kif1([Kif], _, Kif).          % Az egyelemu [Kif] listából Kif építhetõ fel.
kif1(L, MuvL, Kif) :-         % L-bõl a MuvL mûveletekkel felépíthetõ Kif ha
    append(L1, L2, L),        % L elõáll mint L1 és L2 összefûzése ahol
    L1 \= [], L2 \= [],       % L1 nem üres és L2 nem üres, és
    kif1(L1, MuvL, K1),       % L1-bõl a MuvL mûveletekkel felépíthetõ K1, és
    kif1(L2, MuvL, K2),       % L2-bõl a MuvL mûveletekkel felépíthetõ K2, és
    member(M, MuvL),          % M egy MuvL listábeli mûvelet, és
    Kif =.. [M,K1,K2].        % Kif egy az M mûvelettel a K1-bõl és K2-bõl 
                              % felépülõ kétargumentumú kifejezés.
