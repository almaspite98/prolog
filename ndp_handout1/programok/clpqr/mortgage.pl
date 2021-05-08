:- use_module(library(clpr)).

% Hiteltörlesztés számítása: P összegû hitelt
% Time hónapon át évi IntRate kamat mellett havi MP
% részletekben törlesztve Bal a maradványösszeg.
mortgage(P, Time, IntRate, Bal, MP):-
     {Time > 0, Time =< 1,
     Bal = P*(1+Time*IntRate/1200)-Time*MP}.
mortgage(P, Time, IntRate, Bal, MP):-
     {Time > 1},
     mortgage(P*(1+IntRate/1200)-MP, Time-1, IntRate, Bal, MP).



% | ?- mortgage(100000,180,12,0,MP).    % 100000 Ft hitelt 180 
%                                     	% hónap alatt törleszt 12%-os
%                                     	% kamatra, mi a havi részlet?
% MP = 1200.1681 ? 
% 
% 
% | ?- mortgage(P,180,12,0,1200).     	% ugyanez visszafelé
% 
% P = 99985.9968 ? 
% 
% 
% | ?- mortgage(100000,Time,12,0,1300). % 1300 Ft a törlesztõrészlet
%                                       % mi a törlesztési idõ?
% Time = 147.3645 ? 
% 
% 
% | ?- mortgage(P,180,12,Bal,MP). 
% 
% {MP=0.0120*P-0.0020*Bal} ? 
% 
% 
% | ?- mortgage(P,180,12,Bal,MP), ordering([P,Bal,MP]).
% 
% {P=0.1668*Bal+83.3217*MP} ? 

%---------------------------------------------------------------------------
