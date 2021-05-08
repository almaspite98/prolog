:- use_module(library(clpq)).

/*
| ?- {X=Y+4, Y=Z-1, Z=2*X-9}.          	% lineáris egyenlet
X = 6, Y = 2, Z = 3 ? 


| ?- {X+Y+9<4*Z, 2*X=Y+2, 2*X+4*Z=36}. 	% egyenlõtlenség is lehet
{X<29/5},
{Y= -2+2*X},
{Z=9-1/2*X} ? 

| ?- {(Y+X)*(X+Y)/X = Y*Y/X+100}. 	% lineárissá egyszerûsíthetõ

{X=100-2*Y} ? 


| ?- {(Y+X)*(X+Y) = Y*Y+100*X}. 	% így már nem...
clpq:{2*(X*Y)-100*X+X^2=0} ?

| ?- {exp(X+Y+1,2) = 3*X*X+Y*Y}.        % nem lineáris...

clpq:{1+2*X+2*(Y*X)-2*X^2+2*Y=0} ? 


| ?- {exp(X+Y+1,2) = 3*X*X+Y*Y}, X=Y. 	% így már igen...

X = -1/4,
Y = -1/4 ?

| ?- {2 = pow(8, X)}.		% egyes nem-lineárisak is megoldhatók

X = 1/3 ?

---------------------------------------------------------------------------

| ?- {X =< Y}, {X*(Y+1) > X*X+Z}, 
       (   Z = X*(Y-X), {Y < 0}
       ;   Y = X
       ).
                Y = X,  {X-Z>0} ? ;   no

---------------------------------------------------------------------------

| ?- {X =< Y}, {X*(Y+1) > X*X+Z}.

                {X-Y=<0}, clpq:{Z-X-Y*X+X^2<0} ?

| ?- {X =< Y}, {X*(Y+1) > X*X+Z}, Z = X*(Y-X).

                Z = X*(Y-X), {X-Y=<0}, {X>0} ? 

| ?- {X =< Y}, {X*(Y+1) > X*X+Z}, Z = X*(Y-X), {Y < 0}.
no
| ?- {X =< Y}, {X*(Y+1) > X*X+Z}, {Y =< X}.

                Y = X, {X-Z>0} ? 

---------------------------------------------------------------------------

| ?- { 2*X+Y =< 16, X+2*Y =< 11, X+3*Y =< 15, Z = 30*X+50*Y
     }, sup(Z, Sup).

Sup = 310, {Z=30*X+50*Y}, {X+1/2*Y=<8}, {X+3*Y=<15}, {X+2*Y=<11}

---------------------------------------------------------------------------

| ?- { 2*X+Y =< 16, X+2*Y =< 11, X+3*Y =< 15, Z = 30*X+50*Y
     }, maximize(Z).

X = 7, Y = 2, Z = 310

---------------------------------------------------------------------------

| ?- {X >= 0.5, Y >= 0.5}, inf(X+Y, I).

I = 1, {Y>=1/2}, {X>=1/2} ?

| ?- {X >= 0.5, Y >= 0.5}, bb_inf([X,Y], X+Y, I).

I = 2, {X>=1/2}, {Y>=1/2} ?

---------------------------------------------------------------------------

| ?- { 2*X+Y =< 16, X+2*Y =< 11, X+3*Y =< 15, Z = 30*X+50*Y
     }, sup(Z, Sup).

Sup = 310, {Z=30*X+50*Y}, {X+1/2*Y=<8}, {X+3*Y=<15}, {X+2*Y=<11}

---------------------------------------------------------------------------
*/

% Az (X,Y) pont az (1,2) (1,4) (2,4) pontok
% által kifeszített háromszögben van.
hszogben(X,Y) :-
        { X=1*L1+1*L2+2*L3,
          Y=2*L1+4*L2+4*L3,
          L1+L2+L3=1, L1>=0, L2>=0, L3>=0 }.

% | ?- hszogben(X,Y).
%                      {Y=<4}, {X>=1}, {X-1/2*Y=<0} ?
% 
% | ?- hszogben(_, Y).
%                      {Y=<4}, {Y>=2} ?
% 
% | ?- hszogben(X, _).
%                      {X>=1}, {X=<2} ?

%---------------------------------------------------------------------------

% | ?- {X=0.5}, X=0.5.
% no
% | ?- {X=0.5}, X=1/2.
% no
% | ?- {X=0.5}, X=rat(2,4).
% no
% | ?- {X=0.5}, X=rat(1,2).
% X = 1/2 ?
% | ?- {X=5}, X=5.
% no
% | ?- {X=5}, X=rat(5,1).
% X = 5 ?

%---------------------------------------------------------------------------

% Rectangle 1 x Width is covered by distinct squares of size Ss.
filled_rectangle(Width, Ss) :-
  { Width >= 1 }, distinct_squares(Ss),
  filled_hole([-1,Width,1], _, Ss, []).

distinct_squares([]).
distinct_squares([S|Ss]) :-
  { S > 0 }, outof(Ss, S), distinct_squares(Ss).

outof([],     _).
outof([S|Ss], S0) :- { S =\= S0 }, outof(Ss, S0).

% filled_hole(L0, L, Ss0, Ss): Hole in line L0
% filled with squares Ss0-Ss (diff list) gives line L.
filled_hole(L, L, Ss, Ss) :-
  L = [V|_], {V >= 0}.
filled_hole([V|HL], L, [S|Ss0], Ss) :-
  { V < 0 }, placed_square(S, HL, L1),
  filled_hole(L1, L2, Ss0, Ss1), { V1=V+S },
  filled_hole([V1,S|L2], L, Ss1, Ss).

% placed_square(S, HL, L): placing a square on
% HL horizontal line gives (vertical) line L.
placed_square(S, [H,V,H1|L], L1) :-
  { S > H, V=0, H2=H+H1 },
  placed_square(S, [H2|L], L1).
placed_square(S, [S,V|L], [X|L]) :-
  { X=V-S }.
placed_square(S, [H|L], [X,Y|L]) :-
  { S < H, X= -S, Y=H-S }.

% | ?- length(Ss, 9), filled_rectangle(Width, Ss).
%
%     Ss = [15/32,9/16,1/4,7/32,1/8,7/16,1/32,5/16,9/32],
%     Width = 33/32 ?

%---------------------------------------------------------------------------

filled_rectangle1(Width, Ss) :-
  { Width >= 1 }, nonempty_squares(Ss),
  filled_hole([-1,Width,1], _, Ss, []).

nonempty_squares([]).
nonempty_squares([S|Ss]) :-
  { S > 0 }, nonempty_squares(Ss).

%---------------------------------------------------------------------------

% | ?- length(Ss, 3), filled_rectangle1(Width, Ss).
% Ss = [1/2,1,1/2],
% Width = 3/2 ? ;
% Ss = [1,1/2,1/2],
% Width = 3/2 ? ;
% Ss = [1,1,1],
% Width = 3 ? ;
% no

% | ?- filled_rectangle1(W, Ss).
% 
% W =   1, Ss = [  1]
% W =   2, Ss = [  1,  1]
% W = 3/2, Ss = [1/2,  1,1/2]
% W = 3/2, Ss = [  1,1/2,1/2]
% W =   3, Ss = [  1,  1,  1]
% W = 4/3, Ss = [1/3,  1,1/3,1/3]
% W =   1, Ss = [1/2,1/2,1/2,1/2]
% W = 5/3, Ss = [2/3,  1,1/3,1/3]
% W = 4/3, Ss = [  1,1/3,1/3,1/3]
% W = 5/3, Ss = [  1,2/3,1/3,1/3]
% W = 5/3, Ss = [1/3,1/3,  1,2/3]
% W = 5/2, Ss = [1/2,  1,  1,1/2]
% W = 5/3, Ss = [  1,1/3,1/3,2/3]
% W = 5/2, Ss = [  1,1/2,  1,1/2]
% W = 5/2, Ss = [  1,  1,1/2,1/2]
% W =   4, Ss = [  1,  1,  1,  1]
% W = 5/4, Ss = [1/4,  1,1/4,1/4,1/4]
% W = 7/5, Ss = [2/5,  1,2/5,1/5,1/5]
% W = 7/6, Ss = [1/2,2/3,1/2,1/3,1/3]
% W = 6/5, Ss = [3/5,3/5,2/5,2/5,2/5]
% W = 7/5, Ss = [2/5,  1,1/5,1/5,2/5]
% W = 8/5, Ss = [3/5,  1,1/5,2/5,1/5]
% W = 8/5, Ss = [3/5,  1,2/5,1/5,1/5]
% W = 7/4, Ss = [3/4,  1,1/4,1/4,1/4]
% W = 7/6, Ss = [2/3,1/2,1/2,1/3,1/3]
% W = 5/4, Ss = [  1,1/4,1/4,1/4,1/4]
% W = 7/5, Ss = [  1,2/5,2/5,1/5,1/5]
% W = 7/5, Ss = [  1,2/5,1/5,1/5,2/5]
% W = 8/5, Ss = [  1,3/5,1/5,2/5,1/5]
% W = 8/5, Ss = [  1,3/5,2/5,1/5,1/5]
% ...
% 
% | ?- {W = 3/2, S = 1/2, B = 1}, filled_rectangle1(W, [S,B,S]).
%         1      1 Call: {_747=3/2,_843=1/2,_939=1} ? s
%         1      1 Exit: {3/2=3/2,1/2=1/2,1=1} ? 
%         2      1 Call: filled_rectangle1(3/2,[1/2,1,1/2]) ? 
%         3      2 Call: {3/2>=1} ? s
%         3      2 Exit: {3/2>=1} ? 
%         4      2 Call: nonempty_squares([1/2,1,1/2]) ? s
%         4      2 Exit: nonempty_squares([1/2,1,1/2]) ? 
%         5      2 Call: filled_hole([-1,3/2,1],_8057,[1/2,1,1/2],[]) ? 
%         6      3 Call: {-1<0} ? 
%         6      3 Exit: {-1<0} ? 
%         7      3 Call: placed_square(1/2,[3/2,1],_10117) ? s
%         7      3 Exit: placed_square(1/2,[3/2,1],[ -1/2,1,1]) ? 
%         8      3 Call: filled_hole([ -1/2,1,1],_11873,[1,1/2],_11877) ? 
%         9      4 Call: { -1/2>=0} ? 
%         9      4 Fail: { -1/2>=0} ? 
%        10      4 Call: { -1/2<0} ? 
%        10      4 Exit: { -1/2<0} ? 
%        11      4 Call: placed_square(1,[1,1],_13933) ? s
% ?      11      4 Exit: placed_square(1,[1,1],[0]) ? 
%        12      4 Call: filled_hole([0],_15395,[1/2],_15399) ? 
%        13      5 Call: {0>=0} ? 
%        13      5 Exit: {0>=0} ? 
% ?      12      4 Exit: filled_hole([0],[0],[1/2],[1/2]) ? 
%        14      4 Call: {_18051= -1/2+1} ? 
%        14      4 Exit: {1/2= -1/2+1} ? 
%        15      4 Call: filled_hole([1/2,1,0],_11873,[1/2],_11877) ? 
%        16      5 Call: {1/2>=0} ? 
%        16      5 Exit: {1/2>=0} ? 
% ?      15      4 Exit: filled_hole([1/2,1,0],[1/2,1,0],[1/2],[1/2]) ? 
% ?       8      3 Exit: filled_hole([ -1/2,1,1],[1/2,1,0],[1,1/2],[1/2]) ? 
%        17      3 Call: {_22943= -1+1/2} ? 
%        17      3 Exit: { -1/2= -1+1/2} ? 
%        18      3 Call: filled_hole([ -1/2,1/2,1/2,1,0],_8057,[1/2],[]) ? 
%        19      4 Call: { -1/2<0} ? 
%        19      4 Exit: { -1/2<0} ? 
%        20      4 Call: placed_square(1/2,[1/2,1/2,1,0],_26555) ? s
% ?      20      4 Exit: placed_square(1/2,[1/2,1/2,1,0],[0,1,0]) ? 
%        21      4 Call: filled_hole([0,1,0],_28023,[],_28027) ? 
%        22      5 Call: {0>=0} ? 
%        22      5 Exit: {0>=0} ? 
% ?      21      4 Exit: filled_hole([0,1,0],[0,1,0],[],[]) ? 
%        23      4 Call: {_30679= -1/2+1/2} ? 
%        23      4 Exit: {0= -1/2+1/2} ? 
%        24      4 Call: filled_hole([0,1/2,0,1,0],_8057,[],[]) ? 
%        25      5 Call: {0>=0} ? 
%        25      5 Exit: {0>=0} ? 
% ?      24      4 Exit: filled_hole([0,1/2,0,1,0],[0,1/2,0,1,0],[],[]) ? 
% ?      18      3 Exit: filled_hole([ -1/2,1/2,1/2,1,0],[0,1/2,0,1,0],[1/2],[]) ? 
% ?       5      2 Exit: filled_hole([-1,3/2,1],[0,1/2,0,1,0],[1/2,1,1/2],[]) ? 
% ?       2      1 Exit: filled_rectangle1(3/2,[1/2,1,1/2]) ? 
% W = 3/2,
% S = 1/2,
% B = 1 ? 
% yes
% % trace,source_info
% | ?- placed_square(1/2,[3/2,1],L1).
%         1      1 Call: placed_square(1/2,[3/2,1],_913) ? 
%         2      2 Call: {1/2<3/2,_2097= - (1/2),_2101=3/2-1/2} ? 
%         3      3 Call: {1/2<3/2} ? 
%         3      3 Exit: {1/2<3/2} ? 
%         4      3 Call: {_2097= - (1/2),_2101=3/2-1/2} ? 
%         5      4 Call: {_2097= - (1/2)} ? 
%         5      4 Exit: { -1/2= - (1/2)} ? 
%         6      4 Call: {_2101=3/2-1/2} ? 
%         6      4 Exit: {1=3/2-1/2} ? 
%         4      3 Exit: { -1/2= - (1/2),1=3/2-1/2} ? 
%         2      2 Exit: {1/2<3/2, -1/2= - (1/2),1=3/2-1/2} ? 
%         1      1 Exit: placed_square(1/2,[3/2,1],[ -1/2,1,1]) ? 
% L1 = [ -1/2,1,1] ? 
% yes
% | ?- notrace.
% % The debugger is switched off
% yes
% % source_info
% | ?- placed_square(S,[3/2,1],L1).
% S = 3/2,
% L1 = [ -1/2] ? ;
% L1 = [_A,_B,1],
% {S<3/2},
% {_A= -S},
% {_B=3/2-S} ? ;
% no
% % source_info
% | ?- 
