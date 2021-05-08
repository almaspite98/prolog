%-module(khf1).
%-author('almaspite98@gmail.com').
%-vsn('2020.10.12.').
%-export([ main/0]).


helyettesitese(K, _, K):-
	integer(K).
helyettesitese(_, [], 0).
helyettesitese(C, [C-N|_], N).
helyettesitese(K, [_-_|T], H):-
	helyettesitese(K, T, H).


erteke(Kif, Hely, E):-
    Kif =..[Op, A, B],
    erteke(A, Hely, EA),
    erteke(B, Hely, EB),
    Eq =..[Op, EA, EB],
    E is Eq.
erteke(Kif, Hely, E):-
    Kif =.. [Op, A],
    erteke(A, Hely, EA),
    Eq=..[Op, EA],
    E is Eq.
erteke(A, Hely, EA):-
    atomic(A),
    helyettesitese(A, Hely, EA).


	

main :-
	print('hello world'), nl,
%	 helyettesitese(y, [x-1,y-2,z-3], H), print(H),nl,
%	 helyettesitese(u, [x-1,y-2,z-3], H2), print(H2),nl,
%	 helyettesitese(x, [x-1,z-3,x-2], H3), print(H3),nl,
%	  helyettesitese(4, [x-1,y-2,z-3], H4), print(H4),nl,

    erteke(x+y, [x-5,y-3],H), print(H),nl,
    erteke((x+y)*(x+y)+1, [x-1,y-2], E), print(E),nl,
    erteke(x*abs(z)+x, [x-1,z-(-2)], E1), print(E1),nl,
    erteke(-x, [x-5], E2), print(E2),nl,
    erteke(x, [x-5], E3), print(E3),nl,


    nl,print('Bye'), nl,
	nl,halt.
