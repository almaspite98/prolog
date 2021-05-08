os(X,Y) :-
	sz(X,Y).
os(X,Y) :-
	sz(X,Z),
	os(Z,Y).             

generaciok(4).

sz(L, LSz) :-
	generaciok(G), length(L, Len), Len =< G,
	LSz = [MF|L],
	(   MF = '1'
	;   MF = '2'
	).

:- use_module(library(lists)).

portray(L) :-
	L = [_|_], reverse(L, LR),
	atom_chars(A, LR),
	write(A).

end_of_file.


sz(a,b).
sz(a,c).
sz(b,d).
sz(c,e).
sz(c,f).
sz(e,g).

