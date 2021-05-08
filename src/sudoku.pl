:- use_module(library(lists)), use_module(library(clpq)).
:- use_module(library(clpb)).
:- use_module(library(clpfd)).

%:- (   current_prolog_flag(dialect, sicstus)
%   ->  use_module(library(between)),
%       use_module(library(samsort))
%   ;   assert((samsort(L, S) :- sort(0, @=<, L, S)))
%   ).
%:- use_module(library(clpq)).

fv(X, Y, Z):-
    {X=Y+4, Y=Z-1, Z=2*X-9}.

main :-
%    print('cella([[1]], 1, C): '),nl,cella([[1]], 1, C),
    print('cella([[1]], 1, C): '),nl,
    print('cella([[1]], 1, C): '),nl,fv(X,Y, Z),
    print(X),nl,
    print(Y),nl,
    print(Z),nl,


    nl.
