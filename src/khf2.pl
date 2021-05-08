-module(khf2).
-author('csaszar.mat@gmail.com').
-vsn('2015.10.20.').
:- use_module(library(lists)).

%% @spec inc_or_amend(Head::zsak_elem(), Tail::[zsak_elem()], Count::integer(), Element::any()) -> Zsak::zsak_alak().
%% Increment the Count on first match with Element. If there was no such a match amending to the end of the list with 1 as Count
inc_or_amend([], E, [E-1]).
inc_or_amend([H-C | T], E, [H-C1 | T1]) :-
	(E = H ->
		C1 is C + 1,
		T1 = T
	; otherwise ->
		C1 is C,
		inc_or_amend(T, E, T1)
	)
.

% :- pred lista_zsak(list(any)::in, zsak_alak::out).
% lista_zsak(+L, -Zsak): Az L lista zsák-alakja Zsak.
lista_zsak([], []).
lista_zsak([H | T], Bag) :-
	lista_zsak(T, Result),
	inc_or_amend(Result, H, Bag)
.

guess(Max, DontUse, Guess) :-
	Max > 0,
	(Max == DontUse ->
		Max0 is Max - 1,
		guess(Max0, DontUse, Guess)
	; otherwise ->
		(
			Guess is Max
		;
			Max0 is Max - 1,
			guess(Max0, DontUse, Guess)
		)
	)
.

% :- type code          == list(int).
% :- type simple_hint ---> code-int.
% :- pred tipp_kod(int::in, simple_hint::in, code::out).
% tipp_kod(Max, Tipp-S, Kod): Kod a Max paraméternek és a Tipp-S párnak megfelel.
tipp_kod(_, []-S, []) :-
	S == 0
.
tipp_kod(Max, [TippHead | TippTail]-S, Kod) :-
	% Ebből a Tipp-ből kiválasztunk S db elemet permutálva
	% Ezekhez még lenght(Tipp)-ig töltjük 1..Max-al de csak és kizárolag inkrementálisan
	% Ezeket az elemeket permutáljuk
	
	(
		S0 is S - 1,
		tipp_kod(Max, TippTail-S0, Kod0),
		TippGoodPart = [TippHead | Kod0]
	;
		tipp_kod(Max, TippTail-S, Kod0),
		guess(Max, TippHead, TippHead0),
		TippGoodPart = [TippHead0 | Kod0]
	),
	
	Kod = TippGoodPart
.

%tipp_kod(Max, Tipp-S, Kod) :-
%	length(Tipp, LenghtTipp),
%	findall(X, proba(Max, LenghtTipp, X), P),
%	maplist(keysort, P, SP),
%	sort(SP, SortedP),
%	member(BR, SortedP),
%	lista_zsak(Tipp, BT),
%	zsak_metszet(BR, BT, BM),
%	zsak_elemszam(BM, NBM),
%	NBM is S,
%	perms(BR, Kod)
%.



%tipp_kod(Max, [TippHead | TippTail]-S, Kod) :-
%	(S > 0 ->
%		(
%			S0 is S - 1,
%			tipp_kod(Max, TippTail-S0, Kod0),
%			Kod = [TippHead | Kod0]
%		;
%			tipp_kod(Max, TippTail-S, Kod0),
%			guess(Max, TippHead, TippHead0),
%			Kod = [TippHead0 | Kod0]
%		)
%	; otherwise ->
%		tipp_kod(Max, TippTail-S, Kod0),
%		guess(Max, TippHead, TippHead0),
%		Kod = [TippHead0 | Kod0]
%	)
