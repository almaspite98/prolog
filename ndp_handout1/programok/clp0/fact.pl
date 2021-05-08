:- use_module(mininat).

:- block fact00(-,-).
fact00(N, F) :-
        {N = 0, F = 1}.
fact00(N, F) :-
        {N1 = N-1},
        fact0(N1, F1),
	{F = N_*F1_},
	N_=N,
	F1_=F1.

:- block fact0(-,-).
fact0(N, F) :-
        {N = 0, F = 1}.
fact0(N, F) :-
        {N1 = N-1},
        fact0(N1, F1),
	{F = N*F1}.

:- block fact1(-,-).
fact1(N, F) :-
        {N = 0, F = 1}.
fact1(N, F) :-
        {N1 = N-1},
        {F = N*F1},  % korlát mielõbbi felvétele
        fact1(N1, F1).

:- block fact2(-,-).
fact2(N, F) :-
        {N = 0, F = 1}.
fact2(N, F) :-
        {N1 = N-1},
	{F1 >= N1},  % redundáns korlát bevezetése
        {F = N*F1},
        fact2(N1, F1).

:- use_module(library(between)).
:- use_module(library(timeout)).

test_f(Variant) :-
	between(5, 13, N0),
	int_to_peano(N0, N),
	statistics(runtime, _),
	time_out(call(user:Variant, N, F), 10000, Result),
	(   Result = time_out -> F = 'timed out'
	;   true
	),
	statistics(runtime, [_,MSec]),
	format('N = ~|~t~p~2+, F = ~|~t~p~10+, cpu time = ~|~t~3d~8+ sec~n', [N, F, MSec]),
	Result = time_out, !.
test_f(_).

fact_p(0, 1).
fact_p(N, F) :-
	N > 0,
	N1 is N-1,
	fact_p(N1, F1),
	F is N*F1.

test_b(Variant) :-
	between(1, 10, N0), fact_p(N0, F0),
	int_to_peano(F0, F),
	statistics(runtime, _),
	time_out(findall(N, call(user:Variant, N, F), Ns), 10000, Result),
	(   Result == time_out -> N = 'timed out'
	;   Ns = [] -> N = failed
	;   Ns = [N] -> true
	;   N = Ns
	),
	statistics(runtime, [_,MSec]),
	format('N = ~|~t~p~9+, F = ~|~t~p~10+, cpu time = ~|~t~3d~8+ sec~n', [N, F, MSec]),
	Result == time_out, !.
test_b(_).

end_of_file.



| ?- {F=17}, trace, fact00(N, F).
% The debugger will first creep -- showing everything (trace)
        1      1 Call: fact00(_873,17) ? 
        2      2 Call: {_873=0,17=1} ? s
        2      2 Fail: {_873=0,17=1} ? 
        3      2 Call: {_873>=1,_2239=_873-1} ? s
        3      2 Exit: {s(_2239)>=1,_2239=s(_2239)-1} ? 
        -      - Block: fact0(_3719,_3731)
        4      2 Call: {17=_3893*_3895} ? s
?       4      2 Exit: {17=1*17} ? 
        -      - Unblock: fact0(0,17)
        5      2 Call: fact0(0,17) ? s
        5      2 Fail: fact0(0,17) ? 
        4      2 Redo: {17=1*17} ? s
?       4      2 Exit: {17=17*1} ? 
        -      - Unblock: fact0(16,1)
        6      2 Call: fact0(16,1) ? 
        7      3 Call: {16=0,1=1} ? s
        7      3 Fail: {16=0,1=1} ? 
        8      3 Call: {16>=1,_6329=16-1} ? s
        8      3 Exit: {16>=1,15=16-1} ? 
        9      3 Call: fact0(15,_7793) ? s
! Resource error: insufficient memory
        9      3 Exception: fact0(15,_685) ? a
% Execution aborted
% source_info
| ?- 
