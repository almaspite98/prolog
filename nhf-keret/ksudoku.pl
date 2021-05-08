% :- type file        ==  atom.

% :- type sspec ---> s(dist, board).
% :- type dist  == int.
% :- type field == list(info).
% :- type info ---> s; w; int.
% :- type board == list(list(field)).

% :- type ssol == list(list(int)).

% sudoku(SSpec, SSol):
% SSol az SSpec feladványt kielégítõ megoldás.
% :- pred sudoku(sspec::in, ssol::out).

:- module(sudoku_keret,
	  [
	   sudoku_be/2,		% sudoku_be(file::in, sspec::out)
	   sudoku_ki/2,		% sudoku_ki(file::in, sspec::in)
	   megold/2,		% megold(file::in, file::in)
	   stopper/2,		% stopper(file::in, file::in)
           teljes_teszt/1       % teljes_teszt(integer::in)
	  ]
	 ).

:- use_module(library(lists), [nth1/3,select/3]).
:- use_module(library(between), [between/3]).

%-------------- Bejaratok ------------------------------------

% sudoku_be(+File, -Feladvany):
% A File-bol beolvashato a Feladvany altal megadott feladat.
sudoku_be(File, Feladvany) :-
	open_file(File, read, Stream),
	call_cleanup(read_in(Stream, Characters),
		     close(Stream)),
	parse_data(Characters, Feladvany).

% sudoku_ki(+File, +Feladvany):
% A Feladvany feladat megoldasat kiirja a File allomanyba.
sudoku_ki(File, Feladvany) :-
	megoldasok(Feladvany, Megoldasok),
	open_file(File, write, Stream),
	current_output(OOut),
	set_output(Stream),
	call_cleanup(print_solutions(Megoldasok),
		     (set_output(OOut),close(Stream))).

% megoldasok(+Feladvany, -Megoldasok):
% A Feladvany feladat osszes megoldasa
% a Megoldasok rendezett lista.
megoldasok(Feladvany, Megoldasok) :-
	findall(Megoldas,
		user:sudoku(Feladvany, Megoldas),
		Megoldasok0),
	sort(Megoldasok0, Megoldasok).

% megold(+FileIn, +FileOut):
% A FileIn allomanybol beolvasott feladat minden megoldasat
% kiirja a FileOut allomanyba.
megold(FileIn, FileOut) :-
	sudoku_be(FileIn, Feladvany),
	ensure_loaded(user:sudoku),
	megoldasok(Feladvany, Megoldasok),
	open_file(FileOut, write, Stream),
	current_output(OOut),
	set_output(Stream),
	call_cleanup(print_solutions(Megoldasok),
		     (set_output(OOut),close(Stream))).

% stopper(+FileIn, +FileOut):
% Mint megold/2, de a futashoz szukseges idot is kiirja.
stopper(FileIn, FileOut) :-
	stopper(FileIn, FileOut, []).

% stopper(+FileIn, +FileOut, +Options): Mint stopper/2 de elfogad egy
% Options opciolistat.  Ha az opciolista eleme sols(Sols), akkor a
% megoldaslistat egyesiti Sols-sal. Ha az opciolista eleme nonl, akkor a
% a kiiras utan nem emel sort.
stopper(FileIn, FileOut, Options) :-
	sudoku_be(FileIn, Feladvany),
	ensure_loaded(user:sudoku),
	statistics(runtime, [T0|_]),
	megoldasok(Feladvany, Megoldasok),
	statistics(runtime, [T1|_]),
	T is T1 - T0,
	length(Megoldasok, H),
	format('Feladvány: ~a, megoldásszám:~|~t~d~4+, futási idõ:~|~t~3d~8+ s', [FileIn,H,T]),
	(   memberchk(nonl, Options) -> true
	;   nl
	),
	(   memberchk(sols(Sols), Options) -> Sols = Megoldasok
	;   true
	),
	open_file(FileOut, write, Stream),
	current_output(OOut),
	set_output(Stream),
	call_cleanup(print_solutions(Megoldasok),
		     (set_output(OOut),close(Stream))).

%-------------- Allomanyok kezelese --------------------------

% open_file(+File, +Mode, -Stream):
% A File allomanyt megnyitja a Stream folyam formajaban Mode modban.
open_file(user, M, S) :-
	!, standard_stream(M, S).
open_file(File, Mode, Stream) :-
	open(File, Mode, Stream).

% standard_stream(?Mode, ?Stream):
% A Mode modu szabvanyos folyam a Stream.
standard_stream(read, user_input).
standard_stream(write, user_output).

%-------------- Beolvasas ------------------------------------

% read_in(+S, -L):
% S folyambol beolvashato karakterek listaja L.
read_in(S, L) :-
	get_code(S, C),
	read_in(C, S, L).

% read_in(+C, +S, -L):
% Ha C az S folyambol elozoleg beolvasott karakter, akkor L az S-bol
% beolvashato maradek karakterek listaja, C-t is beleertve.
% Ha C = -1, akkor L = [].
read_in(C, _, L) :-
	C =:= -1, !, L = [].
read_in(C, S, [C|L]) :-
	get_code(S, C1),
	read_in(C1, S, L).

% parse_data(+Chars, -Feladvany):
% Chars fuzerbol kielemezheto egy Feladvany feladvanyleiro
parse_data(Chars, Feladvany) :-
	descriptor(Feladvany0, Chars, Rest),
	correct_syntax(Rest),
	consistent(Feladvany0),
	!,
	Feladvany = Feladvany0.
parse_data(_, _) :-
	warning('A bemeneti allomany nem megfelelo formatumu.',[]),
	fail.

% A Feladvany egy n=k*k oldalu negyzetes matrix, amelyben a
% szaminfok 1..n koze esnek, mig a tavolsagkonstans 1..(n-1) koze esik.
consistent(s(D,Mx)) :-
	  length(Mx, N),
	  K is integer(sqrt(N)),
	  N =:= K*K,
	  \+ (member(Row, Mx), \+ length(Row, N)),
	  \+ (member(Row, Mx), member(Is, Row), member(I, Is),
	      integer(I), ( I < 1; I > N) ),
	  1 =< D, D =< N-1.

% correct_syntax(+Rest):
% A Rest fuzer a bemenet helyes maradeka, azaz ures.
correct_syntax(Rest) :-
	Rest = [_|_], !,
	chars_of_type(mid_line, L, Rest, _),
	warning('Nem vart karakter(ek): "~s"', [L]),
	fail.
correct_syntax(_).

%-------------- DCG elemzes ----------------------------------

% descriptor(SSpec): Kielemezheto egy SSpec feladvany-leiro
descriptor(s(K,Board)) -->
	spaces,
	nat_number(K),
	newline,
	board_data(Board),
	chars_of_type(layout, _).

% Kielemezheto egy (tizes szamrendszerbeli) Int termeszetes szam.
nat_number(Int) -->
	chars_of_type(digit, Ds),
	{Ds = [_|_], number_codes(Int, Ds)}.

% board_data(Board): Kielemezheto egy Board sudoku (resz)tablazat.
board_data([R|Rs]) -->
	board_row(R),
	newline, !,
	chars_of_type(layout, _),
	board_data(Rs).
board_data([]) --> [].

% board_row(R): Kielemezheto tablazatmezok egy R listaja
board_row([F|Fs]) -->
	spaces,
	field(F), !,
	board_row(Fs).
board_row([]) --> [].

% field(N, F): Kielemezheto egy N*N-es feladvany F tablazat-mezoje.
field([]) -->
	"-" .
field(Is) -->
	{ Is = [_|_] },
	infos("vsw", Is).

% infos(Ts, N, Is) Kielemezheto egy N*N-es feladvanybeli infok
% Ts tipusu Is listaja.
infos(Ts0, [I|Is]) -->
	info(Ts0, I, Ts1), !,
	infos(Ts1, Is).
infos(_, []) --> [].

% info(Ts0, I, Ts): Kielemezheto egy a Ts0 tipusok 
% egyikebe tartozo I info, Ts az ezutan fennmarado tipusok listaja.
info(Ts0, I, Ts) -->
	{select(T, Ts0, Ts)},
	(   {T = 0'v}, nat_number(I)
	;   [T], 
	    {atom_codes(I, [T])}
	), !.
	
% Kielemezheto egy ujsor.
newline -->
	spaces,
	char_of_type(vertical, _).

% Kielemezheto vizszintes iranyu nem lathato karakterek
% egy sorozata.
spaces -->
	chars_of_type(horizontal, _).

% layout_char(Dir, C):
% horizontal_layout_char(C): C  egy vizszintes iranyu
% nem lathato karakter.
horizontal_layout_char(0' ).
horizontal_layout_char(0'\t).
horizontal_layout_char(0'\r).

% vertical_layout_char(C): C  egy fuggoleges iranyu
% nem lathato karakter.
vertical_layout_char(0'\n).
vertical_layout_char(0'\f).
vertical_layout_char(0'\v).

% Kielemezheto egy Type tipusu C karakter.
char_of_type(Type, C) -->
	[C], {char_type(Type, C)}.

% chars_of_type(Type, Cs) -->
% Kielemezheto Type tipusu karakterek egy Cs listaja.	
chars_of_type(Type, [C|Cs]) -->
	char_of_type(Type, C), !,
	chars_of_type(Type, Cs).
chars_of_type(_, []) --> [].

% char_type(Type, C) :
% A C karakter Type tipusu.
char_type(layout, C) :-
	(   vertical_layout_char(C) -> true
	;   horizontal_layout_char(C)
	).
char_type(digit, D) :-
	D >= 0'0, D =< 0'9.
% Csak kisbetuket fogadunk el.
char_type(alpha, A) :-
	A >= 0'a, A =< 0'z.
char_type(mid_line, C) :-
	\+ vertical_layout_char(C).
char_type(horizontal, C) :-
	horizontal_layout_char(C).
char_type(vertical, C) :-
	vertical_layout_char(C).

%-------------- Kiiratas -------------------------------------

% print_solutions(+Mode, +Megoldasok):
% A Megoldasok megoldaslista minden elemet
% kiirja az aktualis kimenetre.
print_solutions(Mode, Megoldasok) :-
	(  Mode == sorted ->
	    sort(Megoldasok, Rmegoldasok),
	    format('~q.\n', [Rmegoldasok])
	;  Mode == asis ->
	    format('~q.\n', [Megoldasok])
	;  Mode = spretty ->
	    print_pretty_solutions(Megoldasok)
	).

print_pretty_solutions(Megoldasok) :-
	member(Megoldas, Megoldasok),
	write('\n-----'),
	print_board(Megoldas),
	fail.
print_pretty_solutions(_) :-
	write('\n-----\n').

print_solutions(Megoldasok) :-
    print_solutions(spretty, Megoldasok).

% print_board(Tabla)
% Kiirja a Tabla tablat, ami lehet kitoltetlen is
print_board(B) :-
	length(B,H),
	K is integer(round(sqrt(H))),
	B = [R1|_],
	length(R1,W),
	between(1,H,Y),
	nth1(Y,B,R),
	nl,
	((Y mod K =:= 1 ; K == 1) -> nl ; true),
	between(1,W,X),
	nth1(X,R,F),
	Tab is 1+floor(log(10,K*K)+1)-floor(log(10,max(1,F))+1),
        (   between(1, Tab, _), write(' '), fail % Tab db. szóköz kiírása
        ;   true
        ),
	(X > 1, X mod K =:= 1 -> write(' ') ; true),	
	write(F),
	fail.
print_board(_) :- nl.

%-------------- Seged-eljaras ------------------------------

% warning(+Txt, +Args):
% Kiirja a Txt szoveggel es Args argumentumlistaval
% megadott figyelmezteto szoveget.
warning(Txt, Args) :-
	print_message(warning, format(Txt, Args)).

%-------------- ISO kompatibilitas ---------------------------

% Az alabbi cel az ISO Prolog szabvanynak meg nem megfelelo, de
% DEC10 kompatiblis Prolog rendszerekben valo futtatashoz szukseges.

:- (   predicate_property(get_code(_,_), _) -> true
   ;   assert((get_code(S, C) :- get0(S, C))),
       assert((put_code(C) :- put(C))),
       assert((number_codes(N, C) :- number_chars(N, C)))
   ).

%-------------- experimental ---------------------------
% Az alábbi eljárást Eisenberger András ültette át Prologra Erlangból, és egészítette ki a megoldás ellenörzésével.

:- use_module(library(file_systems)).
:- use_module(library(timeout)).
:- use_module(library(samsort)).

% A 'tests' könyvtárban levõ összes "testXXXd.txt" tesztállomány esetén
%  - lefuttatja a tesztet Timeout másodperces idõkorláttal,
%  - ellenõrzi, hogy a testXXXs.txt állományban megadott megoldáshalmazt kapta,
%  - olvasható formában (lásd megold/2) kiírja az eredményt a 'tests_out_pl'
%    könyvtár testXXXt.txt nevû állományába.
% Az állománynevekben az XXX szám tetszõleges hosszúságú lehet.
teljes_teszt(Timeout) :-
	Time_MS is Timeout * 1000,
	(   directory_exists(tests_out_pl) -> true
	;   make_directory(tests_out_pl)
	),
	file_members_of_directory('tests', 'test*d.txt', FoundFiles),
	(   member(BaseName-_AbsPath, FoundFiles),
	    atom_concat('tests/', BaseName, InPath),
	    atom_concat(TestName, 'd.txt', BaseName),
	    atom_concat('tests/', TestName, 's.txt', SolsPath),
	    atom_concat('tests_out_pl/', TestName, 't.txt', OutPath),
            time_out(stopper(InPath, OutPath, [nonl,sols(Sols)]),
		     Time_MS, Result),
            (   Result == success ->
		samsort(Sols, SolsSorted),
		catch(read_term_from_file(SolsPath, SolsRead), _,
		      SolsRead = none),
		(   SolsRead == none -> write(' NINCS MEGOLDÁS\n')
		;   SolsRead == SolsSorted -> write(' HELYES\n')
		;   write(' NEM HELYES\n')
		)
	    ;	format('Feladvány: ~a, túllépte az idõkorlátot (~d s)~n',
		       [InPath,Timeout])
            ),
	    fail
	;   true
	).

% atom_concat(A, B, C, ABC): Az A, B es C atomok összefûzése ABC.
atom_concat(A, B, C, ABC) :-
	atom_concat(A, B, AB),
	atom_concat(AB, C, ABC).

% read_term_from_file(File, Term): A Term Prolog kifejezést beolvassa a
% File állományból.
read_term_from_file(File, Term) :-
	open_file(File, read, Stream),
	call_cleanup(read(Stream, Term),
		     close(Stream)).
