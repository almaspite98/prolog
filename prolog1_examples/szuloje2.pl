% sz(Gy, Sz): Gy szuloje Sz.
sz(im, gi).                           % (sz1)
sz(im, is).                           % (sz2) 
sz(is, sa).                           % (sz3)
sz(is, ge).                           % (sz4)
sz(gi, bg).                           % (sz5)
sz(gi, ch).                           % (sz6)

% Gyerek nagyszülõje Nagyszulo.
nsz(Gyerek, Nagyszulo) :-             % (nsz1)
    sz(Gyerek, Szulo),      
    sz(Szulo, Nagyszulo).

% ffi(X): X ferfi.
ffi(im).                              % (f1)
ffi(is).                              % (f2)
ffi(ge).                              % (f3)
ffi(ch).                              % (f4)


% Gyerek nagyszuloje Nagyszulo.
nsz_hatekonyabb(Gyerek, Nagyszulo) :- 
    nonvar(Gyerek),
    sz(Gyerek, Szulo),      
    sz(Szulo, Nagyszulo).
nsz_hatekonyabb(Gyerek, Nagyszulo) :- 
    var(Gyerek),
    sz(Szulo, Nagyszulo),
    sz(Gyerek, Szulo).


end_of_file.

| ?- nsz(im, NA), ffi(NA).
NA = ch ? ;
NA = ge ? ;
no
| ?- nsz(im, Nsz).
Nsz = ge ? ;
Nsz = sa ? ;
Nsz = ch ? ;
Nsz = bg ? ;
no
| ?- nsz(U, ge).
U = im ? ;
no
| ?- 
