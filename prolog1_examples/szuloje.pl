% szuloje(Gy, Sz): Gy szuloje Sz.
szuloje('Imre', 'Gizella').                   % (sz1)
szuloje('Imre', 'Istvan').                    % (sz2) 
szuloje('Istvan', 'Sarolt').                  % (sz3)
szuloje('Istvan', 'Geza').                    % (sz4)
szuloje('Gizella', 'Burgundi Gizella').       % (sz5)
szuloje('Gizella', 'Civakodo Henrik').        % (sz6)

% Gyerek nagyszülõje Nagyszulo.
nagyszuloje(Gyerek, Nagyszulo) :-             % (nsz1)
    szuloje(Gyerek, Szulo),      
    szuloje(Szulo, Nagyszulo).

% ffi(X): X ferfi.
ffi('Imre').                                  % (f1)
ffi('Istvan').                                % (f2)
ffi('Geza').                                  % (f3)
ffi('Civakodo Henrik').                       % (f4)


% Gyerek nagyszuloje Nagyszulo.
nagyszuloje_hatekonyabb(Gyerek, Nagyszulo) :- 
    nonvar(Gyerek),
    szuloje(Gyerek, Szulo),      
    szuloje(Szulo, Nagyszulo).
nagyszuloje_hatekonyabb(Gyerek, Nagyszulo) :- 
    var(Gyerek),
    szuloje(Szulo, Nagyszulo),
    szuloje(Gyerek, Szulo).


end_of_file.

| ?- nagyszuloje('Imre', NA), ffi(NA).
NA = 'Civakodo Henrik' ? ;
NA = 'Geza' ? ;
no
| ?- nagyszuloje('Imre', Nsz).
Nsz = 'Geza' ? ;
Nsz = 'Sarolt' ? ;
Nsz = 'Civakodo Henrik' ? ;
Nsz = 'Burgundi Gizella' ? ;
no
| ?- nagyszuloje(U, 'Geza').
U = 'Imre' ? ;
no
| ?- 
