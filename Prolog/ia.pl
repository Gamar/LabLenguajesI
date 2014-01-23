%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%% INTELIGENCIA ARTIFICIAL %%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- use_module(library(clpfd)).

%% Heuristica del Tablero %%

evaluarTablero(Tablero,N) :-
  listaPosicionesFicha(false,Tablero,L1),
  evaluarJugador(Tablero,L1,N1),
  listaPosicionesFicha(true,Tablero,L2),
  evaluarJugador(Tablero,L2,N2),
  N is N1 - N2.

evaluarFicha(Tablero,X,Y,N) :-
  get(Tablero,X,Y,Elemento1), 
  (Elemento1 == 2; Elemento1 ==4),
   N is 10,!.

evaluarFicha(Tablero,X,Y,N) :-
  get(Tablero,X,Y,Elemento1), 
  ((Elemento1 == 3, X == 2, N is 7,!);
  (Elemento1 == 1, X == 7, N is 7,!);
   N is 5),!.

evaluarJugador(Tablero,[X:Y],N) :-
  evaluarFicha(Tablero,X,Y,N),!.

evaluarJugador(Tablero,[X:Y|Xs],N) :-
  evaluarFicha(Tablero,X,Y,N1),
  evaluarJugador(Tablero,Xs,N2),
  N is N1 + N2,!.
   

%% Listado de Movimientos %%
listarMovimientos(Tablero,Jugador,L) :-
  listarFichasMovibles(Tablero,Jugador,L1),
  listarMovimientosAux(Tablero,Jugador,L1,L2),
  flatten(L2,L).

listarMovimientosAux(Tablero,Jugador,[X1:Y1|Z1],[L|Z2]) :-
  listarMovimientosFicha(Tablero,X1,Y1,L1),
  movimiento(X1:Y1,L1,L),
  listarMovimientosAux(Tablero,Jugador,Z1,Z2),!.

listarMovimientosAux(Tablero,Jugador,[],[]).

%% Genera movimientos de una ficha %% 
movimiento(X1:Y1,[X2:Y2|Z1],[X1:Y1:X2:Y2|Z2]) :-
  movimiento(X1:Y1,Z1,Z2),!.

movimiento(_,[],[]).

%% Listado de Ficha %%

listarFichasMovibles(Tablero,Jugador,L) :-
  listaPosicionesFicha(Jugador,Tablero,L1),
  listarFichasMoviblesAux(Tablero,L1,L),!.
  
listarFichasMoviblesAux(Tablero,[],[]).
  
listarFichasMoviblesAux(Tablero,[X:Y|L],[X:Y|Zs]) :-
  puedeMover(Tablero,X,Y),
  listarFichasMoviblesAux(Tablero,L,Zs),!.
  
listarFichasMoviblesAux(Tablero,[X:Y|L],Zs) :-
  not(puedeMover(Tablero,X,Y)),
  listarFichasMoviblesAux(Tablero,L,Zs),!.
  
  
%% Lista de Movimientos por Ficha %%

listarMovimientosFicha(Tablero,X,Y,L) :-
  get(Tablero,X,Y,Elemento1),
  (Elemento1 == 1; Elemento1 == 3),
  setof(X2:Y2,movimientoPeon(Tablero,X,Y,X2,Y2),L),!.     

listarMovimientosFicha(Tablero,X,Y,L) :-
  get(Tablero,X,Y,4),
  setof(X2:Y2,movimientoRey(Tablero,true,X,Y,X2,Y2,N),L),!.     

listarMovimientosFicha(Tablero,X,Y,L) :-
  get(Tablero,X,Y,2),
  setof(X2:Y2,movimientoRey(Tablero,false,X,Y,X2,Y2,N),L),!.    

%% Movimiento de Peones %%

% Simple  
movimientoPeon(Tablero,X1,Y1,X2,Y2) :- 
  not(verificarVolverComerPeon(Tablero,X1,Y1)),
  get(Tablero,X1,Y1,3),
  X1-X2 #= 1, %Fichas blancas bajan
  (Y1-Y2 #= -1;Y1-Y2 #= 1),
  get(Tablero,X2,Y2,0).
  
movimientoPeon(Tablero,X1,Y1,X2,Y2) :- 

  not(verificarVolverComerPeon(Tablero,X1,Y1)),
  get(Tablero,X1,Y1,1),
  X1-X2 #= -1, %Fichas negras suben
  (Y1-Y2 #= -1;Y1-Y2 #= 1),
  get(Tablero,X2,Y2,0).

% Comidas  
movimientoPeon(Tablero,X1,Y1,X2,Y2) :- 
  get(Tablero,X1,Y1,3),
  (
    %Posiblidad izquierda
    (X3 #= X1-1, Y3 #= Y1-1,
    get(Tablero,X3,Y3,Elemento2),
    (Elemento2 == 1; Elemento2 == 2),
    X1-X2 #= 2, Y1-Y2 #= 2,
    get(Tablero,X2,Y2,0))
    ;
    %Posiblidad derecha
    (X3 #= X1-1, Y3 #= Y1+1,
    get(Tablero,X3,Y3,Elemento3),
    (Elemento3 == 1; Elemento3 == 2),
    X1-X2 #= 2, Y2-Y1 #= 2,
    get(Tablero,X2,Y2,0))
  ),!.  
   
movimientoPeon(Tablero,X1,Y1,X2,Y2) :- 
  get(Tablero,X1,Y1,1),
  (
    %Posiblidad izquierda
    (X3 #= X1+1, Y3 #= Y1-1,
    get(Tablero,X3,Y3,Elemento2),
    (Elemento2 == 3; Elemento2 == 4),
    X2-X1 #= 2, Y1-Y2 #= 2,
    get(Tablero,X2,Y2,0))
    ;
    %Posiblidad derecha
    (X3 #= X1+1, Y3 #= Y1+1,
    get(Tablero,X3,Y3,Elemento3),
    (Elemento3 == 3; Elemento3 == 4),
    X2-X1 #= 2, Y2-Y1 #= 2,
    get(Tablero,X2,Y2,0))
  ),!.

%% Movimiento de Reyes %%

movimientoRey(Tablero,P,X1,Y1,X2,Y2,N) :- 
  get(Tablero,X1,Y1,4),
  P,
  abs(X2-X1) #= abs(Y2-Y1), %Movimiento diagonal 
  X2 #\= X1, %Realizo un movimiento
  get(Tablero,X2,Y2,0),
  revisarSalto(Tablero,P,X1,Y1,X2,Y2), %No salto fichas de su mismo color
  contarComidas(Tablero,P,X1,Y1,X2,Y2,N), %Cuenta fichas comidas
  ((N == 0, not(verificarVolverComerRey(Tablero,P,X1,Y1)));
  (N == 1, verificarVolverComerRey(Tablero,P,X1,Y1))). 
  

%Fichas Negras
movimientoRey(Tablero,P,X1,Y1,X2,Y2,N) :- 
  get(Tablero,X1,Y1,2),
  not(P),
  abs(X2-X1) #= abs(Y2-Y1), %Movimiento diagonal 
  X2 #\= X1, %Realizo un movimiento
  get(Tablero,X2,Y2,0),
  revisarSalto(Tablero,P,X1,Y1,X2,Y2), %No salto fichas de su mismo color
  contarComidas(Tablero,P,X1,Y1,X2,Y2,N), %Cuenta fichas saltadas
  ((N == 0, not(verificarVolverComerRey(Tablero,P,X1,Y1)));
  (N == 1, verificarVolverComerRey(Tablero,P,X1,Y1))). 


%
% Revisa los saltos de un rey a partir de la siguiente posicion, no puede saltar 
% fichas de su mismo color.
% revisarSalto(in Tablero,in P,in X1,in Y1,in X2,in Y2)
%
revisarSalto(Tablero,P,X1,Y1,X2,Y2) :-
  X1 #< X2, Y1 #< Y2,
  Xn is X1+1, Yn is Y1+1, 
  revisarSaltoAux(Tablero,P,Xn,Yn,X2,Y2).
  
revisarSalto(Tablero,P,X1,Y1,X2,Y2) :-
  X1 #< X2, Y1 #> Y2,
  Xn is X1+1, Yn is Y1-1,
  revisarSaltoAux(Tablero,P,Xn,Yn,X2,Y2).  

revisarSalto(Tablero,P,X1,Y1,X2,Y2) :-
  X1 #> X2, Y1 #< Y2,
  Xn is X1-1, Yn is Y1+1,
  revisarSaltoAux(Tablero,P,Xn,Yn,X2,Y2).

revisarSalto(Tablero,P,X1,Y1,X2,Y2) :-
  X1 #> X2, Y1 #> Y2,
  Xn is X1-1, Yn is Y1-1,
  revisarSaltoAux(Tablero,P,Xn,Yn,X2,Y2).

%
% Revisa las posicions saltadas.
% Alcanzo la posicion objetivo. 
% revisarSaltoAux(in Tablero,in P,in X1,in Y1,in X2,in Y2)
%
revisarSaltoAux(_,_,X,Y,X,Y).
  
revisarSaltoAux(Tablero,P,X1,Y1,X2,Y2) :-  
  X1 < X2, Y1 < Y2, 
  Xn is X1+1, Yn is Y1+1, 
  get(Tablero,X1,Y1,Ficha),
  (Ficha == 0; 
  (P, Ficha =\= 3, Ficha =\= 4);      
  (not(P), Ficha =\= 1, Ficha =\= 2)),
  revisarSaltoAux(Tablero,P,Xn,Yn,X2,Y2).
  
revisarSaltoAux(Tablero,P,X1,Y1,X2,Y2) :-  
  X1 < X2, Y1 > Y2, 
  Xn is X1+1, Yn is Y1-1, 
  get(Tablero,X1,Y1,Ficha),
  (Ficha == 0; 
  (P, Ficha =\= 3, Ficha =\= 4);      
  (not(P), Ficha =\= 1, Ficha =\= 2)),
  revisarSaltoAux(Tablero,P,Xn,Yn,X2,Y2).
  
revisarSaltoAux(Tablero,P,X1,Y1,X2,Y2) :-  
  X1 > X2, Y1 < Y2, 
  Xn is X1-1, Yn is Y1+1, 
  get(Tablero,X1,Y1,Ficha),
  (Ficha == 0; 
  (P, Ficha =\= 3, Ficha =\= 4);      
  (not(P), Ficha =\= 1, Ficha =\= 2)),
  revisarSaltoAux(Tablero,P,Xn,Yn,X2,Y2).

revisarSaltoAux(Tablero,P,X1,Y1,X2,Y2) :-  
  X1 > X2, Y1 > Y2, 
  Xn is X1-1, Yn is Y1-1, 
  get(Tablero,X1,Y1,Ficha),
  (Ficha == 0; 
  (P, Ficha =\= 3, Ficha =\= 4);      
  (not(P), Ficha =\= 1, Ficha =\= 2)),
  revisarSaltoAux(Tablero,P,Xn,Yn,X2,Y2).

%% Realizacion de Movimientos %%
realizarMovimiento(Tablero,Jugador,X1,Y1,X2,Y2,NuevoTablero) :-
  (not(comioPeon(Tablero,X1,Y1,X2,Y2,_)),
   not(comioRey(Tablero,Jugador,X1,Y1,X2,Y2,_))), 
   ((mover(Tablero,X1,Y1,X2,Y2,NT2),
   not(coronar(NT2,Jugador,X2,Y2,_)),
   mover(Tablero,X1,Y1,X2,Y2,NuevoTablero),!);
   (mover(Tablero,X1,Y1,X2,Y2,NT2),
   coronar(NT2,Jugador,X2,Y2,NuevoTablero),!)),
   !.

realizarMovimiento(Tablero,Jugador,X1,Y1,X2,Y2,NuevoTablero) :-
  ((comioPeon(Tablero,X1,Y1,X2,Y2,NT),!); 
   (comioRey(Tablero,Jugador,X1,Y1,X2,Y2,NT),!)), 
   
   ((mover(NT,X1,Y1,X2,Y2,NT3),
   not(coronar(NT3,Jugador,X2,Y2,_)),!);
   (mover(NT,X1,Y1,X2,Y2,NT2),
   coronar(NT2,Jugador,X2,Y2,NuevoTablero),!)),
   
   (((not(verificarVolverComerPeon(NT3,X2,Y2)) , 
    not(verificarVolverComerRey(NT3,Jugador,X2,Y2)),
    NuevoTablero = NT3,!
    ));
   (((verificarVolverComerPeon(NT3,X2,Y2)) ;
   (verificarVolverComerRey(NT3,Jugador,X2,Y2))),
   listarMovimientosFicha(NT3,X2,Y2,[X3:Y3|Z]),
   realizarMovimiento(NT3,Jugador,X2,Y2,X3,Y3,NuevoTablero),!)),
   !.

testProc(X) :- imprimirTablero(X).
