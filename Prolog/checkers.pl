%
% Proyecto II - Checkers
%
% Integrantes:
%   Gamar Azuaje #10-10051.
%   Rosangelis Garcia #10-11247.
%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% IMPORTS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:-use_module(library(lists)).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% INICIALIZACION %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%
% Tablero inicial del juego.
%
tableroInicial(	
	[[5,1,5,1,5,1,5,1],
	[1,5,1,5,1,5,1,5],
	[5,1,5,1,5,1,5,1],
	[0,5,0,5,0,5,0,5],
	[5,0,5,0,5,0,5,0],
	[3,5,3,5,3,5,3,5],
	[5,3,5,3,5,3,5,3],
	[3,5,3,5,3,5,3,5]]).

%
% Inicializar el jugador 1.
% inicializarJugador(in-out Jugador)
%
inicializarJugador(Jugador) :-
    Jugador = true.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TURNO %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%
% Realiza el cambio de turno de cada jugador.
% turno(in Tablero,in Jugador)
%
turno(Tablero,Jugador):-
  imprimirJugador(Jugador),
  assert(jugadorActual(Jugador)),
  assert(tableroActual(Tablero)),!.

%
% Cambia el jugador.
% cambiarJugador(in Jugador, in NuevoJugador)
%
%Fichas Blancas
cambiarJugador(Jugador,NuevoJugador) :-
  Jugador,
  NuevoJugador = false.

%Fichas Negras
cambiarJugador(Jugador,NuevoJugador) :-
  not(Jugador), 
  NuevoJugador = true.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% JUGAR %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%
% Inicializacion del juego.
%
jugar :-
  %Borrar informacion anterior
  abolish(jugadorActual/1),
  abolish(tableroActual/1),
  abolish(inicializado/1),
  
  assert(inicializado(X)),
  write('Desea jugar contra la maquina (S/N)?'),nl,
  repeat,
  get_code(R), (R == 78; R == 83),
  tipoJugador(R,Jug1,Jug2),
  tableroInicial(Tablero),
  inicializarJugador(Blancas),
  nl,write('Comenzo el juego'),nl,
  imprimirTablero(Tablero),nl,
  turno(Tablero,Blancas).
  
tipoJugador(78,humano,humano).
tipoJugador(83,humano,maquina).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% JUGADA %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%
% Ejecucion del juego, realiza el movimiento si es posible
% jugada(in X1,in Y1,in X2,in Y2)
%

jugada(X1,Y1,X2,Y2) :- 
  inicializado(X),
  jugadorActual(Jugador),
  tableroActual(Tablero),
  verificarJugada(Tablero,Jugador,X1,Y1,X2,Y2),
  %Verifica si el peon ya comio
  ((comioPeon(Tablero,X1,Y1,X2,Y2,TableroActual), 
    mover(TableroActual,X1,Y1,X2,Y2,NuevoTablero))
  ;
  (not(comioPeon(Tablero,X1,Y1,X2,Y2,TableroActual)), 
    mover(Tablero,X1,Y1,X2,Y2,NuevoTablero))),
  

  %Verifica si el peon se corona
  (
    (coronar(NuevoTablero,Jugador,X2,Y2,NM), 
      imprimirMov(Jugador,NM),
      retract(jugadorActual(Jugador)),
      retract(tableroActual(Tablero)),
      (
        (verificarVolverComerPeon(NM,X2,Y2),
        imprimirComerJugador(Jugador),
        turno(NM,Jugador))
      ;
        (not(verificarVolverComerPeon(NM,X2,Y2)), 
        cambiarJugador(Jugador,NuevoJugador),
        turno(NM,NuevoJugador))
      )
    )
    ;
    (not(coronar(NuevoTablero,Jugador,X2,Y2,NM)),
      imprimirMov(Jugador,NuevoTablero),
      retract(jugadorActual(Jugador)),
      retract(tableroActual(Tablero)),
      (
        (verificarVolverComerPeon(NuevoTablero,X2,Y2),
        imprimirComerJugador(Jugador),
        turno(NuevoTablero,Jugador))
      ;
        (not(verificarVolverComerPeon(NuevoTablero,X2,Y2)),
        cambiarJugador(Jugador,NuevoJugador),
        turno(NuevoTablero,NuevoJugador))
      )
    )
  ),!.



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% VERIFICACIONES %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%
% Verifica la jugada introducida por el usuario.
% verificarJugada(in Tablero,in Jugador,in X1,in Y1,in X2,in Y2)
%
verificarJugada(Tablero,Jugador,X1,Y1,X2,Y2) :-
  verificarFicha(Tablero,Jugador,X1,Y1),
  verificarPosicion(Tablero,X1,Y1,X2,Y2),
  (%If es un peon
    (verificarPeon(Tablero,X1,Y1,X2,Y2); verificarComidaPeon(Tablero,X1,Y1,X2,Y2))
  ; %es un rey
    (verificarRey(Tablero,Jugador,X1,Y1,X2,Y2,N)
    
    %Si N es 1 
    
    )
  ).

%
% Verifica que la posicion introducida es posible.
% verificarPosicion(in Tablero,in Jugador,in X1,in Y1,in X2,in Y2)
%
verificarPosicion(Tablero,X1,Y1,X2,Y2) :- 
  get(Tablero,X1,Y1,Elemento1), Elemento1 \= 5,
  get(Tablero,X2,Y2,Elemento2), Elemento2 == 0,
  %Movimientos diagonales
  abs(X2-X1) =:= abs(Y2-Y1).

%
% Verifica que el jugador pueda mover dicha ficha.
% verificarFicha(in Tablero,in Jugador,in X1,in Y1)
%
%Fichas Blancas
verificarFicha(Tablero,Jugador,X1,Y1) :-
  Jugador,
  get(Tablero,X1,Y1,Elemento1),
  (Elemento1 == 3; Elemento1 == 4),!.

%Fichas Negras    
verificarFicha(Tablero,Jugador,X1,Y1) :-
  not(Jugador),
  get(Tablero,X1,Y1,Elemento1),
  (Elemento1 == 1; Elemento1 == 2),!. 

%
% Verifica que el movimiento es valido para un peon.
% verificarPeon(in Tablero,in X1,in Y1,in X2,in Y2)
%
%Fichas Blancas    
verificarPeon(Tablero,X1,Y1,X2,Y2) :- 
  get(Tablero,X1,Y1,Elemento1),
  Elemento1 == 3,
  X1-X2 =:= 1, %Fichas blancas bajan
  (Y1-Y2 =:= -1;Y1-Y2 =:= 1),!.
 
%Fichas Negras   
verificarPeon(Tablero,X1,Y1,X2,Y2) :- 
  get(Tablero,X1,Y1,Elemento1),
  Elemento1 == 1,
  X1-X2 =:= -1, %Fichas negras suben
  (Y1-Y2 =:= -1;Y1-Y2 =:= 1),!.  

%
% Verifica si el peon puede comer una ficha.
% verificarComidaPeon(in Tablero,in X1,in Y1,in X2,in Y2)
%
%Fichas Blancas   
verificarComidaPeon(Tablero,X1,Y1,X2,Y2) :- 
  get(Tablero,X1,Y1,Elemento1),
  Elemento1 == 3,
  (
    %Posiblidad izquierda
    (X3 is X1-1, Y3 is Y1-1,
    get(Tablero,X3,Y3,Elemento2),
    (Elemento2 == 1; Elemento2 == 2),
    X1-X2 =:= 2, Y1-Y2 =:= 2)
    ;
    %Posiblidad derecha
    (X3 is X1-1, Y3 is Y1+1,
    get(Tablero,X3,Y3,Elemento3),
    (Elemento3 == 1; Elemento3 == 2),
    X1-X2 =:= 2, Y2-Y1 =:= 2)
  ),!.  

%Fichas Negras    
verificarComidaPeon(Tablero,X1,Y1,X2,Y2) :- 
  get(Tablero,X1,Y1,Elemento1),
  Elemento1 == 1,
  (
    %Posiblidad izquierda
    (X3 is X1+1, Y3 is Y1-1,
    get(Tablero,X3,Y3,Elemento2),
    (Elemento2 == 3; Elemento2 == 4),
    X2-X1 =:= 2, Y1-Y2 =:= 2)
    ;
    %Posiblidad derecha
    (X3 is X1+1, Y3 is Y1+1,
    get(Tablero,X3,Y3,Elemento3),
    (Elemento3 == 3; Elemento3 == 4),
    X2-X1 =:= 2, Y2-Y1 =:= 2)
  ),!.


%
% Verifica si el peon puede comer otra ficha.
% verificarVolverComerPeon(in Tablero,in X1,in Y1)
%
%Fichas Blancas 
verificarVolverComerPeon(Tablero,X1,Y1) :- 
  get(Tablero,X1,Y1,Elemento1),
  Elemento1 == 3,
  (
    %Posiblidad izquierda
    (X3 is X1-1, Y3 is Y1-1,
    get(Tablero,X3,Y3,Elemento2),
    (Elemento2 == 1; Elemento2 == 2),
    X2 is X1-2, Y2 is Y1-2,
    get(Tablero,X2,Y2,Vacio),
    Vacio == 0,
    X1-X2 =:= 2, Y1-Y2 =:= 2)
    ;
    %Posiblidad derecha
    (X3 is X1-1, Y3 is Y1+1,
    get(Tablero,X3,Y3,Elemento3),
    (Elemento3 == 1; Elemento3 == 2),
    X2 is X1-2, Y2 is Y1+2,
    get(Tablero,X2,Y2,Vacio),
    Vacio == 0,
    X1-X2 =:= 2, Y2-Y1 =:= 2)
  ),!.  

%Fichas Negras    
verificarVolverComerPeon(Tablero,X1,Y1) :- 
  get(Tablero,X1,Y1,Elemento1),
  Elemento1 == 1,
  (
    %Posiblidad izquierda
    (X3 is X1+1, Y3 is Y1-1,
    get(Tablero,X3,Y3,Elemento2),
    (Elemento2 == 3; Elemento2 == 4),
    X2 is X1+2, Y2 is Y1-2,
    get(Tablero,X2,Y2,Vacio),
    Vacio == 0,
    X2-X1 =:= 2, Y1-Y2 =:= 2)
    ;
    %Posiblidad derecha
    (X3 is X1+1, Y3 is Y1+1,
    get(Tablero,X3,Y3,Elemento3),
    (Elemento3 == 3; Elemento3 == 4),
    X2 is X1+2, Y2 is Y1+2,
    get(Tablero,X2,Y2,Vacio),
    Vacio == 0,
    X2-X1 =:= 2, Y2-Y1 =:= 2)
  ),!.



%Obtiene el limite de la primera diagonal
% |1| | 
% | |2|
obtenerLimiteD1(X,Y,Xl,Yl) :-
  Xn is X + 1,
  Yn is Y + 1,
  ((Xn < 8, Yn < 8, obtenerLimiteD1(Xn,Yn,Xl,Yl));
   ((Xn == 8; Yn == 8), Xl is Xn, Yl is Yn)),!. 

%Obtiene el limite de la segunda diagonal
% | |1| 
% |2| |
obtenerLimiteD2(X,Y,Xl,Yl) :-
  Xn is X + 1,
  Yn is Y - 1,
  ((Xn < 8, Yn > 1, obtenerLimiteD2(Xn,Yn,Xl,Yl));
   ((Xn == 8; Yn == 1), Xl is Xn, Yl is Yn)),!. 

%Obtiene el limite de la tercera diagonal
% | |2| 
% |1| |   
obtenerLimiteD3(X,Y,Xl,Yl) :-
  Xn is X - 1,
  Yn is Y + 1,
  ((Xn > 1, Yn < 8, obtenerLimiteD3(Xn,Yn,Xl,Yl));
   ((Xn == 1; Yn == 8), Xl is Xn, Yl is Yn)),!.    

%Obtiene el limite de la cuarta diagonal
% |2| | 
% | |1|
obtenerLimiteD4(X,Y,Xl,Yl) :-
  Xn is X - 1,
  Yn is Y - 1,
  ((Xn > 1, Yn > 1, obtenerLimiteD4(Xn,Yn,Xl,Yl));
   ((Xn == 1; Yn == 1), Xl is Xn, Yl is Yn)),!. 

verificarVolverComerRey(Tablero,Jugador,X1,Y1) :-
   ((obtenerLimiteD1(X1,Y1,Xl,Yl),
   Xl =\= X1, Yl =\= Y1, 
   obtenerSigPosicion(X1,Y1,Xl,Yl,Xn,Yn),
   buscarProxComida(Tablero,Jugador,Xn,Yn,Xl,Yl),!);
   (obtenerLimiteD2(X1,Y1,Xl,Yl),
   Xl =\= X1, Yl =\= Y1, 
   obtenerSigPosicion(X1,Y1,Xl,Yl,Xn,Yn),
   buscarProxComida(Tablero,Jugador,Xn,Yn,Xl,Yl),!);
   (obtenerLimiteD3(X1,Y1,Xl,Yl),
   Xl =\= X1, Yl =\= Y1, 
   obtenerSigPosicion(X1,Y1,Xl,Yl,Xn,Yn),
   buscarProxComida(Tablero,Jugador,Xn,Yn,Xl,Yl),!);
   (obtenerLimiteD4(X1,Y1,Xl,Yl),
   Xl =\= X1, Yl =\= Y1, 
   obtenerSigPosicion(X1,Y1,Xl,Yl,Xn,Yn),
   buscarProxComida(Tablero,Jugador,Xn,Yn,Xl,Yl),!)).

%Pasa por una casilla vacia
buscarProxComida(Tablero,Jugador,X1,Y1,X2,Y2) :-
    X1 =\= X2, Y1=\= Y2, 
    get(Tablero,X1,Y1,Elemento1),
    Elemento1 == 0,
    obtenerSigPosicion(X1,Y1,X2,Y2,Xn,Yn),
    buscarProxComida(Tablero,Jugador,Xn,Yn,X2,Y2),!.

%Blancas debe pasar por una negra    
buscarProxComida(Tablero,Jugador,X1,Y1,X2,Y2) :-
    X1 =\= X2, Y1=\= Y2, 
    get(Tablero,X1,Y1,Elemento1),
    Jugador,(Elemento1 == 1; Elemento1 == 2),
    obtenerSigPosicion(X1,Y1,X2,Y2,Xn,Yn),
    get(Tablero,Xn,Yn,Elemento2),
    Elemento2 == 0, %Sig casilla debe ser vacia
    !.
    
%Negras debe pasar por una blanca
buscarProxComida(Tablero,Jugador,X1,Y1,X2,Y2) :-
    X1 =\= X2, Y1 =\= Y2, 
    get(Tablero,X1,Y1,Elemento1),
    not(Jugador), (Elemento1 == 3; Elemento1 == 4),
    obtenerSigPosicion(X1,Y1,X2,Y2,Xn,Yn),
    get(Tablero,Xn,Yn,Elemento2),
    Elemento2 == 0, %Sig casilla debe ser vacia
    !.


%
% Verifica que el movimiento es valido para un rey.
% verificarRey(in Tablero,in P,in X1,in Y1,in X2,in Y2)
%
%Fichas Blancas
verificarRey(Tablero,P,X1,Y1,X2,Y2,N) :- 
  get(Tablero,X1,Y1,Ficha),
  P,
  Ficha == 4, 
  abs(X2-X1) =:= abs(Y2-Y1), %Movimiento diagonal 
  X2 =\= X1, %Realizo un movimiento
  revisarSalto(Tablero,P,X1,Y1,X2,Y2), %No salto fichas de su mismo color
  contarComidas(Tablero,P,X1,Y1,X2,Y2,N), %Cuenta fichas comidas
  (N == 0; N == 1), %No salto mas de dos fichas
  !.

%Fichas Negras
verificarRey(Tablero,P,X1,Y1,X2,Y2,N) :- 
  get(Tablero,X1,Y1,Ficha),
  not(P),
  Ficha == 2,
  abs(X2-X1) =:= abs(Y2-Y1), %Movimiento diagonal 
  X2 =\= X1, %Realizo un movimiento
  revisarSalto(Tablero,P,X1,Y1,X2,Y2), %No salto fichas de su mismo color
  contarComidas(Tablero,P,X1,Y1,X2,Y2,N), %Cuenta fichas comidas
  (N == 0; N == 1), %No salto mas de dos fichas
  !.

%
% Revisa los saltos de un rey a partir de la siguiente posicion, no puede saltar 
% fichas de su mismo color.
% revisarSalto(in Tablero,in P,in X1,in Y1,in X2,in Y2)
%
revisarSalto(Tablero,P,X1,Y1,X2,Y2) :-
  X1 < X2, Y1 < Y2,
  Xn is X1+1, Yn is Y1+1, 
  revisarSaltoAux(Tablero,P,Xn,Yn,X2,Y2),!.
  
revisarSalto(Tablero,P,X1,Y1,X2,Y2) :-
  X1 < X2, Y1 > Y2,
  Xn is X1+1, Yn is Y1-1,
  revisarSaltoAux(Tablero,P,Xn,Yn,X2,Y2),!.  

revisarSalto(Tablero,P,X1,Y1,X2,Y2) :-
  X1 > X2, Y1 < Y2,
  Xn is X1-1, Yn is Y1+1,
  revisarSaltoAux(Tablero,P,Xn,Yn,X2,Y2),!.

revisarSalto(Tablero,P,X1,Y1,X2,Y2) :-
  X1 > X2, Y1 > Y2,
  Xn is X1-1, Yn is Y1-1,
  revisarSaltoAux(Tablero,P,Xn,Yn,X2,Y2),!.

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
  revisarSaltoAux(Tablero,P,Xn,Yn,X2,Y2),!.
  
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



%Contar fichas saltadas

%Alcanzo la posicion objetivo

contarComidas(Tablero,P,X1,Y1,X2,Y2,N) :-
  X1 < X2, Y1 < Y2,
  Xn is X1+1, Yn is Y1+1, 
  contarComidasAux(Tablero,P,Xn,Yn,X2,Y2,N),!.

contarComidas(Tablero,P,X1,Y1,X2,Y2,N) :-
  X1 < X2, Y1 > Y2,
  Xn is X1+1, Yn is Y1-1, 
  contarComidasAux(Tablero,P,Xn,Yn,X2,Y2,N),!.

contarComidas(Tablero,P,X1,Y1,X2,Y2,N) :-
  X1 > X2, Y1 < Y2,
  Xn is X1-1, Yn is Y1+1, 
  contarComidasAux(Tablero,P,Xn,Yn,X2,Y2,N),!.

contarComidas(Tablero,P,X1,Y1,X2,Y2,N) :-
  X1 > X2, Y1 > Y2,
  Xn is X1-1, Yn is Y1-1, 
  contarComidasAux(Tablero,P,Xn,Yn,X2,Y2,N),!.

contarComidasAux(_,_,X,Y,X,Y,0).

contarComidasAux(Tablero,P,X1,Y1,X2,Y2,N) :-
  X1 < X2, Y1 < Y2, 
  Xn is X1+1, Yn is Y1+1, 
  get(Tablero,X1,Y1,Ficha),
  ((Ficha == 0, contarComidasAux(Tablero,P,Xn,Yn,X2,Y2,N)); 
   (P, (Ficha == 1; Ficha == 2),  %Blancas
    contarComidasAux(Tablero,P,Xn,Yn,X2,Y2,N2), N is N2+1);
   (not(P), (Ficha == 3; Ficha == 4), %Negras
    contarComidasAux(Tablero,P,Xn,Yn,X2,Y2,N2), N is N2+1)),!.  

contarComidasAux(Tablero,P,X1,Y1,X2,Y2,N) :-
  X1 < X2, Y1 > Y2, 
  Xn is X1+1, Yn is Y1-1, 
  get(Tablero,X1,Y1,Ficha),
  ((Ficha == 0, contarComidasAux(Tablero,P,Xn,Yn,X2,Y2,N)); 
   (P, (Ficha == 1; Ficha == 2),  %Blancas
    contarComidasAux(Tablero,P,Xn,Yn,X2,Y2,N2), N is N2+1);
   (not(P), (Ficha == 3; Ficha == 4), %Negras
    contarComidasAux(Tablero,P,Xn,Yn,X2,Y2,N2), N is N2+1)),!.
    
contarComidasAux(Tablero,P,X1,Y1,X2,Y2,N) :-
  X1 > X2, Y1 < Y2, 
  Xn is X1-1, Yn is Y1+1, 
  get(Tablero,X1,Y1,Ficha),
  ((Ficha == 0, contarComidasAux(Tablero,P,Xn,Yn,X2,Y2,N)); 
   (P, (Ficha == 1; Ficha == 2),  %Blancas
    contarComidasAux(Tablero,P,Xn,Yn,X2,Y2,N2), N is N2+1);
   (not(P), (Ficha == 3; Ficha == 4), %Negras
    contarComidasAux(Tablero,P,Xn,Yn,X2,Y2,N2), N is N2+1)),!.
    
contarComidasAux(Tablero,P,X1,Y1,X2,Y2,N) :-
  X1 > X2, Y1 > Y2, 
  Xn is X1-1, Yn is Y1-1, 
  get(Tablero,X1,Y1,Ficha),
  ((Ficha == 0, contarComidasAux(Tablero,P,Xn,Yn,X2,Y2,N)); 
   (P, (Ficha == 1; Ficha == 2),  %Blancas
    contarComidasAux(Tablero,P,Xn,Yn,X2,Y2,N2), N is N2+1);
   (not(P), (Ficha == 3; Ficha == 4), %Negras
    contarComidasAux(Tablero,P,Xn,Yn,X2,Y2,N2), N is N2+1)),!.



%
% Verifica que el peon se encuentra en la posicion para poder coronarse.
% verificarCoronacion(in Jugador,in X1,in Y1)
%
%Fichas Blancas
verificarCoronacion(Jugador,X1,_) :-
  Jugador,
  X1 =:= 1,!.

%Fichas Negras    
verificarCoronacion(Jugador,X1,_) :-
  not(Jugador),
  X1 =:= 8,!.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% COMER FICHAS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%
% Realiza la accion de cuando un peon come una ficha.
% comioPeon(in Tablero,in X1,in Y1,in X2,in Y2,out NM)
%
%Fichas Blancas
%Fichas Blancas   
comioPeon(Tablero,X1,Y1,X2,Y2,NM) :- 
  get(Tablero,X1,Y1,Elemento1),
  Elemento1 == 3,
  (
    %Posiblidad izquierda
    (X3 is X1-1, Y3 is Y1-1,
    get(Tablero,X3,Y3,Elemento2),
    (Elemento2 == 1; Elemento2 == 2),
    X1-X2 =:= 2, Y1-Y2 =:= 2,
    eliminarFicha(Tablero,X3,Y3,NM))
    ;
    %Posiblidad derecha
    (X3 is X1-1, Y3 is Y1+1,
    get(Tablero,X3,Y3,Elemento3),
    (Elemento3 == 1; Elemento3 == 2),
    X1-X2 =:= 2, Y2-Y1 =:= 2,
    eliminarFicha(Tablero,X3,Y3,NM))
  ),!.  

%Fichas Negras    
comioPeon(Tablero,X1,Y1,X2,Y2,NM) :- 
  get(Tablero,X1,Y1,Elemento1),
  Elemento1 == 1,
  (
    %Posiblidad izquierda
    (X3 is X1+1, Y3 is Y1-1,
    get(Tablero,X3,Y3,Elemento2),
    (Elemento2 == 3; Elemento2 == 4),
    X2-X1 =:= 2, Y1-Y2 =:= 2,
    eliminarFicha(Tablero,X3,Y3,NM))
    ;
    %Posiblidad derecha
    (X3 is X1+1, Y3 is Y1+1,
    get(Tablero,X3,Y3,Elemento3),
    (Elemento3 == 3; Elemento3 == 4),
    X2-X1 =:= 2, Y2-Y1 =:= 2,
    eliminarFicha(Tablero,X3,Y3,NM))
  ),!.  


comioPeon(Tablero,X1,Y1,X2,Y2,NM) :- 
  get(Tablero,X1,Y1,Elemento1),
  Elemento1 == 3,
  (
    %Posiblidad izquierda
    (X3 is X1-1, Y3 is Y1-1,
    get(Tablero,X3,Y3,Elemento2),
    (Elemento2 == 1; Elemento2 == 2),
    X1-X2 =:= 2, Y1-Y2 =:= 2,
    eliminarFicha(Tablero,X3,Y3,NM))
    ;
    %Posiblidad derecha
    (X3 is X1-1, Y3 is Y1+1,
    get(Tablero,X3,Y3,Elemento3),
    (Elemento3 == 1; Elemento3 == 2),
    X1-X2 =:= 2, Y2-Y1 =:= 2,
    eliminarFicha(Tablero,X3,Y3,NM))
  ),!.  

%Fichas Negras    

%Not done yet
comioRey(Tablero,Jugador,X1,Y1,X2,Y2,NM) :-
    verificarRey(Tablero,Jugador,X1,Y1,X2,Y2,N),
    (N == 1; N == 3),
    obtenerSigPosicion(X1,Y1,X2,Y2,Xn,Yn),
    buscarComidaRey(Tablero,Xn,Yn,X2,Y2,Xp,Yp),
    Xp =\= X2, Yp =\= Y2,
    eliminarFicha(Tablero,Xp,Yp,NM),
    !.
    
    
%Obtiene la siguiente posicion diagonal
obtenerSigPosicion(X1,Y1,X2,Y2,X3,Y3) :-
    X1 < X2, Y1 < Y2,
    X3 is X1+1, Y3 is Y1+1,!.
    
obtenerSigPosicion(X1,Y1,X2,Y2,X3,Y3) :-
    X1 < X2, Y1 > Y2,
    X3 is X1+1, Y3 is Y1-1,!.
    
obtenerSigPosicion(X1,Y1,X2,Y2,X3,Y3) :-
    X1 > X2, Y1 < Y2,
    X3 is X1-1, Y3 is Y1+1,!.    
    
obtenerSigPosicion(X1,Y1,X2,Y2,X3,Y3) :-
    X1 > X2, Y1 > Y2,
    X3 is X1-1, Y3 is Y1-1,!.    

%Busca la comida de un rey

%Retorna la posicion destino
%Porque no encontro ficha comida
buscarComidaRey(Tablero,X,Y,X,Y,X,Y).

%Consiguio la ficha
buscarComidaRey(Tablero,X1,Y1,X2,Y2,X3,Y3) :-
    get(Tablero,X1,Y1,Elemento1),
    Elemento1 =\= 0,
    X3 is X1, Y3 is Y1,!.

%Paso por una casilla vacia,
%Busca la siguiente    
buscarComidaRey(Tablero,X1,Y1,X2,Y2,X3,Y3) :-
    get(Tablero,X1,Y1,Elemento1),
    Elemento1 == 0,
    obtenerSigPosicion(Tablero,X1,Y1,X2,Y2,Xn,Yn),
    buscarComidaRey(Tablero,Xn,Yn,X2,Y2,X3),!.
    
    
  


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%% EJECUCION Y MODIFICACION DE MOVIMIENTOS %%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%
% Obtiene un elemento dentro de una lista en una posicion dada.
% get(in M,in F,in C,out Elemento)
%
get(M,F,C,Elemento) :-    
  nth1(F,M,Filas),
  nth1(C,Filas,Elemento).

%
% Inserta un elemento dentro de una lista en una posicion dada.
% set(in [_C|L],in 1,in X,out [X|L])
%
set([_C|L],1,X,[X|L]):- !.
set([C|L],N,X,[C|R]):-
  N1 is N-1, set(L,N1,X,R).

%
% Reemplaza un elemento dentro de una matriz en una posicion dada.
% reemplazar(in M,in F,in C,in Elemento, out NM)
%
reemplazar(M,F,C,Elemento, NM) :-
  nth1(F,M,Filas), 
  set(Filas,C,Elemento,NuevaFila),
  set(M,F,NuevaFila,NM).         

%
% Mueve una ficha a una posicion dada.
% mover(in Tablero,in X1,in Y1,in X2,in Y2,out NM)
%
mover(Tablero,X1,Y1,X2,Y2,NM) :-
  get(Tablero,X1,Y1,Elemento1),
  get(Tablero,X2,Y2,Elemento2),
  reemplazar(Tablero,X1,Y1,Elemento2,Z),
  reemplazar(Z,X2,Y2,Elemento1,NM).

%
% Elimina la ficha del tablero.
% eliminarFicha(in Tablero,in X1,in Y1,out NM)

eliminarFicha(Tablero,X1,Y1,NM) :-
  reemplazar(Tablero,X1,Y1,0,NM).

%
% Corona a un peon.
% coronar(in Tablero,in Jugador,in X1,in Y1,out NM)
%
coronar(Tablero,Jugador,X1,Y1,NM) :-
  get(Tablero,X1,Y1,Elemento1),
  Elemento1 \= 2,
  Elemento1 \= 4,
  verificarCoronacion(Jugador,X1,Y1),
  reemplazarCorona(Tablero,X1,Y1,NM),!.


%
% Realiza el cambio para que un peon se vuelva rey.
% get(in Tablero,in X1,in Y1,out NM)
%
reemplazarCorona(Tablero,X1,Y1,NM) :-
  get(Tablero,X1,Y1,Elemento1),
  Elemento2 is Elemento1 + 1, 
  reemplazar(Tablero,X1,Y1,Elemento2,NM).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% IMPRESIONES %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%
% Imprime en pantalla de quien es el turno. 
% imprimirJugador(in Jugador)
%
%Fichas Blancas
imprimirJugador(Jugador) :-
    Jugador,
    write('Juega jugador 1'),nl.

%Fichas Negras 
imprimirJugador(Jugador) :-
    not(Jugador),
    write('Juega jugador 2'),nl.


%
% Imprime en pantalla el movimiento realizado por el jugador. 
%
% imprimirMov(in Jugador, in Tablero)
%Fichas Blancas
imprimirMov(Jugador,Tablero) :-
    Jugador,
    nl,write('Movimiento jugador 1:'),nl,
    imprimirTablero(Tablero),nl.

%Fichas Negras 
imprimirMov(Jugador,Tablero) :-
    not(Jugador),
    nl,write('Movimiento jugador 2:'),nl,
    imprimirTablero(Tablero),nl.


%
% Imprime en pantalla que el jugador puede realizar otro movimiento. 
%
% imprimirComerJugador(in Jugador)
%Fichas Blancas
imprimirComerJugador(Jugador) :-
    Jugador,
    nl,write('Puede volver a jugar el jugador 1.'),nl.

%Fichas Negras 
imprimirComerJugador(Jugador) :-
    not(Jugador),
    nl,write('Puede volver a jugar el jugador 2.'),nl.

  
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%% IMPRIMIR TABLERO EN PANTALLA %%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%
% Caracteres del juego.
%
escribir(0) :- write('  ').

%Fichas Negras
%Peon
escribir(1) :- write('< ').
%Rey
escribir(2) :- write('<<').

%Fichas Blancas
%Peon
escribir(3) :- write('> ').
%Rey
escribir(4) :- write('>>').

escribir(5) :- write('  ').


%
% Imprime el tablero del juego en pantalla.
%
imprimirTablero(Tablero) :-
  nl, write('    1    2    3    4    5    6    7    8'), nl,
  imprimirLinea(1,Tablero),
  write('    1    2    3    4    5    6    7    8'), nl,!.


%
% Imprime las filas de la matriz.
%
imprimirLinea(_,[]).
imprimirLinea(N,[Linea|Cola]) :-
  write(N), mostrarCaracteres(Linea), write(' '), write(N), nl,
  N2 is N+1,
  imprimirLinea(N2, Cola).


%
% Imprime los elementos de una lista.
%
mostrarCaracteres([]).
mostrarCaracteres([Elemento|Cola]) :-
  write(' |'), escribir(Elemento), write('|'),
  mostrarCaracteres(Cola).


%
%  DEBUGGING BOARD
%

tableroDebug(
	[
	[5,0,5,0,5,0,5,0],
	[1,5,3,5,1,5,1,5],
	[5,0,5,1,5,3,5,0],
	[0,5,0,5,2,5,0,5],
	[5,0,5,4,5,0,5,0],
	[0,5,0,5,0,5,1,5],
	[0,0,5,0,5,0,5,0],
	[0,5,0,5,0,5,0,5]]).



% END checkers.pl
