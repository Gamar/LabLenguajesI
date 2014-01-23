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
:- [ia].

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
  abolish(fichaAnterior/2),
  abolish(tipoJugador/3),

  assert(inicializado(X)),
  assert(fichaAnterior(-1,-1)),
  write('Desea jugar contra la maquina (S/N)?'),nl,
  repeat,
  get_code(R), (R == 78, Jug2 = humano; R == 83, Jug2 = maquina),
  assert(tipoJugador(R,humano,Jug2)),
  tableroInicial(Tablero),
  inicializarJugador(Blancas),
  nl,write('Comenzo el juego'),nl,
  imprimirTablero(Tablero),nl,
  turno(Tablero,Blancas).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% JUGADA %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%
% Revisa si realizo una comida anteriormente
% revisarAnterior(in X,in Y)
%
revisarAnterior(X,Y) :-
  fichaAnterior(X,Y),
  X > 0,
  Y > 0.

%
% Obtiene el primero de lista de coordenadas
%
primero([L1:L2:L3:L4|Ls],L1,L2,L3,L4).

%
% Ejecucion del juego, realiza el movimiento si es posible
% jugada(in X1,in Y1,in X2,in Y2)
%
% Jugada humano-humano
jugada(X1,Y1,X2,Y2) :- 
  tipoJugador(_,humano,X),
  X == humano,
  jugada_aux(X1,Y1,X2,Y2),!.

% Jugada humano-maquina
jugada(X1,Y1,X2,Y2) :- 
  tipoJugador(_,humano,X),
  X == maquina,
 
  jugadorActual(Jugador),
  
  %Juega humano
  jugada_aux(X1,Y1,X2,Y2),
  
  
  tableroActual(NuevoTablero),
  
  (not(verificarVolverComerPeon(NuevoTablero,X2,Y2)) , 
  not(verificarVolverComerRey(NuevoTablero,Jugador,X2,Y2))),

  %Juega maquina
  jugadorActual(Jugador2),
  tableroActual(NuevoTablero),
  listarMovimientos(NuevoTablero,Jugador2,L),
  primero(L,L1,L2,L3,L4),

  %Imprime la simulacion del terminal
  nl,nl,write('?- jugada('),
  write(L1),write(','),
  write(L2),write(','),
  write(L3),write(','),
  write(L4),write(').'),nl,

  realizarMovimiento(NuevoTablero,Jugador2,L1,L2,L3,L4,NuevoTablero2),
  imprimirTablero(NuevoTablero2),  
    
  retract(tableroActual(NuevoTablero)),
  assert(tableroActual(NuevoTablero2)),
  verificarFinJuego(NuevoTablero2,Jugador2,Bool), 
  cambiarJugador(Jugador2,NuevoJugador2),
  ((Bool, turno(NuevoTablero2,NuevoJugador2)); not(Bool)),!.

% Jugada humano-maquina
jugada(X1,Y1,X2,Y2) :- 
  tipoJugador(_,humano,X),
  X == maquina,

  %Juega humano
  jugada_aux(X1,Y1,X2,Y2),
  
  jugadorActual(Jugador),
  tableroActual(NuevoTablero),
  
  (verificarVolverComerPeon(NuevoTablero,X2,Y2) ; 
   verificarVolverComerRey(NuevoTablero,Jugador,X2,Y2)),!.


jugada_aux(X1,Y1,X2,Y2) :-
  inicializado(X),
  jugadorActual(Jugador),
  tableroActual(Tablero),
  verificarJugada(Tablero,Jugador,X1,Y1,X2,Y2),
  ((not(revisarAnterior(X,Y))); %No comio el turno anterior
  (revisarAnterior(X,Y),  %Comio el turno anterior
   X =:= X1, Y =:= Y1,    %Debe comer con la misma ficha
   (comioPeon(Tablero,X1,Y1,X2,Y2,_) ; 
    comioRey(Tablero,Jugador,X1,Y1,X2,Y2,_)),
   retract(fichaAnterior(X,Y)))
  ),
  (
    ((comioPeon(Tablero,X1,Y1,X2,Y2,TableroActual) ; 
      comioRey(Tablero,Jugador,X1,Y1,X2,Y2,TableroActual)), 
      mover(TableroActual,X1,Y1,X2,Y2,NuevoTablero),
      (
        (coronar(NuevoTablero,Jugador,X2,Y2,NM), 
          imprimirMov(Jugador,NM),
          retract(jugadorActual(Jugador)),
          retract(tableroActual(Tablero)),     
          (
            (verificarVolverComerPeon(NM,X2,Y2),
             imprimirComerJugador(Jugador), 
             assert(fichaAnterior(X2,Y2)), %Debe usar esa ficha el sig. turno
             turno(NM,Jugador))
             ;
            (not(verificarVolverComerPeon(NM,X2,Y2)),
             verificarFinJuego(NM,Jugador,Bool), 
             cambiarJugador(Jugador,NuevoJugador),
             ((Bool, turno(NM,NuevoJugador)) ; not(Bool))
            )
          )
        )
      ;
        (not(coronar(NuevoTablero,Jugador,X2,Y2,NM)),
          imprimirMov(Jugador,NuevoTablero),
          retract(jugadorActual(Jugador)),
          retract(tableroActual(Tablero)),
          (
            ((verificarVolverComerPeon(NuevoTablero,X2,Y2) ; 
              verificarVolverComerRey(NuevoTablero,Jugador,X2,Y2)),
             imprimirComerJugador(Jugador),
             assert(fichaAnterior(X2,Y2)), %Debe usar esa ficah el sig. turno
             turno(NuevoTablero,Jugador))
             ;
            ((not(verificarVolverComerPeon(NuevoTablero,X2,Y2)) , 
              not(verificarVolverComerRey(NuevoTablero,Jugador,X2,Y2))),
              verificarFinJuego(NuevoTablero,Jugador,Bool), 
              cambiarJugador(Jugador,NuevoJugador),
              ((Bool, turno(NuevoTablero,NuevoJugador)) ; not(Bool))
            )
          )
        )
      )
    )
  ;
    ((not(comioPeon(Tablero,X1,Y1,X2,Y2,TableroActual)) ,
      not(comioRey(Tablero,Jugador,X1,Y1,X2,Y2,TableroActual))), 
      mover(Tablero,X1,Y1,X2,Y2,NuevoTablero),
      (
        (coronar(NuevoTablero,Jugador,X2,Y2,NM), 
          imprimirMov(Jugador,NM),
          retract(jugadorActual(Jugador)),
          retract(tableroActual(Tablero)),     
          cambiarJugador(Jugador,NuevoJugador),
          turno(NM,NuevoJugador)
        )
        ;
        (not(coronar(NuevoTablero,Jugador,X2,Y2,NM)),
          imprimirMov(Jugador,NuevoTablero),
          retract(jugadorActual(Jugador)),
          retract(tableroActual(Tablero)),
          cambiarJugador(Jugador,NuevoJugador),
          turno(NuevoTablero,NuevoJugador)
        )
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
    (verificarPeon(Tablero,X1,Y1,X2,Y2);
      verificarComidaPeon(Tablero,X1,Y1,X2,Y2))
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

%
% Verifica la cantidad de fichas de un jugador.
% verificarCantidadFicha(in Tablero, in Jugador)
%
%Fichas Blancas
verificarCantidadFicha(Tablero,Jugador) :-
    Jugador,
    ElemPeon is 3,
    ElemRey is 4,
    flatten(Tablero,Y),
    (pertenece(ElemPeon,Y) ; pertenece(ElemRey,Y)).

%Fichas Negras 
verificarCantidadFicha(Tablero,Jugador) :-
    not(Jugador),
    ElemPeon is 1,
    ElemRey is 2,
    flatten(Tablero,Y),
    (pertenece(ElemPeon,Y) ; pertenece(ElemRey,Y)).

%
% Verifica si se finalizo el juego.
% verificarCantidadFicha(in Tablero, in Jugador, out Bool)
%
%Fichas Blancas
verificarFinJuego(Tablero,Jugador,Bool) :-
  Jugador,
  ((verificarCantidadFicha(Tablero,not(Jugador)), Bool = true)
  ;
  (not(verificarCantidadFicha(Tablero,not(Jugador))),
    Bool = false,
    imprimirGanador(Jugador))
 ).

%Fichas Negras 
verificarFinJuego(Tablero,Jugador,Bool) :-
  not(Jugador),
  ((verificarCantidadFicha(Tablero,true), Bool = true)
  ;
  (not(verificarCantidadFicha(Tablero,true)),
    Bool = false,
    imprimirGanador(Jugador))
 ).

%
% Obtiene el limite de la primera diagonal
% |1| | 
% | |2|
% obtenerLimiteD1(in X,in Y,out Xl,out Yl)
%
obtenerLimiteD1(X,Y,Xl,Yl) :-
  Xn is X + 1,
  Yn is Y + 1,
  ((Xn < 8, Yn < 8, obtenerLimiteD1(Xn,Yn,Xl,Yl));
   ((Xn == 8; Yn == 8), Xl is Xn, Yl is Yn)),!. 

%
% Obtiene el limite de la segunda diagonal
% | |1| 
% |2| |
% obtenerLimiteD2(in X,in Y,out Xl,out Yl)
%
obtenerLimiteD2(X,Y,Xl,Yl) :-
  Xn is X + 1,
  Yn is Y - 1,
  ((Xn < 8, Yn > 1, obtenerLimiteD2(Xn,Yn,Xl,Yl));
   ((Xn == 8; Yn == 1), Xl is Xn, Yl is Yn)),!. 

%
% Obtiene el limite de la tercera diagonal
% | |2| 
% |1| |   
% obtenerLimiteD3(in X,in Y,out Xl,out Yl)
%
obtenerLimiteD3(X,Y,Xl,Yl) :-
  Xn is X - 1,
  Yn is Y + 1,
  ((Xn > 1, Yn < 8, obtenerLimiteD3(Xn,Yn,Xl,Yl));
   ((Xn == 1; Yn == 8), Xl is Xn, Yl is Yn)),!.    

%
% Obtiene el limite de la cuarta diagonal
% |2| | 
% | |1|
% obtenerLimiteD4(in X,in Y,out Xl,out Yl)
%
obtenerLimiteD4(X,Y,Xl,Yl) :-
  Xn is X - 1,
  Yn is Y - 1,
  ((Xn > 1, Yn > 1, obtenerLimiteD4(Xn,Yn,Xl,Yl));
   ((Xn == 1; Yn == 1), Xl is Xn, Yl is Yn)),!. 

%
% Verifica si el rey puede volver a comer
% verificarVolverComerRey(in Tablero,in Jugador,in X1,in Y1)
%
verificarVolverComerRey(Tablero,Jugador,X1,Y1) :-
   get(Tablero,X1,Y1,Elemento1),
   (Elemento1 == 2; Elemento1 == 4),
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


%
% Busca la proxima comida del rey.
% buscarProxComida(in Tablero,in Jugador,in X1,in Y1,in X2,in Y2)
%
%Pasa por una casilla vacia
buscarProxComida(Tablero,Jugador,X1,Y1,X2,Y2) :-
    X1 =\= X2, Y1=\= Y2, 
    get(Tablero,X1,Y1,0),
    obtenerSigPosicion(X1,Y1,X2,Y2,Xn,Yn),
    buscarProxComida(Tablero,Jugador,Xn,Yn,X2,Y2),!.

%Blancas debe pasar por una negra    
buscarProxComida(Tablero,Jugador,X1,Y1,X2,Y2) :-
    X1 =\= X2, Y1=\= Y2, 
    (get(Tablero,X1,Y1,1); get(Tablero,X1,Y1,2)),
    Jugador,
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

%
% Contar fichas saltadas 
% contarComidas(in Tablero,in P,in X1,in Y1,in X2,in Y2,in N)
%
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
%%%%%%%%%%%%%%%%%%%%%%%%% VERIFICACION DE MOVIMIENTOS %%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%
% Verifica que una ficha posea movimientos
% puedeMover(in Tablero,in X,in Y)
%
% Peones
puedeMover(Tablero,X,Y) :- 
  verificarVolverComerPeon(Tablero,X,Y),!.
  
puedeMover(Tablero,X,Y) :-
  get(Tablero,X,Y,Elemento1),    
  Elemento1 == 1, %Negras bajan
  ((obtenerLimiteD1(X,Y,Xl1,Yl1),
   obtenerSigPosicion(X,Y,Xl1,Yl1,X1,Y1),
   get(Tablero,X1,Y1,Ficha1),    
    Ficha1 == 0);
  (obtenerLimiteD2(X,Y,Xl2,Yl2),
   obtenerSigPosicion(X,Y,Xl2,Yl2,X2,Y2),
   get(Tablero,X2,Y2,Ficha2),    
    Ficha2 == 0)),!.
  
puedeMover(Tablero,X,Y) :-
  get(Tablero,X,Y,Elemento1),    
  Elemento1 == 3, %Blancas suben
  ((obtenerLimiteD3(X,Y,Xl3,Yl3),
   obtenerSigPosicion(X,Y,Xl3,Yl3,X3,Y3),
   get(Tablero,X3,Y3,Ficha3),    
    Ficha3 == 0);
  (obtenerLimiteD4(X,Y,Xl4,Yl4),
   obtenerSigPosicion(X,Y,Xl4,Yl4,X4,Y4),
   get(Tablero,X4,Y4,Ficha4),    
    Ficha4 == 0)),!.  

%Reyes    
puedeMover(Tablero,X,Y) :- 
  get(Tablero,X,Y,Elemento1), %Si el rey puede comer
  ((Elemento1 == 4,
  verificarVolverComerRey(Tablero,true,X,Y));
  (Elemento1 == 2,
  verificarVolverComerRey(Tablero,false,X,Y)
  )),!.    
    
puedeMover(Tablero,X,Y) :-
  get(Tablero,X,Y,Elemento1),    
  (Elemento1 == 2; Elemento1 == 4), %Reyes
  ((obtenerLimiteD1(X,Y,Xl1,Yl1),
   obtenerSigPosicion(X,Y,Xl1,Yl1,X1,Y1),
   get(Tablero,X1,Y1,Ficha1),    
    Ficha1 == 0);
  (obtenerLimiteD2(X,Y,Xl2,Yl2),
   obtenerSigPosicion(X,Y,Xl2,Yl2,X2,Y2),
   get(Tablero,X2,Y2,Ficha2),    
    Ficha2 == 0);
  (obtenerLimiteD3(X,Y,Xl3,Yl3),
   obtenerSigPosicion(X,Y,Xl3,Yl3,X3,Y3),
   get(Tablero,X3,Y3,Ficha3),    
    Ficha3 == 0);
  (obtenerLimiteD4(X,Y,Xl4,Yl4),
   obtenerSigPosicion(X,Y,Xl4,Yl4,X4,Y4),
   get(Tablero,X4,Y4,Ficha4),    
    Ficha4 == 0)),!. 
    
%
% Verifica que un jugador pueda realizar jugadas
% puedeJugar(in Jugador,in Tablero)
%
puedeJugar(Jugador,Tablero) :-
  listaPosicionesFicha(Jugador,Tablero,L),
   puedeJugarAux(Tablero,L).

puedeJugarAux(Tablero,[X:Y|Z]) :-
  puedeMover(Tablero,X,Y),!.
  
puedeJugarAux(Tablero,[X:Y|Z]) :-
  not(puedeMover(Tablero,X,Y)),
  puedeJugarAux(Tablero,Z),!.  

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% COMER FICHAS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%
% Realiza la accion de cuando un peon come una ficha.
% comioPeon(in Tablero,in X1,in Y1,in X2,in Y2,out NM)
%
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
 
%
% Realiza la accion de cuando un rey come una ficha.
% comioRey(in Tablero,in Jugador,in X1,in Y1,in X2,in Y2,out NM)
%
comioRey(Tablero,Jugador,X1,Y1,X2,Y2,NM) :-
    verificarRey(Tablero,Jugador,X1,Y1,X2,Y2,N),
    (N == 1; N == 3),
    obtenerSigPosicion(X1,Y1,X2,Y2,Xn,Yn),
    buscarComidaRey(Tablero,Xn,Yn,X2,Y2,Xp,Yp),
    Xp =\= X2, Yp =\= Y2,
    eliminarFicha(Tablero,Xp,Yp,NM),
    !.
    
%
% Obtiene la siguiente posicion diagonal
% obtenerSigPosicion(in X1,in Y1,in X2,in Y2,out X3, out Y3)
%
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

%
% Busca la comida de un rey
% buscarComidaRey(in Tablero,in X1,in Y1,in X2,in Y2,out X3, out Y3)
%
%Retorna la posicion destino
%Porque no encontro ficha comida
buscarComidaRey(Tablero,X,Y,X,Y,X,Y).

%Consiguio la ficha
buscarComidaRey(Tablero,X1,Y1,X2,Y2,X3,Y3) :-
    get(Tablero,X1,Y1,Elemento1),
    Elemento1 =\= 0,
    X3 is X1, Y3 is Y1,!.

%Paso por una casilla vacia, busca la siguiente    
buscarComidaRey(Tablero,X1,Y1,X2,Y2,X3,Y3) :-
    get(Tablero,X1,Y1,Elemento1),
    Elemento1 == 0,
    obtenerSigPosicion(Tablero,X1,Y1,X2,Y2,Xn,Yn),
    buscarComidaRey(Tablero,Xn,Yn,X2,Y2,X3),!.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%% EJECUCION Y MODIFICACION DE MOVIMIENTOS %%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%
% Obtiene la posicion de un elemento en la lista
% get_pos(in E,in [E|_],out P)
%
get_pos(E,[E|_],1).
get_pos(E,[_|C],P):- get_pos(E,C,P1), P is P1 + 1.

%
% Elimina el ultimo elemento de la lista
% deleteend(in L,out L1)
%
deleteend(L,L1):- addend(_,L1,L),!. 
addend(X, [], [X]). 
addend(X, [C|R], [C|R1]):- addend(X, R, R1),!. 

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
% Verifica si un elemento pertenece a la lista.
% pertenece(in X,in [X|_])
%
pertenece(X,[X|_]).
pertenece(X,[_|R]):- pertenece(X,R),!. 

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
%
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

%
% Encuentra la posicion de la columna en la que esta una ficha
% posicion_columnas(in Fila,in NumeroFila,in Elemento,in NuevaLista)
%
posicion_columnas(Fila,NumeroFila,Elemento,[]) :-
  not(get_pos(Elemento,Fila,Posicion)),!.  

posicion_columnas(Fila,NumeroFila,Elemento,[NumeroFila:Posicion|NuevaLista]) :-
  get_pos(Elemento,Fila,Posicion),
  set(Fila,Posicion,0,NuevaFila),
  posicion_columnas(NuevaFila,NumeroFila,Elemento,NuevaLista),!.

%
% Encuentra la posicion de una ficha en el tablero
% posicion_columnas(in Fila,in NumeroFila,in Elemento,out NuevaLista)
%
lista_posiciones([],[Lista|NuevaLista],NumeroFila,Elemento).
lista_posiciones([X|Xs],[Lista|NuevaLista],NumeroFila,Elemento) :-  
  posicion_columnas(X,NumeroFila,Elemento,Lista),
  NumeroFilaAux is NumeroFila + 1,
  lista_posiciones(Xs,NuevaLista,NumeroFilaAux,Elemento).

%
% Encuentra la posicion de las fichas de un jugador
% listaPosicionesFicha(in Jugador,in Tablero,out Lista)
%
%Fichas Blancas
listaPosicionesFicha(Jugador,Tablero,Lista) :-
  Jugador,
  Elem1 is 3, Elem2 is 4,  
  lista_posiciones(Tablero,L1,1,Elem1), flatten(L1,L2), 
  deleteend(L2,L3), deleteend(L3,L4),
  lista_posiciones(Tablero,L5,1,Elem2), flatten(L5,L6), 
  deleteend(L6,L7), deleteend(L7,L8),
  append(L4,L8,Lista),!.

%Fichas Negras
listaPosicionesFicha(Jugador,Tablero,Lista) :-
  not(Jugador),
  Elem1 is 1, Elem2 is 2,  
  lista_posiciones(Tablero,L1,1,Elem1), flatten(L1,L2), 
  deleteend(L2,L3), deleteend(L3,L4),
  lista_posiciones(Tablero,L5,1,Elem2), flatten(L5,L6), 
  deleteend(L6,L7), deleteend(L7,L8),
  append(L4,L8,Lista),!.

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

%
% Imprime en pantalla el jugador ganador. 
%
% imprimirGanador(in Jugador)
%Fichas Blancas
imprimirGanador(Jugador) :-
    Jugador,
    nl,write('FELICIDADES el ganador es el jugador 1.'),nl.

%Fichas Negras 
imprimirGanador(Jugador) :-
    not(Jugador),
    nl,write('FELICIDADES el ganador es el jugador 2.'),nl.

  
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


%% DEBUG

tableroDebug(
    [[5,3,5,1,5,0,5,0],
    [1,5,1,5,0,5,0,5],
    [5,2,5,1,5,0,5,0],
    [0,5,0,5,2,5,0,5],
    [5,0,5,0,5,0,5,0],
    [0,5,0,5,0,5,1,5],
    [0,0,5,0,5,0,5,0],
    [0,5,0,5,1,5,0,5]]).

% END checkers.pl
