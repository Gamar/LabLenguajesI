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
  %imprimirTablero
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
% Ejecucion del juego, verifica si el movimiento es posible y lo realiza.
% jugada(in X1,in Y1,in X2,in Y2)
%
jugada(X1,Y1,X2,Y2) :- 
  inicializado(X),
  %verificarJugada
  jugadorActual(Jugador),
  tableroActual(Tablero),
  mover(Tablero,X1,Y1,X2,Y2,NM),
  imprimirMov(Jugador),
  coronar(NM,Jugador,X2,Y2,NuevoTablero),
  %imprimirTablero
  retract(jugadorActual(Jugador)),
  retract(tableroActual(Tablero)),
  cambiarJugador(Jugador,NuevoJugador),
  turno(NuevoTablero,NuevoJugador),!.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% VERIFICACIONES %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%
% Verifica la jugada introducida por el usuario.
% verificarJugada(in Tablero,in Jugador,in X1,in Y1,in X2,in Y2,out NuevoTablero)
%
verificarJugada(Tablero,Jugador,X1,Y1,X2,Y2,NuevoTablero) :-
  verificarFicha(Tablero,Jugador,X1,Y1),
  verificarPosicion(Tablero,X1,Y1,X2,Y2),
  (verificarPeon(Tablero,X1,Y1,X2,Y2); comidaPeon(Tablero,X1,Y1,X2,Y2)).
  

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
% verificarFicha(in Tablero,in Jugador,in X1,in Y1,in X2,in Y2)
%
%Fichas Blancas    
verificarPeon(Tablero,X1,Y1,X2,Y2) :- 
  get(Tablero,X1,Y1,Elemento1),
  Elemento1 == 3,
  X1-X2 =:= 1,
  (Y1-Y2 =:= -1;Y1-Y2 =:= 1),!.
 
%Fichas Negras   
verificarPeon(Tablero,X1,Y1,X2,Y2) :- 
  get(Tablero,X1,Y1,Elemento1),
  Elemento1 == 1,
  X1-X2 =:= -1,
  (Y1-Y2 =:= -1;Y1-Y2 =:= 1),!.  


%
% Verifica que el peon se encuentra en la posicion para poder coronarse.
% verificarCoronacion(in Jugador,in X1,in Y1)
%
%Fichas Blancas
verificarCoronacion(Jugador,X1,Y1) :-
  Jugador,
  X1 =:= 1,!.

%Fichas Negras    
verificarCoronacion(Jugador,X1,Y1) :-
  not(Jugador),
  X1 =:= 8,!.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% COMER FICHAS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%
% Realiza la accion de cuando un peon come una ficha.
% comidaPeon(in Tablero,in X1,in Y1,in X2,in Y2)
%
%Fichas Blancas   
comidaPeon(Tablero,X1,Y1,X2,Y2) :- 
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
comidaPeon(Tablero,X1,Y1,X2,Y2) :- 
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
% imprimirJugador(in Jugador)
%Fichas Blancas
imprimirMov(Jugador,Tablero) :-
    Jugador,
    write('Movimiento jugador 1:'),nl,
    imprimirTablero(Tablero).

%Fichas Negras 
imprimirMov(Jugador,Tablero) :-
    not(Jugador),
    write('Movimiento jugador 2:'),nl,
    imprimirTablero(Tablero).

  
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


% END checkers.pl
