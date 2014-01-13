%Proyecto 2 - Checkers

%imports
:-use_module(library(lists)).

tableroInicial(	
	[[5,1,5,1,5,1,5,1],
	[1,5,1,5,1,5,1,5],
	[5,1,5,1,5,1,5,1],
	[0,5,0,5,0,5,0,5],
	[5,0,5,0,5,0,5,0],
	[3,5,3,5,3,5,3,5],
	[5,3,5,3,5,3,5,3],
	[3,5,3,5,3,5,3,5]]).

tableroTest(	
	[[5,1,5,1,5,1,5,1],
	[1,5,1,5,1,5,1,5],
	[5,1,5,1,5,1,5,1],
	[0,5,0,5,3,5,0,5],
	[5,0,5,0,5,0,5,0],
	[3,5,3,5,3,5,3,5],
	[5,3,5,3,5,3,5,3],
	[3,5,3,5,3,5,3,5]]). 
	
	

tableroTest2(	
	[[5,3,5,4],
	[0,5,0,5],
	[5,0,5,0],
	[1,5,2,5]]). 	
 
inicializarJugador(Jugador) :-
    Jugador = true.

imprimirJugador(Player) :-
    Player,
    write('Juega jugador 1'),nl.

imprimirJugador(Player) :-
    not(Player),
    write('Juega jugador 2'),nl.
    
imprimirMov(Player,Tablero) :-
    Player,
    write('Movimiento jugador 1:'),nl.
    %ImprimirTablero

imprimirMov(Player,Tablero) :-
    not(Player),
    write('Movimiento jugador 2:'),nl.
    %ImprimirTablero

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
  write('Comenzo el juego'),nl,
  turno(Tablero,Blancas).
  
tipoJugador(78,humano,humano).
tipoJugador(83,humano,maquina).

turno(Tablero,Jugador):-
  %imprimirTablero
  imprimirJugador(Jugador),
  assert(jugadorActual(Jugador)),
  assert(tableroActual(Tablero)),!.

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


%Verificacion de la jugada introducida por el jugador
verificarJugada(Tablero,Jugador,X1,Y1,X2,Y2,NuevoTablero) :-
  verificarFicha(Jugador,Tablero,X1,Y1),
  verificarPosicion(Tablero,X1,Y1,X2,Y2),
  (verificarPeon(Tablero,X1,Y1,X2,Y2); comidaPeon(Tablero,X1,Y1,X2,Y2)).
  

verificarPosicion(Tablero,X1,Y1,X2,Y2) :- 
  get(Tablero,X1,Y1,Elemento1), Elemento1 \= 5,
  get(Tablero,X2,Y2,Elemento2), Elemento2 == 0,
  %Movimientos diagonales
  abs(X2-X1) =:= abs(Y2-Y1).
  
  %Blancas
verificarFicha(Jugador,Tablero,X1,Y1) :-
  Jugador,
  get(Tablero,X1,Y1,Elemento1),
  (Elemento1 == 3; Elemento1 == 4),!.

%Negras    
verificarFicha(Jugador,Tablero,X1,Y1) :-
  not(Jugador),
  get(Tablero,X1,Y1,Elemento1),
  (Elemento1 == 1; Elemento1 == 2),!. 

%Blancos    
verificarPeon(Tablero,X1,Y1,X2,Y2) :- 
  get(Tablero,X1,Y1,Elemento1),
  Elemento1 == 3,
  X1-X2 =:= 1,
  (Y1-Y2 =:= -1;Y1-Y2 =:= 1),!.
 
%Negros  
verificarPeon(Tablero,X1,Y1,X2,Y2) :- 
  get(Tablero,X1,Y1,Elemento1),
  Elemento1 == 1,
  X1-X2 =:= -1,
  (Y1-Y2 =:= -1;Y1-Y2 =:= 1),!.  
    
%Blancos Comidas    
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

%Negros Comidas    
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

coronar(Tablero,Jugador,X1,Y1,NM) :-
  get(Tablero,X1,Y1,Elemento1),
  Elemento1 \= 2,
  Elemento1 \= 4,
  verificarCoronacion(Jugador,X1,Y1),
  reemplazarCorona(Tablero,X1,Y1,NM),!.
  


%Blancas
verificarCoronacion(Jugador,X1,Y1) :-
  Jugador,
  X1 =:= 1,!.

%Negras    
verificarCoronacion(Jugador,X1,Y1) :-
  not(Jugador),
  X1 =:= 8,!.

cambiarJugador(Jugador,NuevoJugador) :-
  Jugador, 
  NuevoJugador = false.

cambiarJugador(Jugador,NuevoJugador) :-
  not(Jugador), 
  NuevoJugador = true.  
  
 
escribir(0) :- write('  ').
escribir(1) :- write('< ').
escribir(2) :- write('<<').
escribir(3) :- write('> ').
escribir(4) :- write('>>').
escribir(5) :- write('  ').

get(M,F,C,Elemento) :-    
  nth1(F,M,Filas),
  nth1(C,Filas,Elemento).

set([_C|L],1,X,[X|L]):- !.
set([C|L],N,X,[C|R]):-
  N1 is N-1, set(L,N1,X,R).

reemplazar(M,F,C,Elemento, NM) :-
  nth1(F,M,Filas), 
  set(Filas,C,Elemento,NuevaFila),
  set(M,F,NuevaFila,NM).         
 
mover(Tablero,X1,Y1,X2,Y2,NM) :-
  get(Tablero,X1,Y1,Elemento1),
  get(Tablero,X2,Y2,Elemento2),
  reemplazar(Tablero,X1,Y1,Elemento2,Z),
  reemplazar(Z,X2,Y2,Elemento1,NM).

eliminarFicha(Tablero,X1,Y1,NM) :-
  reemplazar(Tablero,X1,Y1,0,NM).
  
reemplazarCorona(Tablero,X1,Y1,NM) :-
  get(Tablero,X1,Y1,Elemento1),
  Elemento2 is Elemento1 + 1, 
  reemplazar(Tablero,X1,Y1,Elemento2,NM).
     

      
 
   
