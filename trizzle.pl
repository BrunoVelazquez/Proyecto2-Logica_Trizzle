:- use_rendering(table).

% desplazar(+der, +Num, +Cant, +Tablero, -EvolTablero):
% Predicado principal del juego, desplaza la fila o columna 
% elegida en una cantidad de lugares hacia una direccion tambien elegida.
desplazar(der, Num, Cant, Tablero, [Tablero1, Tablero2, Tablero3, Tablero4]) :-
    moverFila(Num, Cant, Tablero, Tablero1),
    append(Tablero1, [], Aux),
    recorrerCol(Aux, Num, Tablero2),
    burbujearTablero(Tablero2, Tablero3),
    rellenar(Tablero3, Tablero4).

desplazar(izq, Num, Cant, Tablero, [Tablero1, Tablero2, Tablero3, Tablero4]) :-
    moverFila(Num, (-Cant), Tablero, Tablero1),
    append(Tablero1, [], Aux),
    recorrerCol(Aux, Num, Tablero2),
    burbujearTablero(Tablero2, Tablero3),
    rellenar(Tablero3, Tablero4).

desplazar(arriba, Num, Cant, Tablero, [Tablero1, Tablero2, Tablero3, Tablero4]) :-
    moverCol(Num, (-Cant), Tablero, Tablero1),
    append(Tablero1, [], Aux),
    recorrerFil(Aux, Num, Tablero2),
    burbujearTablero(Tablero2, Tablero3),
    rellenar(Tablero3, Tablero4).

desplazar(abajo, Num, Cant, Tablero, [Tablero1, Tablero2, Tablero3, Tablero4]) :-
    moverCol(Num, Cant, Tablero, Tablero1),
    append(Tablero1, [], Aux),
    recorrerFil(Aux, Num, Tablero2),
    burbujearTablero(Tablero2, Tablero3),
    rellenar(Tablero3, Tablero4).

% desplazar_columna(+N, +Cant, +Tablero, -D):
% Desplaza la N-esima columna del tablero en una cant de posiciones, pueden ser arriba o abajo.
moverCol(N, Cant, Tablero, D) :-
    transpose(Tablero, Trasp),
    moverFila(N, Cant, Trasp, D1),
    transpose(D1, D).

% desplazar_columna(+N, +Cant, +Tablero, -D):
% Desplaza la N-esima fila del tablero en una cantidad de posiciones, puede ser izquierda o derecha.
moverFila(N, Cant, [T|Ts], [T|D]) :-
    N > 0,
    N1 is N-1,
    moverFila(N1, Cant, Ts, D).
moverFila(0, Cant, [T|Ts], D) :-
    rotarN(Cant, T, D1),
    append([D1], Ts, D).

% rotarN(+N, +LIni, -LFin):
% Desplaza una lista N posiciones a derecha o izquierda, segun el signo de N.
rotarN(0, L, L).
rotarN(N, L1, L2) :-
    N < 0,
    rotar("izq", L1, L),
    N1 is N+1,
    rotarN(N1, L, L2).
rotarN(N, L1, L2) :-
    N > 0,
    rotar("der", L1, L),
    N1 is N-1,
    rotarN(N1, L, L2).

rotar("der", L, [T|H]) :- append(H, [T], L).
rotar("izq", [H|T], L) :- append(T, [H], L).

% rellenar(+Tablero_Entrada,-Tablero_Salida)
% Tablero_Salida es resultado de rellenar las apariciones de "x" (casillas vacías) en Tablero_Entrada
rellenar([],[]).
rellenar([Lista_Head1|Listas1],[Lista_Head2|Listas2]):-
    reemplazar_Xs(Lista_Head1,Lista_Head2),
    rellenar(Listas1,Listas2).

% getMamushkaRandom(-M)
% Retorna una mamushka M de tamaño pequeño con color aleatorio
% v = verde
% a = azul
% r = rojo
getMamushkaRandom(Out):-
    random(1,4,Tag),
    nth0(Tag,[_X,v,a,r],Color),
    atom_concat(Color,1,Out).

% reemplazar_Xs(+ListaIn,-ListaOut)
% ListaOut es resultado de reemplazar las apariciones de "x" (casillas vacías) en ListaIn,
% por mamushkas pequeñas de color aleatorio
reemplazar_Xs([], []). 
reemplazar_Xs([O|T], [R|T2]) :-
    O = x,
    getMamushkaRandom(R),
    reemplazar_Xs(T, T2).
reemplazar_Xs([H|T], [H|T2]) :-
    dif(H,x),
    reemplazar_Xs(T, T2).
