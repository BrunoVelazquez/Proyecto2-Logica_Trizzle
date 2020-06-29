:- use_rendering(table).

% desplazar(+der, +Num, +Cant, +Tablero, -EvolTablero):
% Predicado principal del juego, desplaza la fila o columna 
% elegida en una cantidad de lugares hacia una direccion tambien elegida.
desplazar(der, Num, Cant, Tablero, [Tablero1, Tablero2, Tablero3, Tablero4]) :-
    moverFila(Num, Cant, Tablero, Tablero1),
    append(Tablero1, [], Aux),
    recorrerCol(Aux, Num, Tablero2),
    Tablero1 \= Tablero2,
    burbujearTablero(Tablero2, Tablero3),
    rellenar(Tablero3, Tablero4).
desplazar(der, Num, Cant, Tablero, EvoTablero) :-
    moverFila(Num, Cant, Tablero, EvoTablero).

desplazar(izq, Num, Cant, Tablero, [Tablero1, Tablero2, Tablero3, Tablero4]) :-
    moverFila(Num, (-Cant), Tablero, Tablero1),
    append(Tablero1, [], Aux),
    recorrerCol(Aux, Num, Tablero2),
    Tablero1 \= Tablero2,
    burbujearTablero(Tablero2, Tablero3),
    rellenar(Tablero3, Tablero4).
desplazar(izq, Num, Cant, Tablero, EvoTablero) :-
    moverFila(Num, (-Cant), Tablero, EvoTablero).

desplazar(arriba, Num, Cant, Tablero, [Tablero1, Tablero2, Tablero3, Tablero4]) :-
    moverCol(Num, (-Cant), Tablero, Tablero1),
    append(Tablero1, [], Aux),
    recorrerFil(Aux, Num, Tablero2),
    Tablero1 \= Tablero2,
    burbujearTablero(Tablero2, Tablero3),
    rellenar(Tablero3, Tablero4).
desplazar(arriba, Num, Cant, Tablero, EvoTablero) :-
    moverCol(Num, (-Cant), Tablero, EvoTablero).

desplazar(abajo, Num, Cant, Tablero, [Tablero1, Tablero2, Tablero3, Tablero4]) :-
    moverCol(Num, Cant, Tablero, Tablero1),
    append(Tablero1, [], Aux),
    recorrerFil(Aux, Num, Tablero2),
    Tablero1 \= Tablero2,
    burbujearTablero(Tablero2, Tablero3),
    rellenar(Tablero3, Tablero4).
desplazar(abajo, Num, Cant, Tablero, EvoTablero) :-
    moverCol(Num, Cant, Tablero, EvoTablero).

% moverCol(+N, +Cant, +Tablero, -D):
% Desplaza la N-esima columna del tablero en una cant de posiciones, pueden ser arriba o abajo.
moverCol(N, Cant, Tablero, D) :-
    transpose(Tablero, Trasp),
    moverFila(N, Cant, Trasp, D1),
    transpose(D1, D).

% moverFila(+N, +Cant, +Tablero, -D):
% Desplaza la N-esima fila del tablero en una cantidad de posiciones, puede ser izquierda o derecha.
moverFila(N, Cant, [T|Ts], [T|D]) :-
    N > 0,
    N < 5,
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

% burbujearTablero(+Tablero_entrada,-Tablero_salida)
% Tablero_salida es resultado de subir las apariciones de x (casillas vacías) en Tablero_entrada,
% cayendo así las mamushkas hacia el fondo del tablero.
burbujearTablero(TableroIn, TableroOut):-
    transpose(TableroIn,TableroTraspuesto),
    eliminar_apariciones_x(TableroTraspuesto,Tablero_sin_x),
	agregar_apariciones_x(Tablero_sin_x,Tablero_con_x),
    transpose(Tablero_con_x,TableroOut)
    ,!.
/*
 * burbujearTablero([[1,2,3,4,5],
                  [6,7,8,9,10],
                  [11,12,13,14,15],
                  [16,17,18,19,20],
                  [21,22,23,24,25]],X).*/

% agregar_apariciones_X(+Tablero_entrada,-Tablero_salida)
% Tablero_salida es resultado de agregar x's (casillas vacias) a Tablero_entrada,
% tal que el tablero vuelva a tener una dimensión 5x5
agregar_apariciones_x([],[]).
agregar_apariciones_x([T_in|Ts_in],[T_out|Ts_out]):-
    agregar_x(T_in,T_out),
    agregar_apariciones_x(Ts_in,Ts_out).

% agregar_x(+Lista_entrada,-Lista_salida)
% Lista_salida es resultado de agregar al principio de Lista_entrada,
% la cantidad de x's necesaria para que Lista_entrada tenga un tamaño de 5
agregar_x(ListaIn,ListaOut):-
    length(ListaIn,Length_in),
    N is (5-Length_in),
    crear_lista_x(N,Lista_X),
    append(Lista_X,ListaIn,ListaOut).

% crear_lista_X(+N,-Lista_salida)
% Lista_salida es una lista compuesta de N x's
crear_lista_x(0,[]):-!.
crear_lista_x(Tamaño,[x|Xs]):-
	Tamaño1 is (Tamaño-1),
    crear_lista_x(Tamaño1,Xs).

% eliminar_apariciones_x(+Old,-New)
% New es una lista de sublistas, tal que el elemento x es eliminado de todas las sublistas de la lista Old 
eliminar_apariciones_x([],[]).
eliminar_apariciones_x([T_in|Ts_in],[T_out|Ts_out]):-
    delete(T_in,x,T_out),
    eliminar_apariciones_x(Ts_in,Ts_out).

/*-----------------------*/
% transpose(+Matriz_entrada,-Matriz_salida)
% Matriz_salida es la matriz traspuesta de Matriz_entrada
%
% Código extraído de la librería CLPFD
transpose([], []).
transpose([F|Fs], Ts):-
    transpose(F, [F|Fs], Ts).

transpose([], _, []).
transpose([_|Rs], Ms, [Ts|Tss]) :-
    lists_firsts_rests(Ms, Ts, Ms1),
    transpose(Rs, Ms1, Tss).

lists_firsts_rests([], [], []).
lists_firsts_rests([[F|Os]|Rest], [F|Fs], [Os|Oss]) :-
    lists_firsts_rests(Rest, Fs, Oss).
/*-----------------------*/

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
    nth1(Tag,[v,a,r],Color),
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

% recorrerCol(+Tablero, +Principal, -Res):
% Recorre todas las columnas del tablero, luego colapsa todas las mamushkas que pueda sobre el elemento Principal.
recorrerCol(Tablero, Principal, Res):-
    transpose(Tablero, TableroTransp),      
    recorrerFil(TableroTransp, Principal, Aux),
    transpose(Aux, Res).

% recorrerFil(+Tablero, +Principa, -Res):
% Recorre todas las filas del tablero, luego colapsa todas las mamushkas que pueda sobre el elemento Principal.
recorrerFil(Tablero, Principal, Res):-
    recorrerFilAux(Tablero, Principal, 0, R1),
    buscarCruzadas(R1, Principal, Res).

recorrerFilAux(Tablero, Principal, 4, Res):-
    getFila(Tablero, 4, Fila),
    buscarConsecutivos(Fila, Principal, R),
    intercambiar(Tablero, 4, R, Res).

recorrerFilAux(Tablero, Principal, N, Res):-
    N1 is N+1,
    getFila(Tablero, N, Fila),
    buscarConsecutivos(Fila, Principal, R),
    intercambiar(Tablero, N, R, Aux),
    recorrerFilAux(Aux, Principal, N1, Res).

% getFila(+Tablero, +Pos, +F):
% recibe como un tablero, una posicion y almacena la fila en dicha posición en la variable F.
getFila([A | _Tail], 0, A).
getFila([_A | Tail], N, F):-
    N1 is N-1,
    getFila(Tail, N1, F).

% intercambiar(+List1, +Pos, +Element, -List2):
% Intercambia el elemento en la posicion N de las lista List1 por el elemento E y almacena el resultado en List2.
intercambiar([_A | Tail], 0, E, [E | Tail]).

intercambiar([A | Tail], N, E, T):-
    N1 is N-1,
    intercambiar(Tail, N1, E, R),
    append([A], R, T).

% getCol(+Tablero, +N, +F):
% Recibe como parametro una lista de listas (un tablero), una posición y almacena la columna en dicha posicion en la variable F.
getCol(Tablero, N, F):-
    transpose(Tablero, TableroTransp),
    getFila(TableroTransp, N, F).

% buscarCruzadas(+Tablero, +Principal, -Res):
% Verifica si se produjeron colapsos cruzados sobre la columna principal en el Tablero. Utiliza el predicado auxiliar cruzadasAux/8.
buscarCruzadas(Tablero, Principal, Res):-
    getCol(Tablero, Principal, C0),
    posList(C0, 0, P0),
    posList(C0, 1, P1),
    posList(C0, 2, P2),
    posList(C0, 3, P3),
    posList(C0, 4, P4),
	cruzadasAux(Tablero, Principal, P0, P1, P2, P3, P4, Res).

cruzadasAux(Tablero, Principal, P0, P1, P2, P3, P4, T4):-
    P0 == P1,
    P1 == P2,
    P2 == P3,
    P3 == P4,
    getFila(Tablero, 0, L0),
    getFila(Tablero, 1, L1),
    getFila(Tablero, 2, L2),
    getFila(Tablero, 3, L3),
    getFila(Tablero, 4, L4),
    cruzar(L0, Principal, Aux1),
    cruzar(L1, Principal, Aux2),
    cruzar(L2, Principal, Aux3),
    cruzar(L3, Principal, Aux4),
    cruzar(L4, Principal, Aux5),
    intercambiar(Tablero, 0, Aux1, T0),
    intercambiar(T0, 1, Aux2, T1),
    intercambiar(T1, 2, Aux3, T2),
    intercambiar(T2, 3, Aux4, T3),
    intercambiar(T3, 4, Aux5, T4).

cruzadasAux(Tablero, Principal, P0, P1, P2, P3, _P4, T3):-
    P0 == P1,
    P1 == P2,
    P2 == P3,
    getFila(Tablero, 0, L0),
    getFila(Tablero, 1, L1),
    getFila(Tablero, 2, L2),
    getFila(Tablero, 3, L3),
    cruzar(L0, Principal, Aux1),
    cruzar(L1, Principal, Aux2),
    cruzar(L2, Principal, Aux3),
    cruzar(L3, Principal, Aux4),
    intercambiar(Tablero, 0, Aux1, T0),
    intercambiar(T0, 1, Aux2, T1),
    intercambiar(T1, 2, Aux3, T2),
    intercambiar(T2, 3, Aux4, T3).

cruzadasAux(Tablero, Principal, _P0, P1, P2, P3, P4, T3):-
    P1 == P2,
    P2 == P3,
    P3 == P4,
    getFila(Tablero, 1, L0),
    getFila(Tablero, 2, L1),
    getFila(Tablero, 3, L2),
    getFila(Tablero, 4, L3),
    cruzar(L0, Principal, Aux1),
    cruzar(L1, Principal, Aux2),
    cruzar(L2, Principal, Aux3),
    cruzar(L3, Principal, Aux4),
    intercambiar(Tablero, 1, Aux1, T0),
    intercambiar(T0, 2, Aux2, T1),
    intercambiar(T1, 3, Aux3, T2),
    intercambiar(T2, 4, Aux4, T3).

cruzadasAux(Tablero, Principal, P0, P1, P2, _P3, _P4, T2):-
    P0 == P1,
    P1 == P2,
    getFila(Tablero, 0, L0),
    getFila(Tablero, 1, L1),
    getFila(Tablero, 2, L2),
    cruzar(L0, Principal, Aux1),
    cruzar(L1, Principal, Aux2),
    cruzar(L2, Principal, Aux3),
    intercambiar(Tablero, 0, Aux1, T0),
    intercambiar(T0, 1, Aux2, T1),
    intercambiar(T1, 2, Aux3, T2).

cruzadasAux(Tablero, Principal, _P0, P1, P2, P3, _P4, T2):-
    P1 == P2,
    P2 == P3,
    getFila(Tablero, 1, L0),
    getFila(Tablero, 2, L1),
    getFila(Tablero, 3, L2),
    cruzar(L0, Principal, Aux1),
    cruzar(L1, Principal, Aux2),
    cruzar(L2, Principal, Aux3),
    intercambiar(Tablero, 1, Aux1, T0),
    intercambiar(T0, 2, Aux2, T1),
    intercambiar(T1, 3, Aux3, T2).

cruzadasAux(Tablero, Principal, _P0, _P1, P2, P3, P4, T2):-
    P2 == P3,
    P3 == P4,
    getFila(Tablero, 2, L0),
    getFila(Tablero, 3, L1),
    getFila(Tablero, 4, L2),
    cruzar(L0, Principal, Aux1),
    cruzar(L1, Principal, Aux2),
    cruzar(L2, Principal, Aux3),
    intercambiar(Tablero, 2, Aux1, T0),
    intercambiar(T0, 3, Aux2, T1),
    intercambiar(T1, 4, Aux3, T2).

cruzadasAux(Tablero, Principal, _P0, _P1, _P2, _P3, _P4, T4):-
    getFila(Tablero, 0, L0),
    getFila(Tablero, 1, L1),
    getFila(Tablero, 2, L2),
    getFila(Tablero, 3, L3),
    getFila(Tablero, 4, L4),
    cruzar2(L0, Principal, Aux1),
    cruzar2(L1, Principal, Aux2),
    cruzar2(L2, Principal, Aux3),
    cruzar2(L3, Principal, Aux4),
    cruzar2(L4, Principal, Aux5),
    intercambiar(Tablero, 0, Aux1, T0),
    intercambiar(T0, 1, Aux2, T1),
    intercambiar(T1, 2, Aux3, T2),
    intercambiar(T2, 3, Aux4, T3),
    intercambiar(T3, 4, Aux5, T4).

% cruzar(+List1, +PosPrincipal, +List2):
% Verifica que haya en la lista List1 el elemento x. Si lo encontro, entonces hubo un colapso la lista. Se intercambia la mamushka por la misma aumentada. Se almacena la lista nueva. En el caso de que no haya colapsos en la columna principal, se llama al predicado cruzar2/3.
cruzar(Lista, Principal, L2):-
    member(x, Lista),
    posList(Lista, Principal, M),
    aumentar(M, MAum),
    intercambiar(Lista, Principal, MAum, L2).

cruzar(Lista, Principal, L2):-
    intercambiar(Lista, Principal, x, L2).

cruzar2(Lista, Principal, L2):-
    member(x, Lista),
    posList(Lista, Principal, M),
    aumentar(M, MAum),
    intercambiar(Lista, Principal, MAum, L2).

cruzar2(Lista, _Principal, Lista).

% aumentar(+M, -N):
% Recibe una mamushka y la aumenta de tamaño.
aumentar(r1, r2).
aumentar(r2, r3).
aumentar(r3, r3).

aumentar(v1, v2).
aumentar(v2, v3).
aumentar(v3, v3).

aumentar(a1, a2).
aumentar(a2, a3).
aumentar(a3, a3).

% buscarConsecutivos(+List1, +Principal, -List2):
% verifica que hayan, al menos, 3 elementos consecutivos iguales en List1. Los colapsa sobre el elemento Principal. Utiliza el predicado auxiliar consecutivosAux/7.
buscarConsecutivos(L, Principal, F):-
    posList(L, 0, P0),
    posList(L, 1, P1),
    posList(L, 2, P2),
    posList(L, 3, P3),
    posList(L, 4, P4),
    consecutivosAux(L, Principal, P0, P1, P2, P3, P4, F).

buscarConsecutivos(L, _Principal, L).

consecutivosAux(L, Principal, P0, P1, P2, P3, P4, L5):-
    P0 == P1,
    P1 == P2,
    P2 == P3,
    P3 == P4,
    posList(L, Principal, M),
    intercambiar(L, 0, x, L0),
    intercambiar(L0, 1, x, L1),
    intercambiar(L1, 2, x, L2),
    intercambiar(L2, 3, x, L3),
    intercambiar(L3, 4, x, L4),
    intercambiar(L4, Principal, M, L5).

consecutivosAux(L, Principal, P0, P1, P2, P3, _P4, L4):-
    P0 == P1,
    P1 == P2,
    P2 == P3,
    posList(L, Principal, M),
    intercambiar(L, 0, x, L0),
    intercambiar(L0, 1, x, L1),
    intercambiar(L1, 2, x, L2),
    intercambiar(L2, 3, x, L3),
    intercambiar(L3, Principal, M, L4).

consecutivosAux(L, Principal, _P0, P1, P2, P3, P4, L4):-
    P1 == P2,
    P2 == P3,
    P3 == P4,
    posList(L, Principal, M),
    intercambiar(L, 1, x, L0),
    intercambiar(L0, 2, x, L1),
    intercambiar(L1, 3, x, L2),
    intercambiar(L2, 4, x, L3),
    intercambiar(L3, Principal, M, L4).


consecutivosAux(L, Principal, P0, P1, P2, _P3, _P4, L3):-
    P0 == P1,
    P1 == P2,
    posList(L, Principal, M),
    intercambiar(L, 0, x, L0),
    intercambiar(L0, 1, x, L1),
    intercambiar(L1, 2, x, L2),
    intercambiar(L2, Principal, M, L3).

consecutivosAux(L, Principal, _P0, P1, P2, P3, _P4, L3):-
    P1 == P2,
    P2 == P3,
    posList(L, Principal, M),
    intercambiar(L, 1, x, L0),
    intercambiar(L0, 2, x, L1),
    intercambiar(L1, 3, x, L2),
    intercambiar(L2, Principal, M, L3).

consecutivosAux(L, Principal, _P0, _P1, P2, P3, P4, L3):-
    P2 == P3,
    P3 == P4,
    posList(L, Principal, M),
    intercambiar(L, 2, x, L0),
    intercambiar(L0, 3, x, L1),
    intercambiar(L1, 4, x, L2),
    intercambiar(L2, Principal, M, L3).

% posList(+List, +Pos, -Ele):
% Es un predicado auxiliar que toma el elemento en la lista en la posicion P y lo almacena en la variable E.
posList([A | _Tail], 0, A).

posList([_A | Tail], Pos, E):-
    P1 is Pos-1,
    posList(Tail, P1, E).