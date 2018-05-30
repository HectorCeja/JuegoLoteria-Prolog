:-dynamic chorroF/1,centroF/1,esquinaF/1,chorroString/1,centroString/1,esquinaString/1.

%EQUIPO: CEJA GOMEZ HECTOR / ESEBERRE TORRES JUAN DANIEL
%Mi base de conocimiento
loteria([1,2,3,4,5,6,7,8,9,10,
        11,12,13,14,15,16,17,18,19,20,
        21,22,23,24,25,26,27,28,29,30,
        31,32,33,34,35,36,37,38,39,40,
        41,42,43,44,45,46,47,48,49,50,
        51,52,53,54]).
loteriaNombres(["El gallo","El diablito","La dama","EL catrín","El paraguas","La sirena",
                "La escalera","La botella","El barril","EL árbol","El melon",
                "El valiente","El gorrito","La muerte","La pera","La bandera",
                "El bandolón","El viloncello","La garza","El pájaro",
		"La mano","La bota","La luna","El cotorro","El borracho","El negrito",
                "EL corazón","La sandía","El tambor",
		"El camarón","Las jaras","El músico","La araña",
		"El soldado","La estrella","El cazo","El mundo","El apache","El nopal",
		"El alacrán","La rosa","La calavera","La campana","El cantarito",
		"El venado","El sol","La corona","La chalupa","El pino",
		"El pescado","La palma","La maceta","El arpa","La rana"]).

cartaBase([1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16]).

chorroF(0).
chorroString('').
centroF(0).
centroString('').
esquinaF(0).
esquinaString('').

%crear predicado para quitar elemento de cartas LISTO
%crear predicado para imprimir las cartas  LISTO
%crear predicado para actualizar el mazo LISTO
%crear predicado para actualizar las cartas LISTOOOOOOOOOOOOOOOOOOO
%crear predicado para jugada de chorro LISTO
%crear predicado para esquinas LISTO
%crear predicado para centro LISTO
%crear predicado para llenas LISTO
%validar que si ya salio una jugadano se vuelva a generar LISTOOOOOOO

%Comienzan predicados
%--------------------------------------------------------------------------------------------------------------------------
darSalto(H,R):- R<3, write(H), write(' ').
darSalto(H,R):- (R>3,R<7), write(H), write(' ').
darSalto(H,R):- (R>7,R<11), write(H), write(' ').
darSalto(H,R):- (R>11,R<15), write(H), write(' ').
darSalto(H,R):- R=3, write(H), nl.
darSalto(H,R):- R=7, write(H), nl.
darSalto(H,R):- R=11, write(H), nl.
darSalto(H,R):- R=15, write(H), nl.
%--------------------------------------------------------------------------------------------------------------------------
imprimirCarta([],0):- nl.
imprimirCarta([H|T],R):-  
                      imprimirCarta(T,R1),
                      darSalto(H,R1),
                      R is R1+1.
%--------------------------------------------------------------------------------------------------------------------------
indice([H|_], H, 1).
indice([_|T], E, Indice):-
  indice(T, E, Indice1),
  Indice is Indice1+1.
%--------------------------------------------------------------------------------------------------------------------------
quitar(N,[N|T],T).
quitar(N,[H1|T1],[H1|T2]):-
    quitar(N,T1,T2).
%--------------------------------------------------------------------------------------------------------------------------
obtenerValorPosicion([P|_], 1, P).
obtenerValorPosicion([_|T], N, R):-
          N2 is N - 1,
          obtenerValorPosicion(T, N2, R).
	
%--------------------------------------------------------------------------------------------------------------------------
largo(Loteria,Y):- length(Loteria,Y).
%--------------------------------------------------------------------------------------------------------------------------
obtenerCartaAleatoria(L,R):- L>1,L1 is L+1, random(1,L1,R).
obtenerCartaAleatoria(L,R):- L=1, R is 1.
%--------------------------------------------------------------------------------------------------------------------------
agregarNumerosCarta(_,_,[], 0).
agregarNumerosCarta(Loteria,Carta,[H|T], S):-
          largo(Loteria,L), 
          obtenerCartaAleatoria(L,Pos),
          obtenerValorPosicion(Loteria,Pos, ElementoQuitado),
          quitar(ElementoQuitado, Loteria, LoteriaAux),
          append([ElementoQuitado],CartaAux, Carta),
          agregarNumerosCarta(LoteriaAux,CartaAux,T, S2),
          S is S2 + H.
%--------------------------------------------------------------------------------------------------------------------------
llenarCartas(Carta1):-
      loteria(Loteria),
      cartaBase(CartaBase),
      agregarNumerosCarta(Loteria,Carta1,CartaBase,R).
%--------------------------------------------------------------------------------------------------------------------------
replace(E, [E|T], NE, [NE|T]).
replace(E, [H1|T], NE, [H1|NT]):-
	replace(E, T, NE, NT).
%--------------------------------------------------------------------------------------------------------------------------
ponerCero(CartaJuego,Carta,Carta):- not(member(CartaJuego,Carta)).
ponerCero(CartaJuego,Carta,CartaAux):-
    member(CartaJuego,Carta), 
    indice(Carta,CartaJuego,Indice),
    replace(CartaJuego,Carta,0,CartaAux).

%--------------------------------------------------------------------------------------------------------------------------
impresionJugadores(Carta1,Carta2,Carta3,LongitudCarta3):- 
    LongitudCarta3>0,

    write('---------------'),      nl,
    write('CARTA JUGADOR 1: '),    nl,
    imprimirCarta(Carta1,_),       nl,
    write('CARTA JUGADOR 2: '),    nl,
    imprimirCarta(Carta2,_),       nl,
    write('CARTA JUGADOR 3: '),    nl,
    imprimirCarta(Carta3,_),       nl,
    write('---------------'),      nl.

impresionJugadores(Carta1,Carta2,Carta3,LongitudCarta3):- 
    LongitudCarta3=0,
    write('---------------'),   nl,
    write('CARTA JUGADOR 1: '), nl,
    imprimirCarta(Carta1,_),    nl,
    write('CARTA JUGADOR 2: '), nl,
    imprimirCarta(Carta2,_),    nl,
    write('---------------'),   nl.
%--------------------------------------------------------------------------------------------------------------------------
miniChorro(CartaJugador,P1,P2,P3,P4,FlagCH, N):- predicado4Posiciones(CartaJugador,[P1,P2,P3,P4]),
	FlagCH=0, write('El jugador '), write(N), write(' hizo CHORRO!'),  asserta(chorroF(1)),
concat('HIZO CHORRO: JUGADOR ', N, String),
asserta(chorroString(String)), nl.
%--------------------------------------------------------------------------------------------------------------------------
jugadaChorroMain(CartaJugador,[H|T],FlagH,Numero).

%--------------------------------------------------------------------------------------------------------------------------
jugadaChorro([],_,_).
jugadaChorro(CartaJugador,FlagCH,Numero):-
     (miniChorro(CartaJugador,1,2,3,4,FlagCH,Numero);
     miniChorro(CartaJugador,5,6,7,8,FlagCH,Numero);
     miniChorro(CartaJugador,9,10,11,12,FlagCH,Numero);
     miniChorro(CartaJugador,13,14,15,16,FlagCH,Numero);
     miniChorro(CartaJugador,1,5,9,13,FlagCH,Numero);
     miniChorro(CartaJugador,2,6,10,14,FlagCH,Numero);
     miniChorro(CartaJugador,3,7,11,15,FlagCH,Numero);
     miniChorro(CartaJugador,4,8,12,16,FlagCH,Numero);
     miniChorro(CartaJugador,4,7,10,13,FlagCH,Numero);
     miniChorro(CartaJugador,1,6,11,16,FlagCH,Numero)).

jugadaChorro(CartaJugador,FlagCH,Numero):-
     not(miniChorro(CartaJugador,1,2,3,4,FlagCH,Numero);
     miniChorro(CartaJugador,5,6,7,8,FlagCH,Numero);
     miniChorro(CartaJugador,9,10,11,12,FlagCH,Numero);
     miniChorro(CartaJugador,13,14,15,16,FlagCH,Numero);
     miniChorro(CartaJugador,1,5,9,13,FlagCH,Numero);
     miniChorro(CartaJugador,2,6,10,14,FlagCH,Numero);
     miniChorro(CartaJugador,3,7,11,15,FlagCH,Numero);
     miniChorro(CartaJugador,4,8,12,16,FlagCH,Numero);
     miniChorro(CartaJugador,4,7,10,13,FlagCH,Numero);
     miniChorro(CartaJugador,1,6,11,16,FlagCH,Numero)).
%--------------------------------------------------------------------------------------------------------------------------
jugadaEsquina([],_,_).
jugadaEsquina(CartaJugador,FlagES,Numero):-
     (
      predicado4Posiciones(CartaJugador,[1,4,13,16]),FlagES=0, write('El jugador '), write(Numero),write(' hizo ESQUINAS'),asserta(esquinaF(1)), 
      concat('HIZO ESQUINAS: JUGADOR ', Numero, String),
      asserta(esquinaString(String)),nl
     ).
jugadaEsquina(CartaJugador,FlagES,Numero):-
     not(
      (predicado4Posiciones(CartaJugador,[1,4,13,16]),FlagES=0, write('El jugador '), write(Numero),write(' hizo ESQUINAS'))
     ).
%--------------------------------------------------------------------------------------------------------------------------
jugadaCentro([],_,_).
jugadaCentro(CartaJugador,FlagCE,Numero):-
     (
      predicado4Posiciones(CartaJugador,[6,7,10,11]),FlagCE=0, write('El jugador '), write(Numero),write(' hizo CENTRO'), asserta(centroF(1)),
      concat('HIZO CENTRO: JUGADOR ', Numero, String),
      asserta(centroString(String)), nl
     ).
jugadaCentro(CartaJugador,FlagCE,Numero):-
     not(
      (predicado4Posiciones(CartaJugador,[6,7,10,11]),FlagCE=0, write('El jugador '), write(Numero),write(' hizo CENTRO'))
     ).
%------------------
predicado4Posiciones(_,[]).
predicado4Posiciones(CartaJugador,[H|T]):-
      obtenerValorPosicion(CartaJugador,H, P),
      P=0,
      predicado4Posiciones(CartaJugador,T).
%------------------
predicadoLlena([]).
predicadoLlena([0|T]):-
     predicadoLlena(T).
%--------------------------------------------------------------------------------------------------------------------------
jugadaLlena([],_).
jugadaLlena(CartaJugador,Numero):-
     (
      predicadoLlena(CartaJugador), write('EL JUGADOR '), write(Numero), write(' HIZO LLENAS!!'), nl, nl
     ).
%--------------------------------------------------------------------------------------------------------------------------
%--------------------------------------------------------------------------------------------------------------------------
jugadaLlenaFinalizar(CartaJugador,Numero):-
     (predicadoLlena(CartaJugador)).
%--------------------------------------------------------------------------------------------------------------------------
jugadaLlena(CartaJugador,Numero):-
     not(
      (predicadoLlena(CartaJugador),  write('El jugador '), write(Numero),write(' hizo LLENA'))
     ).
%--------------------------------------------------------------------------------------------------------------------------
quitarElementoMazo(Carta1,Carta2,Carta3,Carta1Aux,Carta2Aux,Carta3Aux,
                   Loteria,LoteriaNombres,LoteriaAux,LoteriaNombresAux,Pos):-
    
    chorroF(FlagCH),esquinaF(FlagES),centroF(FlagCE), chorroString(StringChorro), 
    centroString(StringCentro), esquinaString(StringEsquina),
    
    largo(Loteria,L), 
    obtenerCartaAleatoria(L,Pos),
     
    obtenerValorPosicion(Loteria,Pos, NumeroCarta),
    obtenerValorPosicion(LoteriaNombres,Pos,NombreCarta), 

    write(NumeroCarta), write('. '), write(NombreCarta), nl,   


    ponerCero(NumeroCarta,Carta1,Carta1Aux), 
    ponerCero(NumeroCarta,Carta2,Carta2Aux), 
    ponerCero(NumeroCarta,Carta3,Carta3Aux),
    

    largo(Carta3,LargoCarta3),
    impresionJugadores(Carta1Aux,Carta2Aux,Carta3Aux,LargoCarta3),
    
    jugadaChorro(Carta1,FlagCH,1), jugadaChorro(Carta2,FlagCH,2), jugadaChorro(Carta3,FlagCH,3),

    jugadaCentro(Carta1Aux,FlagCE,1), jugadaCentro(Carta2Aux,FlagCE,2), jugadaCentro(Carta3Aux,FlagCE,3),

    jugadaEsquina(Carta1Aux,FlagES,1), jugadaEsquina(Carta2Aux,FlagES,2), jugadaEsquina(Carta3Aux,FlagES,3),    
    
    
    jugadaLlena(Carta1Aux,1),
    jugadaLlena(Carta2Aux,2),
    jugadaLlena(Carta3Aux,3),
     
    
    write(StringChorro),nl,
    write(StringCentro),nl,
    write(StringEsquina),nl,
    
    quitar(NumeroCarta, Loteria, LoteriaAux), 
    quitar(NombreCarta, LoteriaNombres, LoteriaNombresAux),
    write('SIGUIENTE: '),read(Siguiente).
%--------------------------------------------------------------------------------------------------------------------------
tirarCartas(Carta1,Carta2,Carta3,_,_):- largo(Carta3,LargoCarta3),
                                        (
                                         ((jugadaLlenaFinalizar(Carta1,1);jugadaLlenaFinalizar(Carta2,2)));
                                         (LargoCarta3>0, jugadaLlenaFinalizar(Carta3,3))
                                        ),
                                        write('FINALIZÓ EL JUEGO POR LLENAS.').
tirarCartas(_,_,_,[],[]):- write('SE ACABARON LAS CARTAS.').
tirarCartas(Carta1,Carta2,Carta3,Loteria,LoteriaNombres):-
     
     quitarElementoMazo(Carta1,Carta2,Carta3,Carta1Aux,Carta2Aux,Carta3Aux,
                        Loteria,LoteriaNombres,LoteriaAux,LoteriaNombreAux,Pos),
     tirarCartas(Carta1Aux,Carta2Aux,Carta3Aux,LoteriaAux,LoteriaNombreAux).
%--------------------------------------------------------------------------------------------------------------------------
validarJugadores(C,C):- C=2;C=3.
validarJugadores(C,R):- (C=<1; C>=4),
	write("Solo deben ser 2 o 3 jugadores, ingreselos de nuevo: "),
	read(C1),
	validarJugadores(C1,C1).
%--------------------------------------------------------------------------------------------------------------------------
jugar:-
      asserta(chorroString('')), asserta(centroString('')), asserta(esquinaString('')),
      asserta(chorroF(0)), asserta(centroF(0)), asserta(esquinaF(0)),

      loteria(Loteria),
      loteriaNombres(LoteriaNombres),
      

      write('JUEGO DE LA LOTERIA'), nl,
      write('Ingrese numero de jugadores:'), read(NumeroJugadores), validarJugadores(NumeroJugadores, JugadoresValidados),
      ( (JugadoresValidados=2,llenarCartas(Carta1),llenarCartas(Carta2)) ;
        (JugadoresValidados=3,llenarCartas(Carta1),llenarCartas(Carta2),llenarCartas(Carta3)) ),
      largo(Carta3,LargoCarta3),
      write('CARTAS GENERADAS: '), nl,
      impresionJugadores(Carta1,Carta2,Carta3,LargoCarta3),
      write('COMENZAR: '), nl, read(AlgoParaContinuar),
      tirarCartas(Carta1,Carta2,Carta3,Loteria,LoteriaNombres).

      


