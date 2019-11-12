comb(L, K, L):- length(L,K), !.
comb(_L,0,[]):-!.
comb([H|T],K,[H|TC]):- K1 is K-1, comb(T, K1, TC).
comb([_|T],K,Co):-comb(T,K,Co).


camino(G,A,B,[A,B]):-member([A,B],G).
camino(G,A,B,[A|C]):-member([A,X],G), camino(G,X,B,C), \+ member(A,C),!.

ciclo(G,A,B,[A,B]):-memberchk([A,B],G).
ciclo(G,A,B,[A|C]):-memberchk([A,X],G), ciclo(G,X,B,C).

hamilton(G,_,_):- conexo(G,B),B==false,false,!.
hamilton(G,_,L23):- conexo(G,B),B==true,quitaPeso(G,L),flatten(L,L1),eliminar(L1,[],L2),member(X,L2),valida_hamilton(L,X),!,member(X1,L2),member(X2,L1),X1\==X2,setof(X3,camino(L,X1,X2,X3),L23).

hamilton_aux(G):-member(X,G),valida_hamilton(G,X),!.


conexo(G,false):- quitaPeso(G,L), flatten(L,L1), eliminar(L1,[],L3), domino(L3,[X,Y]), \+ camino(L,X,Y,_),!,false.
conexo(G,true):- quitaPeso(G,L), flatten(L,L1), eliminar(L1,[],L3), domino(L3,[X,Y]),  camino(L,X,Y,_),true,!.


%si X tiene una arista dirigida a Y.
adyacente(X,Y,G) :- member([X,Y],G).

ciclo(G,P):-quitaPeso(G,L), flatten(L,L1),eliminar(L1,[],L3),member(X,L3),ciclo_aux(L,X,P).

ciclo_aux(G,A,P) :- 
   adyacente(B,A,G), camino(G,A,B,P1), length(P1,L), L > 1, append(P1,[A],P),!.

%valida si todos los nodos son de grado mayor a 1
valida_hamilton([[_A,B]|_T],E):-B==E,true,!.
valida_hamilton([[_A,B]|[]],E):-B==E,true,!.
valida_hamilton([[_A,B]|T],E):-B\==E,valida_hamilton(T,E),!.
valida_hamilton([[_A,B]|[]],E):-B\==E,false,!.



quitaPeso(G,L):-quitaPeso_aux2(G,[],L).
quitaPeso_aux2([],A,A):-!.
quitaPeso_aux2([H|T],A,L):-quitaPeso_aux1(H,L1),append(A,L1,L2),quitaPeso_aux2(T,L2,L).
quitaPeso_aux1([H1,T1,_],L):-append([],[[H1,T1]],L).

eliminar([],A,A1):-sort(A,A1),!.
eliminar([H|T],A,L):-member(H,T),eliminar(T,A,L),!.
eliminar([H|T],A,L):- \+ member(H,T),append(A,[H],L1),eliminar(T,L1,L),!.

my_member(A,G):-member(A,G).

matriz(G,L):-quitaPeso(G,L1),flatten(L1,L2),eliminar(L2,[],L3),product(L3,L3,L4),length(L3,LE),

matriz_aux(L4,L1,LE,LE,[],[],LE,L).
matriz_aux([],_G,_,_,A,_A1,_O,A):-!.
matriz_aux([H|T],G,1,C,A,A1,O,L):-member(H,G)-> C1 is C-1,append(A1,[1],A12),append(A,[A12],A2), matriz_aux(T,G,O,C1,A2,[],O,L),!;
C1 is C-1,append(A1,[0],A12),append(A,[A12],A2), matriz_aux(T,G,O,C1,A2,[],O,L),!.
matriz_aux([H|T],G,F,C,A,A1,O,L):- member(H,G)->  F1 is F-1,append(A1,[1],A2),matriz_aux(T,G,F1,C,A,A2,O,L),!;
 F1 is F-1,append(A1,[0],A3),matriz_aux(T,G,F1,C,A,A3,O,L),!.

product(A,B,C) :-
    findall([X,Y],(member(X,A),member(Y,B)),C).

domino(L,[X,Y]):-member(X,L), member(Y,L), X\=Y.

