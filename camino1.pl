camino(G,A,B,[A,B]):-member([A,B],G).
camino(G,A,B,[A|C]):-member([A,X],G), camino(G,X,B,C), \+ member(A,C),!.

quitaPeso(G,L):-quitaPeso_aux2(G,[],L).
quitaPeso_aux2([],A,A):-!.
quitaPeso_aux2([H|T],A,L):-quitaPeso_aux1(H,L1),append(A,L1,L2),quitaPeso_aux2(T,L2,L).
quitaPeso_aux1([H1,T1,_],L):-append([],[[H1,T1]],L).

eliminar([],A,A):-!.
eliminar([H|T],A,L):-member(H,T),eliminar(T,A,L).
eliminar([H|T],A,L):- \+ member(H,T),append(A,[H],L1),eliminar(T,L1,L2),sort(L2,L).

matriz(G,L):-quitaPeso(G,L1),flatten(L1,L2),eliminar(L2,[],L3),product(L3,L3,L4),length(L3,LE),
matriz_aux(L4,L1,LE,LE,[],[],LE,L).

matriz_aux([],_G,_,_,A,_A1,_O,A):-!.
matriz_aux([H|T],G,1,C,A,A1,O,L):-member(H,G)-> C1 is C-1,append(A1,[1],A12),append(A,[A12],A2), matriz_aux(T,G,O,C1,A2,[],O,L),!;
C1 is C-1,append(A1,[0],A12),append(A,[A12],A2), matriz_aux(T,G,O,C1,A2,[],O,L),!.
matriz_aux([H|T],G,F,C,A,A1,O,L):- member(H,G)->  F1 is F-1,append(A1,[1],A2),matriz_aux(T,G,F1,C,A,A2,O,L);
 F1 is F-1,append(A1,[0],A3),matriz_aux(T,G,F1,C,A,A3,O,L).

product(A,B,C) :-
    findall([X,Y],(member(X,A),member(Y,B)),C).

domino(L,[X,Y]):-member(X,L), member(Y,L), X\=Y.

fconexo(G,false):- quitaPeso(G,L), listanodos(G,L2), domino(L2,[X,Y]), \+ camino(L,X,Y,_),!,false.
fconexo(G,true):- quitaPeso(G,L), listanodos(G,L2), domino(L2,[X,Y]),  camino(L,X,Y,_),true,!.

conexo(G,Z):-nodirigido(G,[],L), fconexo(L,Z).

nodirigido([],L,L):-!.
nodirigido([[A,B,C]|T],L,X):- append(L,[[A,B,C],[B,A,C]],L1), nodirigido(T,L1,X). 

listanodos(G,L):- quitaPeso(G, L0), flatten(L0, L1), eliminar(L1,[],L).

edge(G,A):-member(A,G).
edge(G,[B,A]):-member([A,B],G).

%busca(G, Camino):-quitaPeso(G,L),listanodos(G,L1), domino(L1,[A,B]), member([A,B],L).
%camino(G,A,B,[A|C]):-member([A,X],G), camino(G,X,B,C), \+ member(A,C),!.

%hamilton(G, Camino, Costo):-conexo(G), listanodos(G, [H|T]), quitaPeso(G,[[A,B]|R]), \+member(H,Camino).    
%hamilton(G, Camino, Costo):- conexo(G), listanodos(G, L1), quitaPeso(G,L2).

