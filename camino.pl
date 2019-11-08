comb(L, K, L):- length(L,K), !.
comb(_L,0,[]):-!.
comb([H|T],K,[H|TC]):- K1 is K-1, comb(T, K1, TC).
comb([_|T],K,Co):-comb(T,K,Co).

camino(G,A,B,[A,B]):-member([A,B],G).
camino(G,A,B,[A|C]):-member([A,X],G), camino(G,X,B,C), \+ member(A,C).

quitaPeso(G,L):-quitaPeso_aux2(G,[],L).
quitaPeso_aux2([],A,A):-!.
quitaPeso_aux2([H|T],A,L):-quitaPeso_aux1(H,L1),append(A,L1,L2),quitaPeso_aux2(T,L2,L).
quitaPeso_aux1([H1,T1,_],L):-append([],[[H1,T1]],L).

eliminar([],A,A):-!.
eliminar([H|T],A,L):-member(H,T),eliminar(T,A,L).
eliminar([H|T],A,L):- \+ member(H,T),append(A,[H],L1),eliminar(T,L1,L2),sort(L2,L).

my_member(A,G):-member(A,G).

matriz(G,L):-quitaPeso(G,L1),flatten(L1,L2),eliminar(L2,[],L3),product(L3,L3,L).



product(A,B,C) :-
    findall([X,Y],(member(X,A),member(Y,B)),C).

