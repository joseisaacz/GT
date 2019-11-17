comb(L, K, L):- length(L,K), !.
comb(_L,0,[]):-!.
comb([H|T],K,[H|TC]):- K1 is K-1, comb(T, K1, TC).
comb([_|T],K,Co):-comb(T,K,Co).


%camino(G,A,_,_):- \+ member([A,_],G),!,false.
%camino(G,A,B,[A,B]):-member([A,B],G).
%camino(G,A,B,[A|C]):-member([A,X],G),member([X,A],G) ,camino(G,X,B,C), \+ member(A,C),!.

camino(G,_,B,_):-flatten(G,L1),\+ member(B,L1),!,false.
camino(G,A,_,_):-flatten(G,L1),\+ member(A,L1),!,false.
camino(G,A,B,_):- \+ member([A,B],G),false,!.
camino(G,A,B,[A,B]):-member([A,B],G).
camino(G,A,B,[A|C]):-adyacente(A,X,G),X\==B,camino(G,X,B,C), \+ member(A,C).
camino(G,A,B,_):- \+ adyacente(A,B,G),false,!.

camino_aux(G,A,B,_):- \+ adyacente(A,B,G),false.
camino_aux(G,_,B,_):-flatten(G,L1),\+ member(B,L1),!,false.
camino_aux(G,A,_,_):-flatten(G,L1),\+ member(A,L1),!,false.
camino_aux(G,A,B,_):- \+ member([A,B],G),false.
camino_aux(G,A,B,[A,B]):-member([A,B],G).

%camino_bool(G,_,B,_):-flatten(G,L1),\+ member(B,L1),!,false.
%camino_bool(G,A,_,_):-flatten(G,L1),\+ member(A,L1),!,false.
%camino_bool(G,A,B,[A,B]):- \+ member([A,B],G),false,!.
%camino_bool(G,A,B,[A,B]):-member([A,B],G),!.
%camino_bool(G,A,B,[A|C]):-member([A,X],G), camino(G,X,B,C),!, \+ member(A,C),!.


hamilton(G,_,_):- \+ conexo(G),false,!.
hamilton(G,CO,R):- conexo(G),quitaPeso(G,L),flatten(L,L1),eliminar(L1,[],L2),member(X,L2),valida_hamilton(L,X),!,member(X1,L2),member(Y,L2),X1\==Y,camino(L,X1,Y,CA),flatten(CA,CA1),eliminar(CA1,[],CA2),CA2==L2,append(CA,[],R),calcula_costo(CA,G,0,CO).

chamilton(G,_,_):- \+ conexo(G),false,!.
chamilton(G,CO,R):- conexo(G),quitaPeso(G,L),flatten(L,L1),eliminar(L1,[],L2),member(X,L2),valida_hamilton(L,X),!,member(X1,L2),member(Y,L2),X1\==Y,camino(L,X1,Y,CA),flatten(CA,CA1),eliminar(CA1,[],CA2),CA2==L2,chamilton_aux(L,CA,X1,Y,R1),calcula_costo(R1,G,0,CO),append([],R1,R).


chamilton_aux(G,C,X,Y,C):- \+adyacente(Y,X,G).
chamilton_aux(G,C,X,Y,R):- adyacente(Y,X,G),append(C,[X],R).
%conexo(G):- quitaPeso(G,L),flatten(L,L1), eliminar(L1,[],L3),comb(L3,2,[X,Y]) ,conexo_aux(L,X,Y), true.
%conexo(G):- quitaPeso(G,L),flatten(L,L1), eliminar(L1,[],L3),comb(L3,2,[X,Y]) ,\+ conexo_aux(L,X,Y),format("~NNo es Conexo",""), !,false.

%conexo(G):- quitaPeso(G,L),flatten(L,L1), eliminar(L1,[],L3),domino(L3,[X,Y]) ,conexo_aux(L,X,Y), true.
%conexo(G):- quitaPeso(G,L),flatten(L,L1), eliminar(L1,[],L3),domino(L3,[X,Y]) ,\+ conexo_aux(L,X,Y),format("~NNo es Fuerte Conexo",""), !,false.
%conexo_aux(G,X,Y):- \+ camino_bool(G,X,Y,_),!,false.
%conexo_aux(G,X,Y):-camino_bool(G,X,Y,_),!,true.
%si X tiene una arista dirigida a Y.
adyacente(X,Y,G) :- member([X,Y],G).

ciclo(G,P):-quitaPeso(G,L),flatten(L,L1),eliminar(L1,[],L3),member(X,L3), ciclo_aux(L,X,P).

ciclo_aux(G,A,P) :- 
   adyacente(B,A,G), camino(G,A,B,P1), length(P1,L), L > 1, append(P1,[A],P),!.

%valida si todos los nodos son de grado mayor a 1
valida_hamilton([[_A,B]|_T],E):-B==E,true,!.
valida_hamilton([[_A,B]|[]],E):-B==E,true,!.
valida_hamilton([[_A,B]|T],E):-B\==E,valida_hamilton(T,E),!.
valida_hamilton([[_A,B]|[]],E):-B\==E,false,!.

calcula_costo([_T],_G,A,A):-!.
calcula_costo([H|T],G,A,R):-nth(1,T,B),member([H,B,X],G),A1 is A+X,calcula_costo(T,G,A1,R).

quitaPeso(G,L):-quitaPeso_aux2(G,[],L).
quitaPeso_aux2([],A,A):-!.
quitaPeso_aux2([H|T],A,L):-quitaPeso_aux1(H,L1),append(A,L1,L2),quitaPeso_aux2(T,L2,L).
quitaPeso_aux1([H1,T1,_],L):-append([],[[H1,T1]],L).

eliminar([],A,A1):-sort(A,A1),!.
eliminar([H|T],A,L):-member(H,T),eliminar(T,A,L),!.
eliminar([H|T],A,L):- \+ member(H,T),append(A,[H],L1),eliminar(T,L1,L),!.


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


vertices(G,R):-flatten(G,R1),eliminar(R1,[],R).

max_length([],Aux,Aux):-!.
max_length([H|T],Aux,Ret):-length(H,L1),L1<Aux,max_length(T,Aux,Ret).
max_length([H|T],Aux,Ret):-length(H,L1),L1>=Aux,max_length(T,L1,Ret).
max_length([H|T],Ret):-length(H,L1),max_length([H|T],L1,Ret).

min_length([],Aux,Aux):-!.
min_length([H|T],Aux,Ret):-length(H,L1),L1>Aux,min_length(T,Aux,Ret).
min_length([H|T],Aux,Ret):-length(H,L1),L1=<Aux,min_length(T,L1,Ret).
min_length([H|T],Ret):-length(H,L1),min_length([H|T],L1,Ret).
%min_length([H|T],Ret):-length(H,L1),min_length([H|T],L1,Ret)
%distancias([[H|T]|T1],Aux,Ret):-length([H|T],N),N==1,append(Aux,[H|T],Aux1),distancias(T1,Aux1,Ret).
distancias([H|T],Ret):-distancias_aux([H|T],[],Ret).


max_dist_length([H|T],Ret):-max_length(H,L),max_dist_length([H|T],L,Ret).

max_dist_length([],Len,Len):-!.
max_dist_length([H|T],Len,Ret):-nth(1,H,L),length(L,L1),L1>=Len,max_dist_length(T,L1,Ret).
max_dist_length([H|T],Len,Ret):-nth(1,H,L),length(L,L1),L1<Len,max_dist_length(T,Len,Ret).

max_dist([H|T],Ret):-max_dist_length([H|T],Len),max_dist([H|T],[],Len,Ret).
max_dist([],Aux,_Len,Aux):-!.
max_dist([H|T],Aux,Len,Ret):-nth(1,H,L),length(L,L1),L1\==Len,max_dist(T,Aux,Len,Ret).
max_dist([H|T],Aux,Len,Ret):-nth(1,H,L),length(L,L1),L1==Len,append(Aux,H,A1),max_dist(T,A1,Len,Ret).
distancias_aux([],Aux,Aux):-!.
distancias_aux([H|T],Aux,Ret):-min_length(H,L1),distancias_aux1(H,[],L1,R1),append(Aux,[R1],Aux1),distancias_aux(T,Aux1,Ret).
distancias_aux1([],Aux,_Len,Aux):-!.
distancias_aux1([H|T],Aux,Len,Ret):-length(H,L1),L1>Len,distancias_aux1(T,Aux,Len,Ret),!.
distancias_aux1([H|T],Aux,Len,Ret):-length(H,L1),L1=<Len,append(Aux,[H],Aux1),distancias_aux1(T,Aux1,L1,Ret),!.
conexo_aux(_G,[],_L,_Nds):-!.
conexo_aux(G,[H|T],[D],Nds):- camino_bool(G,H,D),  conexo_aux(G,T,Nds,Nds).
conexo_aux(G,[H|T],[H1|T1],Nds):- camino_bool(G,H,H1),  conexo_aux(G,[H|T],T1,Nds).
conexo_aux(G) :- vertices(G, Nds), conexo_aux(G,Nds,Nds,Nds),!.

camino_bool(_Grafo,Destino,Destino,_Ruta):-!.
camino_bool(Grafo,Origen,Destino,Ruta):- \+ member(Origen,Ruta), append(Ruta,[Origen],Ruta2), member([Origen,Z],Grafo), camino_bool(Grafo,Z,Destino,Ruta2),!.
camino_bool(Grafo,Origen,Destino):- camino_bool(Grafo,Origen,Destino,[]),!.

quitaDir([],Aux,Aux):-!.
quitaDir([[A,B]|T],Aux,X):- append(Aux,[[A,B]],Aux2), append(Aux2,[[B,A]],Aux3), quitaDir(T,Aux3,X).
quitaDir(G,X):- quitaDir(G,[],X).

conexo(G):-quitaPeso(G,L),quitaDir(L,L1),conexo_aux(L1).
fuerteconexo(G):-quitaPeso(G,L),conexo_aux(L).
diametro(G,Ret):-quitaPeso(G,L),vertices(L,Vts),findall(X2,(member(X,Vts),member(Y,Vts),X\==Y,setof(X3,camino(L,X,Y,X3),X2)),X1),distancias(X1,Dis),max_dist(Dis,R1),member(X,R1),nth(1,X,Fi),last(X,La),
append([],[Fi,La],Ret).


%setof(X1,camino(L,X,Y,X1),X2),write(X2).
