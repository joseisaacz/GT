camino(G,A,B,[A,B]):-member([A,B],G).
camino(G,A,B,[A|C]):-member([A,X],G), camino(G,X,B,C), \+ member(A,C).


