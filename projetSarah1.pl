:- consult(atbox).


/* Remplacement de concepts non atomique*/

replace(and(C1,C2),and(RC1,RC2)) :- replace(C1,RC1), replace(C2,RC2), !.
replace(or(C1,C2),or(RC1,RC2)) :- replace(C1,RC1), replace(C2,RC2), !.
replace(all(R,C),all(R,RC)) :- replace(C,RC) ,!.
replace(some(R,C),some(R,RC)) :- replace(C,RC) ,!.
replace(not(C),not(RC)) :- replace(C,RC) ,!.
replace(C,C) :- setof(X,cnamea(X),L) , member(C,L),!.
replace(C,RC) :- setof(X,cnamena(X),L) , member(C,L) , equiv(C,RC),!.

/*negation des propostions*/

nnf(not(and(C1,C2)),or(NC1,NC2)):- nnf(not(C1),NC1), 
nnf(not(C2),NC2),!.
nnf(not(or(C1,C2)),and(NC1,NC2)):- nnf(not(C1),NC1), 
nnf(not(C2),NC2),!.
nnf(not(all(R,C)),some(R,NC)) :- nnf(not(C),NC),!.
nnf(not(some(R,C)),all(R,NC)):- nnf(not(C),NC),!.
nnf(not(not(X)),X):-!.
nnf(not(X),not(X)):-!.
nnf(and(C1,C2),and(NC1,NC2)):- nnf(C1,NC1),nnf(C2,NC2),!.
nnf(or(C1,C2),or(NC1,NC2)):- nnf(C1,NC1), nnf(C2,NC2),!.
nnf(some(R,C),some(R,NC)):- nnf(C,NC),!.
nnf(all(R,C),all(R,NC)) :- nnf(C,NC),!.
nnf(X,X).


/* verification syntaxique*/

concept(and(C1,C2)) :- concept(C1), concept(C2), !.
concept(or(C1,C2)) :- concept(C1), concept(C2), !.
concept(all(R,C)) :- concept(C) ,concept(R),!.
concept(some(R,C)) :- concept(C),concept(R) ,!.
concept(not(C)) :- concept(C) ,!.
concept(C) :- cnamea(C),!.
concept(C) :- rname(C),!.
concept(C) :- cnamena(C),!.

/* Lecture */

lecture([X|L]):-
    read(X), 
    X \= fin, !, 
lecture(L).
lecture([]).

enleve(X,[X|L],L) :-!.
enleve(X,[Y|L],[Y|L2]) :- enleve(X,L,L2).

compteur(1).

genere(Nom) :- compteur(V),nombre(V,L1),
               concat([105,110,115,116],L1,L2),
               V1 is V+1,
               dynamic(compteur/1),
               retract(compteur(V)),
               dynamic(compteur/1),
               assert(compteur(V1)),nl,nl,nl,
               name(Nom,L2).
nombre(0,[]).
nombre(X,L1) :- 
                R is (X mod 10), 
                Q is ((X-R)//10),
                chiffre_car(R,R1),
                char_code(R1,R2),
                nombre(Q,L),
                concat(L,[R2],L1).
chiffre_car(0,"0").
chiffre_car(1,"1").
chiffre_car(2,"2").
chiffre_car(3,"3").
chiffre_car(4,"4").
chiffre_car(5,"5").
chiffre_car(6,"6").
chiffre_car(7,"7").
chiffre_car(8,"8").
chiffre_car(9,"9").
