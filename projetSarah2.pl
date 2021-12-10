:- consult(fonction).
:- consult(atbox).

concate([],L1,L1).
concate([X|Y],L1,[X|L2]) :- concat(Y,L1,L2).


/* _________________________PARTIE  2__________________________________*/


saisie_et_traitement_prop_a_demontrer(Abi,Abi1,Tbox) :-
    nl,write("Entrez   le   numero   du   type   de   proposition   que   vous   voulez demontrer :"),
    nl,write("1 Une instance donnee appartient a un conand(a,b).cept donne."),
    nl,write("2 Deux concepts nelements en commun(ils ont uneintersection vide)."),
    nl,read(R),
    suite(R,Abi,Abi1,Tbox),!.

suite(1,Abi,Abi1,Tbox) :- acquisition_prop_type1(Abi,Abi1,Tbox),!.
suite(2,Abi,Abi1,Tbox) :- acquisition_prop_type2(Abi,Abi1,Tbox),!.
suite(R,Abi,Abi1,Tbox) :-
    nl,
    write("Cette reponse est incorrecte."),
    nl, saisie_et_traitement_prop_a_demontrer(Abi,Abi1,Tbox).

/* propo 1*/

acquisition_prop_type1(Abi,Abi1,Tbox) :- 
    nl,write("Entrez Instance  "),
    nl,read(I),nl,write("lectrue  instance"),
    nl,write("Entrez Concept  "),
    nl,read(C), nl,write("lectrue  concept"),
    concept(C), nl,write("concept") ,replace(C,RC),nl,write("replace done") ,nnf(not(RC),NRC),
    nl,write("nnf done"),
    concat([((I,NRC))],Abi,Abi1),
    nl,write("la a box apres ajout "),
    nl,write(Abi1).

acquisition_prop_type2(Abi,Abi1,Tbox) :- 
    nl,write("entrer la propositon à demontrer en commencat par concep1"),
    nl,read(C1),
    nl,write("entrez le concept2"),
    nl,read(C2),
    concept(C1),
    concept(C2),
    /*genere(Nom)*/ 
    replace(and(C1,C2),and(RC1,RC2)),
    nl,write("replace pour la 2eme formule done"),
    nl,write("nnf pour la 2eme formule done"),
    /*inst(insta,and(C1,C2)),*/
    concat([((inst,or(NC1,NC2)))],Abi,Abi1),
    nl,write("la a box apres ajout "),
    nl,write(Abi1).
   

/* ________________________________PARTIE  3__________________________________*/

tri_Abox([],_,_,_,_,_).
tri_Abox([(I,some(R,C))|Abi],[(I,some(R,C))|Lie],Lpt,Li,Lu,Ls) :- tri_Abox(Abi,Lie,Lpt,Li,Lu,Ls),!.
tri_Abox([(I,all(R,C))|Abi],Lie,[(I,all(R,C))|Lpt],Li,Lu,Ls) :-tri_Abox(Abi,Lie,Lpt,Li,Lu,Ls),!.
tri_Abox([(I,and(C1,C2))|Abi],Lie,Lpt,[(I,and(C1,C2))|Li],Lu,Ls) :- tri_Abox(Abi,Lie,Lpt,Li,Lu,Ls),!.
tri_Abox([(I,or(C1,C2))|Abi],Lie,Lpt,Li,[(I,or(C1,C2))|Lu],Ls) :- tri_Abox(Abi,Lie,Lpt,Li,Lu,Ls),!.
tri_Abox([(I,C)|Abi],Lie,Lpt,Li,Lu,[(I,C)|Ls]) :- setof(X,cnamea(X),L), member(C,L) , tri_Abox(Abi,Lie,Lpt,Li,Lu,Ls),!.

complete_some([some(R,C)|Lie],Lpt,Li,Lu,Ls,[(I,genere(Nom),R)|Abr]):- 
    evolue((Nom,C), Lie, Lpt, Li, Lu, Ls, Lie1, Lpt1, Li1, Lu1, Ls1),
    resolution(Lie1, Lpt1, Li1, Lu1, Ls1,Abr).

transformation_and(Lie, Lpt,[I,(and(C1,C2))|Li],Lu,Ls,Abr) :- 
    evolue((I,C1), Lie, Lpt, Li, Lu, Ls, Lie1, Lpt1, Li1, Lu1, Ls1),
    evolue((I,C2), Lie1, Lpt1, Li1, Lu1, Ls1, Lie2, Lpt2, Li2, Lu2, Ls2),
    resolution(Lie2, Lpt2, Li2, Lu2, Ls2,Abr).

deduction_all(Lie,[(I,all(R,C))|Lpt],Li,Lu,Ls,[(I,X,R)|Abr]) :-
    evolue((X,C), Lie, Lpt, Li, Lu, Ls, Lie1, Lpt1, Li1, Lu1, Ls1),
    resolution(Lie1, Lpt1, Li1, Lu1, Ls1,Abr).
    
transformation_or(Lie,Lpt,Li,[(I,or(C1,C2))|Lu],Ls,Abr) :-
    evolue((I,C1), Lie, Lpt, Li, Lu, Ls, Lie1, Lpt1, Li1, Lu1, Ls1),
    evolue((I,C2), Lie, Lpt, Li, Lu, Ls, Lie2, Lpt2, Li2, Lu2, Ls2),
    resolution(Lie1, Lpt1, Li1, Lu1, Ls1,Abr),
    resolution(Lie2, Lpt2, Li2, Lu2, Ls2,Abr).

evolue((b,C), Lie, Lpt, Li, Lu, Ls, Lie1, Lpt1, Li1, Lu1, Ls1) :-
    evolue((b,C), Lie, Lpt, Li, Lu, Ls, Lie1, Lpt1, Li1, Lu1,[(b,C) | Ls1]).

evolue((I,some(R,C)), Lie, Lpt, Li, Lu, Ls, Lie1, Lpt1, Li1, Lu1, Ls1):-
    evolue((I,some(R,C)),Lie, Lpt, Li, Lu, Ls, [(I,some(R,C))|Lie1], Lpt1, Li1, Lu1, Ls1).

evolue((I,all(R,C)), Lie, [(I,all(R,C))|Lpt], Li, Lu, Ls, Lie1, Lpt1, Li1, Lu1, Ls1):-
    evolue((I,all(R,C)), Lie, Lpt, Li, Lu, Ls, Lie1, [(I,all(R,C))|Lpt1], Li1, Lu1, Ls1).

evolue((I,and(C1,C2)), Lie, Lpt, [(I,and(C1,C2))|Li], Lu, Ls, Lie1, Lpt1, Li1, Lu1, Ls1):-
    evolue((I,and(C1,C2)), Lie, Lpt,Li, Lu, Ls, Lie1, Lpt1,  [(I,and(C1,C2))|Li1], Lu1, Ls1).

evolue((I,or(C1,C2)), Lie, Lpt, Li,[(I,or(C1,C2))| Lu], Ls, Lie1, Lpt1, Li1, Lu1, Ls1) :-
	evolue((I,or(C1,C2)), Lie, Lpt, Li,Lu, Ls, Lie1, Lpt1, Li1, [(I,or(C1,C2))| Lu1], Ls1).


/*Etape 3*/

resolution(Lie,Lpt,Li,Lu,Ls,Abr) :-
    complete_some(Lie,Lpt,Li,Lu,Ls,Abr),
    transformation_and(Lie,Lpt,Li,Lu,Ls,Abr),
    deduction_all(Lie,Lpt,Li,Lu,Ls,Abr),
    transformation_or(Lie,Lpt,Li,Lu,Ls,Abr).

/*__________________________PARTIE 1____________________________________*/

premiere_etape(Tbox,Abi,Abr) :-
    setof((X,Y),equiv(X,Y),Tbox),
    setof((X,Y),inst(X,Y),Abi),
    setof((X,Y,Z),instR(X,Y,Z),Abr).

/*__________________________PARTIE 2____________________________________*/

deuxieme_etape(Abi,Abi1,Tbox) :-
    saisie_et_traitement_prop_a_demontrer(Abi,Abi1,Tbox).

/*__________________________PARTIE 3____________________________________*/

troisieme_etape(Abi,Abr) :- 
    tri_Abox(Abi,Lie,Lpt,Li,Lu,Ls),
    resolution(Lie,Lpt,Li,Lu,Ls,Abr),
    nl,write('Youpiiiiii, on a demontre la proposition initiale !!!').

/*__________________________PROGRAME____________________________________*/

programe :-
    premiere_etape(Tbox,Abi,Abr),
    deuxieme_etape(Abi,Abi1,Tbox),
    troisieme_etape(Abi1,Abr).
