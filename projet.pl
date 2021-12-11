%                                                 PROJET
%                                              LRC - M1 DAC
%                                     KRISNI Almehdi - J'DAY Achraf
%                               https://github.com/krisninho2000/Projet_LRC

% ##########################################################################################

% PARTIE PREPARATION

% ##########################################################################################

% TBox
equiv(sculpteur,and(personne,some(aCree,sculpture))).
equiv(auteur,and(personne,some(aEcrit,livre))).
equiv(editeur,and(personne,and(not(some(aEcrit,livre)),some(aEdite,livre)))).
equiv(parent,and(personne,some(aEnfant,anything))).

cnamea(personne).
cnamea(livre).
cnamea(objet).
cnamea(sculpture).
cnamea(anything).
cnamea(nothing).
cnamena(auteur).

cnamena(editeur).
cnamena(sculpteur).
cnamena(parent).

iname(michelAnge).
iname(david).
iname(sonnets).
iname(vinci).
iname(joconde).

rname(aCree).
rname(aEcrit).
rname(aEdite).
rname(aEnfant).

% ------------------------------------------------------------------------------------------

% ABox
% ABox d'instances
inst(michelAnge,personne). % Signifie que Michel-Ange est une personne
inst(david,sculpture).
inst(sonnets,livre).
inst(vinci,personne).
inst(joconde,objet).

% ABox de relations
instR(michelAnge, david, aCree). % Signifie que Michel-Ange a créé David
instR(michelAnge, sonnets, aEcrit).
instR(vinci, joconde, aCree).

% ------------------------------------------------------------------------------------------

% Règles de mises sous forme normale négative (NNF)
nnf(not(and(C1,C2)),or(NC1,NC2)) :- nnf(not(C1),NC1),
                                    nnf(not(C2),NC2),!.
nnf(not(or(C1,C2)),and(NC1,NC2)) :- nnf(not(C1),NC1),
                                    nnf(not(C2),NC2),!.
nnf(not(all(R,C)),some(R,NC)) :- nnf(not(C),NC),!.
nnf(not(some(R,C)),all(R,NC)) :- nnf(not(C),NC),!.
nnf(not(not(X)),X) :- !.
nnf(not(X),not(X)) :- !.
nnf(and(C1,C2),and(NC1,NC2)) :- nnf(C1,NC1),nnf(C2,NC2),!.
nnf(or(C1,C2),or(NC1,NC2)) :- nnf(C1,NC1), nnf(C2,NC2),!.
nnf(some(R,C),some(R,NC)) :- nnf(C,NC),!.
nnf(all(R,C),all(R,NC)) :- nnf(C,NC),!.
nnf(X,X).

% ------------------------------------------------------------------------------------------

% Règles de création de concepts
concept(and(C1,C2)) :- concept(C1), concept(C2), !.
concept(or(C1,C2)) :- concept(C1), concept(C2), !.
concept(all(R,C)) :- concept(C) ,concept(R), !.
concept(some(R,C)) :- concept(C),concept(R) ,!.
concept(not(C)) :- concept(C) ,!.
concept(C) :- cnamea(C), !.
concept(C) :- rname(C), !.
concept(C) :- cnamena(C), !.

% ------------------------------------------------------------------------------------------

% Prédicat prédéfini testant l'appartenance d'un élément X à une liste L
% member(X, L).

% ------------------------------------------------------------------------------------------

% Règles de remplacement (replace)
replace(and(C1,C2),and(RC1,RC2)) :- replace(C1,RC1), replace(C2,RC2), !.
replace(or(C1,C2),or(RC1,RC2)) :- replace(C1,RC1), replace(C2,RC2), !.
replace(all(R,C),all(R,RC)) :- replace(C,RC), !.
replace(some(R,C),some(R,RC)) :- replace(C,RC), !.
replace(not(C),not(RC)) :- replace(C,RC), !.
replace(C,C) :- setof(X,cnamea(X),L), member(C,L), !.
replace(C,RC) :- setof(X,cnamena(X),L), member(C,L), equiv(C,RC), !.

% ------------------------------------------------------------------------------------------

% Méthode réalise la concaténation de deux listes L1 et L2 et renvoie la liste L3
concat([],L1,L1).
concat([X|Y],L1,[X|L2]) :- concat(Y,L1,L2).

% ------------------------------------------------------------------------------------------

% Méthode réalisant la suppression de X de la liste L1 et renvoie la liste résultante L2
enleve(X,[X|L],L) :- !.
enleve(X,[Y|L],[Y|L2]) :- enleve(X,L,L2).

% ------------------------------------------------------------------------------------------

% Méthode de génération d'un nouvel identificateur qui est fourni en sortie dans Nom
genere(Nom) :-  compteur(V), nombre(V,L1),
                concat([105,110,115,116],L1,L2),
                V1 is V+1,
                dynamic(compteur/1),
                retract(compteur(V)),
                dynamic(compteur/1),
                assert(compteur(V1)), nl, nl, nl,
                name(Nom,L2).

nombre(0,[]).
nombre(X,L1) :- R is (X mod 10),
                Q is ((X-R)//10),
                chiffre_car(R,R1),
                char_code(R1,R2),
                nombre(Q,L),
                concat(L,[R2],L1).

chiffre_car(0,'0').
chiffre_car(1,'1').
chiffre_car(2,'2').
chiffre_car(3,'3').
chiffre_car(4,'4').
chiffre_car(5,'5').
chiffre_car(6,'6').
chiffre_car(7,'7').
chiffre_car(8,'8').
chiffre_car(9,'9').

compteur(1).

% ##########################################################################################

% PARTIE 1

% ##########################################################################################

% Cette méthode permet de créer des listes allant contenir la TBox, la ABox d'instances et la ABox de rôles.
% Ces listes évolueront au fur et à mesure qu'on soumettra des propositions à la démonstration.
premiere_etape(Tbox, Abi, Abr) :-
    setof((X, Y), equiv(X, Y), Tbox),
    setof((X, Y), inst(X, Y), Abi),
    setof((X, Y, Z), instR(X, Y, Z), Abr).

% ##########################################################################################

% PARTIE 2

% ##########################################################################################

% Méthode de la deuxième étape
deuxieme_etape(Abi,Abi1,Tbox) :-
    saisie_et_traitement_prop_a_demontrer(Abi,Abi1,Tbox).

% ------------------------------------------------------------------------------------------

% Mise en place du code fourni par l'énoncé
saisie_et_traitement_prop_a_demontrer(Abi,Abi1,Tbox) :-
    nl,write('Entrer le numéro du type de proposition que l"on souhaite démontrer:'),
    nl,write('1 Une instance donnée appartient à un concept donne.'),
    nl,write('2 Deux concepts nelements en commun(ils ont uneintersection vide).'),
    nl,read(R),
    suite(R,Abi,Abi1,Tbox), !.

% ------------------------------------------------------------------------------------------

suite(1,Abi,Abi1,Tbox) :-
    acquisition_prop_type1(Abi,Abi1,Tbox), !.

% ------------------------------------------------------------------------------------------

suite(2,Abi,Abi1,Tbox) :-
    acquisition_prop_type2(Abi,Abi1,Tbox), !.

% ------------------------------------------------------------------------------------------

suite(R,Abi,Abi1,Tbox) :-
    nl,write('Cette option n"existe pas : '),write(R),
    nl, saisie_et_traitement_prop_a_demontrer(Abi,Abi1,Tbox).

% ------------------------------------------------------------------------------------------
% On met en place les méthodes utilisées plus haut
% ------------------------------------------------------------------------------------------

% Méthode permettant l'acquisition de propositions du type 1 (I : C)
acquisition_prop_type1(Abi,Abi1,Tbox) :-
    % On entre le type de l'instance
    nl, write('Veuillez entrer l"instance :'),
    nl, read(I),

    % On récupère la liste des noms d'instances et on vérifie l'appartenance de I à L
    setof(X, iname(X), L),
    member(I,L),

    % On entre le concept
    nl, write('Veuillez entrer le concept :'),
    nl, read(C),

    % On effectue les manipulations sur le concept
    % On formalise le concept (C), on le remplace (RC) puis on effectue sa négation (NRC)
    concept(C),
    replace(C, RC),
    nnf(not(RC),NRC),

    % On ajoute l'élément (I, NRC) à la ABox
    concat([((I,NRC))],Abi,Abi1),

    % On affiche la ABox après modification
    nl, write(Abi1).

% ------------------------------------------------------------------------------------------

% Méthode permettant l'acquisition de proposition du type 2 : (C1 and C2 compris dans 'neg')
acquisition_prop_type2(Abi,Abi1,Tbox) :-
    % On entre le concept 1
    nl, write('Veuillez entrer le concept 1 :'),
    nl, read(C1),

    % On entre le concept 2
    nl, write('Veuillez entrer le concept 2 :'),
    nl, read(C2),

    % On effectue les manipulations sur les concepts
    % On formalise les concepts (C1 et C2) puis on effectue le remplacement (RC1 et RC2)
    concept(C1), concept(C2),
    replace(and(C1, C2), and(RC1, RC2)),

    % On génère une instance et on ajoute l'élément (I : C1 and C2) à la ABox
    genere(Inst),
    concat([((Inst,and(RC1,RC2)))], Abi, Abi1),

    % On affiche la ABox après modification
    nl, write(Abi1).

% ##########################################################################################

% PARTIE 3

% ##########################################################################################

% ##########################################################################################

% MAIN

% ##########################################################################################

exec :- premiere_etape(TBox, Abi, Abr),
        deuxieme_etape(Abi, Abi1, Tbox).