%                                                            PROJET
%                                                         LRC - M1 DAC
%                                                 KRISNI Almehdi - J'DAY Achraf
%                                          https://github.com/krisninho2000/Projet_LRC

% ------------------------------------------------------------------------------------------

% PARTIE PREPARATION

% ------------------------------------------------------------------------------------------

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

% Règles de remplacement (replace)

% ------------------------------------------------------------------------------------------

% Règles de concatenation d'éléments (concat)
concate([],L1,L1).
concate([X|Y],L1,[X|L2]) :- concat(Y,L1,L2).

% ##########################################################################################

% PARTIE 1

% Cette méthode permet de créer des listes allant contenir la TBox, la ABox d'instances et la ABox de rôles.
% Ces listes évolueront au fur et à mesure qu'on soumettra des propositions à la démonstration.
premiere_etape(Tbox, Abi, Abr) :-
    setof((X, Y), equiv(X, Y), Tbox),
    setof((X, Y), inst(X, Y), Abi),
    setof((X, Y, Z), instR(X, Y, Z), Abr).

% ##########################################################################################

% PARTIE 2

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
    nl,write('Cette réponse est incorrecte.'),nl,
    saisie_et_traitement_prop_a_demontrer(Abi,Abi1,Tbox).

% ------------------------------------------------------------------------------------------
% On met en place les méthodes nécessaires

% Méthode permettant l'acquisition de propositions du type 1 (I : C)
acquisition_prop_type1(Abi,Abi1,Tbox) :-
    % On entre le type de l'instance
    nl, write('Veuillez entrer l"instance :'),
    nl, read(I),

    % On entre le concept
    nl, write('Veuillez entrer le concept :'),
    nl, read(C),

    % On effectue les manipulations sur le concept lu
    % On crée un concept C (C), on le remplace (RC) puis on effectue sa négation (NRC)
    concept(C), replace(C, RC), nnf(not(RC),NRC),

    % On ajoute l'élément (I, NRC) à la ABox
    concat([((I,NRC))],Abi,Abi1),

    % On affiche la ABox après modification
    nl, write(Abi1).

% ##########################################################################################