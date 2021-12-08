%                                                            PROJET
%                                                         LRC - M1 DAC
%                                                  KRISNI Almehdi - JDAY Achraf
%                                           https://github.com/krisninho2000/Projet_LRC

% Partie TBox
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

% Partie ABox
% ABox d'instances
inst(michelAnge,personne).
inst(david,sculpture).
inst(sonnets,livre).
inst(vinci,personne).
inst(joconde,objet).

% ABox de relations
instR(michelAnge, david, aCree).
instR(michelAnge, sonnets, aEcrit).
instR(vinci, joconde, aCree).

% Partie 1
% Cette méthode permet de créer des listes allant contenir la TBox, la ABox d'instances et la ABox de rôles.
% Ces listes évolueront au fur et à mesure qu'on soumettra des propositions à la démonstration.
premiere_etape(Tbox, Abi, Abr) :-
    setof((X, Y), equiv(X, Y), Tbox),
    setof((X, Y), inst(X, Y), Abi),
    setof((X, Y, Z), instR(X, Y, Z), Abr).

% Partie 2

