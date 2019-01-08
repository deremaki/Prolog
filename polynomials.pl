poly([]).
hasNoTheSamePower([],Power).
hasNoTheSamePower([(Factor,Power)|Tail],PowerCheck):- Power\=PowerCheck.
poly([(Factor,Power)|Tail]):- hasNoTheSamePower(Tail,Power),poly(Tail).

add_terms((Factor1,Power), (Factor2,Power),(Factor3,Power)) :-
    Factor3 is Factor1+Factor2.

add_terms((Factor1,Power1), (Factor2,Power2), Poly) :-
    poly([(Factor1,Power1), (Factor2,Power2)]), 
    Poly = poly([(Factor1,Power1), (Factor2,Power2)]).

add_term(Poly1, Term, Poly2) :-
    poly(Poly1),
    Term = (Factor1, Power),
    Term2 = (Factor2, Power),
    Poly1 = [Term2|Tail],
    Factor3 is Factor1+Factor2,
    Poly2 = [(Factor3, Power)|Tail].

add_term([], Term, Poly2) :-
    Poly2 = [Term].
add_term(Poly1, Term, Poly2) :-
    writeln('add' + Poly1 + ' '+ Term),
    poly(Poly1),
    Term = (Factor1, Power1),
    Term2 = (Factor2, Power2),
    Poly1 = [Term2|Tail],
    Power1\=Power2,
    add_term(Tail,Term, Poly),
    Poly2= [Term2 | Poly].

add_poly([], Poly2, Poly3) :-
    poly(Poly2),
    Poly2 = Poly3.
add_poly(Poly2,[], Poly3) :-
    poly(Poly2),
    Poly2 = Poly3.
add_poly(Poly1,Poly2,Poly3):-
    poly(Poly1),
    poly(Poly2),
    Poly1 = [Term1|Tail1],
    add_term(Poly2,Term1, Poly2u),
    Polyn = Poly2u,
    add_poly(Tail1,Polyn,Poly),
    Poly3 = Poly.

diff_poly([],[]).
diff_poly(Poly, DiffPoly) :-
    Poly = [Term1|Tail1],
    DiffPoly = [Term2|Tail2],
    Term1 = (Factor1, Power1),
    Term2 = (Factor2, Power2),
    Factor2 is Power1*Factor1,
    Power2 is Power1 - 1,
    diff_poly(Tail1,Tail2).

integral_poly([],[]).
integral_poly(Poly, IntPoly) :-
    Poly = [Term1|Tail1],
    IntPoly = [Term2|Tail2],
    Term1 = (Factor1, Power1),
    Term2 = (Factor2, Power2),
    Factor2 is Factor1 / (Power1 +1) ,
    Power2 is Power1 + 1,
    integral_poly(Tail1,Tail2).