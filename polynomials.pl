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
    %writeln('add' + Poly1 + ' '+ Term),
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

reverse_factors([],[]).
reverse_factors(Poly1, ReversedPoly) :-
    Poly1 = [Term1|Tail1],
    ReversedPoly = [Term2|Tail2],
    Term1 = (Factor1, Power1),
    Term2 = (Factor2, Power2),
    Factor2 is -Factor1,
    Power2 is Power1,
    reverse_factors(Tail1,Tail2).

sub_poly(Poly1, Poly2, Sub) :-
    reverse_factors(Poly2, Reversed),
    add_poly(Poly1, Reversed, Sub).

power_poly([], Term, []).
power_poly(Poly, Term, Poly2) :-
    Poly = [Term1 | Tail1],
    Poly2 = [Term2 | Tail2],
    Term1 = (Factor1, Power1),
    Term2 = (Factor2, Power2),
    Term = (Factor, Power),
    Factor2 is Factor1*Factor,
    Power2 is Power1+Power,
    power_poly(Tail1, Term, Tail2).

mul_poly([],[],[]).
mul_poly(Poly1, Poly2, Mul) :-
    Poly2 = [Term2 | Tail2],
    (list_empty(Tail2) -> power_poly(Poly1, Term2, Mul);
    list_not_empty(Tail2) -> (power_poly(Poly1, Term2, PowerPoly), mul_poly(Poly1, Tail2, MulPoly), add_poly(PowerPoly, MulPoly, Mul))
    ).
    

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
    Factor2 is Factor1 / (Power1 +1),
    Power2 is Power1 + 1,
    integral_poly(Tail1,Tail2).

print_poly([]).
print_poly([Term|Tail]) :-
    Term = (Factor, Power),
    abs(Factor, AbsFactor),
    (is_positive(Factor) -> write(" + "); is_negative(Factor) -> write(" - ")),
    write(AbsFactor), write("x^"), write(Power),
    print_poly(Tail).

degree([], Deg).
degree([(Factor, Power)], Deg) :-
    Deg = Factor.
degree([(Factor, Power)|Tail], Deg) :-    
    degree(Tail, Deg),
    Deg >= Factor.

is_positive(X) :- X >= 0.
is_negative(X) :- X < 0.

list_empty([]).
list_not_empty([_|_]).
