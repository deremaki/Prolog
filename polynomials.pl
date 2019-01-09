poly([]).
hasNoTheSamePower([],Power).
hasNoTheSamePower([(Factor,Power)|Tail],PowerCheck):- 
    % checking if polynomial have only one term with certain power
    Power\=PowerCheck.
poly([(Factor,Power)|Tail]) :-
    % checking if it is a valid polynomial
    hasNoTheSamePower(Tail,Power),poly(Tail).

add_terms((Factor1,Power), (Factor2,Power),(Factor3,Power)) :-
    % adding 
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
    %add single term to polynomial
    %example usage: add_term([(1,3), (2,2), (3,1)], (2,2), Add). Add = [(1,3), (4,2), (3,1)]
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
    %addition of polynomials
    %example usage: add_poly([(3, 3),(2, 2),(1, 1)], [(1,3), (1,2), (-3,1)], Add). Add =  [(4,3),(3,2),(-2,1)]
    poly(Poly1),
    poly(Poly2),
    Poly1 = [Term1|Tail1],
    add_term(Poly2,Term1, Poly2u),
    Polyn = Poly2u,
    add_poly(Tail1,Polyn,Poly),
    Poly3 = Poly.

reverse_factors([],[]).
reverse_factors(Poly1, ReversedPoly) :-
    %all factors are reversed
    %example usage: reversed_factors([(1,2), (2,1), (3,0)], Reversed). Reversed = [(-1,2), (-2,1), (-3,0)]
    Poly1 = [Term1|Tail1],
    ReversedPoly = [Term2|Tail2],
    Term1 = (Factor1, Power1),
    Term2 = (Factor2, Power2),
    Factor2 is -Factor1,
    Power2 is Power1,
    reverse_factors(Tail1,Tail2).

sub_poly(Poly1, Poly2, Sub) :-
    %substract by reversing Poly2 and add to Poly1
    %example usage: sub_poly([(3, 3),(2, 2),(1, 1)], [(1,3), (1,2), (-3,1)], Sub). Sub =  [(2,3), (1,2), (4,1)]
    reverse_factors(Poly2, Reversed),
    add_poly(Poly1, Reversed, Sub).

power_poly([], Term, []).
power_poly(Poly, Term, Poly2) :-
    %multiply Poly by one Term
    %example usage: power_poly([(3, 3),(2, 2),(1, 1)], (1, 2), Power). Power =  [(3, 5), (2, 4), (1, 3)]
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
    %multiply Poly1 by Poly2
    %example usage: power_poly([(3, 3),(2, 2),(1, 1)], [(1, 2)], Power). Power =  [(3, 5), (2, 4), (1, 3)]
    Poly2 = [Term2 | Tail2],
    (list_empty(Tail2) -> power_poly(Poly1, Term2, Mul);
    list_not_empty(Tail2) -> (power_poly(Poly1, Term2, PowerPoly), mul_poly(Poly1, Tail2, MulPoly), add_poly(PowerPoly, MulPoly, Mul))
    ).
    

diff_poly([],[]).
diff_poly(Poly, DiffPoly) :-
    %calculate differential of the Poly
    %example usage: diff_poly([(3,3),(3,2),(1,1)], Diff). Diff = [(9, 2),  (6, 1),  (1, 0)] 
    Poly = [Term1|Tail1],
    DiffPoly = [Term2|Tail2],
    Term1 = (Factor1, Power1),
    Term2 = (Factor2, Power2),
    Factor2 is Power1*Factor1,
    Power2 is Power1 - 1,
    diff_poly(Tail1,Tail2).

integral_poly([],[]).
integral_poly(Poly, IntPoly) :-
    %calculate integral of the Poly
    %example usage: integral_poly([(3,3),(3,2),(1,1)], Int). Int = [(0.75, 4),  (1, 3),  (0.5, 2)] 
    Poly = [Term1|Tail1],
    IntPoly = [Term2|Tail2],
    Term1 = (Factor1, Power1),
    Term2 = (Factor2, Power2),
    Factor2 is Factor1 / (Power1 +1),
    Power2 is Power1 + 1,
    integral_poly(Tail1,Tail2).

print_poly([]).
print_poly([Term|Tail]) :-
    %prints single term in human readible way
    %example usage: print_poly ([(-3 4)]) -> - 3x^4
    Term = (Factor, Power),
    abs(Factor, AbsFactor),
    (is_positive(Factor) -> write(" + "); is_negative(Factor) -> write(" - ")),
    write(AbsFactor), write("x^"), write(Power),
    print_poly(Tail).

%returns degree of the polynomial
%example usage: degree ([(3, 3),(2, 2),(1, 1)]) -> 3
degree([], Deg).
degree([(Factor, Power)], Deg) :-
    Deg = Power.
degree([(Factor, Power)|Tail], Deg) :-    
    degree(Tail, Deg),
    Deg >= Power.

%helper predicates
is_positive(X) :- X >= 0.
is_negative(X) :- X < 0.

list_empty([]).
list_not_empty([_|_]).
