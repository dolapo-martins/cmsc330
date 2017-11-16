female(alice).
male(bob).
male(charlie).
father(bob, charlie).
mother(alice, charlie).

son(X, Y) :- father(Y, X), male(X).
son(X, Y) :- mother(Y, X), male(X).

blond(X) :-
  father(Father, X),
  blond(Father),
  mother(Mother, X),
  blond(Mother).

bigger(horse, duck).
bigger(duck, gnat).

is_bigger(X,Y) :- bigger(X,Y).
is_bigger(X,Y) :- bigger(X,Z), is_bigger(Z,Y).

increment(X,Y) :-
  Y is X+1.

addN(X,N,Y) :-
  Y is X+N.

addN(X,0,X).
addN(X,N,Y) :-
  X1 is X+1,
  N1 is N-1,
  addN(X1,N1, Y).

factorial(0,1).
factorial(N,F) :-
  N > 0,
  N1 is N-1,
  factorial(N1, F1),
  F is N * F1.

%[1, 2, 3, 4]
doubler([], []). % Base case
doubler(L, X) :-
    L = [H|T],
    doubler(T, Y),
    Z is H * 2,
    X = [Z|Y].

even_ele([],[]). % Base case
even_ele(L, X) :-
  L = [H|T],
  even_ele(T, Y),
  1 is mod(H, 2),
  X = Y. % Odd case
even_ele(L, X) :-
  L = [H|T],
  even_ele(T, Y),
  0 is mod(H, 2),
  X = [H|Y].
