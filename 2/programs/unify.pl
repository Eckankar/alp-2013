p(X,Y) :- a(X), b(Y), X = Y.

a(1).
b(1).

a(2).

b(3).

a([X|XS]).
b([1,2|XS]).