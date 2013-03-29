enjoys(vincent,X) :- burger(X), \+ bigkahuna(X).

burger(X) :- bigmac(X).
burger(X) :- bigkahuna(X).
burger(X) :- whopper(X).

bigmac(a).
bigkahuna(b).
whopper(c).