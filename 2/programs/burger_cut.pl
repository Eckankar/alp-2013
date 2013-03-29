enjoys(vincent,X) :- burger(X), nafbkh(vincent,X).

nafbkh(vincent,X) :- bigkahuna(X), !, fail.
nafbkh(vincent,X).

burger(X) :- bigmac(X).
burger(X) :- bigkahuna(X).
burger(X) :- whopper(X).

bigmac(a).
bigkahuna(b).
whopper(c).