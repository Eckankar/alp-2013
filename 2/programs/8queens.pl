solve(P) :-
     perm([1,2,3,4,5,6,7,8],P),
     test(P).

perm([],[]).
perm([A|As],Bs) :- perm(As,Bs1), insert(A,Bs1,Bs).

insert(A,Bs,[A|Bs]).
insert(A,[B|Bs],[B|Bs1]) :- insert(A,Bs,Bs1).

test([]).
test([A|As]) :- test1(A,A,As), test(As).

test1(B,C,[]).
test1(B,C,[D|Ds]) :-
   inc(B,B1), \+ B1=D, inc(C1,C), \+ C1=D, test1(B1,C1,Ds).

inc(0,0).
inc(0,1).
inc(1,2).
inc(2,3).
inc(3,4).
inc(4,5).
inc(5,6).
inc(6,7).
inc(7,8).
inc(8,9).
inc(9,9).