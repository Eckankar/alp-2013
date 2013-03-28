list([]).
list([A|As]) :- list(As).

append([],Bs,Bs) :- list(Bs).
append([A|As],Bs,[A|Cs]) :- append(As,Bs,Cs).