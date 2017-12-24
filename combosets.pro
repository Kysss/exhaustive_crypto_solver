perm(s(A,B),p(A,B)).
perm(s(A,B),p(B,A)).

perm(s(A,B,C),p(A,X,Y)) :- perm(s(B,C),p(X,Y)).
perm(s(A,B,C),p(B,X,Y)) :- perm(s(A,C),p(X,Y)).
perm(s(A,B,C),p(C,X,Y)) :- perm(s(A,B),p(X,Y)).

perm(s(A,B,C,D),p(A,X,Y,Z)) :- perm(s(B,C,D),p(X,Y,Z)).
perm(s(A,B,C,D),p(B,X,Y,Z)) :- perm(s(A,C,D),p(X,Y,Z)).
perm(s(A,B,C,D),p(C,X,Y,Z)) :- perm(s(A,B,D),p(X,Y,Z)).
perm(s(A,B,C,D),p(D,X,Y,Z)) :- perm(s(A,B,C),p(X,Y,Z)).

perm(s(A,B,C,D,E),p(A,X,Y,Z,W)) :- perm(s(B,C,D,E),p(X,Y,Z,W)).
perm(s(A,B,C,D,E),p(B,X,Y,Z,W)) :- perm(s(A,C,D,E),p(X,Y,Z,W)).
perm(s(A,B,C,D,E),p(C,X,Y,Z,W)) :- perm(s(B,A,D,E),p(X,Y,Z,W)).
perm(s(A,B,C,D,E),p(D,X,Y,Z,W)) :- perm(s(B,C,A,E),p(X,Y,Z,W)).
perm(s(A,B,C,D,E),p(E,X,Y,Z,W)) :- perm(s(B,C,D,A),p(X,Y,Z,W)).

permutations(A,B) :-
	perm(s(A,B),p(V,W)),
	write(V),write(W),write(' '),
	fail.

permutations(A,B,C) :-
	perm(s(A,B,C),p(V,W,X)),
	write(V),write(W),write(X),write(' '),
	fail.

permutations(A,B,C,D) :-
	perm(s(A,B,C,D),p(V,W,X,Y)),
	write(V),write(W),write(X),write(Y),write(' '),
	fail.

permutations(A,B,C,D,E) :-
	perm(s(A,B,C,D,E),p(V,W,X,Y,Z)),
	write(V),write(W),write(X),write(Y),write(Z),write(' '),
	fail.

combos(set(N1,N2,N3),combo(N1,N2),extras(N3)).
combos(set(N1,N2,N3),combo(N1,N3),extras(N2)).
combos(set(N1,N2,N3),combo(N2,N3),extras(N1)).

combos(set(N1,N2,N3,N4),combo(N1,N2),extras(N3,N4)).
combos(set(N1,N2,N3,N4),combo(N1,N3),extras(N2,N4)).
combos(set(N1,N2,N3,N4),combo(N1,N4),extras(N2,N3)).
combos(set(N1,N2,N3,N4),combo(N2,N3),extras(N1,N4)).
combos(set(N1,N2,N3,N4),combo(N2,N4),extras(N1,N3)).
combos(set(N1,N2,N3,N4),combo(N3,N4),extras(N1,N2)).

combos(set(N1,N2,N3,N4,N5),combo(N1,N2),extras(N3,N4,N5)).
combos(set(N1,N2,N3,N4,N5),combo(N1,N3),extras(N2,N4,N5)).
combos(set(N1,N2,N3,N4,N5),combo(N1,N4),extras(N2,N3,N5)).
combos(set(N1,N2,N3,N4,N5),combo(N1,N5),extras(N2,N3,N4)).
combos(set(N1,N2,N3,N4,N5),combo(N2,N3),extras(N1,N4,N5)).
combos(set(N1,N2,N3,N4,N5),combo(N2,N4),extras(N1,N3,N5)).
combos(set(N1,N2,N3,N4,N5),combo(N2,N5),extras(N1,N3,N4)).
combos(set(N1,N2,N3,N4,N5),combo(N3,N4),extras(N1,N2,N5)).
combos(set(N1,N2,N3,N4,N5),combo(N3,N5),extras(N1,N2,N4)).
combos(set(N1,N2,N3,N4,N5),combo(N4,N5),extras(N1,N2,N3)).