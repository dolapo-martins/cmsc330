/*---------------------------------------------------------------

   CMSC 330 Project 6 - Maze Solver and SAT in Prolog

   NAME: Dolapo Martins

*/


%%%%%%%%%%%%%%%%%%%%%%
% Part 1 - Recursion %
%%%%%%%%%%%%%%%%%%%%%%

% ackermann - M and N are the two arguments, and R is the result. Cf http://mathworld.wolfram.com/AckermannFunction.html for the definition of the ackermann function
%M, N > 0
ackermann(0, N, R) :-
	R is N+1.
ackermann(M, 0, R) :-
	M > 0,
	M1 is M-1,
	ackermann(M1, 1, R).
ackermann(M,N,R) :-
	M > 0,
	N > 0,
	M1 is M-1,
	N1 is N-1,
	ackermann(M, N1, R1),
	ackermann(M1, R1 , R).

% prod - R is product of entries in list L
prod([], 1).
prod([H|T],R) :-
	prod(T, S),
	R is H * S, !.

% fill - R is list of N copies of X
fill(0,_,[]). %doesn't matter what you give as X, N = 0 -> return []
fill(N,X,R) :-
	M is N-1,
	H = X,
	R = [H|T],
	fill(M, X, T), !.

% genN - R is value between 0 and N-1, in order
genN(N,R) :-
	N >= 0,
	genN(0, N, R).
genN(X, N, X). %returns next num, then next rule evaluated
genN(X, N, R) :-
	X < N - 1,
	X1 is X + 1,
	genN(X1, N, R).

% genXY - R is pair of values [X,Y] between 0 and N-1, in lexicographic order

genXY(N,R) :-
	genN(N, R1),
	X is R1,
	genN(N, R2),
	Y is R2,
	R = [X, Y].

% flat(L,R) - R is elements of L concatentated together, in order

flat([], []).
flat(L, R) :-
	L = [[H|T]| T1],
	flat(T1, R1),
	append([H|T], R1, R), !.

flat(L,R) :-
	L = [H|T],
	flat(T, R1),
	append([H], R1, R), !.


% is_prime(P) - P is an integer; predicate is true if P is prime.

is_prime(2).
is_prime(3).
is_prime(P) :-
	P > 3,
	D is floor(sqrt(P)),
	is_prime(P, D).
is_prime(N, N).
is_prime(N, X) :-
	N > X,
	0 =\= N mod X,
	X1 is X + 1,
	is_prime(N, X1), !.

% in_lang(L) - L is a list of atoms a and b; predicate is true L is in the language accepted by the following CFG:
/*
CFG
S -> T | V
T -> UU
U -> aUb | ab
V -> aVb | aWb
W -> bWa | ba
*/
s(X) :-
	t(X).
s(X) :-
	v(X).
t(X) :-
	append(A, B, X),
	u(A),
	u(B).
u([a,b]).
u([a|T]) :-
	append(F, [b], T),
	u(F).
v([a|T]) :-
	append(F, [b], T),
	v(F).
v([a|T]) :-
	append(F, [b], T),
	w(F).
w([b,a]).
w([b|T]) :-
	append(F, [a], T),
	w(F).
in_lang(L) :-
	s(L).

%%%%%%%%%%%%%%%%%%%%%%%%
% Part 2 - Maze Solver %
%%%%%%%%%%%%%%%%%%%%%%%%

% stats(U,D,L,R) - number of cells w/ openings up, down, left, right
stats(U,D,L,R) :-
	findall(Dir, cell(_, _, Dir, _), A),
	flat(A, M),
	count(M, 0,0,0,0, U, D, L, R), !.

count([[]|T], A, B, C, D, X, Y, Z, M) :-
	count(T, A, B, C, D, X, Y, Z, M).
count([], A, B, C, D, X, Y, Z, M) :-
	X is A,
	Y is B,
	Z is C,
	M is D.
count([u|T], X, Y, Z, M, A, B, C, D) :-
	X1 is X+1,
	count(T, X1, Y, Z, M, A, B, C, D).
count([d|T], X, Y, Z, M, A, B, C, D) :-
	Y1 is Y+1,
	count(T, X, Y1, Z, M, A, B, C, D).
count([l|T], X, Y, Z, M, A, B, C, D) :-
	Z1 is Z+1,
	count(T, X, Y, Z1, M, A, B, C, D).
count([r|T], X, Y, Z, M, A, B, C, D) :-
	M1 is M+1,
	count(T, X, Y, Z, M1, A, B, C, D).

% validPath(N,W) - W is weight of valid path N rounded to 4 decimal places
validPath(N,W) :-
	maze(Size,_,_,_,_),
	path(Name, Sx, Sy, DirList),
	validPath(N, Weight, Sx, Sy, Size, DirList, Name),
	round4(Weight, W).

validPath(N, 0, Sx, Sy, Size, [], Name) :-
	%weight at cell is 0 if you have no more directions to go
	N = Name.
validPath(N,W,X,Y, Size, [Dir|T], Name) :-
	%there is a cell at x y - with dirs and weights
	cell(X, Y, DirList, Wts),
	%that cell has the first dir of list in its list of dirs
	contains(Dir, DirList, Wts, Wt),
	%change based on that valid dir
	changeDir(X, Y, Dir, Nx, Ny, Size),
	%proceed with new position and rest of dirs list
	validPath(N, Wn, Nx, Ny, Size, T, Name),
	W is Wt+Wn.

contains(D, [D|T], [Wt|Wts], X) :-
	%this direction is in the list, get corresponding weight
	X is Wt.
contains(D, [Hd|Td], [Hw|Tw], X) :-
		contains(D, Td, Tw, X).

changeDir(X,Y,u, Nx, Ny, E) :-
	Y > 0,
	Nx is X,
	Ny is Y-1.
changeDir(X,Y,d, X, Ny, E) :-
	E1 is E-1,
	E1 > Y,
	Nx is X,
	Ny is Y+1.
changeDir(X,Y,l, Nx, Y, E) :-
	X > 0,
	Nx is X-1,
	Ny is Y.
changeDir(X,Y,r, Nx, Y, E) :-
	E1 is E-1,
	E1 > X,
	Nx is X+1,
	Ny is Y.

round4(X,Y) :- T1 is X*10000, T2 is round(T1), Y is T2/10000.

% findDistance(L) - L is list of coordinates of cells at distance D from start
findDistance(L) :-
	maze(Size,Sx, Sy, _, _),
	DL = [[0, [Sx,Sy]]],
	getNeighbors(DL, Size, [], [], G),!,
	sort(G, NewL),
	origin(NewL, [], Rest, First),
	group(Rest, First, N),
	sort(N, L). %start pos is at dist 0 from itself

origin([[D, [[X,Y]]]|T], [], R, O) :-
	%only the first thing will be at distance 0 -> remove it
	append([[D, [[X,Y]]]], [], O),
	R = T.

group([], L, L).
group([[D, [[X, Y]]]|T], [[D, [[A, B] | T1]] | T2], L) :-
	!,
	append([[X,Y]], [[A,B]], ALevel),
	append(ALevel, T1, ALevel1),
	sort(ALevel1, Cells),
	append([[D, Cells]], T2, Next),
	group(T, Next, L).
group([[D, [[X, Y]]]|T], Lst, L) :-
	append([[D, [[X, Y]]]], Lst, L1),
	group(T, L1, L).



getNeighbors([], Sz, VList, FList, FList) :- !.
%getNeighbors([[D, [[X, Y]|DT]|T], Sz, VList, FList1, FList) :-
getNeighbors([[D, [X, Y]]|T], Sz, VList, FList1, FList) :-
	X1 is X,
	Y1 is Y,
	cell(X1,Y1, Dirs,_), %get possible paths out via cell
	D1 is D+1, %each neighbor at +1 units
	append([[X,Y]], VList, NVList), %update visited
	search(D1, X, Y, Sz, Dirs, NVList, T, Lst), %get all neighbors
	append([[D, [[X,Y]]]], FList1, FList2),
	getNeighbors(Lst, Sz, NVList, FList2, FList). %bfs on all neighbors

search(D, X, Y, Sz, [], VList, L, L) :- !.
search(D, X, Y, Sz, [Dir| T], VList, L, L1) :-
	changeDir(X, Y, Dir, Nx, Ny, Sz), %!,
	member([Nx, Ny], VList), !,
	search(D, X, Y, Sz, T, VList, L, L1).
search(D, X, Y, Sz, [Dir|T], VList, L, L1) :-
		changeDir(X, Y, Dir, Nx, Ny, Sz), %!,
		append(L, [[D, [Nx, Ny]]], L2),
		search(D, X, Y, Sz, T, VList, L2, L1).

% solve - True if maze is solvable, fails otherwise.

solve :-
	maze(Size,_,_,Ex,Ey),
	findDistance(L),
	encountered(L, [], Coords),
	member([Ex, Ey], Coords).

encountered([], CList, CList) :- !.
encountered([[D, [H|T]]|T1], CList, CoordList) :-
		append([H], CList, NCList),
		process(T, NCList),
		encountered(T1, NCList, CoordList).

process([], CoordList).
process([H|T], CoordList) :-
	append([H], CoordList, NCoordList),
	process(T, NCoordList).


%%%%%%%%%%%%%%%%
% Part 3 - SAT %
%%%%%%%%%%%%%%%%



% eval(F,A,R) - R is t if formula F evaluated with list of
%                 true variables A is true, false otherwise

eval(F,A,R) :- fail.

% varsOf(F,R) - R is list of free variables in formula F

varsOf(F,R) :- fail.

% sat(F,R) - R is a list of true variables that satisfies F

sat(F,R) :- fail.

% Helper Function
% subset(L, R) - R is a subset of list L, with relative order preserved

subset([], []).
subset([H|T], [H|NewT]) :- subset(T, NewT).
subset([_|T], NewT) :- subset(T, NewT).
