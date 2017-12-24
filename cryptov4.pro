
%%=============================================================================================%%
%%                                                                                             %%
%% File: cryptov4.pro                                                                          %%
%% Author: YingyingXia                                                                         %%
%% Description: Crypto problem generation and solution - exhaustive search.                    %%
%%                                                                                             %%
%%=============================================================================================%%

% Big Picture, a Crypto problems of the form:
%
%   problem(numbers(N1,N2,N3,N4,N5),goal(G))
%
% is added to the KB, and an exhaustive problem solver will grab the problems
% from the KB and add solutions of the form:
% 
%   solution(Solution)
%
% to the KB. Auxilliary programs serve to nicely print out problems and solutions.
% (The KR for a Solution is defined recursively in terms of expressions as:
%   ex(Operand,Operator,Operand)
% where an operand can be a number or an expression and an operator is one of the 
% four basic arithmetic operators)

%-----------------------------------------------------------------------------------------------------------------------------------
%-----------------------------------------------------------------------------------------------------------------------------------

% load files that operations are dependant on

:- consult('gv2.pro').
:- consult('combosets.pro').

%-----------------------------------------------------------------------------------------------------------------------------------
%-----------------------------------------------------------------------------------------------------------------------------------

%===================================== CryptoV1===================================================================

% Establish the problem parameters
establishCryptoProblemParameters :-
  declare(lo,0),
  declare(hi,15).

:- establishCryptoProblemParameters.

% Generate a random number within the desired range
generateRandomCryptoNumber(N) :-
  valueOf(lo,Lo),
  valueOf(hi,Hi),
  Upper is Hi + 1,
  random(Lo,Upper,N).

% Generate one crypto problem and assert it to the knowledge base
generateRandomCryptoProblem :-
  generateRandomCryptoNumber(N1),
  generateRandomCryptoNumber(N2),
  generateRandomCryptoNumber(N3),
  generateRandomCryptoNumber(N4),
  generateRandomCryptoNumber(N5),
  generateRandomCryptoNumber(G),
  addCryptoProblemToKB(N1,N2,N3,N4,N5,G).

addCryptoProblemToKB(N1,N2,N3,N4,N5,G) :-
  retract(problem(_,_)),
  assert(problem(numbers(N1,N2,N3,N4,N5),goal(G))).
addCryptoProblemToKB(N1,N2,N3,N4,N5,G) :-
  assert(problem(numbers(N1,N2,N3,N4,N5),goal(G))).

% Display the problem in the form NUMBERS = {N1, N2, N3, N4, N5} GOAL = G
displayProblem :-
  problem(numbers(N1,N2,N3,N4,N5),goal(G)),
  write("NUMBERS = {"),
  write(N1),write(" "),write(N2),write(" "),write(N3),
  write(" "),write(N4),write(" "),write(N5),
  write("} GOAL = "),write(G),nl.

% genone :: generate and display one crypto problem
genone :-
  generateRandomCryptoProblem,
  displayProblem.

% generate(N) :: Generate N problems and display them
generate(1) :-
  genone.
generate(N) :-
  genone,
  Next is N - 1,
  generate(Next).

%-----------------------------------------------------------------------------------------------------------------------------------
%-----------------------------------------------------------------------------------------------------------------------------------

%========================================== CryptoV2 and V3===========================================================

%Substitute Method
substitute(New, Old, ex(Old, O, Z), ex(New, O, Z)).
substitute(New, Old, ex(X, O, Old), ex(X, O, New)).
substitute(New, Old, ex(X, O, Z), ex(Q, O, Z)) :-
  substitute(New, Old, X, Q).
substitute(New, Old, ex(X,O,Z), ex(X,O,Q)) :-
  substitute(New, Old, Z, Q).

%Order 2 Solver
crypto(N1,N2,Goal,ex(N1,+,N2)) :- Goal is (N1 + N2).
crypto(N1,N2,Goal,ex(N1,-,N2)) :- Goal is (N1 - N2).
crypto(N1,N2,Goal,ex(N2,-,N1)) :- Goal is (N2 - N1).
crypto(N1,N2,Goal,ex(N1,*,N2)) :- Goal is (N1 * N2).
crypto(N1,N2,Goal,ex(N1,/,N2)) :- N2 > 0, Goal is (N1 / N2).
crypto(N1,N2,Goal,ex(N2,/,N1)) :- N1 > 0, Goal is (N2 / N1).

%Order 3 Solver
crypto(N1,N2,N3,G,Expr):-
  combos( set(N1,N2,N3), combo(A,B), extras(C)),
  crypto(A, B, SG, SGE),
  crypto(C, SG, G, UGE),
  substitute(SGE, SG, UGE, Expr).

%Order 4 Solver
crypto(N1,N2,N3,N4,G,Expr) :-
  combos( set(N1,N2,N3,N4), combo(A,B), extras(C,D)),
  crypto(A, B, SG, SGE),
  crypto(C, D, SG, G, UGE),
  substitute(SGE, SG, UGE, Expr).

%Order 5 Solver
crypto(N1,N2,N3,N4,N5,G,Expr) :-
  combos( set(N1,N2,N3,N4,N5), combo(A,B), extras(C,D,E)),
  crypto(A, B, SG, SGE),
  crypto(C, D, E, SG, G, UGE),
  substitute(SGE, SG, UGE, Expr).

%-----------------------------------------------------------------------------------------------------------------------------------
% Establish/Internalize a specific crypto problem

establishSpecificCryptoProblem(N1,N2,N3,N4,N5,G) :-
  addCryptoProblemToKB(N1,N2,N3,N4,N5,G).

%-----------------------------------------------------------------------------------------------------------------------------------
% Solve an internalized problem - random or otherwise
% - decompositionally, placing its solution into the KB.

solveProblemDecompositionally :-
  getProblemFromKnowledgeBase(N1,N2,N3,N4,N5,G),
  crypto(N1,N2,N3,N4,N5,G,Expression),
  addCryptoSolutionToKB(Expression).
solveProblemDecompositionally :-
  write('No solution to this one!'), nl.

getProblemFromKnowledgeBase(N1,N2,N3,N4,N5,G) :-
  problem(numbers(N1,N2,N3,N4,N5),goal(G)).

addCryptoSolutionToKB(Expression) :-
  retract(solution(_)),
  assert(solution(Expression)).
addCryptoSolutionToKB(Expression) :-
  assert(solution(Expression)).

%-----------------------------------------------------------------------------------------------------------------------------------
% Display the solution, assuming a problem has been solved.

displaySolution :-
  write('Solution: '),
  solution(S),
  displayResult(S),
  nl.
displaySolution.

displayResult(ex(A,O,B)) :-
  number(A),number(B),
  write(' ( '),write(A),write(' '),write(O),write(' '),write(B),write(' )').
displayResult(ex(A,O,B)) :-
  number(A),B = ex(A1,O1,B1),
  write('( '),write(A),write(' '),write(O),write(' '),
  displayResult(ex(A1,O1,B1)),write(' )').
displayResult(ex(A,O,B)) :-
  number(B),A = ex(A1,O1,B1),
  write('( '),displayResult(ex(A1,O1,B1)),write(' '),
  write(O),write(' '),write(B),write(' )').
displayResult(ex(A,O,B)) :-
  A = ex(A1,O1,B1), B = ex(A2,O2,B2),
  write('( '),displayResult(ex(A1,O1,B1)),write(' '),write(O),
  write(' '),displayResult(ex(A2,O2,B2)),write(' )').

%--------------------------------------------------------------------------------------------------------------------
% Crypto problem sover -- Solves a random problem

solve(random) :-
  generateRandomCryptoProblem,
  displayProblem,
  solveProblemDecompositionally,
  displaySolution.

%--------------------------------------------------------------------------------------------------------------------
% Crypto problem solver -- Solves a specific problem

solve(numbers(N1,N2,N3,N4,N5),goal(G)) :-
  establishSpecificCryptoProblem(N1,N2,N3,N4,N5,G),
  displayProblem,
  solveProblemDecompositionally,
  displaySolution.

%--------------------------------------------------------------------------------------------------------------------
% Program to generate and solve random crypto problems

demo(0).
demo(N) :-
  solve(random),
  K is N - 1,
  demo(K).