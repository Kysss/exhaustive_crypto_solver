:-consult('combosets.pro').
:-consult('gv2.pro').
%%:-consult('lp.pro').

%:----------------------------------------------
doubleton:-
	problem(numbers(N1,N2,N3,N4,N5),_),
	combos(set(N1,N2,N3,N4,N5),combo(A,B),_),
	A = B.
	
doubleton(doubleton(A,B),rest(C,D,E)):-
	problem(numbers(N1,N2,N3,N4,N5),_),
	combos(set(N1,N2,N3,N4,N5),combo(A,B),extras(C,D,E)),
	A=B.

pairQuotientGoal:-
	problem(numbers(N1,N2,N3,N4,N5),goal(G)),
	combos(set(N1,N2,N3,N4,N5),combo(A,B),G),
	not(B=0),
	A/B =:= G.
pairQuotientGoal(pairQuotientGoal(A,B),rest(C,D,E)):-
	problem(numbers(N1,N2,N3,N4,N5),goal(G)),
	combos(set(N1,N2,N3,N4,N5),combo(A,B),extras(C,D,E)),
	not(B=0),
	A/B =:= G.
pairQuotientGoal:-
	problem(numbers(N1,N2,N3,N4,N5),goal(G)),
	combos(set(N1,N2,N3,N4,N5),combo(A,B),G),
	not(B=0),
	not(G=0),
	A/B =:= 1/G.
pairQuotientGoal(pairQuotientGoal(A,B),rest(C,D,E)):-
	problem(numbers(N1,N2,N3,N4,N5),goal(G)),
	combos(set(N1,N2,N3,N4,N5),combo(A,B),extras(C,D,E)),
	not(B=0),
	not(G=0),
	A/B =:= 1/G.

pairSumGoal:-
	problem(numbers(N1,N2,N3,N4,N5),goal(G)),
	combos(set(N1,N2,N3,N4,N5),combo(A,B),G),
	A+B =:= G.
pairSumGoal(pairSumGoal(A,B),rest(C,D,E)):-
	problem(numbers(N1,N2,N3,N4,N5),goal(G)),
	combos(set(N1,N2,N3,N4,N5),combo(A,B),extras(C,D,E)),
	A+B =:= G.

pairDiffGoal:-
	problem(numbers(N1,N2,N3,N4,N5),goal(G)),
	combos(set(N1,N2,N3,N4,N5),combo(A,B),G),
	A-B =:= G.
pairDiffGoal(pairDiffGoal(A,B),rest(C,D,E)):-
	problem(numbers(N1,N2,N3,N4,N5),goal(G)),
	combos(set(N1,N2,N3,N4,N5),combo(A,B),extras(C,D,E)),
	A-B =:= G.
	
pairDiffGoal:-
	problem(numbers(N1,N2,N3,N4,N5),goal(G)),
	combos(set(N1,N2,N3,N4,N5),combo(A,B),G),
	A-B =:= -1*G.
pairDiffGoal(pairDiffGoal(A,B),rest(C,D,E)):-
	problem(numbers(N1,N2,N3,N4,N5),goal(G)),
	combos(set(N1,N2,N3,N4,N5),combo(A,B),extras(C,D,E)),
	A-B =:= -1*G.

pairProductGoal:-
	problem(numbers(N1,N2,N3,N4,N5),goal(G)),
	combos(set(N1,N2,N3,N4,N5),combo(A,B),G),
	A*B =:= G.
pairProductGoal(pairProductGoal(A,B),rest(C,D,E)):-
	problem(numbers(N1,N2,N3,N4,N5),goal(G)),
	combos(set(N1,N2,N3,N4,N5),combo(A,B),extras(C,D,E)),
	A*B =:= G.
	
other_numbers(special(N1),others(N2,N3,N4,N5)):-
	problem(numbers(N1,N2,N3,N4,N5),goal(_)).
other_numbers(special(N2),others(N1,N3,N4,N5)):-
	problem(numbers(N1,N2,N3,N4,N5),goal(_)).
other_numbers(special(N3),others(N1,N2,N4,N5)):-
	problem(numbers(N1,N2,N3,N4,N5),goal(_)).
other_numbers(special(N4),others(N1,N2,N3,N5)):-
	problem(numbers(N1,N2,N3,N4,N5),goal(_)).
other_numbers(special(N5),others(N1,N2,N3,N4)):-
	problem(numbers(N1,N2,N3,N4,N5),goal(_)).


%------------------------------------------------
%solve the problem heuristically -- assuming internalization

rule(1,situation1,action1).
rule(2,situation2,action2).
rule(3,situation3,action3).
rule(4,situation4,action4).
rule(5,situation5,action5).
rule(6,situation6,action6).
rule(7,situation7,action7).
rule(8,situation8,action8).


%------------------------------------------------
% code borrowed from the old crypto solver file

% Establish the problem parameters
establishCryptoProblemParameters :-
  declare(lo,0),
  declare(hi,9).

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

establishCryptoProblem(numbers(N1,N2,N3,N4,N5),goal(G)):-
	addCryptoProblemToKB(N1,N2,N3,N4,N5,G).
	
solve(numbers(N1,N2,N3,N4,N5),goal(G)):-
	establishCryptoProblem(numbers(N1,N2,N3,N4,N5),goal(G)),
	displayProblem,
	solveProblemHeuristically,
	displaySolution.
	


  
  
displayProblem :-
  problem(numbers(N1,N2,N3,N4,N5),goal(G)),
  write("Problem: NUMBERS = {"),
  write(N1),write(" "),write(N2),write(" "),write(N3),
  write(" "),write(N4),write(" "),write(N5),
  write("} GOAL = "),write(G),nl.
	
displaySolution :-
  write('Solution: '),
  solution(S),
  displayResult(S),
  eraseSolution,
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

  
eraseProblem:- retract(problem(_,_)).
eraseSolution:- retract(solution(_)).

%--------------------------------------------------------------
solveProblemHeuristically:-
	rule(Number,Situation,Action),
	write('considering rule '), write(Number),write(' ...'), nl,
	Situation,
	write('application of rule '), write(Number),write(' produces '),
	Action.
	
solveProblemHeuristically.

%-------------------------------------------------------
%heuristic1

situation1:-
	problem(Numbers,Goal),
	Goal = goal(0),
	Numbers=numbers(N1,N2,N3,N4,N5),
	member(0,[N1,N2,N3,N4,N5]).
	
action1:-
	problem(Numbers,_),
	Numbers=numbers(N1,N2,N3,N4,N5),
	assert(solution(ex(N1,*,ex(N2,*,ex(N3,*,ex(N4,*,N5)))))).
	
	
	
%-------------------------------------------------
%heuristic2

situation2:-
	problem(numbers(N1,N2,N3,N4,N5),goal(G)),
	member(G,[N1,N2,N3,N4,N5]),
	member(0,[N1,N2,N3,N4,N5]),
	not(G=0).
action2:-
	problem(_,goal(G)),
	other_numbers(special(G),others(A,B,C,D)),
	assert(solution(ex(G,+,ex(A,*,ex(B,*,ex(C,*,D)))))).
	
%------------------------------------------------
%heuristic3

situation3:-

	problem(_,goal(0)),
	doubleton.
	
action3:-
	problem(Numbers,_),
	doubleton(doubleton(A,B),rest(C,D,E)),
	assert(solution(ex(ex(A,-,B),*,ex(C,*,ex(D,*,E))))).
	
%------------------------------------------------
%heuristic4

situation4:-
	problem(numbers(N1,N2,N3,N4,N5),goal(G)),
	G=1,
	doubleton(doubleton(A,B),rest(C,D,E)),
	member(0,[C,D,E]).
	

action4:-
	problem(numbers(N1,N2,N3,N4,N5),goal(G)),
	doubleton(doubleton(A,B),rest(C,D,E)),
	assert(solution(ex(ex(A,/,B),+,ex(ex(C,*,D),*,E)))).
	
	

%------------------------------------------------
%heuristic5	

situation5:-
	problem(numbers(N1,N2,N3,N4,N5),goal(G)),
	pairQuotientGoal(pairQuotientGoal(A,B),rest(C,D,E)),
	not(B=0), 
	member(0,[C,D,E]).

action5:-
	problem(numbers(N1,N2,N3,N4,N5),goal(G)),
	pairQuotientGoal(pairQuotientGoal(A,B),rest(C,D,E)),
	A/B =:= G,
	assert(solution(ex(ex(A,/,B),+ ,ex(ex(C,*,D),*,E)))).

action5:-
	problem(numbers(N1,N2,N3,N4,N5),goal(G)),
	pairQuotientGoal(pairQuotientGoal(A,B),rest(C,D,E)),
	B/A =:= G,
	assert(solution(ex(ex(B,/,A),+ ,ex(ex(C,*,D),*,E)))).

%------------------------------------------------
%heuristic6

situation6:-
	problem(numbers(N1,N2,N3,N4,N5),goal(G)),
	pairSumGoal(pairSumGoal(A,B),rest(C,D,E)),
	member(0,[C,D,E]).
	
action6:-
	problem(numbers(N1,N2,N3,N4,N5),goal(G)),
	pairSumGoal(pairSumGoal(A,B),rest(C,D,E)),
	assert(solution(ex(ex(A,+,B),+ ,ex(ex(C,*,D),*,E)))).
	
%------------------------------------------------
%heuristic7

situation7:-
	problem(numbers(N1,N2,N3,N4,N5),goal(G)),
	pairDiffGoal(pairDiffGoal(A,B),rest(C,D,E)),
	member(0,[C,D,E]).
	
action7:-
	problem(numbers(N1,N2,N3,N4,N5),goal(G)),
	pairDiffGoal(pairDiffGoal(A,B),rest(C,D,E)),
	A-B =:= G,
	assert(solution(ex(ex(A,-,B),+ ,ex(ex(C,*,D),*,E)))).

action7:-
	problem(numbers(N1,N2,N3,N4,N5),goal(G)),
	pairDiffGoal(pairDiffGoal(A,B),rest(C,D,E)),
	A-B =:= -1*G,
	assert(solution(ex(ex(B,-,A),+ ,ex(ex(C,*,D),*,E)))).
	
%------------------------------------------------
%heuristic8

situation8:-
	problem(numbers(N1,N2,N3,N4,N5),goal(G)),
	pairProductGoal(pairProductGoal(A,B),rest(C,D,E)),
	member(0,[C,D,E]).
	
action8:-
	problem(numbers(N1,N2,N3,N4,N5),goal(G)),
	pairProductGoal(pairProductGoal(A,B),rest(C,D,E)),
	assert(solution(ex(ex(A,*,B),+ ,ex(ex(C,*,D),*,E)))).
%------------------------------------------------
%task 5 : demo

demo:-
	generateRandomCryptoProblem,
	displayProblem,
	solveProblemHeuristically,
	displaySolution.
	
demo(0).
demo(N):-
	demo,
	K is N-1,
	demo(K).
	
