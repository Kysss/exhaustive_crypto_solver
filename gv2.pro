%%%% FILE: gv2.pro
%%%% TYPE: Prolog source
%%%% Line: very simple global variable ADT
%%%% Date: 2017/9/19

%% Essential functionality

declare(Var,Val) :-
	retract(binding(Var,_)),
	assert(binding(Var,val)).
declare(Var,Val) :-
	assert(binding(Var,Val)).
  
bind(Variable,Value) :-
	retract(binding(Variable, _)),
	assert(binding(Variable, Value)).

valueOf(Var) :-
	retract(binding(Var,_)).

	
valueOf(Variable,Value) :-
	binding(Variable, Value).
	
undeclare(Var) :-
	retract(binding(Var,_)).
	
%% Binding display functionality

displayBindings :-
	binding(Variable,Value),
	write(Variable),write(' -> '),write(Value),nl,
	fail.
displayBindings.


%%%%%%SAME AS DISPLAYBINDINGS ABOVE%%%%%%%%%%%%
bindings :-
	binding(Variable,Value),
	write(Variable),write(' -> '),write(Value),nl,
	fail.
bindings.



%%Arithmetic operator functionality

inc(Variable):-
	retract( binding(Variable,Value)),
	NewValue is Value + 1,
	assert(binding(Variable,NewValue)).
	
dec(Variable):-
	retract(binding(Variable,Value)),
	NewValue is Value - 1,
	assert(binding(Variable, NewValue)).
	
add(Variable, Number) :-
	retract(binding(Variable, Value)),
	NewValue is Value + Number,
	assert(binding(Variable,NewValue)).
	
add(Variable1,Variable2,Sum) :-
	binding(Variable1, Value1),
	binding(Variable2, Value2),
	NewValue is Value1 + Value2,
	declare(Sum,NewValue).
	
	
	
sub(Variable,Number) :-
	retract(binding(Variable,Value)),
	NewValue is Value - Number,
	assert(binding(Variable, NewValue)).
	
sub(Variable1, Variable2, Difference) :-
	binding(Variable1, Value1),
	binding(Variable2, Value2),
	NewValue is Value1 - Value2,
	declare(Difference,NewValue).
	
	
	
mul(Variable,Number) :-
	retract(binding(Variable,Value)),
	NewValue is Value * Number,
	assert(binding(Variable, NewValue)).
	
	
mul(Variable1, Variable2, Product) :-
	binding(Variable1, Value1),
	binding(Variable2, Value2),
	NewValue is Value1 * Value2,
	declare(Product, NewValue).
	


div(Variable,Number) :-
	retract(binding(Variable, Value)),
	NewValue is Value / Number,
	assert(binding(Variable, NewValue)).
	
div(Variable1, Variable2, Quotient) :-
	binding(Variable1, Value1),
	binding(Variable2, Value2),
	NewValue is Value1 / Value2,
	declare(Quotient, NewValue).
	
pow(Variable1, Variable2, Power) :-
	binding(Variable1, Value1),
	binding(Variable2, Value2),
	NewValue is Value1^Value2,
	declare(Power,NewValue).
	

