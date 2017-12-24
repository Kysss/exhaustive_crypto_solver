% The interpreter

:- consult("io.pro").
:- consult("gv1.pro").
:- consult("combosets.pro").
:- consult("lp.pro"). 

% DCG corresponding to the CFG provided
sentence --> simpleproblemcommand.
sentence --> simpleproblemquery.
sentence --> randomproblemcommand.
simpleproblemcommand --> [use],numberzzz,[to],[make],goal,[.].
simpleproblemcommand --> [write],goal,[in],[terms],[of],numberzzz,[.].
randomproblemcommand --> [use],[whatever],[to],[make],[whatever],[.].
simpleproblemquery --> [can],[you],[make],goal,separator,numberzzz,[?].
separator --> [from].
separator --> [with].
goal --> number.
numberzzz --> number,[and],number,[and],number,[and],number,[and],number.
numberzzz --> number,number,number,number,[and],number.
numberzzz --> [the],[first],[five],[positive],[numbers].
numberzzz --> [numbers],[zero],[through],[four].
numberzzz --> [numbers],[one],[through],[five].
numberzzz --> [numbers],[two],[through],[six].
numberzzz --> [numbers],[three],[through],[seven].
numberzzz --> [numbers],[four],[through],[eight].
numberzzz --> [numbers],[five],[through],[nine].
numberzzz --> [the],[odd],[numbers].
numberzzz --> [five],pluralnumber.
numberzzz --> [four],pluralnumber,[and],[one],number.
numberzzz --> [one],number,[and],[four],pluralnumber.
numberzzz --> [two],pluralnumber,[and],[three],pluralnumber.
numberzzz --> [three],pluralnumber,[and],[two],pluralnumber.
numberzzz --> [two],pluralnumber,[and],[two],pluralnumber,[and],[one],number.
numberzzz --> [one],number,[and],[two],pluralnumber,[and],[two],pluralnumber.
numberzzz --> [two],pluralnumber,[and],[one],number,[and],[two],pluralnumber.
number --> [zero].
number --> [one].
number --> [two].
number --> [three].
number --> [four].
number --> [five].
number --> [six].
number --> [seven].
number --> [eight].
number --> [nine].
pluralnumber --> [zeros].
pluralnumber --> [ones].
pluralnumber --> [twos].
pluralnumber --> [threes].
pluralnumber --> [fours].
pluralnumber --> [fives].
pluralnumber --> [sixes].
pluralnumber --> [sevens].
pluralnumber --> [eights].
pluralnumber --> [nines].


% recognizer
%----------------------------------------------------
recognizer :-
	read_sentence(S),
sentence(S,[]),
	write('ok'),nl,
	recognizer.
	
recognizer :-
	write('Not a sentence ... '), nl,
	recognizer.