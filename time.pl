% Kennissystemen opdracht 4
% Common Sense Reasoning

:- op(700, xfx, before).
:- op(700, xfx, after).
:- op(700, xfx, concurrent).
:- op(700, xfx, before_or_concurrent).
:- op(700, xfx, concurrent_or_after).

:- dynamic(event/1).
:- dynamic(relation/1).

ask:-
    read(UserInput),
    parse(UserInput), !,
    trans,
    prettyPrint.

ask:-
	write('Dit staat al in de knowledgebase. Probeer opnieuw.\n').

parse(relation(X after Y)):-
	checkDuplicates(relation(Y before X)), !, 
  	assert(relation(Y before X)).	

parse(UserInput):-
  	checkDuplicates(UserInput), !, 
  	assert(UserInput).

trans:-
	event(X), event(Y), event(Z),
	X \= Y, X \= Z, Y \= Z,
	relation((X before Y)),
	relation((Y before Z)),
	checkDuplicates(relation(X before Z)),
	assert(relation(X before Z)),
	write(relation(X before Z)),
	write(' is toegevoegd aan de database.\n') ,!, trans. 

trans:-
	event(X), event(Y), event(Z),
	X \= Y, X \= Z, Y \= Z,
	relation((X concurrent Y)),
	relation((Y concurrent Z)),
	checkDuplicates(relation(X concurrent Z)), !, 
	assert(relation(X concurrent Z)),
	write(relation(X concurrent Z)),
	write(' is toegevoegd aan de database.\n') ,!, trans. 

trans:-
	write('Geen transitieve relaties meer om toe te voegen.\n'), !.


% checkDuplicates/1 takes an event of relation and checks whether it should be added to our database. 
% For all cases, data should not be added when it already occurs in the database.
% For concurrent relations, the opposite should not yet exist in the database as well.
checkDuplicates(Input):-
	\+ Input,
	((Input = relation(X concurrent Y),
		\+ relation(Y concurrent X))
	;  
	Input = relation(X after Y)
	;
	Input = relation(X before Y)
	;
	Input = relation(X before_or_concurrent Y)
	;
	Input = relation(X concurrent_or_after Y)
	;
	Input = event(X)).




%%%%%%%%%%%%%%%%%%%%%%% PRINT FUNCTIES %%%%%%%%%%%%%%%%%%%%%%%%%

prettyPrint:-
	findall(X, relation(X), Lijst),
	prettyPrint2(Lijst).

prettyPrint2([ Rel1 | Relaties] ):-
	rewrite(Rel1),
	prettyPrint2(Relaties).


% Rewrite/1 rewrites several relation types in a way that can be implemented in the timeline.
rewrite(X before Y):-
	write(X),
	write(' - '),
	write(Y), nl.
	
rewrite(X concurrent Y):-
	write(X),
	write(','),
	write(Y), nl.

rewrite(X before_or_concurrent Y):-
	rewrite(X before Y),
	rewrite(X concurrent Y).

rewrite(X concurrent_or_after Y):-
	rewrite(X concurrent Y), 
	rewrite(Y before X).

go1:-
	assert(event('a')),
	assert(event('b')),
	assert(event('c')),
	assert(relation(a before b)),
	assert(relation(b before c)),
	trans,
	prettyPrint.

go2:-
	assert(event('a')),
	assert(event('b')),
	assert(event('c')),
	assert(relation(a before b)),
	assert(relation(b concurrent c)),
	trans,
	prettyPrint.

