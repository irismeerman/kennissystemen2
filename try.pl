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
    checkDuplicates(UserInput), !, 
    assert(UserInput),
    trans,
    prettyPrint.

ask:-
	write('Dit staat al in de knowledgebase. Probeer opnieuw.\n').

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
	write('Geen transitieve relaties toegevoegd.\n'), !.

checkDuplicates(Input):-
	\+ Input.

prettyPrint:-
	findall(X, relation(X), Lijst),
	prettyPrint2(Lijst).

prettyPrint2([ Rel1 | Relaties] ):-
	rewrite(Rel1),
	prettyPrint2(Relaties).
	
rewrite(X before Y):-
	write(X),
	write(' - '),
	write(Y).

rewrite(X after Y):-
	retract(X after Y),
	assert(relation(X before Y)).

rewrite(X concurrent Y):-
	write(X),
	write(', '),
	write(Y).

rewrite(X before_or_concurrent Y):-
	rewrite(X before Y), nl,
	rewrite(X concurrent Y).

rewrite(X concurrent_or_after Y):-
	rewrite(X concurrent Y), nl,
	rewrite(X after Y).

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
	assert(relation(b after a)),
	assert(relation(b concurrent c)),
	trans,
	prettyPrint.

