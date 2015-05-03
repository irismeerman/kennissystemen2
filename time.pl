% Kennissystemen opdracht 4
% Common Sense Reasoning

% Idee: 
% 1. Verzamel alle 'before' relaties (findall)
% 2. Zet alle gevonden events in een lijst (a before b. b before c wordt([a,b,c]))
% 3. Remove dubbele events in de lijst somehow
% 4. Verzamel alle 'concurrent' relaties (findall)
% 5. Op iedere plek waar een element staat dat voorkomt in een 'concurrent' predicaat,
% 	 plaats daar een lijst met de concurrent events.

:- op(700, xfx, before).
:- op(700, xfx, after).
:- op(700, xfx, concurrent).
:- op(700, xfx, before_or_concurrent).
:- op(700, xfx, concurrent_or_after).

:- dynamic(event/1).
:- dynamic(relation/1).

% point\0 is het start predicaat voor het invoegen van kennis aan de knowledgebase.
point:-
    read(UserInput),
    parse(UserInput), !,
    trans,
    prettyPrint.

point:-
	write('Dit staat al in de knowledgebase. Probeer opnieuw.\n').

% before\1 gaat alle before relaties in een lijst zetten in de goede
% volgorde. Daarvoor gebruikt hij rightsequence. Before gebruikt 1 argument,
% dat is dus het argument wat hij teruggeeft/print als je before(X) vraagt in 
% de terminal.
before(CompleteList):-
	findall([X,Y], relation(X before Y), Befores),
	%write(Befores), nl,
	rightSequence(Befores, CompleteList),
	write(CompleteList).

% rightsequence\2 checkt of de eerste before relatie X before Y dezelfde
% Y heeft als waar de volgende relatie mee begint. Indien dat het geval is,
% voegt hij de X en Y toe aan de Complete Lijst. Hij gaat vervolgens verder met
% de volgende relaties in de lijst.
rightSequence([Relation,Relation2 | [Tail]], CompleteList):-
	write("absadjfaf"), nl,
	write(Relation),
	Relation = [X,Y],
	Relation2 = [Y,_],
	append(X, CompleteList, CompleteList),
	append(Y, CompleteList, CompleteList),
	rightSequence([Relation2 | Tail], CompleteList).

% Als de volgende relatie niet met hetzelfde element begint als waar de
% vorige relatie mee eindigde (zoals in rightsequence hierboven), dan moet je de
% relatie overslaan (dus dit slaat transitief toegevoegde relaties over).
rightSequence([_, Relation2 | [Tail]], CompleteList):-
	rightSequence([Relation2 | [Tail]], CompleteList).

% Als de lijst leeg is hoeft rightSequence\2 niets te doen.
rightSequence([], _).

% parse\1 zet 'after'-input om naar 'before' input en voeg toe aan knowledgebase.
parse(relation(X after Y)):-
	checkDuplicates(relation(Y before X)), !, 
  	assert(relation(Y before X)).	

% parse\1 checkt of de input al in de knowledgebase staat.
parse(UserInput):-
  	checkDuplicates(UserInput), !, 
  	assert(UserInput).

% trans\0 past transiviteitsregel toe op before events
trans:-
	event(X), event(Y), event(Z),
	X \= Y, X \= Z, Y \= Z,
	relation((X before Y)),
	relation((Y before Z)),
	checkDuplicates(relation(X before Z)),
	assert(relation(X before Z)),
	write(relation(X before Z)),
	write(' is toegevoegd aan de database.\n') ,!, trans. 

% trans\0 past transiviteitsregel toe op concurrent events
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

% chechDuplicates\1 kijkt of de input al in de knowledgebase staat en of het een geldige
% input regel is.
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
	rewrite(X after Y).

%%%%%%%%%%%%%%%%%%%%% BUILD IN INFO %%%%%%%%%%%%%%%%%%%


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

