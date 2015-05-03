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

% ask\0 is het start predicaat voor het invoegen van kennis aan de knowledgebase.
ask:-
    read(UserInput),
    parse(UserInput), !,
    trans,
    prettyPrint.

ask:-
	write('Dit staat al in de knowledgebase. Probeer opnieuw.\n').

% Before/0 zoekt alle before-relaties en gaat alle elementen in één lijst zetten.
% Er geldt wel de voorwaarde dat de relaties in de goede volgorde moeten worden ingegeven.
before:-
	findall([X,Y], relation(X before Y), Befores),
	Befores = [[Elem1, Elem2] | T],
	goodList(Befores, [Elem1, Elem2]).

goodList([X, []], _).

% Hier controleert hij of de voorgaande relatie eindigt met hetzelfde waar de volgende
% relatie mee begint en voegt het volgende element dan toe aan de lijst.
goodList(Befores, CompleteList):-
	Befores = [[D,X], [X,Y] | T],
	append(CompleteList, [Y], NewComplete),
	write(NewComplete),
	goodList([[X,Y] | T], NewComplete).

% Als de nieuwe relatie in inhaakt op de voorgaande relatie, dan sla je hem over (op deze
% manier haal je dus de transitieve relaties er uit)
goodList(Befores, CompleteList):-
	Befores = [[O, P], [X, Y] | T],
	goodList([ [X,Y] | T], CompleteList).


% parse\1 zet 'after'-input om naar 'before' input en voeg toe aan knowledgebase.
parse(relation(X after Y)):-
	checkDuplicates(relation(Y before X)), !, 
  	assert(relation(Y before X)).	

% parse\1 checkt of de input al in de knowledgebase staat.
parse(UserInput):-
  	checkDuplicates(UserInput), !, 
  	assert(UserInput).


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
