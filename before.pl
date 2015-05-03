% Kennissystemen opdracht 4
% Common Sense Reasoning
%
% Iris Meerman
% Arend van Dormalen


% Aanmaken van verschillende operatoren voor types relaties
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
    timePrint.

% point\0 faalt als een event of relaties al in de database staat, dan wordt deze
% case aangeroepen.
point:-
	write('Dit staat al in de knowledgebase. Probeer opnieuw.\n').

% Before\0 zoekt alle before-relaties en gaat alle elementen in één lijst zetten.
% Er geldt wel de voorwaarde dat de relaties in de goede volgorde moeten worden ingegeven.
before:-
	findall([X,Y], relation(X before Y), Befores),
	Befores = [[Elem1, Elem2] | _],
	goodList(Befores, [Elem1, Elem2]).

% Basecase goodList\2
goodList([_, []], _).

% Hier controleert hij of de voorgaande relatie eindigt met hetzelfde waar de volgende
% relatie mee begint en voegt het volgende element dan toe aan de lijst.
goodList(Befores, CompleteList):-
	Befores = [[_,X], [X,Y] | T],
	append(CompleteList, [Y], NewComplete),
	write(NewComplete),
	goodList([[X,Y] | T], NewComplete).

% Als de nieuwe relatie in inhaakt op de voorgaande relatie, dan sla je hem over (op deze
% manier haal je dus de transitieve relaties er uit)
goodList(Befores, CompleteList):-
	Befores = [_, [X, Y] | T],
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


% timePrint\0 vind alle relaties tussen events en stopt deze in een lijst
timePrint:-
	findall(X, relation(X), Lijst),
	timePrint2(Lijst).

% timePrint2\0 gaat recursief door de lijst van relaties heen en print de inhoud
timePrint2([ Rel1 | Relaties] ):-
	rewrite(Rel1),
	timePrint2(Relaties).


% rewrite\1 schrijft relaties om naar een grafische weergave, die deel van een tijdlijn uit kan maken

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

% First small knowledgebase
go1:-
	assert(event('a')),
	assert(event('b')),
	assert(event('c')),
	assert(relation(a before b)),
	assert(relation(b before c)),
	trans,
	timePrint.

% Second small knowledgebase
go2:-
	assert(event('d')),
	assert(event('e')),
	assert(event('f')),
	assert(relation(e after d)),
	assert(relation(e concurrent f)),
	trans,
	timePrint.


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

% Wanneer beide gevallen niet waar zijn, is er een melding dat er geen 
% relaties over zijn. Deze case is altijd waar.
trans:-
	write('Geen transitieve relaties meer om toe te voegen.\n'), !.
