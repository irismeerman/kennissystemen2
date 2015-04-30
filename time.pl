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
    parse(UserInput),
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

operators([after, before, concurrent]).

is_operator(X):-
	operators(List),
	member(X, List), !.

trans:-
	%is_operator(Op),
	%write(Op),
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
	write(Y), nl.

rewrite(X after Y):-
	write(Y),
	write(' - '),
	write(X), nl.
	
rewrite(X concurrent Y):-
	write(X),
	write(', '),
	write(Y), nl.

rewrite(X before_or_concurrent Y):-
	rewrite(X before Y),
	rewrite(X concurrent Y).

rewrite(X concurrent_or_after Y):-
	rewrite(X concurrent Y), 
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



/* --- A simple forward chaining rule interpreter --- */

%% forward:-
%%     new_derived_fact( P ),
%%     !,
%%     write( 'Derived:' ), write_ln( P ),
%%     assert( fact( P )),
%%     forward
%%     ;
%%     nl.

%% new_derived_fact( Conclusion ):-
%%     if Condition then Conclusion,
%%     not( fact( Conclusion ) ),
%%     composed_fact( Condition ).

%% composed_fact( Condition ):-
%%     fact( Condition ).

%% composed_fact( Condition1 and Condition2 ):-
%%     composed_fact( Condition1 ),
%%     composed_fact( Condition2 ).

%% composed_fact( Condition1 or Condition2 ):-
%%     composed_fact( Condition1 )
%%     ;
%%     composed_fact( Condition2 ).
