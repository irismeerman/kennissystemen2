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
    write(UserInput).

trans:-
	event(X), event(Y), event(Z),
	X \= Y, X \= Z, Y \= Z,
	relation((X before Y)),
	relation((Y before Z)),
	assert(relation(X before Z)).

checkDuplicates(Input):-
	\+ Input.

makeList:-
	findall(X, relation(X), Lijst),
	prettyPrint(Lijst).

prettyPrint([ Rel1 | Relaties] ):-
	rewrite(Rel1),
	prettyPrint(Relaties).
	
rewrite(X before Y):-
	write(X),
	write(' - '),
	write(Y).

rewrite(X after Y):-
	write(Y),
	write(' - '),
	write(X).
	
rewrite(X concurrent Y):-
	write(X),
	write(', '),
	write(Y).

go1:-
	assert(event('a')),
	assert(event('b')),
	assert(event('c')),
	assert(relation(a before b)),
	assert(relation(b before c)).

go2:-
	assert(event('a')),
	assert(event('b')),
	assert(event('c')),
	assert(relation(b after a)),
	assert(relation(b concurrent c)).



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
