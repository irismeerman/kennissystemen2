% Kennissystemen opdracht 4
% Common Sense Reasoning

:- op(700, xfx, before).
:- op(700, xfx, after).
:- op(700, xfx, concurrent).
:- op(700, xfx, before_or_concurrent).
:- op(700, xfx, concurrent_or_after).

ask:-
    read(UserInput),
    assert(event(UserInput)).




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
