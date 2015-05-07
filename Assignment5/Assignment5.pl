%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Assignment 5
% Kennissystemen UvA
% Iris Meerman (10330984)
% Arend van Dormalen (10615199)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



go :-
	calc_values(M1, M2, M3, A1, A2),
	write('Please enter actual outputs in a list: [value1, value2]'), nl,
	read([Val1, Val2]),
	check_values(Val1, Val2, A1, A2, [], List),
	write(List)
	.


% Predicting the correct behaviour
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
calc_values(M1, M2, M3, A1, A2) :-
	exp_value(m1, 3, 2, M1),
	exp_value(m2, 3, 2, M2),
	exp_value(m3, 3, 2, M3),
	exp_value(a1, M1, M2, A1),
	exp_value(a2, M2, M3, A2),
	write('Expected Output F: '),
	write(A1), nl,
	write('Expected Output G: '),
	write(A2), nl.

exp_value(X, Val1, Val2, M1) :-
	is_multiplier(X),
	M1 is Val1*Val2, !.

exp_value(X, Val1, Val2, M1) :-
	is_adder(X),
	M1 is Val1+Val2, !.

is_adder(X) :-
	adders(List),
	member(X, List), !.

is_multiplier(X) :-
	multipliers(List),
	member(X, List), !.

% Finding discrepancies
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
check_values(Val1, Val2, A1, A2, List, OutputList) :-
	((
	\+ Val1 = A1,
	append([Val1], List, OutputList1)
	);(
	\+ Val2 = A2,
	append([Val2], List, OutputList2)
	)),
	append(OutputList1, OutputList2, OutputList),
	write('No problems encountered.').

% Factbase
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
adders([a1,a2]).
multipliers([m1,m2,m3]).
