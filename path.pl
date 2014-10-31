% This is a Prolog file


path_one(A,A,[[A],0]) :- true.
path_one(A,B,[ [A|Q] ,Cost]) :-
	arc(A,X,D),
	path_one(X, B, [Q,P]),
	Cost is P + D 
.
path(A,B, [MinPath, MinCost]) :-
	% setof will get the sorted list of all distinct Costs. 
	% Since CostList is sorted, the first one is minimum cost.
	setof(Cost, AnyExistingPath^path_one(A, B, [AnyExistingPath, Cost]), [MinCost|_]),

	% find all [path,cost] pairs
	setof(PathCost, path_one(A, B, PathCost), PathCostList),

	% Find one [MinPath, MinCost] pair
	% This is able to find all such pairs, press ";" to get more paths
	member([MinPath, MinCost], PathCostList)
.
init(yes):-
	arc(a,d,V),
	NewV is V+1,
	write(V),
	retract(arc(a,d,V)),
	assert( arc(a,d,NewV))
.
after_init(X):-
	path(a,m,X).

