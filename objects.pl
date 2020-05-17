:- module(objects, [goal/3,
					goal/4, 
					strategy/3, 
					strategy/4,
					body/4, 
					argument/3,
					explanation/4,
					subtree/3,
					subtree/4]).


argument(Number, rl, [Number, rl]).
argument(Number, mt, [Number, mt]).
argument(Number, ph, [Number, ph]).

explanation(argument(N1, B1), 
			argument(N2, B2), 
			argument(N3, B3), [A, B, C]) :-
	argument(N1, B1, A), 
	argument(N2, B2, B),
	argument(N3, B3, C).


% physic body (masse, k-Factor, rotation speed)
%body(Mass, KFac, V).
% body/2
body(body(Mass, KFac, V), [Mass, KFac, V]).
% body/4
body(Level, Mass, V, body(Mass, KFac, V)) .
	% prop('kFactor', JSFkt),
	% apply(JSFkt, [Level], KFac).


% all GSN Elements: gsnElement(ID, body, Explanation, asList)
%goal(1, body(1000, KFac, V), Explanation).
%strategy(1, body(1000, KFac, V), Explanation).
%solution(1, body(1000, KFac, V), Explanation).

goal(exp, E, goal(ID,  _), goal(ID, E)).
goal(exp, goal(_,  E), E).
goal(id, goal(ID,  _), ID).



strategy(exp, E, strategy(ID,  _), strategy(ID,  E)).
strategy(exp, strategy(_,  E), E).
strategy(id, strategy(ID,  _), ID).


subtree(id, subtree(goal(A, _), _, _,  _), A).
subtree(goal, subtree(A, _, _,   _), A).
subtree(str, subtree(_, A, _,   _), A).
subtree(childs, subtree(_, _, A, _), A).
subtree(childs, Childs, subtree(G, S, _,  P), subtree(G, S, Childs,  P)).
