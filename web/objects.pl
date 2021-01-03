%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% objects %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% physic body (masse, k-Factor, rotation speed)
%body(Mass, KFac, V).
% body/2
body(body(Mass, KFac, V), [Mass, KFac, V]).
% body/4
body(Level, Mass, V, body(Mass, KFac, V)) :-
	prop('kFactor', JSFkt),
	apply(JSFkt, [Level], KFac).

% all GSN Elements: gsnElement(ID, body, Explanation, asList)
%goal(1, body(1000, KFac, V), Explanation).
%strategy(1, body(1000, KFac, V), Explanation).
%solution(1, body(1000, KFac, V), Explanation).
goal(body, goal(_, Body, _), Body).
goal(body, Body, goal(ID, _, E), goal(ID, Body, E)).
goal(id, goal(ID, _, _), ID).
goal(mass, goal(_, body(M, _, _), _), M).
goal(explanation, goal(_, _, E), E).

goal(level, Goal, Level, Goal2) :-
	goal(body, Goal, body(Mass, _, V)),
	body(Level, Mass, V, Body),
	goal(body, Body, Goal, Goal2).

solution(body, solution(_, Body, _), Body).
solution(body, Body, solution(ID, _, E), solution(ID, Body, E)).
solution(id, solution(ID, _, _), ID).
solution(mass, solution(_, body(M, _, _), _), M).

solution(level, Goal, Level, Goal2) :-
	solution(body, Goal, body(Mass, _, V)),
	body(Level, Mass, V, Body),
	solution(body, Body, Goal, Goal2).



element(mass, Element, M) :-
	arg(2, Element, body(M, _, _)).

element(mass, M, solution(ID, body(_, K, _), E), solution(ID, body(M, K, 0), E)) :- !.
element(mass, M, strategy(ID, body(_, K, _), E), strategy(ID, body(M, K, 0), E)) :- !.
element(mass, M, goal(ID, body(_, K, _), E), goal(ID, body(M, K, 0), E)) :- !.

element(Element, ID, Body, E) :-
	arg(1, Element, ID),
	arg(2, Element, Body),
	arg(3, Element, E).

strategy(body, strategy(_, Body, _), Body).
strategy(body, Body, strategy(ID, _, E), strategy(ID, Body, E)).
strategy(id, strategy(ID, _, _), ID).
strategy(mass, strategy(_, body(M, _, _), _), M).

strategy(explanation, strategy(_, _, Exp), Exp).

strategy(level, Strategy, Level, Strategy2) :-
	strategy(body, Strategy, body(Mass, _, V)),
	body(Level, Mass, V, Body),
	strategy(body, Body, Strategy, Strategy2).

subtree(id, subtree(goal(A, _, _), _, _, _, _, _), A).
subtree(goal, subtree(A, _, _,  _, _, _), A).
subtree(str, subtree(_, A, _,  _, _, _), A).
subtree(childs, subtree(_, _, A, _, _, _), A).
subtree(childs, Childs, subtree(G, S, _, M, P, I), subtree(G, S, Childs, M, P, I)).
subtree(mass, subtree(_, _, _ , A, _, _), A).
subtree(mass, M, subtree(G, S, C, _, P, I), subtree(G, S, C, M, P, I)).
subtree(embodyData, subtree(goal(A, _, _), _, _, M, _, _), A, M).
subtree(iv, subtree(goal(_, _, _), _, _, _, _, I), I).
subtree(iv, IV , subtree(G, S, C, M, P, _), subtree(G, S, C, M, P, IV)).

subtreeAddMass(Mass, subtree(G, S, C, M, P,I), subtree(G, S, C, M2, P, I)) :-
	M2 is M + Mass.
	% write('T: Mass'), write(M), write(Mass).s