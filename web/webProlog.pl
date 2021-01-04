
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Tau Prolog code for the Prolog part in the browser
% This is the bridge between the graphics in Processing (P5js) and user and the
% the game itself provided by the Prolog server (SWI Prolog via Pengines)
% It takes information from Pengine and triggers querys to Pengine
% as response to user actions
% 
% Author: Hans N. Beck (c)
%
% License: MIT 
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- use_module(library(dom)).
:- use_module(library(js)).
:- use_module(library(lists)).  
:- use_module(library(random)).  

% setup the initial next player and the deck costume
init :-
	write('start init'),
	holdTerm(0, gsnCounter),
	holdTerm(callback(updateSubtreeSolution), solution),
	holdTerm(callback(updateSubtreeStrategy), strategy),
	holdTerm(callback(updateSubtreeChild), child),
	holdTerm(callback(updateSubtreeEmbodiment), update),
	holdTerm(callback(addSubtree), newSubtree),
	holdTerm(types(rl, mt, ph), types),
	setupGame(Subtree, List),
	write('Tau Prolog: done 99'), write(Subtree), write(List). 

	

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% structure coder %%%%%%%%%%%%%%%%%%%%%%%%%%%%

% the explanation of every gsn element like goal, solution or stragegy is
% its definition of explanation types: rule, measurement or phenomenon
% search the explanation of a given element

explanationFromID(ID, Explanation) :-
	state(gsnPalette, List),
	%write('T:PAltette'), write(List),
	expByID(List, ID, Explanation).

expByID([], _, _) :-!.

expByID([H|T], ID, Exp) :-
	arg(1, H, ID),
	arg(3, H, Exp),!.

expByID([H|T], ID, Exp) :-
 	arg(1, H, ID2),
	arg(3, H, _),
	expByID(T, ID, Exp).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% !(S,S*) %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%% goal Explanation generators %%%%%%%%%%%%%%

goalExp --> g_measurement, g_phenomenon, g_law.

g_measurement --> { random_between(2,8, I) }, [[I, mt]].
g_phenomenon --> { random_between(2,8, I) }, [[I, ph]].
g_law --> { random_between(2,8, I) }, [[I, rl]].

genExplanation(goal, Explanation) :-
	phrase(goalExp, Explanation).

%%% strategy explanation generator
% which in fact is a rule generator
strategyExp(N1, T1, N2, T2, N3, T3) --> head(N1, T1), body1(N2, T2), body2(N3, T3).

head(N1, T1) --> [[N1, T1]].
body1(N2, T2) --> [[N2, T2]].
body2(N3, T3) --> [[N3, T3]].

nextType(I, I3, T) :-
	state(types, types(A, B, C)),
	I2 is I + 1, 
	(I2 == 4 -> I3 = 1 ; I3 = I2),
	arg(I3, types(A,B,C), T).

genExplanation(strategy, Explanation) :-
	random_between(1,3, I),
	nextType(I, I2, T1), 
	nextType(I2, I3, T2), 
	nextType(I3, _, T3),
	random_between(1, 3, Type),
	ruletype(Type, N1, N2, N3),
	phrase(strategyExp(N1, T1, N2, T2, N3, T3), Explanation).

ruletype(1, 2, 1, 1).
ruletype(2, 1, 1, 0).
ruletype(3, 1, 0, 1).

% Solution generator
genExplanation(solution, [[1, E]]) :-
	random_between(1,3, N),
	state(types, types(A, B, C)),
	arg(N, types(A,B,C), E).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  The action interface - every action triggers a query over pengine to
% SWI Prolog in order to do a game move
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% principle: the query contains bounded variables (bounded via Tau)
% Strings indicating unbounded variable names and of course - as first element -
% the name of the functor in SWI Prolog.
% the functor will be queried via Pengine. The bounded variables are Tau Prolog
% terms which are stringified. The variable names will be bound by the 
% SWI Prolog query. These are returned as JSON Objects and will addad to the Tau
% Prolog knowledge base. 

% Example "Current" will be bound by SWI Prolog with the card(...) drawn.
% The terhm card(...) will be added to the Tau prolog knowledge base as 
% state(current, card(...)) via the takeResult predicate here. 


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Generators %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

setupGame(Subtree, List3) :-
	firstSubtree(Subtree, Root),
	write('Setup'), write(Subtree),
	goal(explanation, Root, [[N1, T1], [N2, T2], [N3, T3]]),
	Sum is N1 + N2 + N3,
	%write('Generate palette : '), write(Sum),
	palette(strategy, Sum, List), 
	%write('T: Strat List'), write(List),
	palette(solution, Sum, List2),
	%write('Slution list'), write(List2),
	append(List, List2, List3),
	holdTerm(List3, gsnPalette).

% GL = Goal, St = Strukture, So = Solution, 
% the query to start the games
	
firstSubtree(Subtree, Root) :-
	gsnElement(goal, Root),
	elementCause(goal, 1, Root, Root2 ),
	goalAsSubtree(Root2, root, Subtree),
	realSubtree(1, Root2, root, Subtree),
	holdTerm(Subtree, gsnTree).

palette(Type, Sum, List) :-
	palette(Type, Sum, 1, [], List).

palette(_, 0, _, L, L ) :- !.

palette(Type, N, Level, List, List3) :-
	gsnElement(Type,  Element),
	elementCause(Type, Level, Element, Element2 ),
	embodyElement(Type, Element2, _),
	append(List, [Element2], List2),
	Level2 is Level + 1, 
	N2 is N - 1,
	palette( Type, N2, Level2, List2, List3).


%% only for debugs
showTree :-
	state(gsnTree, Tree), 
	writeHTML('Tauhtml', Tree, _).



% the element of a goal and a stragegy are the explanation types
% rl = rule = normative setting, mt= measurement, ph = phenomenon.



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% !(S, S*)  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%  First structure , then cause
gsnElement(Type, Element) :-
	genExplanation(Type, Explanation),
	state(gsnCounter, No),
	No2 is No + 1, 
	holdTerm(No2, gsnCounter),
	Element =.. [Type, No2, _, Explanation].

gsnElement(Type, Explanation, Element) :-
	state(gsnCounter, No),
	No2 is No + 1, 
	holdTerm(No2, gsnCounter),
	Element =.. [Type, No2, _, Explanation].

gsnElement(ID, Type, Explanation, Element) :-
	Element =.. [Type, ID, _, Explanation].

% causes
elementCause(solution, Level,  Element, Element2) :-
	body(Level, 0, 0, Body),
	solution(body, Body, Element, Element2).

elementCause(goal, Level,  Element, Element2) :-
	body(Level, 100, 0, Body),
	goal(body, Body, Element, Element2).

elementCause(strategy, Level,  Element, Element2) :-
	body(Level, 0, 0, Body),
	strategy(body, Body, Element, Element2).


% every goal is a subtree
goalAsSubtree(Goal, root, Subtree) :-
	goal(mass, Goal, M),
	newInterval(M, IV),
	Subtree = subtree(Goal, [], [], M, root, IV),!. 

goalAsSubtree(Goal, Parent, Subtree) :-
	goal(mass, Goal, M),
	newInterval(M, IV),
	subtree(id, Parent, ID),
	Subtree = subtree(Goal, [], [], M, ID, IV).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% !(S, C)  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% M is a mass
newInterval(M, IV) :-
	X2 is M / 100,
	X1 is -X2,
	IV = [X1, X2].

% [0,0] is here the application specific notation of empty cutset
cutSet([], [_, _], [0, 0]).
cutSet([_,_], [], [0, 0]).
cutSet([], [], [0, 0]).

cutSet([A, B], [C, D], [X,Y]) :-
	write('vor der sortieung'), 
	msort([A, B, C, D], [_, X, Y, _]),
	write('nach der sortierung'),
	inInterval(X, [A,B]), !.

cutSet([_, _], [_, _], [0,0]) :-
	write('BUMMER').

inInterval(X, [A, B]) :-
	X >= A, 
	X < B.

midInterval([A, B], C) :-
	C is (B-A)/2 + A.


updateParents(Subtree, Subtree2) :-
	subtree(parent, Subtree, Parent),
	
	subtree(iv, Subtree, IV),

	

% goal child is strategy and solution
newGoalChild(Type, ID, Level, Subtree, Subtree2) :-
	%write('T: Goal child'), write(Type), write(Subtree),
	explanationFromID(ID, Explanation),
	gsnElement(ID, Type, Explanation, Element),
	elementCause(Type, Level, Element, Element2 ),
	element(mass, 100, Element2, Element3),
	subtreePlusElement(Level, Type, Element3, Subtree, Subtree2),
	%write('T:New Treee'), write(Subtree2),
	realElement(Type, Element3, Subtree2),
	updateParents(Subtree2, Subtree3).
	

%%%%%%%%%%%%%%%%%%%%%%%%%%%%% >(F, S, S*) %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% comming from interactions
% here Interface to Javascript - JS calls addGoalChild
% in this form more a matter of test
% StID is subtree ID = ID of head goal of this subtree
addGoalChild(Type,TreeID, ID) :-
	state(gsnTree, Tree),
	write('T:current Tree'), write(Tree), write(Type),
	updateTree(TreeID, ID, Type, 2, Tree, Tree2),
	write('T: new Tree'), write(Tree2),
	holdTerm(Tree2, gsnTree).

%%% go througt the tree
updateTree(TreeID, ID, Type, Level, Tree, Tree2) :-
	subtree(id, Tree, TreeID),
	newGoalChild(Type, ID, Level, Tree, Tree2),!.
%%%
updateTree(TreeID, ID, Type, Level, Tree, Tree4) :-
	subtree(childs, Tree, Childs),
	updateChilds(TreeID, ID, Type, Level, Childs, Childs2, NewMass),
	subtree(childs, Childs2, Tree, Tree2),
	subtreeAddMass(NewMass, Tree2, Tree3),
	newInterval(NewMass, IV),
	subtree(iv, IV, Tree3, Tree4).

% update the childs of a tree
updateChilds(TreeID, ID, Type,  Level,  [], [],  _) :- false.

% go over all child subtrees
updateChilds(TreeID, ID, Type, Level, [H | T],  [H2 | T],  NewMass) :-
	Level2 is Level + 2, 
	subtree(mass, H, OldMass),
	updateTree(TreeID, ID, Type, Level2, H, H2),! ,
	subtree(mass, H2, MassChilds),
	NewMass is MassChilds - OldMass.

updateChilds(TreeID, ID, Type, Level, [H | T],  [H | T2], NewMass) :-
	updateChilds(TreeID, ID, Type, Level, T, T2, NewMass).


shiftInterval(_, [], []).

shiftInterval(X, [A, B], [A2, B2]) :-
	A2 is A + X, 
	B2 is B + X.

%%%%%%%%%%%%%%%%%%%%%%%%%%% !(S,S*) %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% a goal gets a strategy, means the related subtree gets the strategy
% per definition there cannot be childs

subtreePlusElement(Level, strategy, Element, subtree(Goal, [], [], M, Parent, IV), Subtree3) :-
	Subtree2 = subtree(Goal, Element, [], M, Parent, IV),
	newStrategyGoals(Element, Level, Subtree2, Subtree3).

subtreePlusElement(Level, solution, Element, subtree(Goal, [], [], M, Parent, IV), Subtree2) :-
	element(mass, Element, M2),
	M3 is M + M2, 
	Subtree2 = subtree(Goal, Element, [], M3, Parent, IV).

subtreePlusSubtree(GoalAsSubtree, root, root) :-!.

% new goal as subtree will be added to a parent
%%%% here add the interval
subtreePlusSubtree(GoalAsSubtree, Parent, Parent4) :-
	subtree(childs, Parent, Childs), 
	write('TAU: OLD CHild'), write(Childs),
	subtree(mass, GoalAsSubtree, M),
	relaxInterval(GoalAsSubtree, Childs, [], Child2),
	subtreeAddMass(M, Parent, Parent2),
	subtree(childs, Child2, Parent2, Parent3),
	updateInterval(Parent3, Parent4).
	
updateInterval(Subtree, Subtree2) :-
	subtree(childs, Subtree, Childs),
	nth0(0, Childs, C1),
	last(Childs, C2),
	subtree(iv, C1, [A, _]),
	subtree(iv, C2, [_, B]),	
	subtree(iv, [A, B], Subtree, Subtree2).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
newStrategyGoals(Strategy, Level, Subtree, Subtree3) :-
	subtree(goal, Subtree, G),
	goal(explanation, G, EG),
	strategy(explanation, Strategy, ES),
	transform(ES, EG, E),
	Level2 is Level + 1, 
	newGoalSet(E, ES, Level2, Subtree, Subtree3).

newGoalSet(Explanation, Rule, Level, Parent, Parent2) :-
	expandExpl(Explanation, Rule, ListofExpl),
	nextGoal(ListofExpl, Level, Parent, Parent2), 
	realChilds(Level, Parent2).

nextGoal([], Level, Subtree, Subtree) :- !.
nextGoal([E| Tail], Level, Parent, Subtree3) :-
	gsnElement(goal, E, Element),
	elementCause(goal, Level, Element, Element2 ),
	goalAsSubtree(Element2, Parent, ChildSubtree),
	subtreePlusSubtree(ChildSubtree, Parent, Parent2),
	nextGoal(Tail, Level, Parent2, Subtree3).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%% Intervalhandling 

relaxInterval(GoalAsSubtree, [], [],[GoalAsSubtree]) :- !.

relaxInterval(GoalAsSubtree, [], L, L2) :- 
	append(L, [GoalAsSubtree], L2 ),!.

relaxInterval(GoalAsSubtree, [C | Tail], L, Childs2) :-
	subtree(iv, GoalAsSubtree, IV), 
	midInterval(IV, X1),
	subtree(iv, C, CIV),
	midInterval(CIV, X2),
	X2 < X1, 
	append(L, [C], L2 ),
	relaxInterval(GoalAsSubtree, Tail, L2, Childs2), !.

relaxInterval(GoalAsSubtree, Childs, L,  Childs2) :-
	shiftAllIntervals(left, L, GoalAsSubtree, L2),
	shiftAllIntervals(right, Childs, GoalAsSubtree, R2),
	append(L2, [GoalAsSubtree], L3),
	append(L3, R2, Childs2).

shiftAllIntervals(_, [], _ , []) :- !.

shiftAllIntervals(_, A, 0 , A) :- !.

shiftAllIntervals(Dir, Childs, GoalAsSubtree, Childs2) :-
	(Dir == left -> last(Childs, C);
					nth0(0, Childs, C)),
	subtree(iv, C, IV1),
	subtree(iv, GoalAsSubtree, IV), 
	cutSet(IV1, IV, [A,B]),
	Delta is B-A, 
	moveAllChilds(Dir, Delta, Childs, Childs2).
	

moveAllChilds(_, _, [], []) :- !.

moveAllChilds(Dir, Delta, [C | Tail], [C2 | Tail2]) :-
	subtree(iv, C, IV),
	moveIntervals(Dir, IV, Delta, IV2),
	subtree(iv, IV2, C, C2),
	moveAllChilds(Dir, Delta, Tail, Tail2).


moveIntervals(_, [], _, []).

moveIntervals(left, IV , X, IV2 ) :-
	X2 is -X,
	shiftInterval(X2, IV, IV2).

moveIntervals(right, IV, X, IV2 ) :-
	shiftInterval(X, IV, IV2).

%%%%%%%%%%%%%%%% strategy transformation rule application %%%%%%%%%%%%%%%%%%%%%%

%%%% Transformation step 1

ruleheadApply([H | _], List) --> rulehead(H, List).
rulebodyApply([ _ | Body], List) --> rulebody(Body, List).

rulehead([_, _], []) --> [].
rulehead([N, T], [H | _]) -->  {	H = [N2, T], 
									(applyCondition(N, N2, N3) ->
										E = [N3, T]; 
										E = [] )}, 
									[E],!.

rulehead([N, T], [_ | Tail]) -->  rulehead([N, T], Tail). 

rulebody( [], _) -->[].
rulebody([H | T], List) --> ebody(H, List), rulebody(T, List).

ebody(_,[]) --> [].
ebody(A, [A2 | _]) --> { matchArgument(A, A2, New) },
							[New], !.

ebody(A, [_ | T]) --> ebody(A, T).

applyCondition(N, N2, N3) :-
	N3 is N2 - N, 
	N3 > -1, 
	N > 0.

matchArgument([N1, T], [N2, T], [N3, T]) :-
	N3 is N1 + N2.

% apply a strategyrule given as Explanation ES
transform(ES, EG, Result2) :-
	phrase(ruleheadApply(ES, EG), A),
	(A == [[]] -> 
		Result2 = EG;
		phrase(rulebodyApply(ES, EG), Result),
		append(A, Result, Result2)	
	).

%% %%%%% Transformation step 2
rulesort([], _ ) --> [].
rulesort([A | Tail], List) --> ruleElem(A, List), 
									rulesort(Tail, List).

ruleElem(_, [] )  --> [].
ruleElem([_, T], [H2 | _] ) --> {H2 = [N2, T]},
								 	[[N2, T]],!. 
ruleElem([N1, T], [_ | Tail] ) --> ruleElem([N1, T], Tail),!.

%% % sort explanation in the sequence of the rule types
%% % then compar
expandExpl(Explanation, Rule,  ListofExpl) :-
	phrase(rulesort(Rule, Explanation), Exp2),
	expand2(Rule, Exp2, [], ListofExpl).
	
% case 0: subtraction not possible because to low count of 
% required argument type
expand2(_, [[0, T], A2, A3], L1, ListofExpl ) :-
	countNonZero([[0, T], A2, A3], 0, Z, [], SortArguments),
	(Z > 1 -> append(L1, [[[0, T], A2, A3]], ListofExpl);
			splitNonZero(SortArguments, ListofExpl)
	), !.
	
% splitting by substraction can be done
expand2(Rule, Explanation, L1, ListofExpl ) :-
	subtract(Rule, Explanation, Result),
	expand3(Rule, Explanation, Result, L1, ListofExpl).

%%% Cases of outcomes and resulting rule of splitting
% case b)
expand3(_, Expl,  [[0, _], [0, _], [0, _]], L1,  L2) :- 
	append(L1, [Expl], L2), !.
% fall c)
expand3(_, Expl, [[N, _], _, _], L1, L2) :-
	N < 0 , 
	append(L1, [Expl], L2), !.

expand3(_, _, Result, L1, L3) :-
	countNonZero(Result, 0, _, [], [[N, T1], [0, T2], [0, T3]]),
	splitNonZero( [[N, T1], [0, T2], [0, T3]], L),
	append(L1, L, L3), !.

% case a)
expand3(Rule, _, [[0, T], A2, A3], L1, L3) :-
	append(L1, [Rule], L2), 
	append(L2, [[[0, T], A2, A3]], L3), !.

expand3([R1, [_, TR], R3], [_, A2, _], [AA1, [N2, _], AA3], L1, L3) :-
	N2 < 0 ,
	append(L1, [[R1, [0, TR], R3]], L2),
	append(L2, [[AA1, A2, AA3]], L3), !.

expand3([R1, R2, [_, TR]], [_, _, A3], [AA1, AA2, [N3, _]], L1, L3) :-
	N3 < 0 ,
	append(L1, [[R1, R2, [0, TR]]], L2),
	append(L2, [[AA1, AA2, A3]], L3), !.


%% % allgemeiner Fall,
expand3(Rule, _, Result, L1, L4) :-
	append(L1,  [Rule], L2), 
	expand2(Rule, Result, L2, L4).

subtract([], _, []) :- !. 
subtract([H| T], [H2 | T2], [H3 | T3]) :-
	subtract2(H, H2, H3), 
	subtract(T, T2, T3).

subtract2([N1, T], [N2, T], [N3, T]) :-
	N3 is N2 - N1.
	%(N3 < 0 -> N4 = N2; N4 = N3).

countNonZero([], Z, Z, L1, L1) :- !. 
countNonZero([[N, T] | Tail], Z, Z3, L1, L3) :-
	(N =\= 0 -> Z2 is Z + 1,
				append([[N, T]], L1, L2); 
				Z2 is Z,
				append(L1, [[N, T]], L2)
	),
	countNonZero(Tail, Z2, Z3, L2, L3).

% must be sorted
splitNonZero([[N, T] | Tail], ListofExpl) :-
	split([ [1, T] | Tail], N, [], ListofExpl).

split(_, 0, L, L) :- !.
split(E, N, L, L3) :-
	append(L, [E], L2),
	N2 is N - 1, 
	split(E, N2, L2, L3).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%