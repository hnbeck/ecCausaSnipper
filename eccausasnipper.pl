%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% 
% Author: Hans N. Beck (c)
% Last Change: 20.05.2020
%
% License: MIT 
%
% Approach: Replication Variant Q1.1 Q3.1, means every node
% implement the rules, pseudo time is turn number and
% messages are stringifyd queries
%
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- module(ecCausaSnipper, [createGame/2, 
							startGame/2, 
							genGoalChilds/3,
							genInitalObjects/2,
							addGoalChild/4,
							showTree/1]).

:- use_module(objects).
:- use_module(library(pengines)).
:- use_module(library(sandbox)).

% :- dynamic card/4.

:- dynamic current_store/2.
:- dynamic current_process/4.
:- dynamic types/3.
:- multifile sandbox:safe_primitive/1.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Note: this is the server side Prolog level of the application. It implements
% the game rules, the objects of the games and their logical structure.
% Display, geometry and such things are responsibility of the browser, which is
% the Javascript or Tau-Prolog level
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



%%%%%%%%%%%%%%%%%%%%%%%%%%%% Server Code %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Code by Anne Ogborn from Ludum Dare44 Game

createGame(_, 'EC Causa Snipper - Enjoy') :-
	pengine_self(PengineID),
	current_process(PengineID, _, _, _),
	!,
	debug(ld(redundant), 'game already created', []).

createGame(N, Msg) :-
	pengine_self(PengineID),
	startGame(N, Msg),
	thread_at_exit(killGame(PengineID)).

sandbox:safe_primitive(ecCausaSnipper:createGame(_,_)).

killGame(PengineID) :-
	current_process(PengineID, PID, _, _),
	process_kill(PID).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% persistence %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

store(Term) :-
	pengine_self(PengineID),
	asserta(current_store(PengineID, Term)).

access(Term) :-
	pengine_self(PengineID),
	current_store(PengineID, Term).

update(Term) :-
	pengine_self(Session),
	retractall(current_store(Session, Term)), 
	asserta(current_store(PengineID, Term)).

retractall_store(Term) :-
    pengine_self(Session),
    retractall(current_store(Session, Term)).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%% play actions %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% start the game by doing some create initial data which is
% a list of available strategies
% a list of available solutions
startGame(N, 'start') :-
	store(types(rl, mt, ph)),
	store(gsnCounter(0)).
	
genInitalObjects(Subtree, gsnPalette(List3)) :-
	newRoot(root, Subtree),
	pengine_debug('SWI goal is ~w ~n',[Subtree]),
	store(gsnTree(Subtree)),
	access(rootExp([[N1, T1], [N2, T2], [N3, T3]])),
	Sum = N1 + N2 + N3,
	genGoalChilds(strategy, Sum, List),
	genGoalChilds(solution, Sum, List2),
	append(List, List2, List3),
	pengine_debug('List~w ~n',[List3]),
	store(gsnPalette(List3)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% structure coder %%%%%%%%%%%%%%%%%%%%%%%%%%%%

% the explanation of every gsn element like goal, solution or stragegy is
% its definition of explanation types: rule, measurement or phenomenon

explanationFromID(ID, Explanation) :-
	% pengine_debug('inside serach~w ~w~n',[ID, Explanation]),
	access(gsnPalette(List)),
	expByID(List, ID, Explanation).

expByID([], _, _) :-!.

expByID([H|T], ID, Exp) :-
	arg(1, H, ID),
	arg(2, H, Exp),!.

expByID([H|T], ID, Exp) :-
 	arg(1, H, ID2),
	arg(2, H, _),
	expByID(T, ID, Exp).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% !(S,S*) %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%% goal Explanation generator
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
	access(types(A, B, C)),
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
	access(types(A,B,C)),
	arg(N, types(A,B,C), E).


%%%%%%%%%%%%%%%% strategy transformation rule application %%%%%%%%%%%%%%%%%%%%%%

%%%% Transformation step 1

ruleheadApply([H | _], List) --> rulehead(H, List).
rulebodyApply([ _ | Body], List) --> rulebody(Body, List).

rulehead([N, T], []) --> [].
rulehead([N, T], [H | Tail]) -->  {	H = [N2, T], 
									(applyCondition(N, N2, N3) ->
										E = [N3, T]; 
										E = [] )}, 
									[E],!.

rulehead([N, T], [H | Tail]) -->  rulehead([N, T], Tail). 

rulebody( [], List) -->[].
rulebody([H | T], List) --> body(H, List), rulebody(T, List).

body(A,[]) --> [].
body(A, [A2 | T]) --> { matchArgument(A, A2, New) },
							[New], !.

body(A, [A2 | T]) --> body(A, T).

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

%%%%% Transformation step 2
rulesort([], _ ) --> [].
rulesort([A | Tail], List) --> ruleElem(A, List), 
									rulesort(Tail, List).

ruleElem(A, [] )  --> [].
ruleElem([N1, T], [H2 | Tail] ) --> {H2 = [N2, T]},
								 	[[N2, T]],!. 
ruleElem([N1, T], [H2 | Tail] ) --> ruleElem([N1, T], Tail),!.

% sort explanation in the sequence of the rule types
% then compar
expandExpl(Explanation, Rule,  ListofExpl) :-
	phrase(rulesort(Rule, Explanation), Exp2),
	%pengine_debug('SWI: sortet ~w rule ~w ~n',[Exp2, Rule]),
	expand2(Rule, Exp2, [], ListofExpl),
	pengine_debug('SWI: Expl List ~w ~n',[ListofExpl]).
	
% case 0: subtraction not possible because to low count of 
% required argument type
expand2(Rule, [[0, T], A2, A3], L1, ListofExpl ) :-
	countNonZero([[0, T], A2, A3], 0, Z, [], SortArguments),
	(Z > 1 -> append(L1, [[[0, T], A2, A3]], ListofExpl);
			splitNonZero(SortArguments, ListofExpl)
	), !.
	
% splitting by substraction can be done
expand2(Rule, Explanation, L1, ListofExpl ) :-
	subtract(Rule, Explanation, Result),
	pengine_debug('SWI: subtract ~w rule ~w ~n',[Result, Rule]),
	expand3(Rule, Explanation, Result, L1, ListofExpl).

%%% Cases of outcomes and resulting rule of splitting
% case b)
expand3(_, Expl,  [[0, T], [0, T2], [0, T3]], L1,  L2) :- 
	append(L1, [Expl], L2), !.
% fall c)
expand3(_, Expl, [[N, T], A2, A3], L1, L2) :-
	N < 0 , 
	append(L1, [Expl], L2), !.

expand3(_, Expl, Result, L1, L3) :-
	countNonZero(Result, 0, Z, [], [[N, T1], [0, T2], [0, T3]]),
	%pengine_debug('SWI: count ~w rule ~w ~n',[Z, Result]),
	splitNonZero( [[N, T1], [0, T2], [0, T3]], L),
	append(L1, L, L3), !.

% case a)
expand3(Rule, _, [[0, T], A2, A3], L1, L3) :-
	append(L1, [Rule], L2), 
	append(L2, [[[0, T], A2, A3]], L3), !.

expand3([R1, [_, TR], R3], [A1, A2, A3], [AA1, [N2, T], AA3], L1, L3) :-
	N2 < 0 ,
	append(L1, [[R1, [0, TR], R3]], L2),
	append(L2, [[AA1, A2, AA3]], L3), !.

expand3([R1, R2, [_, TR]], [A1, A2, A3], [AA1, AA2, [N3, T]], L1, L3) :-
	N3 < 0 ,
	append(L1, [[R1, R2, [0, TR]]], L2),
	append(L2, [[AA1, AA2, A3]], L3), !.


% allgemeiner Fall,
expand3(Rule, Expl, Result, L1, L4) :-
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
	%pengine_debug('SWI: count zero N ~w T ~w ~n',[N, T]),
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


%%%%%%%%%%%%%%%%%%%%%%%%%%%%% objects generators %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% build gsn element if ID is not known
newGSN(Type, Explanation, Element) :-
	access(gsnCounter(No)),
	No2 is No + 1, 
	store(gsnCounter(No2)),
	Element =.. [Type, No2, Explanation].
% if ID is known
newGSN(Type, ID,  Explanation, Element) :-
	Element =.. [Type, ID, Explanation].

% goal generator
genRoot(Goal) :-
	genExplanation(goal, Explanation),
	store(rootExp(Explanation)),
	newGSN(goal, Explanation, Goal).

genGoal(Goal, Explanation) :-
	newGSN(goal, Explanation, Goal).

% goal child generator - which can be a solution or a strategy
genGoalChilds(Type, Element) :-
	genExplanation(Type, Explanation),
	newGSN(Type, Explanation, Element).
% generate some number of goal childs in as a list
genGoalChilds(Type, N, List) :-
	genGoalChilds(Type, N, [], List).

genGoalChilds(_ , 0, List, List) :- !.

genGoalChilds(Type, N, List, List3) :-
	N2 is N -1,
	genGoalChilds(Type, Element),
	append(List, [Element], List2),
	genGoalChilds(Type, N2, List2, List3).

% embeds the goal in a new subtree
goalAsSubtree(Goal, root, Subtree) :-
	Subtree = subtree(Goal, [], [], root),!. 

goalAsSubtree(Goal, Parent, Subtree) :-
	subtree(id, Parent, ID),
	Subtree = subtree(Goal, [], [], ID).

%%%%%%%%%%%%%%%%%%%%%%%%%%%% structure generators %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

newRoot(Parent, NewSubtree) :-
	genRoot(Goal),
	goalAsSubtree(Goal, Parent, NewSubtree),
	pengine_debug('New Goal ~w Subtree2 ~w Parent ~w ~n',[Goal, NewSubtree, Parent]),
	subtreePlusGoal(NewSubtree, Parent, Parent2).

% build a new goal as a subtree. A goal is always the head of a subtree
newGoal(Explanation, Parent, Parent2, NewSubtree) :-
	genGoal(Goal, Explanation),
	goalAsSubtree(Goal, Parent, NewSubtree),
	%pengine_debug('New Goal ~w Subtree2 ~w Parent ~w ~n',[Goal, NewSubtree, Parent]),
	subtreePlusGoal(NewSubtree, Parent, Parent2).
	%pengine_debug('new Parent ~w ~n',[Parent2]).

% build a new Goal child and add it to the subtree
newGoalChild(ID, Type, Subtree, Subtree2) :-
	explanationFromID(ID, Explanation),
	pengine_debug('SWI: Explanation new ~w Subtree ~w ~n',[Explanation, Subtree]),
	newGSN(Type, ID,  Explanation, Element),
	subtreePlusElement(Type, Element, Subtree, Subtree2),
	pengine_debug('SWI Subtree2 ~w~n',[Subtree2]).

% add new goals as subtree and add it to the strategy
% the childs depend on the strategy
newStrategyGoals(Strategy, Subtree, Subtree3) :-
	subtree(explanation, Subtree, EG), 
	strategy(exp, Strategy, ES),
	pengine_debug('SWI: before transform ~w ~w~n',[ES, EG]),
	transform(ES, EG, E),
	pengine_debug('SWI: after transform ~w~n',[E]),
	newGoalSet(E, ES, Subtree, Subtree3).

newGoalSet(Explanation, Rule, Subtree, Subtree3) :-
	expandExpl(Explanation, Rule, ListofExpl),
	nextGoal(ListofExpl, Subtree, Subtree3).

nextGoal([], Subtree, Subtree) :- !.
nextGoal([E| Tail], Subtree, Subtree3) :-
	newGoal(E, Subtree, Subtree2, _),
	nextGoal(Tail, Subtree2, Subtree3).



%%%%%%%%%%%%%%%%%%%%%%%%%%%% >(F, S, S*) %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

addGoalChild(Type, TreeID, ID, NewSubTree) :-
	access(gsnTree(Tree)),
	access(gsnPalette(List)),
	pengine_debug('SWI: The List before add~w~n ~w ~w~n',[List, TreeID, ID]),
	updateTree(TreeID, ID, Type, Tree, Tree2, NewSubTree),
	pengine_debug('SWI: The Tree ~w~n',[Tree2]),
	store(gsnTree(Tree2)).

updateTree(TreeID, ID, Type, Tree, Tree2, Tree2) :-
	subtree(id, Tree, TreeID),
	pengine_debug('SWI: The Tree found ~w ID ~w~n',[Tree, TreeID]),
	newGoalChild(ID, Type, Tree, Tree2),!.

updateTree(TreeID, ID, Type, Tree, Tree3, NewSubtree) :-
	subtree(childs, Tree, Childs),
	pengine_debug('SWI: St search : ~w -> ~w ~n',[Tree, Childs]),
	updateChilds(TreeID, ID, Type, Childs, Childs2, NewSubtree),
	subtree(childs, Childs2, Tree, Tree3).

% update the childs of a tree
updateChilds(TreeID, ID, _, [], [], _) :- false.

% go over all child subtrees
updateChilds(TreeID, ID, Type, [H|T], [H2|T], NewSubtree) :-
	updateTree(TreeID, ID, Type, H, H2, NewSubtree),!.

updateChilds(TreeID, ID, Type, [H|T],  [H|T2], NewSubtree) :-
	updateChilds(TreeID, ID, Type, T, T2, NewSubtree).



%%%%%%%%%%%%%%%%%%%%%%%%%%% !(S,S*) %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% structure modifying rules

% a goal gets a strategy, means the related subtree gets the strategy
% per definition there cannot be childs

% add a strategy as goal child to the subtree the goal is head of
% this includes adding the child goals of the strategy
subtreePlusElement(strategy, Element, subtree(Goal, [], [], Parent), Subtree3) :-
	Subtree2 = subtree(Goal, Element, [], Parent),
	newStrategyGoals(Element, Subtree2, Subtree3).

% add a solution as goal child to the subtree the goal is head of
subtreePlusElement(solution, Element, subtree(Goal, [], [], Parent), Subtree2) :-
	Subtree2 = subtree(Goal, Element, [], Parent).

% special case if the goal is the root goal
subtreePlusGoal(GoalAsSubtree, root, root) :-!.

% add the goal as subtree to a parent subtree
subtreePlusGoal(GoalAsSubtree, Parent, Parent3) :-
	subtree(childs, Parent, Childs), 
	append(Childs, [GoalAsSubtree], Childs2), 
	subtree(childs, Childs2, Parent, Parent3).

%%%%%%%%%%%%%%%%%%%%%%%%%%% +(S,F) %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


showTree(Tree) :-
	access(gsnTree(Tree)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%%%%%%%%%%%%%%%%%%%%%%%%%%% effect rules %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



%%%%%%%%%%%%%%%%%%%%%%%%%%%%% game and winning rules %%%%%%%%%%%%%%%%%%%%%%%%%%%


% needed for pengine - declare safe predicate
% sandbox:safe_primitive(ecCausaSnipper:listDeck(_,_)).

