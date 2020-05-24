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
							genGoal/1, 
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
	store(gsnCounter(0)),
	genGoalChilds(strategy, N, List),
	genGoalChilds(solution, N, List2),
	append(List, List2, List3),
	%pengine_debug('List~w ~n',[List3]),
	store(gsnPalette(List3)).

genInitalObjects(Subtree, gsnPalette(List)) :-
	newGoal(root, _, Subtree),
	pengine_debug('SWI goal is ~w ~n',[Subtree]),
	store(gsnTree(Subtree)),
	access(gsnPalette(List)).

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

%%%
genExplanation(strategy, Explanation) :-
	random_between(1,3, I),
	 access(types(A, B, C)),
	%types(A, B, C) = types(rl, mt, ph),
	arg(I, types(A,B,C), E),
	I2 is I + 1, 
	(I2 == 4 -> I3 = 1 ; I3 = I2),
	arg(I3, types(A,B,C), E2),
	I4 is I3 + 1, 
	(I4 == 4 -> I5 = 1 ; I5 = I4),
	arg(I5, types(A,B,C), E3),
	random_between(0, 8, N2),
	random_between(0, 8, N3),
	explanation(argument(1, E),
				argument(N2, E2), 
				argument(N3, E3),
				Explanation).
%%%
genExplanation(goal, Explanation) :-
	random_between(0, 8, N1),
	random_between(0, 8, N2),
	random_between(0, 8, N3),
	explanation(argument(N1, rl),
				argument(N2, mt), 
				argument(N3, ph),
				Explanation).

genExplanation(solution, [[1, E]]) :-
	random_between(1,3, N),
	access(types(A,B,C)),
	arg(N, types(A,B,C), E).


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
genGoal(Goal) :-
	genExplanation(goal, Explanation),
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

% build a new goal as a subtree. A goal is always the head of a subtree
newGoal(Parent, Parent2, NewSubtree) :-
	genGoal(Goal),
	goalAsSubtree(Goal, Parent, NewSubtree),
	%pengine_debug('New Goal ~w Subtree2 ~w Parent ~w ~n',[Goal, NewSubtree, Parent]),
	subtreePlusGoal(NewSubtree, Parent, Parent2).
	%pengine_debug('new Parent ~w ~n',[Parent2]).

% build a new Goal child and add it to the subtree
newGoalChild(ID, Type, Subtree, Subtree2) :-
	explanationFromID(ID, Explanation),
	% pengine_debug('Explanation ~w Subtree ~w~n',[Explanation, Subtree]),
	newGSN(Type, ID,  Explanation, Element),
	subtreePlusElement(Type, Element, Subtree, Subtree2),
	pengine_debug('SWI Subtree2 ~w~n',[Subtree2]).

% add new goals as subtree and add it to the strategy
% the childs depend on the strategy
newStrategyGoals(Strategy, Subtree2, Subtree4) :-
	newGoal(Subtree2, Subtree3, _),
	newGoal( Subtree3, Subtree4, _).


%%%%%%%%%%%%%%%%%%%%%%%%%%%% >(F, S, S*) %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

addGoalChild(Type, TreeID, ID, NewSubTree) :-
	access(gsnTree(Tree)),
	access(gsnPalette(List)),
	%pengine_debug('The List~w~n ~w ~w~n',[List, TreeID, ID]),
	updateTree(TreeID, ID, Type, Tree, Tree2, NewSubTree),
	pengine_debug('SWI The Tree ~w~n',[Tree2]),
	store(gsnTree(Tree2)).

updateTree(TreeID, ID, Type, Tree, Tree2, Tree2) :-
	subtree(id, Tree, TreeID),
	% pengine_debug('The Tree found ~w ID ~w~n',[Tree, TreeID]),
	newGoalChild(ID, Type, Tree, Tree2),!.

updateTree(TreeID, ID, Type, Tree, Tree3, NewSubtree) :-
	subtree(childs, Tree, Childs),
	% pengine_debug('St search ~w ::: ~w ~n',[Tree, Childs]),
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
	newStrategyGoals(Strategy, Subtree2, Subtree3).

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

