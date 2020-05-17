%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% 
% Author: Hans N. Beck (c)
% Last Change: 07.01.2020
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
							genElement/3,
							genInitalObjects/2,
							addGoalChild/3,
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
% the game rules, the objects of the games (the cards), their logical structure
% It declares which structural changes are associated with the playing actions. 
% But it says nothing about where the cards laying, if we have a desk or 
% using only our hands. It says nothing about the order of the cards or time. 
% That all is not a natural part of the game itself.
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

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% persistence %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


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

% idea: there is player 1 and player 2. In every turn, one is the active player
% after a play action, the active and passive player changes
% Init the game
% playGame(	-P1: first player, 
% 			-P2: scnd player, 
%			-Flag: go or turn or stop indicating if game contiune and how
%			-Msg: a message string)


startGame(N, 'start') :-
	store(types(rl, mt, ph)),
	store(gsnCounter(0)),
	genElement(strategy, N, List),
	genElement(solution, N, List2),
	append(List, List2, List3),
	%pengine_debug('List~w ~n',[List3]),
	store(gsnList(List3)).

genInitalObjects(Goal, gsnList(List)) :-
	genGoal(Goal),
	pengine_debug('goal is ~w ~n',[Goal]),
	goalAsSubtree(Goal, root, Subtree),
	store(gsnTree(Subtree)),
	access(gsnList(List)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% objects generators %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

explanationFromID(ID, Explanation) :-
	% pengine_debug('inside serach~w ~w~n',[ID, Explanation]),
	access(gsnList(List)),
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
	random_between(0, 8, N1),
	random_between(0, 8, N2),
	random_between(0, 8, N3),
	explanation(argument(N1, rl),
				argument(N2, mt), 
				argument(N3, ph),
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

%%%% 
% builds goal()
newGSN(Type, Explanation, Element) :-
	access(gsnCounter(No)),
	No2 is No + 1, 
	store(gsnCounter(No2)),
	Element =.. [Type, No2, Explanation].
% ID may be known
newGSN(Type, ID,  Explanation, Element) :-
	Element =.. [Type, ID, Explanation].

%%% % in future explanation will be some kind of randomness
genGoal(Goal) :-
	genExplanation(goal, Explanation),
	newGSN(goal, Explanation, Goal).

%%% % in future explanation is defined from other place
genElement(Type, Element) :-
	genExplanation(Type, Explanation),
	newGSN(Type, Explanation, Element).

%%%
genElement(Type, N, List) :-
	genElement(Type, N, [], List).

%%%
genElement(_ , 0, List, List) :- !.
%%%
genElement(Type, N, List, List3) :-
	N2 is N -1,
	genElement(Type, Element),
	append(List, [Element], List2),
	genElement(Type, N2, List2, List3).



% new goal bedeutet new subtree - immer
% add it to a parent
%%%
newGoal(Parent, Parent2, NewSubtree) :-
	genGoal(Goal),
	goalAsSubtree(Goal, Parent, NewSubtree),
	%pengine_debug('New Goal ~w Subtree2 ~w Parent ~w ~n',[Goal, NewSubtree, Parent]),
	subtreePlusGoal(NewSubtree, Parent, Parent2).
	%pengine_debug('new Parent ~w ~n',[Parent2]).

%%%
newGoalChild(ID, Type, Subtree, Subtree2) :-
	explanationFromID(ID, Explanation),
	pengine_debug('Explanation ~w Subtree ~w~n',[Explanation, Subtree]),
	newGSN(Type, ID,  Explanation, Element),
	subtreePlusElement(Type, Element, Subtree, Subtree2),
	pengine_debug('Subtree2 ~w~n',[Subtree2]).

% generate the child goals of a strategy
% newChildGoals(Strategy, Subtree2, Subtree2).

newChildGoals(Strategy,  Subtree2, Subtree4) :-
	newGoal(Subtree2, Subtree3, _),
	newGoal( Subtree3, Subtree4, _).


%%%%%%%%%%%%%%%%%%%%%%%%%%%% ?(S,F) %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

addGoalChild(Type, TreeID, ID) :-
	access(gsnTree(Tree)),
	access(gsnList(List)),
	%pengine_debug('The List~w~n ~w ~w~n',[List, TreeID, ID]),
	updateTree(TreeID, ID,  Type, Tree, Tree2),
	pengine_debug('The Tree ~w~n',[Tree2]),
	store(gsnTree(Tree2)).

updateTree(TreeID, ID, Type, Tree, Tree2) :-
	subtree(id, Tree, TreeID),
	% pengine_debug('The Tree found ~w ID ~w~n',[Tree, TreeID]),
	newGoalChild(ID, Type, Tree, Tree2),!.

updateTree(TreeID, ID, Type, Tree, Tree3) :-
	subtree(childs, Tree, Childs),
	% pengine_debug('St search ~w ::: ~w ~n',[Tree, Childs]),
	updateChilds(TreeID, ID, Type, Childs, Childs2),
	subtree(childs, Childs2, Tree, Tree3).

% update the childs of a tree
updateChilds(TreeID, ID, _, [], []) :- false.

% go over all child subtrees
updateChilds(TreeID, ID, Type, [H|T], [H2|T]) :-
	updateTree(TreeID, ID, Type, H, H2),!.

updateChilds(TreeID, ID, Type, [H|T],  [H|T2]) :-
	updateChilds(TreeID, ID, Type, T, T2).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% ?(S,A) %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% !(S,S*) %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% a goal gets a strategy, means the related subtree gets the strategy
% per definition there cannot be childs

%%%
goalAsSubtree(Goal, root, Subtree) :-
	Subtree = subtree(Goal, [], [], root),!. 

goalAsSubtree(Goal, Parent, Subtree) :-
	subtree(id, Parent, ID),
	Subtree = subtree(Goal, [], [], ID).


% a goal gets a strategy, means the related subtree gets the strategy
% per definition there cannot be childs

subtreePlusElement(strategy, Element, subtree(Goal, [], [], Parent), Subtree3) :-
	Subtree2 = subtree(Goal, Element, [], Parent),
	newChildGoals(Strategy, Subtree2, Subtree3).

subtreePlusElement(solution, Element, subtree(Goal, [], [], Parent), Subtree2) :-
	Subtree2 = subtree(Goal, Element, [], Parent).

%%%
subtreePlusGoal(GoalAsSubtree, root, root) :-!.

% new goal as subtree will be added to a parent
subtreePlusGoal(GoalAsSubtree, Parent, Parent3) :-
	subtree(childs, Parent, Childs), 
	append(Childs, [GoalAsSubtree], Childs2), 
	subtree(childs, Childs2, Parent, Parent3).




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% +(S,F) %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


showTree(Tree) :-
	access(gsnTree(Tree)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%%%%%%%%%%%%%%%%%%%%%%%%%%% effect rules %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



%%%%%%%%%%%%%%%%%%%%%%%%%%%%% game and winning rules %%%%%%%%%%%%%%%%%%%%%%%%%%%


% needed for pengine - declare safe predicate
% sandbox:safe_primitive(ecCausaSnipper:listDeck(_,_)).

