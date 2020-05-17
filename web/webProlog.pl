
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

% setup the initial next player and the deck costume
init :-
	holdTerm(0, gsnCounter),
	write('Tau Prolog: done14'). 

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


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Structure Elements 
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% GL = Goal, St = Strukture, So = Solution, 

% 
newTree(Tree) :-
	ressources, 
	newGoal(1, 0, root, _, NewSubtree),
	holdTerm( NewSubtree, gsnTree).

ressources :-
	state(gsnlist, gsnList(List)),
	ressource(List, 1).

ressource([], _) :-!.

ressource([H|T], Level) :-
	H = strategy(ID, Exp),
	genElement(strategy, ID, Level, 0, 0, Strategy),
	holdTerm(ID, gsnCounter),
	realElement(strategy, Strategy, []),
	Level2 is Level + 1, 
	ressource(T, Level2).
	
ressource([H|T], Level) :-
	H = solution(ID, Exp),
	genElement(solution, ID, 4, 100, 0.8, Solution),
	holdTerm(ID, gsnCounter),
	Level2 is Level + 1, 
	realElement(solution, Solution, []),
	ressource(T, Level2).

explanationFromID(ID, Explanation) :-
	state(gsnlist, gsnList(List)),
	expByID(List, ID, Explanation).

expByID([], _, _) :-!.

expByID([H|T], ID, Exp) :-
	arg(1, H, ID),
	arg(2, H, Exp),!.

expByID([H|T], ID, Exp) :-
	expByID(T, ID, Exp).


showTree :-
	state(gsnTree, Tree), 
	writeHTML('Tauhtml', Tree, _).

% the element of a goal and a stragegy are the explanation types
% rl = rule = normative setting, mt= measurement, ph = phenomenon.


transformPattern(RuleList).

transform(rl, Num, ph, Num2).
transform(ph, Num, rl, Num2).
transform(mt, Num, ph, Num2).
transform(rl, Num, ph, Num2, mt, Num3).
transform(ph, Num, rl, Num2, mt, Num3).
transform(mt, Num, ph, Num2, rl, Num3).

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
goal(exp, E, goal(ID, B, _), goal(ID, B, E)).
goal(exp, goal(_, _, E), E).
goal(id, goal(ID, _, _), ID).
goal(mass, goal(_, body(M, _, _), _), M).

goal(level, Goal, Level, Goal2) :-
	goal(body, Goal, body(Mass, _, V)),
	body(Level, Mass, V, Body),
	goal(body, Body, Goal, Goal2).

solution(body, solution(_, Body, _), Body).
solution(body, Body, solution(ID, _, E), solution(ID, Body, E)).
solution(exp, E, solution(ID, B, _), solution(ID, B, E)).
solution(exp, solution(_, _, E), E).
solution(id, solution(ID, _, _), ID).
solution(mass, solution(_, body(M, _, _), _), M).

solution(level, Goal, Level, Goal2) :-
	solution(body, Goal, body(Mass, _, V)),
	body(Level, Mass, V, Body),
	solution(body, Body, Goal, Goal2).


strategy(body, strategy(_, Body, _), Body).
strategy(body, Body, strategy(ID, _, E), strategy(ID, Body, E)).
strategy(exp, E, strategy(ID, B, _), strategy(ID, B, E)).
strategy(exp, strategy(_, _, E), E).
strategy(id, strategy(ID, _, _), ID).
strategy(mass, strategy(_, body(M, _, _), _), M).

strategy(level, Strategy, Level, Strategy2) :-
	strategy(body, Strategy, body(Mass, _, V)),
	body(Level, Mass, V, Body),
	strategy(body, Body, Strategy, Strategy2).


% subtree definition
% this is per definition the root of the tree
%subtree(goal(A,B,C), [], [], Mass, root).
%subtree(goal(A,B,C), [], [], Mass, Parent).
%subtree(goal(A,B,C), strategy(A,B,C), [], Mass, Parent).
%subtree(goal(A,B,C), strategy(A,B,C), SubtreeList, Mass, Parent).

subtree(id, subtree(goal(A, _, _), _, _, _, _), A).
subtree(goal, subtree(A, _, _,  _, _), A).
subtree(str, subtree(_, A, _,  _, _), A).
subtree(childs, subtree(_, _, A, _, _), A).
subtree(childs, Childs, subtree(G, S, _, M, P), subtree(G, S, Childs, M, P)).
subtree(mass, subtree(_, _, _ , A, _), A).
subtree(mass, M, subtree(G, S, C, _, P), subtree(G, S, C, M, P)).

%%%%%%%%%%%%%%%%%%%%% Generators %%%%%%%%%%%%%%%%%%%%%%%%

newGSN(Type, Body, Explanation, Element) :-
	state(gsnCounter, No),
	No2 is No + 1, 
	holdTerm(No2, gsnCounter),
	Element =.. [Type, No2, Body, Explanation].

newGSN(Type, ID, Body, Explanation, Element) :-
	Element =.. [Type, ID, Body, Explanation].

% in future explanation will be some kind of randomness
genGoal(Level, V, Goal) :-
	state(goal, A),
	state(goal, goal(ID, Explanation)),
	body(Level, 100, V, Body),
	newGSN(goal, Body, Explanation, Goal).	

% in future explanation is defined from other place
genElement(Type, ID, Level, Mass, V, Element) :-
	explanationFromID(ID, Explanation),
	body(Level, Mass, V, Body),
	newGSN(Type, ID, Body, Explanation, Element).

% new goal bedeutet new subtree - immer
% add it to a parent
newGoal(Level, V, Parent, Parent2, NewSubtree) :-
	genGoal(Level, V,  Goal),
	goalAsSubtree(Goal, Parent, NewSubtree),
	subtreePlusGoal(NewSubtree, Parent, Parent2),
	realSubtree(Level, Goal, Parent2, NewSubtree).


newGoalChild(Type, ID, Level, Subtree, Subtree2) :-
	genElement(Type, ID,  Level, 100, 0, Element),
	subtreePlusElement(Level, Type, Element, Subtree, Subtree2),
	realElement(Type, Element, Subtree2).
	

% generate the child goals of a strategy
newChildGoals(Strategy, Level, Subtree2, Subtree4) :-
	Level2 is Level + 1, 
	newGoal(Level2, 1, Subtree2, Subtree3, _),
	newGoal(Level2, -1, Subtree3, Subtree4, _).

%%%%%%%%%%%%%%%%%%%% modifications %%%%%%%%%%%%%%%%%%%%%%%%%%%%

% in this form more a matter of test
% StID is subtree ID = ID of head goal of this subtree
addGoalChild(Type,TreeID, ID) :-
	state(gsnTree, Tree),
	updateTree(TreeID, ID, Type,  2, Level2, Tree, Tree2),
	holdTerm(Tree2, gsnTree).

%%% go througt the tree
updateTree(TreeID, ID, Type, Level, Level, Tree, Tree2) :-
	subtree(id, Tree, TreeID),
	newGoalChild(Type, ID, Level, Tree, Tree2),!.

updateTree(TreeID, ID, Type, Level, Level2, Tree, Tree3) :-
	subtree(childs, Tree, Childs),
	subtree(mass, Tree, Mass),
	updateChilds(TreeID, ID, Type, Level, Level2, Childs, Childs2, Mass, NewMass),
	subtree(childs, Childs2, Tree, Tree2),
	subtree(mass, NewMass, Tree2, Tree3).

% update the childs of a tree
updateChilds(TreeID, ID, Type,  Level, Level, [], [], Mass, Mass) :- false.

% go over all child subtrees
updateChilds(TreeID, ID, Type, Level, Level3, [H | T],  [H2 | T], Mass, NewMass) :-
	Level2 is Level + 2, 
	updateTree(TreeID, ID, Type,  Level2, Level3, H, H2),! ,
	subtree(mass, H2, MassChilds), 
	NewMass is Mass + MassChilds.

updateChilds(TreeID, ID, Type, Level, Level2, [H | T],  [H | T2], Mass, Mass2) :-
	updateChilds(TreeID, ID, Type, Level, Level2, T, T2, Mass, Mass2).


goalAsSubtree(Goal, root, Subtree) :-
	goal(mass, Goal, M),
	Subtree = subtree(Goal, [], [], M, root),!. 

goalAsSubtree(Goal, Parent, Subtree) :-
	goal(mass, Goal, M),
	subtree(id, Parent, ID),
	Subtree = subtree(Goal, [], [], M, ID).


% a goal gets a strategy, means the related subtree gets the strategy
% per definition there cannot be childs

subtreePlusElement(Level, strategy, Element, subtree(Goal, [], [], M, Parent), Subtree3) :-
	strategy(mass, Element, M2),
	M3 is M + M2, 
	Subtree2 = subtree(Goal, Element, [], M3, Parent),
	newChildGoals(Element, Level, Subtree2, Subtree3).

subtreePlusElement(Level, solution, Element, subtree(Goal, [], [], M, Parent), Subtree2) :-
	solution(mass, Element, M2),
	M3 is M + M2, 
	Subtree2 = subtree(Goal, Element, [], M3, Parent).

subtreePlusGoal(GoalAsSubtree, root, root) :-!.

% new goal as subtree will be added to a parent
subtreePlusGoal(GoalAsSubtree, Parent, Parent3) :-
	subtree(childs, Parent, Childs), 
	subtree(mass, GoalAsSubtree, M),
	subtree(mass, Parent, M2),
	M3 is M2 + M, 
	subtree(mass, M3, Parent, Parent2),
	append(Childs, [GoalAsSubtree], Childs2), 
	subtree(childs, Childs2, Parent2, Parent3).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Communication Tau Prolog back to JS level
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% write the term to be send via Pengine into a DOM node to get it as string
msg2JS(FktID, Term) :-
	writeHTML('Tauhtml',Term, _),
	prop(FktID, JSFkt),
	apply(JSFkt, [], _).

% write a message into a DOM element to show so that user can see it
msg2JS(DOMId) :-
	state(msg, Msg),
	writeHTML(DOMId, Msg, _).

%%%%%%%%%%%% embodiement %%%%%%%%%%%%%%%%

% setze den neuen Subtree in ein embodyment um und verlinke ihn
realSubtree(1, Goal, root, Subtree) :- 
	embodyGoal(Goal, JSObject),
	embodySubtree(Subtree, JSObject, 1),!.

realSubtree(Level, Goal, Parent, Subtree) :- 
	embodyGoal(Goal, JSObject),
	embodySubtree(Subtree, JSObject, Level),
	syncSubtree(child, Parent, Subtree).

realElement(Type, Element, Subtree) :-
	embodyElement(Type, Element, JSObject),
	write('before sync'), write(Subtree), write(Element),
	syncSubtree(Type, Subtree, JSObject).

% generate PIXI object from this
embodyGoal(Goal, JSObject) :-
	goal(id, Goal, ID),
	goal(body, Goal, Body),
	goal(exp, Goal, E), 
	body(Body, BList),
	prop('gsnElemGenerator', JSFkt),
	apply(JSFkt, ['goal', ID, BList, E], JSObject).


embodyElement(solution, Element, JSObject) :-
	solution(id, Element, ID),
	solution(body, Element, Body),
	solution(exp, Element, E), 
	body(Body, BList),
	prop('gsnElemGenerator', JSFkt),
	apply(JSFkt, [solution, ID, BList, E], JSObject).

embodyElement(strategy, Element, JSObject) :-
	strategy(id, Element, ID),
	strategy(body, Element, Body),
	strategy(exp, Element, E), 
	body(Body, BList),
	prop('gsnElemGenerator', JSFkt),
	apply(JSFkt, [strategy, ID, BList, E], JSObject).

% embody new subtree - it contains only a goal per definition
embodySubtree(subtree(Goal, [], [], _, Parent), JSObject,  Level) :-
	goal(id, Goal, ID),
	goal(mass, Goal, Mass),
	ST = [JSObject, [], [], Mass, Parent],
	prop('addSubtree', JSFkt2),
	apply(JSFkt2, [Level, ST, ID], _),!.


%%%%%%%%%%%%% synchro with JS %%%%%%%%%%%%%%%%%

% communicate JS the new child of a subtree
% +Parent : the subtree which gets the new child
% +NewSubtree: the Subtree which will be the child	
syncSubtree(child, Parent, ChildSubtree) :-
	subtree(id, Parent, ID),
	subtree(id, ChildSubtree, ChID),
	subtree(mass, ChildSubtree, Mass),
	prop('updateSubtreeChild', JSFkt),
	apply(JSFkt, [ID, ChID, Mass], _).

syncSubtree(strategy, [], _) :-!.

syncSubtree(strategy, Subtree, JSObject) :-
	subtree(id, Subtree, ID),
	subtree(mass, Subtree, M),
	prop('updateSubtreeStrategy', JSFkt),
	apply(JSFkt, [ID, JSObject, M], _).

syncSubtree(solution, [], _) :-!.

syncSubtree(solution, Subtree, JSObject) :-
	write('sunc'), write(Subtree),
	subtree(id, Subtree, ID),
	subtree(mass, Subtree, M),
	prop('updateSubtreeSolution', JSFkt),
	apply(JSFkt, [ID, JSObject, M], _).

% write the query term in a non visible DOM element where it can be accessed
% at JS Level
% +Term : a Tau Prolog term
% -HMTLString : the term as String
writeHTML(ID, Term, HTMLString) :-
	get_by_id(ID, HTML),
	open(HTML, write, Stream), 
	write(Stream, Term), 
	write(Stream, '\n'),
	close(Stream),
	get_html(HTML, HTMLString).

% read in from a DOM element
% -Term : a Tau Prolog Term
readHTML(ID, Term) :-
	get_by_id(ID, HTML),
	open(HTML, read, Stream), 
	read(Stream, Term),
	close(Stream).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Player control: at start the SWI Prolog application provides to playerStr
% it is the task of the UI level to determine which player is the active player
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%       Pengine  - Tau Prolog interface
%
%  The following predicates take the answer from a Pengine Query which is 
% given as JS Object and transform it back to a Prolog statement.
% This will be persistent via asserta
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% store the fact under reference of property
% which means if pengine query contains variable P1
% the answer will be included in Tau database as
% state(p1, AnswerTerm)
holdTerm(TauTerm, H) :-
	% write('Taustate '), write(state(H, TauTerm)), % for debug
	retractall(state(H, _)),
	asserta(state(H, TauTerm)).

% Pengine answer is coded as a JS object which is a list of properties
% go through all properties given by the list and parse them
% if all is parsed message is available and can put out
% here, the SWI Prolog applicaton is designed to bind the Variable MSG
% with a message to be displayed in use interface

% property list done, last action is to give back the message in msg
% no drawn card available only send the message
takeResult([], _, _).
	
% takeResult(+Propertylist, +id of the variable containing the js object, -Tau Term)
% H is one property which is identical to the name of a bound variable in Pengine answer!
takeResult([H|T], JSObjectID, Term) :-
	prop(JSObjectID, JSObject),
	prop(JSObject, H, SubJSObject),
	parseTerm(SubJSObject, TauTerm),
	holdTerm(TauTerm, H),
	takeResult(T, JSObjectID, Term).
	

% +JSObjectID: an reference to a JS object containing the answer of a Pengine query
% every variable is a property containing an JS object for its bining
% Example if Penge answer binds P to player(1,name) then there is something lile
% {..."P":{functor:player, args:[1,name]}....}
% -TauTerm: the Pengine answer as Tau Prolog Term

% if element is not defined
parseTerm(Elem, _) :- var(Elem).
% if element is atomic
parseTerm(Elem, Elem) :- atomic(Elem).
% if element is a json object
parseTerm(JSObject, TauTerm) :-
	prop(JSObject, args, ArgList),
	prop(JSObject, functor, Functor),
	parseList(ArgList, TermList),
	append([Functor], TermList, TermList2),
	TauTerm =.. TermList2.
% if elem is a list
parseList([], []).
parseList([Head | Tail ], [Head2 | Tail2]) :-
	(is_list(Head) -> 	
		parseList(Head, Head2);
		(atomic(Head) -> 
			Head2 = Head; 
			parseTerm(Head, Head2)
		)
	),
	parseList(Tail, Tail2).

