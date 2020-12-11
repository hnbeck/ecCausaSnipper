
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
	holdTerm(callback(updateSubtreeSolution), solution),
	holdTerm(callback(updateSubtreeStrategy), strategy),
	holdTerm(callback(updateSubtreeChild), child),
	holdTerm(callback(updateSubtree), update),
	write('Tau Prolog: done 17'). 

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


% GL = Goal, St = Strukture, So = Solution, 
% the query to start the game
startGame(Tree) :-
	newPalette, 
	state(currentsubtree, subtree(goal(ID, E), _, _, _)),
	% write('T: root'), write(E), write(ID),
	newGoal(ID, E, 1, 0, root, _, NewSubtree),
	holdTerm( NewSubtree, gsnTree).

newPalette :-
	state(gsnpalette, gsnPalette(List)),
	%write('T: palette'), write(List),
	newPalette(List, 1).

newPalette([], _) :-!.

newPalette([H|T], Level) :-
	H =.. [Type, ID, Exp],
	% write(ID),
	genElement(Type, ID, Level, Element),
	holdTerm(ID, gsnCounter),
	realElement(Type, Element, []),
	Level2 is Level + 1, 
	newPalette(T, Level2).

% search from the palette
explanationFromID(ID, Explanation) :-
	state(gsnpalette, gsnPalette(List)),
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

% the element of a goal and a stragegy are the explanation types
% rl = rule = normative setting, mt= measurement, ph = phenomenon.


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

element(Element, ID, Body, E) :-
	arg(1, Element, ID),
	arg(2, Element, Body),
	arg(3, Element, E).

element(mass, Element, M) :-
	arg(2, Element, body(M, _, _)).

strategy(body, strategy(_, Body, _), Body).
strategy(body, Body, strategy(ID, _, E), strategy(ID, Body, E)).
strategy(id, strategy(ID, _, _), ID).
strategy(mass, strategy(_, body(M, _, _), _), M).

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
	% write('T: Mass'), write(M), write(Mass).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% !(S, S*)  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

newGSN(Type, Body, Explanation, Element) :-
	state(gsnCounter, No),
	No2 is No + 1, 
	holdTerm(No2, gsnCounter),
	Element =.. [Type, No2, Body, Explanation].

newGSN(Type, ID, Body, Explanation, Element) :-
	Element =.. [Type, ID, Body, Explanation].

% in future explanation will be some kind of randomness
genGoal(Level, V, Goal) :-
	state(goal, goal(ID, Explanation)),
	body(Level, 100, V, Body),
	newGSN(goal, Body, Explanation, Goal).	

genGoal(ID, Explanation, Level, V, Goal) :-
	body(Level, 100, V, Body),
	newGSN(goal, ID,  Body, Explanation, Goal).	

genElement(solution, ID, _, Element) :-
	genElement(solution, ID, 3, 100, 0.6, Element).

genElement(strategy, ID, Level, Element) :-
	genElement(strategy, ID, Level, 0, 0, Element).

% in future explanation is defined from other place
genElement(Type, ID, Level, Mass, V, Element) :-
	explanationFromID(ID, Explanation),
	body(Level, Mass, V, Body),
	newGSN(Type, ID, Body, Explanation, Element).

% new goal bedeutet new subtree - immer
% add it to a parent
newGoal(ID, E, Level, V, Parent, Parent2, NewSubtree) :-
	genGoal(ID, E, Level, V,  Goal),
	goalAsSubtree(Goal, Parent, NewSubtree),
	subtreePlusSubtree(NewSubtree, Parent, Parent2),
	realSubtree(Level, Goal, Parent2, NewSubtree),
	updateAllChilds(Parent2).

updateAllChilds(root) :- 
	write('TAU no UPDATE root'),!.

updateAllChilds(Subtree) :-
	subtree(childs, Subtree, Childs),
	updateAllChilds2(Childs).

updateAllChilds2([]) :- 
	write('TAU no UPDATE'),!.

updateAllChilds2( [C| Tail] ) :-
	write('TAU UPDATE'),
	syncSubtree(update, _, C),
	updateAllChilds2(Tail).
	

% goal child is strategy and solution
newGoalChild(Type, ID, Level, Subtree, Subtree2) :-
	% write('T: Goal child'), write(Type), write(ID),
	genElement(Type, ID,  Level, 100, 0, Element),
	subtreePlusElement(Level, Type, Element, Subtree, Subtree2),
	realElement(Type, Element, Subtree2).
	

% generate the child goals of a strategy
% according that what PEngine has created
newStrategyGoals(Strategy, Level, Subtree2, Subtree3) :-
	state(currentsubtree, subtree(G, S, Childs, _)),
	velocityStart(Childs, V0),
	Level2 is Level + 1, 
	childGoals(Childs, Level2, V0, Subtree2, Subtree3).

childGoals([], _, _, S, S) :- !.

childGoals([C| Childs], Level, V, Subtree, Subtree3 ) :-
	C = subtree(goal(ID, E), A, B, _),
	newGoal(ID, E, Level, V, Subtree, Subtree2, _),
	nextVelocity(V, V2),
	subtree(mass, Subtree2, M2),
	%write('T: Mass afte'), write(M2),
	childGoals(Childs, Level, V2, Subtree2, Subtree3).

goalAsSubtree(Goal, root, Subtree) :-
	goal(mass, Goal, M),
	newInterval(M, IV),
	Subtree = subtree(Goal, [], [], M, root, IV),!. 

goalAsSubtree(Goal, Parent, Subtree) :-
	goal(mass, Goal, M),
	newInterval(M, IV),
	subtree(id, Parent, ID),
	Subtree = subtree(Goal, [], [], M, ID, IV).

velocityStart(Childs, Start) :-
	length(Childs, N),
	Mode is N mod 2, 
	(Mode == 0 ->
		Start = 128;
		Start = 0).

nextVelocity(0, 128) :- !.

nextVelocity(V, V2) :-
	V > 0, 
	V2 is -V.

nextVelocity(V, V2) :-
	V < 0, 
	V2 is (-V) + 128.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%% >(F, S, S*) %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% comming from interactions

% in this form more a matter of test
% StID is subtree ID = ID of head goal of this subtree
addGoalChild(Type,TreeID, ID) :-
	state(gsnTree, Tree),
	%state(currentsubtree,A),
	%write('T: current ST'), write(A),
	updateTree(TreeID, ID, Type, 2, Tree, Tree2),
	% write('T: new Tree'), write(Tree2),
	holdTerm(Tree2, gsnTree).

%%% go througt the tree
updateTree(TreeID, ID, Type, Level, Tree, Tree2) :-
	subtree(id, Tree, TreeID),
	newGoalChild(Type, ID, Level, Tree, Tree2),!.

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
	M3 is M + M2/2, 
	Subtree2 = subtree(Goal, Element, [], M3, Parent, IV).

subtreePlusSubtree(GoalAsSubtree, root, root) :-!.

% new goal as subtree will be added to a parent
%%%% here add the interval
subtreePlusSubtree(GoalAsSubtree, Parent, Parent4) :-
	subtree(childs, Parent, Childs), 
	write('TAU: OLD CHild'), write(Childs),
	write('TAU: GoalsAsSubtree'), write(GoalAsSubtree),
	subtree(mass, GoalAsSubtree, M),
	relaxInterval(GoalAsSubtree, Childs, [], Child2),
	write('TAU: SUBTREEPLUSSUBTREE'), write(Child2),
	subtreeAddMass(M, Parent, Parent2),
	subtree(childs, Child2, Parent2, Parent3),
	updateInterval(Parent3, Parent4).
	
updateInterval(Subtree, Subtree2) :-
	subtree(childs, Subtree, Childs),
	nth0(0, Childs, C1),
	last(Childs, C2),
	subtree(iv, C1, [A, _]),
	subtree(iv, C2, [_, B]),	
	subtree(iv, [-10, 10], Subtree, Subtree2).

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

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Communication Tau Prolog back to JS level
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%% message exchange %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% write the term to be send via Pengine into a DOM node to get it as string
msg2JS(FktID, Term) :-
	writeHTML('Tauhtml',Term, _),
	prop(FktID, JSFkt),
	apply(JSFkt, [], _).

% write a message into a DOM element to show so that user can see it
msg2JS(DOMId) :-
	state(msg, Msg),
	writeHTML(DOMId, Msg, _).


%%%%%%%%%%%%%%%%%%%%%%%%%% embodiment on jS level %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% create an real = embodied subtree
realSubtree(Level, Goal, Parent, Subtree) :- 
	embodyElement(goal, Goal, JSObject),
	embodySubtree(Subtree, JSObject, Level),
	syncSubtree(child, Parent, Subtree).

% embody the given element
realElement(Type, Element, Subtree) :-
	% write('T: embody element'), write(Element),
	embodyElement(Type, Element, JSObject),
	syncSubtree(Type, Subtree, JSObject).


% embody new subtree  on JS level
% means to add a new subtree including its goal to the list on JS level
% this list is relevant for graphics
embodySubtree(subtree(Goal, [], [], _, Parent, IV), JSObject,  Level) :-
	goal(id, Goal, ID),
	goal(mass, Goal, Mass),
	ST = [JSObject, [], [], Mass, Parent, IV],
	write('TAU:'), write(IV),
	prop('addSubtree', JSFkt2),
	apply(JSFkt2, [Level, ST, ID], _).

% creates a body on JS level for the element 
% this means to create a PIXI object for graphics
embodyElement(Type, Element, JSObject) :-
	element(Element, ID, Body, E),
	body(Body, BList),
	prop('gsnElemGenerator', JSFkt),
	apply(JSFkt, [Type, ID, BList, E], JSObject).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%% synchro with JS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

syncSubtree(update, _, Subtree) :-
	write('TAU SYNC'), write(Subtree),
	subtree(id, Subtree, ID),
	subtree(iv, Subtree, IV),
	state(update, callback(FktID)),
	prop(FktID, JSFkt),
	apply(JSFkt, [IV, ID], _),!.

% communicate JS the new child of a subtree
% +Parent : the subtree which gets the new child
% +NewSubtree: the Subtree which will be the child	
syncSubtree(child, root, _ ) :- !.

syncSubtree(_, [], _) :- !.

syncSubtree(child, Parent, ChildSubtree) :-
	subtree(id, Parent, ID),
	subtree(embodyData, ChildSubtree, ChID, M),
	state(child, callback(FktID)),
	prop(FktID, JSFkt),
	apply(JSFkt, [ID, ChID, M], _),!.

syncSubtree(Type, Subtree, JSObject) :-
	subtree(embodyData, Subtree, ID, M),
	state(Type, callback(FktID)),
	prop(FktID, JSFkt),
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
	%write('Taustate '), write(state(H, TauTerm)), % for debug
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

