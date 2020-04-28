
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
	holdTerm(0, noGSNElement),
	write('Tau Prolog: done3').

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

newTree(Tree) :-
	explanation(argument(10, rl),
				argument(4, mt), 
				argument(6, ph),
				Explanation),
	body(1,1000, 0, Body),
	newGSN(goal, Body, Explanation, Goal),
	embodyGoal(Goal, JSObject),
	goalAsSubtree(Goal, root, root, Tree),
	embodySubtree(Tree, JSObject, 1),
	holdTerm( Tree, gsnTree),
	addStrategy(1).

showTree :-
	status(gsnTree, Tree),
	writeHTML('Tauout', Tree,_).

% the element of a goal and a stragegy are the explanation types
% rl = rule = normative setting, mt= measurement, ph = phenomenon.
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
	state(noGSNElement, No),
	No2 is No + 1, 
	holdTerm(No2, noGSNElement),
	Element =.. [Type, No2, Body, Explanation].

% new goal bedeutet new subtree - immer
newGoal(Level, Explanation, Parent, Parent2, NewSubtree) :-
	body(Level, 1000, _, Body),
	newGSN(goal, Body, Explanation, Goal),
	embodyGoal(Goal, JSObject),
	goalAsSubtree(Goal, Parent, Parent2, NewSubtree),
	embodySubtree(NewSubtree, JSObject, Level),
	updateEmbodyChild(Parent2, NewSubtree).

newStrategy(Level, Explanation, Subtree, Subtree2) :-
	body(Level, 1000, _, Body),
	newGSN(strategy, Body, Explanation, Strategy),
	embodyStrategy(Strategy, JSObject),
	subtreePlusSt(Subtree, Strategy, Subtree2),
	updateEmbodySubtree(Subtree2,  JSObject).


%%%%%%%%%%%%%%%%%%%% modifications %%%%%%%%%%%%%%%%%%%%%%%%%%%%

% in this form more a matter of test
% StID is subtree ID = ID of head goal of this subtree
addStrategy(StID) :-
	state(gsnTree, Tree),
	explanation(argument(10, rl),
				argument(4, mt), 
				argument(6, ph),
				Explanation),
	updateSubtree(StID, Explanation, 2, Level2, Tree, Tree2),
	holdTerm(Tree2, gsnTree).

%%% go througt the tree
updateSubtree(ID, E, Level, Level, Tree, Tree2) :-
	subtree(id, Tree, ID),
	newStrategy(Level, E, Tree, Tree2),!.

updateSubtree(ID, E, Level, Level2, Tree, Tree2) :-
	subtree(childs, Tree, Childs),
	updateChilds(ID, E, Childs, Level, Level2, Tree, Tree2).

% update the childs of a tree
updateChilds(ID, E, [], Level, Level, Tree, Tree) :- false.

updateChilds(ID, E, [H | T], Level, Level2, Tree, Tree2) :-
	updateSubTree(ID, E, H,  Level, Level2, Tree, Tree2),!.

updateChilds(ID, E, [H | T], Level, Level4, Tree, Tree2) :-
	Level2 is Level + 1, 
	updateChilds(ID, E, T, Level3, Level4, Tree, Tree2).




goalAsSubtree(Goal, root, _, Subtree) :-
	goal(mass, Goal, M),
	Subtree = subtree(Goal, [], [], M, root),!. 

goalAsSubtree(Goal, Parent, Parent3, Subtree) :-
	goal(mass, Goal, M),
	Subtree = subtree(Goal, [], [], M, Parent), 
	subtree(childs, Parent, Childs), 
	goal(mass, Parent, M2),
	M3 is M2 + M, 
	subtree(mass, M3, Parent, Parent2),
	append(Childs, [Subtree], Childs2), 
	subtree(childs, Child2, Parent2, Parent3).


% a goal gets a strategy, means the related subtree gets the strategy
% per definition there cannot be childs
subtreePlusSt(subtree(Goal, [], [], M, Parent), Strategy, Subtree2) :-
	strategy(mass, Strategy, M2),
	M3 is M + M2, 
	Subtree2 = subtree(Goal, Strategy, [], M3, Parent).


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

% generate PIXI object from this
embodyGoal(Goal, JSObject) :-
	goal(id, Goal, ID),
	goal(body, Goal, Body),
	goal(exp, Goal, E), 
	body(Body, BList),
	prop('gsnElemGenerator', JSFkt),
	apply(JSFkt, ['goal', ID, BList, E], JSObject).

% generate Pixi Object from this
embodyStrategy(Strategy, JSObject) :-
	strategy(id, Strategy, ID),
	strategy(body, Strategy, Body),
	strategy(exp, Strategy, E),
	body(Body, BList),
	prop('gsnElemGenerator', JSFkt),
	apply(JSFkt, ['strategy', ID, BList, E], JSObject).

% without stragegy there can be no childs
embodySubtree(subtree(Goal, [], [], _, _), JSObject,  Level) :-
	goal(id, Goal, ID),
	goal(mass, Goal, Mass),
	ST = [JSObject, [], [], Mass],
	prop('addSubtree', JSFkt2),
	apply(JSFkt2, [Level, ST, ID], _),!.

% thats the pattern if a new strategy is there
	
updateEmbodyChild(Parent2, NewSubtree) :-
	subtree(id, Parent2, ID),
	subtree(id, NewSubtree, NewID),
	prop('updateSubtreeChild', JSFkt),
	apply(JSFkt, [ID, NewID], _).

updateEmbodySubtree(Subtree, JSObject) :-
	subtree(id, Subtree, ID),
	subtree(mass, Subtree, M),
	prop('updateSubtreeStrgy', JSFkt),
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

