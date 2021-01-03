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
% the logic is: either there is a new subtree which is identical to new goal
% or there is a new strategy or solution which needs update of a subtree
realSubtree(Level, Goal, Parent, Subtree) :- 
	embodyElement(goal, Goal, JSObject),
	embodySubtree(Subtree, JSObject, Level),
	syncSubtree(child, Parent, Subtree).

realChilds(Level, Parent) :-
	subtree(childs, Parent, Childs),
	realChilds2(Level, Parent, Childs).

realChilds2(Level, Parent, []).

realChilds2(Level, Parent, [C | T]) :-
	subtree(goal, C, Goal),
	realSubtree(Level, Goal, Parent, C ),
	realChilds2(Level, Parent, T).

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
	state(newSubtree, callback(FktID)),
	prop(FktID, JSFkt2),
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
	subtree(iv, Subtree, IV),
	subtree(embodyData, Subtree, ID, M),
	state(update, callback(FktID)),
	prop(FktID, JSFkt),
	apply(JSFkt, [ID, M, IV], _),!.

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

