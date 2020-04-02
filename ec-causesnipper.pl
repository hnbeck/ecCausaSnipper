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
:- module(ecCausaSnipper, [startGame/1]).

:- use_module(library(pengines)).
:- use_module(library(sandbox)).

% :- dynamic card/4.


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

createGame(_,_, go, 'EC Causa Snipper - Enjoy') :-
	pengine_self(PengineID),
	current_process(PengineID, _, _, _),
	!,
	debug(ld(redundant), 'game already created', []).

createGame(P1, P2, GSNTree, '') :-
	pengine_self(PengineID),
	startGame(GSNTree),
	thread_at_exit(killGame(PengineID)).

sandbox:safe_primitive(ecCausaSnipper:createGame(_,_,_,_)).

killGame(PengineID) :-
	current_process(PengineID, PID, _, _),
	process_kill(PID).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% persistence %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- dynamic current_store/2.
:- dynamic turnCount/1.
:- dynamic bombTurnCount/2.
:- dynamic gsnTree/2.

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
startGame(GSNTree) :-
	genTree('Test', GSNTree).


%%%%%%%%%%%%%%%%%%%%%%%%%%%% objects %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

genTree(ID, Tree) :-
	genElement(goal, ID, 1, RootGoal),
	Tree = gsnTree(ID, RootGoal).

% gsnElement(Type, ID, Childlist, Parent, Level, Tag).
genElement(Type, ID, L, element(Type, ID, [], _, L, isolated)).

child(element(_, _, C, _, _, _), C).
parent(element(_, _, _, P, _, _), P).
level(element(_, _, _, _, L, _), L).

%%%%%%%%%%%%%%%%%%%%%%%%%%%% ?(S,F) %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

leafdown(element) :-
	child(element, C),
	proper_length(C, 0).

isolated(element) :-
	leafdown(element),
	parent(element, P),
	var(P).

leaf(element) :-
	leafdown(element),
	not(isolated).


%%%%%%%%%%%%%%%%%%%%%%%%%%%% !(S,S*) %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%% +(S,F) %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


goalGrow(goal(ID, _, L), goal(ID, Strategy, L)) :-
	L2 is L + 1,
	genStrategy(ID, L2, Strategy).
	
% N is level

strategyGrow(strategy(ID, List, N), strategy(ID, List2, N2)) :-
	N2 is N + 1, 
	genGoal(ID, N2, Goal),
	append(List, [Goal], List2).


genEvidence(ID, N, evidence(ID, N)).

genQuestion(ID, N, question(ID, N)).




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%%%%%%%%%%%%%%%%%%%%%%%%%%% effect rules %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% Genertor is genGoal oder genStrategy
bombElement(Generator, Element) :-
	access(bombTurnCount(Generator, N1)),
	(N1 == 0 ->
		call(Generator, ID, 0, Element), 
		random_between(2, 4, Num), 
		store(bombTurnCount(Generator, Num));
		N2 is N - 1; 
		update(bombTurnCount(Generator, N2))
	).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%% game and winning rules %%%%%%%%%%%%%%%%%%%%%%%%%%%


% needed for pengine - declare safe predicate
% sandbox:safe_primitive(ecCausaSnipper:listDeck(_,_)).

