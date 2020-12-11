%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% A simple web server for the ecCodeSnipper game
% 
%
% Autor: Hans N. Beck (c)
% Last Change: 01.01.2020
%
% License: MIT 
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_files)).
:- use_module(library(http/http_server_files)).
:- use_module(library(http/http_json)).

:- use_module(library(pengines)).
:- use_module(library(sandbox)).

:- use_module(pengine_sandbox:ecCausaSnipper).


:- http_handler(files(.), serve_files,  [prefix]).
:- http_handler(lib(.), serve_files,  [prefix]).
:- http_handler(image(.), serve_files,  [prefix]).
:- http_handler(sound(.), serve_files,  [prefix]).
:- http_handler(root(.), main,  [prefix]).

:- multifile http_json/1.

:- initialization(server(7007)).

http_json:json_type('application/x-javascript').
http_json:json_type('text/javascript').
http_json:json_type('text/x-javascript').
http_json:json_type('text/x-json').
http_json:json_type('text/x-prolog').
http_json:json_type('text/prolog').

http:location(files, '/web', []).
http:location(lib, '/lib', []).
http:location(image, '/graphics', []).
http:location(sound, '/sound', []).


server(Port) :- 
		writef("Starting Server at %d", [Port]),
		http_server(http_dispatch, [port(Port)]).


main(Request) :-
		http_reply_from_files('.', [], Request).

serve_files(Request) :-
		http_reply_from_files(web, [], Request).

serve_files(Request) :-
		http_reply_from_files(graphics, [], Request).

serve_files(Request) :-
		http_reply_from_files(sound, [], Request).

serve_files(Request) :-
		http_reply_from_files(lib, [], Request).

serve_files(Request) :-
		http_404([\p('Sorry could not find')], Request).