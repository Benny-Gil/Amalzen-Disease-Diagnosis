:- module(server, [start_server/0]).
:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_parameters)).
:- use_module(library(http/http_json)).
:- use_module(logic).  % Import diagnosis logic

% Define HTTP handlers
:- http_handler(root(diagnose), diagnose_handler, []).

% Start the HTTP server on port 8080
start_server :-
    http_server(http_dispatch, [port(8090)]),
    format('Server started at http://localhost:8090/~n').

% Handle /diagnose?symptoms=symptom1,symptom2
diagnose_handler(Request) :-
    (   http_parameters(Request, [
            symptoms(SymptomListStr, [optional(true), default("")])
        ])
    ->  split_string(SymptomListStr, ",", " ", SymptomAtoms),  % Convert input to list
        maplist(atom_string, Symptoms, SymptomAtoms),
        findall(Disease, diagnose(Symptoms, Disease), Diseases),
        reply_json(Diseases)
    ;   reply_json(json{error: "Invalid parameters"})
    ).