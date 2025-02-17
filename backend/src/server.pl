:- module(server, [start_server/0]).
:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_parameters)).
:- use_module(library(http/http_json)).
:- use_module(library(http/http_cors)).  % Import CORS library
:- use_module(logic).  % Import diagnosis logic

% Set CORS setting to allow all origins
:- set_setting(http:cors, [*]).


% Define HTTP handlers
:- http_handler(root(diagnose), diagnose_handler, []).
:- http_handler(root(symptoms), symptoms_handler, []).

% Start the HTTP server on port 8090
start_server :-
    http_server(http_dispatch, [port(8090)]),
    format('Server started at http://localhost:8090/~n').

% Handle /diagnose?symptoms=symptom1,symptom2
diagnose_handler(Request) :-
    cors_enable(Request, [methods([get])]),  % Enable CORS for GET requests
    (   http_parameters(Request, [
            symptoms(SymptomListStr, [optional(true), default("")])
        ])
    ->  split_string(SymptomListStr, ",", " ", SymptomAtoms),  % Convert input to list
        maplist(atom_string, Symptoms, SymptomAtoms),
        findall(Disease, diagnose(Symptoms, Disease), Diseases),
        reply_json(Diseases)
    ;   reply_json(json{error: "Invalid parameters"})
    ).

% Handle /symptoms to get the list of all symptoms
symptoms_handler(_) :-
    cors_enable,
    all_symptoms(SortedSymptoms),
    reply_json(SortedSymptoms).

% Enable CORS for all responses
:- multifile http:access_control_allow_origin/2.
http:access_control_allow_origin(_, '*').