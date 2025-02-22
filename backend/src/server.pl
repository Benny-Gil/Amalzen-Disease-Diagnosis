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
    format('Starting server on port 8090...~n'),
    catch(
        http_server(http_dispatch, [port(8090), bind('0.0.0.0')]),  % Bind to all network interfaces
        Error,
        (   format('Failed to start server: ~w~n', [Error]),
            fail
        )
    ),
    format('Server started at http://localhost:8090/~n'),
    wait_forever.

% Keep the server running
wait_forever :-
    repeat,
    sleep(1),
    fail.

stop_server :-
    http_stop_server(8090, []),
    format('Server stopped~n').

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
% :- multifile http:access_control_allow_origin/2.
:- start_server.
http:access_control_allow_origin(_, '*').