:- module(server, [start_server/0]).
:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_parameters)).
:- use_module(library(http/http_json)).
:- use_module(logic).  % Import diagnosis logic

:- use_module(json_injector). % Imported the json_injector module
:- use_module('knowledge.pl').  % Imported the disease_kb module

% Define HTTP handlers
:- http_handler(root(diagnose), diagnose_handler, []).
:- http_handler(root(symptoms), symptoms_handler, []).
:- http_handler(root(diseases), diseases_handler, []). % Added http handler to show list of diseases with symptoms

% Start the HTTP server on port 8090
start_server :-
    load_add_diseases_from_json, % Method for injecting diseases and symptoms from JSON file
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
    all_symptoms(SortedSymptoms),
    reply_json(SortedSymptoms).

% Handle /diseases to get the list of all diseases and their corresponding symptoms
diseases_handler(_) :-
    findall(json{disease: Disease, symptoms: Symptoms},
            (   disease_kb:disease(Disease),
                findall(Symptom, disease_kb:has_symptoms(Disease, Symptom), Symptoms)
            ),
            Diseases),
    reply_json(Diseases).