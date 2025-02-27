:- module(json_injector, [load_add_diseases_from_json/0]).

:- use_module(library(http/json)).
:- use_module('knowledge.pl'). 

% Load diseases and symptoms from the JSON file
load_add_diseases_from_json :-
    open('./diseases.json', read, Stream),
    json_read_dict(Stream, Dict),
    close(Stream),
    dict_pairs(Dict, _, Pairs),
    maplist(add_disease_from_pair, Pairs).

% Add disease from a key-value pair
add_disease_from_pair(Disease-Symptoms) :-
    atom_string(DiseaseAtom, Disease),
    maplist(atom_string, SymptomsAtoms, Symptoms),
    add_disease(DiseaseAtom, SymptomsAtoms).