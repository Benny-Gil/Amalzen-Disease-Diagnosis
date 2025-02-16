:- module(logic, [diagnose/2]).

:- use_module(knowledge).  % Import the knowledge base

% Diagnosis rule: Find diseases matching all given symptoms
diagnose(Symptoms, Disease) :-
    disease(Disease),
    forall(member(Symptom, Symptoms), has_symptom(Disease, Symptom)).
