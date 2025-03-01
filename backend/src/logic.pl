:- module(logic, [diagnose/2, all_symptoms/1]).

:- use_module(knowledge).  % Import the knowledge base

% Diagnosis rule: Find diseases matching all given symptoms
diagnose(Symptoms, Disease) :-
    disease(Disease),
    has_symptoms(Disease, DiseaseSymptoms),
    subset(Symptoms, DiseaseSymptoms).

% Retrieve all unique symptoms from the knowledge base
all_symptoms(SortedSymptoms) :-
    findall(Symptom, (disease(Disease), has_symptoms(Disease, Symptoms), member(Symptom, Symptoms)), SymptomList),
    sort(SymptomList, SortedSymptoms).

% Rule: Find diseases not matching the given symptoms
not_possible_disease(Symptoms, Disease) :-
    disease(Disease),
    has_symptoms(Disease, DiseaseSymptoms),
    \+ subset(Symptoms, DiseaseSymptoms).