% Define the module
:- module(disease_kb, [has_symptoms/2, disease/1]).

% Disease facts
disease(dengue_fever).
disease(mpox).
disease(covid19).
disease(common_cold).
disease(measles).
disease(malaria).

% Symptoms for each disease using lists
has_symptoms(dengue_fever, [fever, headache, muscle_pain, rash, bleeding]).
has_symptoms(mpox, [rash, fever, headache, chills, lymph_nodes]).
has_symptoms(covid19, [fever, cough, shortness_of_breath, sore_throat]).
has_symptoms(common_cold, [sneezing, runny_nose, sore_throat, cough]).
has_symptoms(measles, [fever, cough, runny_nose, rash, red_eyes]).
has_symptoms(malaria, [fever, chills, sweating, headache, nausea, vomiting]).


% Rule to find diseases based on symptoms
possible_disease(Symptom, Disease) :-
    has_symptoms(Disease, Symptom).
