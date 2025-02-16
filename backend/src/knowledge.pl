% Define the module
:- module(disease_kb, [has_symptom/2, disease/1]).

% Disease facts
disease(dengue_fever).
disease(mpox).
disease(covid19).
disease(common_cold).
disease(measles).
disease(malaria).

% Symptoms for each disease
has_symptom(dengue_fever, fever).
has_symptom(dengue_fever, headache).
has_symptom(dengue_fever, muscle_pain).
has_symptom(dengue_fever, rash).
has_symptom(dengue_fever, bleeding).

has_symptom(mpox, rash).
has_symptom(mpox, fever).
has_symptom(mpox, headache).
has_symptom(mpox, chills).
has_symptom(mpox, lymph_nodes).

has_symptom(covid19, fever).
has_symptom(covid19, cough).
has_symptom(covid19, shortness_of_breath).
has_symptom(covid19, sore_throat).

has_symptom(common_cold, sneezing).
has_symptom(common_cold, runny_nose).
has_symptom(common_cold, sore_throat).
has_symptom(common_cold, cough).

has_symptom(measles, fever).
has_symptom(measles, cough).
has_symptom(measles, runny_nose).
has_symptom(measles, rash).
has_symptom(measles, red_eyes).

has_symptom(malaria, fever).
has_symptom(malaria, chills).
has_symptom(malaria, sweating).
has_symptom(malaria, headache).
has_symptom(malaria, nausea).
has_symptom(malaria, vomiting).

% Rule to find diseases based on symptoms
possible_disease(Symptom, Disease) :-
    has_symptom(Disease, Symptom).
