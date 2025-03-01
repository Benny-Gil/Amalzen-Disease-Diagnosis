% Define the module
:- module(disease_kb, [has_symptoms/2, disease/1, add_disease/2]). % Added add_disease function

% Dynamic predicates to allow modification at runtime
:- dynamic has_symptoms/2.
:- dynamic disease/1.

% Disease facts
disease(bone_cancer).
disease(cardiac_arrest).
disease(chickenpox).
disease(common_cold).
disease(covid19).
disease(dengue_fever).
disease(gallstones).
disease(hepatitis).
disease(hepatitisb).
disease(hiv).
disease(influenza).
disease(malaria).
disease(measles).
disease(mpox).
disease(necrosis).
disease(pertussis).
disease(rabies).
disease(salmonella).
disease(stoneman_disease).
disease(tetanus).
disease(tuberculosis).
disease(uti).

% Symptoms for each disease using lists
has_symptoms(bone_cancer, [persistent_bone_pain, swelling_or_lump, weakened_bones, unexplained_weight_loss, fever, night_sweats]).
has_symptoms(cardiac_arrest, [fatigue, dizziness, shortness_of_breath, nausea, chest_pain, heart_palpitations, losing_consciousness]).
has_symptoms(chickenpox, [fever, tiredness, headache, no_appetite, rash]).
has_symptoms(common_cold, [sneezing, runny_nose, sore_throat, cough]).
has_symptoms(covid19, [fever, cough, shortness_of_breath, sore_throat]).
has_symptoms(dengue_fever, [fever, headache, muscle_pain, rash, bleeding]).
has_symptoms(gallstones, [pain_between_shoulder_blades, pain_in_right_shoulder, nausea, vomiting, fever, chills, jaundice, abdominal_bloating, intolerance_of_fatty_foods, belching, indigestion]).
has_symptoms(hepatitis, [dark_urine, pale_stools, fatigue, itching, jaundice, losing_appetite, nausea, vomiting, weight_loss]).
has_symptoms(hepatitisb, [stomach_pain, dark_urine, fever, joint_pain, appetite_loss, upset_stomach, weakness, yellowing_eyes]).
has_symptoms(hiv_aids, [fever, headache, muscle_aches, rash, sore_throat, swollen_lymphs, diarrhea, weight_loss, cough, night_sweats]).
has_symptoms(influenza, [fever, chills, cough, sore_throat, runny_nose, muscle_pain, headache, fatigue, vomiting, diarrhea]).
has_symptoms(malaria, [fever, chills, sweating, headache, nausea, vomiting]).
has_symptoms(measles, [fever, cough, runny_nose, rash, red_eyes]).
has_symptoms(mpox, [rash, fever, headache, chills, lymph_nodes]).
has_symptoms(necrosis, [pain_and_swelling, skin_discoloration, numbness, open_sores, foul_smelling_discharge]).
has_symptoms(pertussis, [severe_cough, runny_nose, vomiting, fatigue, difficulty_breathing]).
has_symptoms(rabies, [fever, muscle_spasms, hydrophobia, hallucinations, paralysis]).
has_symptoms(salmonella, [diarrhea, stomach_cramps, fever, nausea, vomiting, chills, headache, blood_in_stool]).
has_symptoms(stoneman_disease, [malformed_big_toes, painful_swelling, progressive_joint_stiffness, difficulty_moving, restricted_jaw_movement]).
has_symptoms(tetanus, [muscle_stiffness, lockjaw, difficulty_swallowing, seizures, fever]).
has_symptoms(tuberculosis, [cough, chest_pain, coughing_up_blood, fatigue, weight_loss, no_appetite, chills, fever]).
has_symptoms(uti, [painful_urination, increased_urination, lower_back_pain, bloody_urine, pelvic_pain]).

% Rule to find diseases based on symptoms
possible_disease(Symptom, Disease) :-
    has_symptoms(Disease, Symptom).

% Asserting new diseases and its symptoms with debug statements
add_disease(Disease, Symptoms) :-
    format('Adding disease: ~w with symptoms: ~w~n', [Disease, Symptoms]),
    (   assertz(disease(Disease))
    ->  format('Successfully asserted disease: ~w~n', [Disease])
    ;   format('Failed to assert disease: ~w~n', [Disease])
    ),
    (   assertz(has_symptoms(Disease, Symptoms))
    ->  format('Successfully asserted symptoms for disease: ~w~n', [Disease])
    ;   format('Failed to assert symptoms for disease: ~w~n', [Disease])
    ).