# Querying the Knowledge Base

This guide explains how to query the disease knowledge base (knowledge.pl) in SWI-Prolog.

## Loading the Knowledge Base

First, start SWI-Prolog and load the knowledge base:

```prolog
$ cd backend/src
$ swipl
?- [knowledge].
```

## Basic Queries

Here are some useful queries you can run against the knowledge base:

### List All Diseases

To get a list of all diseases in the knowledge base:

```prolog
?- disease(X).
```

This will return each disease one by one. Press `;` to see the next result or `.` to stop.

### Find Symptoms of a Disease

To find the symptoms of a specific disease (e.g., covid19):

```prolog
?- has_symptoms(covid19, Symptoms).
```

This will return a list of symptoms for COVID-19.

### Find Diseases with Specific Symptoms

To find diseases that have specific symptoms:

#### Single Symptom

To find diseases that have fever as a symptom:

```prolog
?- has_symptoms(Disease, Symptoms), 
   member(fever, Symptoms).
```

#### Multiple Symptoms

To find diseases that have both fever and cough:

```prolog
?- has_symptoms(Disease, Symptoms), 
   member(fever, Symptoms), 
   member(cough, Symptoms).
```

### Find Matching Disease Based on a List of Symptoms

If you have a list of symptoms and want to find matching diseases:

```prolog
?- has_symptoms(Disease, DiseaseSymptoms),
   intersection([fever, cough, headache], DiseaseSymptoms, Common),
   length(Common, L),
   L > 0.
```

This will find diseases that share at least one symptom with the provided list.

## Advanced Queries

### Calculate Symptom Match Percentage

To calculate what percentage of a disease's symptoms match the given symptoms:

```prolog
?- has_symptoms(Disease, DiseaseSymptoms),
   intersection([fever, cough, headache], DiseaseSymptoms, Common),
   length(Common, MatchCount),
   length(DiseaseSymptoms, TotalCount),
   Percentage is (MatchCount / TotalCount) * 100.
```

### Find Best Matching Disease

To find the disease with the highest symptom match percentage:

```prolog
?- findall(
      (Disease, Percentage),
      (has_symptoms(Disease, DiseaseSymptoms),
       intersection([fever, cough, headache], DiseaseSymptoms, Common),
       length(Common, MatchCount),
       length(DiseaseSymptoms, TotalCount),
       Percentage is (MatchCount / TotalCount) * 100),
      Results),
   sort(2, @>=, Results, [BestMatch|_]).
```

This will return the disease with the highest match percentage first.
```

## Example Session

Here's an example session showing how to query the knowledge base:

```
?- [knowledge].
true.

?- disease(X).
X = dengue_fever ;
X = mpox ;
X = covid19 ;
X = common_cold ;
X = measles ;
X = malaria ;
X = uti.

?- has_symptoms(covid19, Symptoms).
Symptoms = [fever, cough, shortness_of_breath, sore_throat].

?- has_symptoms(Disease, Symptoms), member(fever, Symptoms), member(cough, Symptoms).
Disease = covid19,
Symptoms = [fever, cough, shortness_of_breath, sore_throat] ;
Disease = measles,
Symptoms = [fever, cough, runny_nose, rash, red_eyes] ;
false.
