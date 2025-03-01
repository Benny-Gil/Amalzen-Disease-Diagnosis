:- initialization(main).
:- use_module(knowledge).
:- use_module(logic).
:- use_module(server).
:- use_module(json_injector). % Imported the json_injector module

% Main function
main :-
    load_add_diseases_from_json, % Method for injecting diseases and symptoms from JSON file
    writeln('\nDisease Diagnosis System'),
    writeln('1. Interactive Mode'),
    writeln('2. Start Web Server'),
    writeln('3. Exit'),
    read(Choice),
    handle_choice(Choice).

% Handle user choice
handle_choice(1) :-
    interactive_mode,
    main.
handle_choice(2) :-
    start_server.
handle_choice(3) :-
    writeln('Goodbye!'), halt.
handle_choice(_) :-
    writeln('Invalid choice, please try again.'), main.

% Interactive diagnosis mode
interactive_mode :-
    read_line_to_string(user_input, _),
    writeln('\nEnter symptoms separated by commas (e.g., fever,cough,sore_throat):'),
    read_line_to_string(user_input, Input),
    split_string(Input, ",", " ", SymptomStrings),
    maplist(atom_string, Symptoms, SymptomStrings),
    findall(Disease, diagnose(Symptoms, Disease), Diseases),
    (   Diseases = [] 
    ->  writeln('No matching disease found.')
    ;   format('Possible diseases: ~w~n', [Diseases])
    ).