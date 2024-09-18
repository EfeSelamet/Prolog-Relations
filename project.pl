% Define dynamic predicates
:- dynamic person/6.
:- dynamic parent/2.
:- dynamic married/2.
:- dynamic male/1.
:- dynamic female/1.
:- dynamic addChild/3.

% Define basic predicates
male() :- male(X).
female() :- female(X).

% Familial relationships
baba(X,Y) :- addChild(Y,X,_), male(X).
anne(X,Y) :- addChild(Y,_,X), female(X).

ogul(X,Y) :- addChild(X,Y,_), male(X); addChild(X,_,Y), male(X).
kiz(X,Y) :- addChild(X,Y,_), female(X); addChild(X,_,Y), female(X).

parent(X,Y) :- baba(X,Y); anne(X,Y).
childOf(X,Y) :- parent(Y,X).

married(X,Y) :- addChild(_,X,Y).

erkekKardes(X,Y) :- parent(Z,X), parent(Z,Y), male(X), X \= Y.
kizKardes(X,Y) :- parent(Z,X), parent(Z,Y), female(X), X \= Y.

kardes(X,Y) :- erkekKardes(X,Y); kizKardes(X,Y).

older(X,Y):-age(X, Age1) > age(Y, Age2).
abla(X,Y) :- kizKardes(X,Y), older(X,Y).
abi(X,Y) :- erkekKardes(X,Y), older(X,Y).

amca(X,Y) :- baba(Z,Y), erkekKardes(X,Z).
hala(X,Y) :- baba(Z,Y), kizKardes(X,Z).
teyze(X,Y) :- anne(Z,Y), kizKardes(X,Z).
dayi(X,Y) :- anne(Z,Y), erkekKardes(X,Z).

dede(X,Y) :- baba(X,Z), parent(Z,Y).
nene(X,Y) :- anne(X,Z), parent(Z,Y).

yegen(X,Y) :- kardes(Y,Z), childOf(X,Z).
kuzen(X,Y) :- parent(Z,Y), kardes(T,Z), childOf(X,T).

eniste(X,Y) :- (teyze(Z,Y); kizKardes(Z,Y); abla(Z,Y); hala(Z,Y)), married(X,Z).
yenge(X,Y) :- (amca(Z,Y); erkekKardes(Z,Y); abi(Z,Y)), married(X,Z).

kayinvalide(X,Y) :- married(Z,Y), anne(X,Z).
kayinpeder(X,Y) :- married(Z,Y), baba(X,Z).
gelin(X,Y) :- childOf(Z,Y), married(X,Z), female(X).
damat(X,Y) :- childOf(X,Y), married(X,Z), male(X).

bacanak(X,Y) :- X \= Y, married(Y,Z), (kizKardes(Z,W); abla(Z,W)), married(X,W).
baldiz(X,Y) :- married(Z,Y), kizKardes(X,Z); married(Y,Z), abla(X,Z).
elti(X,Y) :- female(X), female(Y), married(Y,Z), (erkekKardes(Z,W); abi(Z,W)), married(X,W).
kayinbirader(X,Y) :- male(X), married(Y,Z), (erkekKardes(X,Z); abi(X,Z)).

person('Murat Aslan',_,_, 'male', 1950, 2000).
male('Murat Aslan').
person('Sedanur Aslan',_,_, 'female', 1955, 2010).
female('Sedanur Aslan').

% Rules for adding new members
add_person(NameSurname, FatherName, MotherName, Gender, BirthDate, DeathDate) :-
    assertz(addChild(NameSurname,FatherName,MotherName)),
    (Gender = 'male' -> assertz(male(NameSurname)); assertz(female(NameSurname))),
    assertz(person(NameSurname, FatherName, MotherName, Gender, BirthDate, DeathDate)).

add_new_person(NameSurname, FatherName, MotherName, Gender, BirthDate, DeathDate):-
    write('Enter name: '), read(NameSurname),
    write('Enter father\'s name: '), read(FatherName),
    write('Enter mother\'s name: '), read(MotherName),
    write('Enter gender (male/female): '), read(Gender),
    write('Enter birth date (year): '), read(BirthDate),
    write('Enter death date (year or none if alive): '), read(DeathDate),
    add_person(NameSurname, FatherName, MotherName, Gender, BirthDate, DeathDate),
    write('Person added successfully.').

add_parent_child(Parent, Child) :-
    assertz(parent(Parent, Child)).

% Rule to check if a person is alive
is_alive(NameSurname) :-
    person(NameSurname, _, _, _, _, DeathDate),
    DeathDate = 'none'.

% Rule to calculate age
calculate_age(NameSurname, Age) :-
    person(NameSurname, _, _, _, BirthDate, DeathDate),
    (DeathDate = 'none'
    -> get_current_date(CurrentDate),
       date_diff(BirthDate, CurrentDate, Age)
    ; date_diff(BirthDate, DeathDate, Age)
    ).

% Get current date (stub, should be implemented)
get_current_date(2024).

% Calculate date difference (stub, needs proper implementation)
date_diff(Year1, Year2, Age) :-
    Age is Year2 - Year1.

% Printing the family tree
print_family_tree :-
    findall(NameSurname, person(NameSurname, _, _, _, _, _), Members),
    print_members(Members).

print_members([]).
print_members([NameSurname|Rest]) :-
    format('~w~n', [NameSurname]),
    print_children(NameSurname),
    print_members(Rest).

print_children(Parent) :-
    findall(Child, parent(Parent, Child), Children),
    print_children_list(Children).

print_children_list([]).
print_children_list([Child|Rest]) :-
    format('  ~w~n', [Child]),
    print_children(Child),
    print_children_list(Rest).


% Predicate to add a marriage
add_marriage() :-
    write('1)Add new person to marry: '),nl,
    write('2)Marry from family: '),
    read(Choice),
    marrige_choice(Choice).

    marrige_choice(1):-
        write('Enter name: '), read(NameSurname),
        write('Enter father\'s name: '), read(FatherName),
        write('Enter mother\'s name: '), read(MotherName),
        write('Enter gender (male/female): '), read(Gender),
        write('Enter birth date (year): '), read(BirthDate),
        write('Enter death date (year or none if alive): '), read(DeathDate),
        add_person(NameSurname, FatherName, MotherName, Gender, BirthDate, DeathDate),
        write('Person added successfully.'),
        write('Enter person 2: '),
        read(Person2),
        valid_marriage(NameSurname, Person2),
        assertz(married(NameSurname, Person2)),
        assertz(married(Person2, NameSurname)).

    marrige_choice(2):-
        write('Enter first person name: '), read(Person1),
        write('Enter second person name: '), read(Person2),
        valid_marriage(Person1, Person2),
        assertz(married(Person1, Person2)),
        assertz(married(Person2, Person1)).


% Rule to check if a marriage is valid
valid_marriage(Person1, Person2) :-
    not(invalid_relationship(Person1, Person2)),
    age(Person1, Age1), Age1 >= 18,
    age(Person2, Age2), Age2 >= 18.

invalid_relationship(Person1, Person2) :-
    parent(Person1, Person2);
    anne(Person1, Person2);
    baba(Person1, Person2);
    abla(Person1, Person2);
    kizKardes(Person1, Person2);
    erkekKardes(Person1, Person2);
    abi(Person1, Person2);
    ogul(Person1, Person2);
    kiz(Person1, Person2);
    amca(Person1, Person2);
    hala(Person1, Person2);
    dayi(Person1, Person2);
    teyze(Person1, Person2);
    yegen(Person1, Person2);
    married(Person1, X);
    married(Person2, X).

% Add more invalid relationships as needed

% Age calculation rule
age(NameSurname, Age) :-
    calculate_age(NameSurname, Age).

% CLI for running the family tree application
run :-
    write('Family Tree Application'), nl,
    menu.

menu :-
    write('1. Add Person'), nl,
    write('2. Add Parent-Child Relationship'), nl,
    write('3. Update Birthdate'), nl,
    write('4. Update Deathdate'), nl,
    write('5. Check if Person is Alive'), nl,
    write('6. Calculate Age'), nl,
    write('7. Print Family Tree'), nl,
    write('8. Add Marriage'), nl,
    write('9. Check Relationships'), nl,
    write('10. Exit'), nl,
    write('Choose an option: '),
    read(Choice),
    handle_choice(Choice).

handle_choice(1) :-
    write('Enter name: '), read(NameSurname),
    write('Enter father\'s name: '), read(FatherName),
    write('Enter mother\'s name: '), read(MotherName),
    write('Enter gender (male/female): '), read(Gender),
    write('Enter birth date (year): '), read(BirthDate),
    write('Enter death date (year or none if alive): '), read(DeathDate),
    add_person(NameSurname, FatherName, MotherName, Gender, BirthDate, DeathDate),
    write('Person added successfully.'), nl, menu.

handle_choice(2) :-
    write('Enter parent name: '), read(Parent),
    write('Enter child name: '), read(Child),
    add_parent_child(Parent, Child),
    write('Parent-child relationship added successfully.'), nl, menu.

handle_choice(3) :-
    write('Enter name: '), read(NameSurname),
    write('Enter new birth date (year): '), read(NewBirthDate),
    update_birthdate(NameSurname, NewBirthDate),
    write('Birthdate updated successfully.'), nl, menu.

handle_choice(4) :-
    write('Enter name: '), read(NameSurname),
    write('Enter new death date (year or none if alive): '), read(NewDeathDate),
    update_deathdate(NameSurname, NewDeathDate),
    write('Deathdate updated successfully.'), nl, menu.

handle_choice(5) :-
    write('Enter name: '), read(NameSurname),
    (is_alive(NameSurname) -> write(NameSurname), write(' is alive.'), nl
    ; write(NameSurname), write(' is deceased.'), nl),
    menu.

handle_choice(6) :-
    write('Enter name: '), read(NameSurname),
    calculate_age(NameSurname, Age),
    write(NameSurname), write(' is '), write(Age), write(' years old.'), nl, menu.

handle_choice(7) :-
    print_family_tree, menu.

handle_choice(8) :-
    (add_marriage() -> write('Marriage added successfully.'), nl
    ; write('Invalid marriage. Check age or relationship constraints.'), nl),
    menu.

handle_choice(9) :-
    write('Enter first person name: '), read(Person1),
    write('Enter second person name: '), read(Person2),
    find_relationships(Person1, Person2),
    menu.

handle_choice(10) :-
    write('Exiting...'), nl.

handle_choice(_) :-
    write('Invalid choice, try again.'), nl, menu.

% Update predicates
update_birthdate(NameSurname, NewBirthDate) :-
    retract(person(NameSurname, FatherName, MotherName, Gender, _, DeathDate)),
    assertz(person(NameSurname, FatherName, MotherName, Gender, NewBirthDate, DeathDate)).

update_deathdate(NameSurname, NewDeathDate) :-
    retract(person(NameSurname, FatherName, MotherName, Gender, BirthDate, _)),
    assertz(person(NameSurname, FatherName, MotherName, Gender, BirthDate, NewDeathDate)).

% New predicate to find and display relationships between two people
find_relationships(Person1, Person2) :-
    (parent(Person1, Person2) -> write(Person1), write(' is the parent of '), write(Person2), nl;
    parent(Person2, Person1) -> write(Person1), write(' is a child of '), write(Person2), nl;
    baba(Person1, Person2) -> write(Person1), write(' is the baba of '), write(Person2), nl;
    anne(Person1, Person2) -> write(Person1), write(' is the anne of '), write(Person2), nl;
    ogul(Person1, Person2) -> write(Person1), write(' is the ogul of '), write(Person2), nl;
    kiz(Person1, Person2) -> write(Person1), write(' is the kiz of '), write(Person2), nl;
    kardes(Person1, Person2) -> write(Person1), write(' is the kardes of '), write(Person2), nl;
    abi(Person1, Person2) -> write(Person1), write(' is the abi of '), write(Person2), nl;
    abla(Person1, Person2) -> write(Person1), write(' is the abla of '), write(Person2), nl;
    amca(Person1, Person2) -> write(Person1), write(' is the amca of '), write(Person2), nl;
    hala(Person1, Person2) -> write(Person1), write(' is the hala of '), write(Person2), nl;
    dayi(Person1, Person2) -> write(Person1), write(' is the dayi of '), write(Person2), nl;
    teyze(Person1, Person2) -> write(Person1), write(' is the teyze of '), write(Person2), nl;
    dede(Person1, Person2) -> write(Person1), write(' is the buyukbaba of '), write(Person2), nl;
    nene(Person1, Person2) -> write(Person1), write(' is the buyukanne of '), write(Person2), nl;
    yegen(Person1, Person2) -> write(Person1), write(' is the yegen of '), write(Person2), nl;
    kuzen(Person1, Person2) -> write(Person1), write(' is a kuzen of '), write(Person2), nl;
    eniste(Person1, Person2) -> write(Person1), write(' is the eniste of '), write(Person2), nl;
    yenge(Person1, Person2) -> write(Person1), write(' is the yenge of '), write(Person2), nl;
    kayinvalide(Person1, Person2) -> write(Person1), write(' is the kayinvalide of '), write(Person2), nl;
    kayinpeder(Person1, Person2) -> write(Person1), write(' is the kayinpeder of'), write(Person2), nl;
    gelin(Person1, Person2) -> write(Person1), write(' is the  gelin of '), write(Person2), nl;
    damat(Person1, Person2) -> write(Person1), write(' is the damat of '), write(Person2), nl;
    bacanak(Person1, Person2) -> write(Person1), write(' is the bacanak of '), write(Person2), nl;
    baldiz(Person1, Person2) -> write(Person1), write(' is the baldiz of '), write(Person2), nl;
    elti(Person1, Person2) -> write(Person1), write(' is the elti of '), write(Person2), nl;
    kayinbirader(Person1, Person2) -> write(Person1), write(' is the kayinbirader of '), write(Person2), nl;
    married(Person1, Person2) -> write(Person1), write(' is married to '), write(Person2), nl;
    write('No direct relationship found between '), write(Person1), write(' and '), write(Person2), nl).
