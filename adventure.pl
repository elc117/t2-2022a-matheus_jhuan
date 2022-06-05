/*
 * Adventure game
 * Fonte: https://www.cs.auckland.ac.nz/~j-hamer/07.363/explore.html
 * 
 */

:- dynamic location/2.
location(egg, pen).
location(you, house).

connected(yard, pen).
connected(yard, house).
connected(yard, woods).
is_connected(A, B) :-
    connected(B, A);
    connected(A,B).

all_connected(Current, All) :-
    findall(Other, is_connected(Current, Other), All).

:- dynamic closed/2.
closed(yard, pen).
closed(yard, house).
closed(yard, woods).
is_closed(A, B) :-
    closed(A, B);
    closed(B, A).

can_reach(A, B) :-
    is_connected(A, B),
    not(is_closed(A, B)).

all_reachable(Current, All) :-
    findall(Other, can_reach(Current, Other), All).

:- dynamic in_inventory/1.
in_inventory(bazooka).
in_inventory(key).
in_inventory(paperclip).
in_inventory(fire_extinguisher).
in_inventory(ring).

can_open_a_door(bazooka).
can_open_a_door(key).
can_open_a_door(paperclip).

write_location(Current, LocationToWrite) :-
    is_closed(Current, LocationToWrite),
    write(LocationToWrite), write(' [locked]'), nl,!.

write_location(Current, LocationToWrite) :-
    is_connected(Current, LocationToWrite),
    write(LocationToWrite), nl,!.

write_location(_, _).

look_around :-
    location(you, L),
    all_connected(L, All),
    forall(member(X,All), write_location(L, X)).

move(Player, Destination) :-
    location(Player, Current),
  	can_reach(Current, Destination),
  	retract(location(Player, Current)),
  	assert(location(Player, Destination)).

goto(X) :-
    move(you, X),
  	write('You are in the '), write(X), nl,
    !.
goto(X) :-
    location(you, Y),
    is_connected(X, Y),
    write('The gate is closed'), nl,
    !.
goto(_) :-
  	write('You can\'t get there from here.'), nl.

do_open(A, B) :-
  	(retract(closed(A, B)); retract(closed(B, A))),
	write('Gate opened!').

invalid_object(Object) :-
    not(in_inventory(Object)),
    write('Object not in inventory'),
    !.

try_open(Object, _, _) :-
    invalid_object(Object),
    !.

try_open(Object, _, _) :-
    not(can_open_a_door(Object)),
    write('Object can\'t open a door'),
    !.

try_open(_, Current, Destination) :-
    do_open(Current, Destination).

open(Destination, _) :-
    location(you, Current),
    not(is_connected(Current, Destination)),
    write('This door doesn\'t exists'),
    !.

open(Destination, _) :-
    location(you, Current),
    is_connected(Current, Destination),
    not(is_closed(Current, Destination)),
    write('This door is already opened'),
    !.

open(Destination, Object) :-
    location(you, Current),
    is_connected(Current, Destination),
    try_open(Object, Current, Destination),
    !.

:- dynamic stopped/0.
act([goto, Where]) :-
    goto(Where),
    !.

act([open, Door, with, Object]) :-
    open(Door, Object),
    !.

act([stop]) :-
    assert(stopped),
    write('Goodbye!'),
    nl,
    !.

act([where, am, i]) :-
    location(you, X),
    write(X),
    !.

act([look, around]) :-
    look_around,
    !.

act(_) :-
    write('Invalid action').

read_atoms(Atoms) :-
    read_line_to_codes(user_input, Cs),
    atom_codes(A, Cs),
    atomic_list_concat(Atoms, ' ', A).

loop :- stopped,!.
loop :-
  	prompt(_, 'Type a action...'),
  	read_atoms(Action),
  	act(Action),
  	loop.

start :-
    loop.
