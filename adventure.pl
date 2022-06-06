/*
 * Adventure game
 * Fonte: https://www.cs.auckland.ac.nz/~j-hamer/07.363/explore.html
 * 
 */

:- dynamic object_location/2.
object_location(egg, pen).
object_location(bazooka, yard).
object_location(key, house).

:- dynamic player_location/1.
player_location(house).

:- dynamic creature_location/2.
creature_location(zombie, pen).

connected(yard, pen).
connected(yard, house).
connected(yard, woods).
is_connected(A, B) :-
    connected(B, A);
    connected(A,B).

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

:- dynamic in_inventory/1.

can_open_a_door(axe).
can_open_a_door(bazooka).
can_open_a_door(key).
can_open_a_door(paperclip).

can_kill_creature(axe).
can_kill_creature(bazooka).

write_area(Current, LocationToWrite) :-
    is_closed(Current, LocationToWrite),
    write(LocationToWrite), write(' [locked]'), nl,!.

write_area(Current, LocationToWrite) :-
    is_connected(Current, LocationToWrite),
    creature_location(Creature, LocationToWrite),
    write(Creature), write(' is in '), write(LocationToWrite), nl, !.

write_area(Current, LocationToWrite) :-
    is_connected(Current, LocationToWrite),
    write(LocationToWrite), nl,!.

write_area(_, _).

write_areas :-
    write('Areas:'),
    nl,
    player_location(L),
    forall(is_connected(L, OtherLocation), write_area(L, OtherLocation)).

where_am_i :-
    write('You are in: '),
    player_location(X),
    write(X),
    nl.

write_object(Object) :-
    write(Object),
    nl.

write_objects :-
    player_location(L),
    findall(Object, object_location(Object, L), []),
    !. 

write_objects :-
    write('Objects: '),
    nl,
    player_location(L),
    forall(object_location(Object, L), write_object(Object)).

look_around :-
    where_am_i,
    write_areas,
    write_objects.

inventory :-
    findall(Object, in_inventory(Object), []),
    write('Inventory is empty'), nl.

inventory :-
    write('Inventory: '), nl,
    forall(in_inventory(Object), write_object(Object)).

get_object(Object) :-
    player_location(CurrentLocation),
    not(object_location(Object, CurrentLocation)),
    write('This object is not in this area'),
    nl,
    !.

get_object(Object) :-
    in_inventory(Object),
    write('You already have one of these'),
    nl,
    !.

get_object(Object) :-
    player_location(CurrentLocation),
    object_location(Object, CurrentLocation),
    retract(object_location(Object, CurrentLocation)),
    assert(in_inventory(Object)),
    write('You obtained '), write(Object),
    nl.

move(Destination) :-
    player_location(Current),
  	can_reach(Current, Destination),
  	retract(player_location(Current)),
  	assert(player_location(Destination)).

goto(X) :-
    player_location(Y),
    not(is_connected(Y, X)),
  	write('You can\'t get there from here.'),
    nl,
    !.
goto(X) :-
    creature_location(_, X),
    player_location(Y),
    write_area(Y, X),
    !.
goto(X) :-
    player_location(Y),
    is_closed(X, Y),
    write('The door is closed'), nl,
    !.
goto(X) :-
    move(X),
  	write('You are in the '), write(X), nl,
    !.

kill(Object, _) :-
    invalid_object(Object), !.

kill(Object, _) :-
    not(can_kill_creature(Object)),
    write('This object cannot kill anything'),
    !.

kill(_, Target) :-
    player_location(PlayerLocation),
    creature_location(Target, L),
    not(can_reach(PlayerLocation, L)),
    write('You can\'t reach it'),
    nl,
    !.

kill(_, Target) :-
    not(creature_location(Target, _)),
    write(Target),
    write(' is not around'),
    nl,
    !.

kill(_, Target) :-
    creature_location(Target, L),
    retract(creature_location(Target, L)),
    write('You killed the '), write(Target),
    nl.

do_open(A, B) :-
  	(retract(closed(A, B)); retract(closed(B, A))),
	write('Opened!').

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
    player_location(Current),
    not(is_connected(Current, Destination)),
    write('This door doesn\'t exists'),
    !.

open(Destination, _) :-
    player_location(Current),
    is_connected(Current, Destination),
    not(is_closed(Current, Destination)),
    write('This door is already opened'),
    !.

open(Destination, Object) :-
    player_location(Current),
    is_connected(Current, Destination),
    try_open(Object, Current, Destination),
    !.

:- dynamic stopped/0.

act([go, to, Where]) :-
    goto(Where),
    !.

act([open, Door, with, Object]) :-
    open(Door, Object),
    !.

act([kill, Target, with, Object]) :-
    kill(Object, Target),
    !.

act([show, inventory]) :-
    inventory,
    !.

act([stop]) :-
    assert(stopped),
    write('Goodbye!'),
    nl,
    !.

act([where, am, i]) :-
    where_am_i,
    !.

act([look, around]) :-
    look_around,
    !.

act([get, Object]) :-
    get_object(Object),
    !.

act([help]) :-
    findall(Action, act(Action), Actions),
    write(Actions),
    !.

act(_) :-
    write('Invalid action').

read_atoms(Atoms) :-
    read_line_to_codes(user_input, Cs),
    atom_codes(A, Cs),
    atomic_list_concat(Atoms, ' ', A).

loop :- stopped,!.
loop :- in_inventory(egg), write('Congratulations, you won!'), nl, !.
loop :-
  	prompt(_, 'Type an action...'),
  	read_atoms(Action),
  	act(Action),
  	loop.

start :-
    loop.
