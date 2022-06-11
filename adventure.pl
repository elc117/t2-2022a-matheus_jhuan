/*
 * Adventure game
 * Fonte de inspiração: https://www.cs.auckland.ac.nz/~j-hamer/07.363/explore.html
 * 
 */

/*Game state and rules*/

:- dynamic object_location/2.
object_location(egg, pen).
object_location(bazooka, woods).
object_location(key, house).

:- dynamic player_location/1.
player_location(house).

:- dynamic creature_location/2.
creature_location(zombie, pen).

connected(yard, pen).
connected(yard, house).
connected(yard, woods).

:- dynamic closed/2.
closed(yard, pen).
closed(yard, house).
closed(yard, woods).

:- dynamic in_inventory/1.

can_open_a_door(axe).
can_open_a_door(bazooka).
can_open_a_door(key).
can_open_a_door(paperclip).

can_kill_creature(axe).
can_kill_creature(bazooka).

/*Useful predicates*/

is_connected(A, B) :-
    connected(B, A);
    connected(A,B).

is_closed(A, B) :-
    closed(A, B);
    closed(B, A).

can_reach(A, B) :-
    is_connected(A, B),
    not(is_closed(A, B)).

player_can_see_creature(Creature) :-
    creature_location(Creature, Location),
    player_location(Player),
    can_reach(Location, Player).

:- dynamic stopped/0.

/*Base action predicates*/

move_to(Destination) :-
    player_location(Current),
  	can_reach(Current, Destination),
    \+ creature_location(_, Destination),
  	retract(player_location(Current)),
  	assert(player_location(Destination)).

open(Destination, Object) :-
    in_inventory(Object),
    can_open_a_door(Object),
    player_location(Player),
    is_connected(Player, Destination),
    is_closed(Player, Destination),
    (retract(closed(Player, Destination)); retract(closed(Destination, Player))).

get_object(Object) :-
    player_location(CurrentLocation),
    object_location(Object, CurrentLocation),
    retract(object_location(Object, CurrentLocation)),
    assert(in_inventory(Object)).

kill(Object, Target) :-
    can_kill_creature(Object),
    player_location(PlayerLocation),
    creature_location(Target, L),
    can_reach(PlayerLocation, L),
    retract(creature_location(Target, L)).

ask_to_leave(Creature) :-
    player_can_see_creature(Creature),
    creature_location(Creature, L),
    retract(creature_location(Creature, L)).

act([go, to, Where]) :- move_to(Where).
act([open, Door, with, Object]) :- open(Door, Object).
act([get, Object]) :- get_object(Object).
act([kill, Target, with, Object]) :- kill(Object, Target).
act([can, you, leave, Creature]) :- ask_to_leave(Creature).

/*I/O action predicates*/
write_area(Current, LocationToWrite) :-
    is_closed(Current, LocationToWrite),
    write(LocationToWrite), write(' [locked]'), nl,
    !.
write_area(Current, LocationToWrite) :-
    is_connected(Current, LocationToWrite),
    creature_location(Creature, LocationToWrite),
    write('The '), write(Creature), write(' is blocking the entrance to the '), write(LocationToWrite), nl,
    !.
write_area(Current, LocationToWrite) :-
    is_connected(Current, LocationToWrite),
    write(LocationToWrite), nl,
    !.
write_area(_, _).

write_areas :-
    write('Areas:'), nl,
    player_location(L),
    forall(is_connected(L, OtherLocation), write_area(L, OtherLocation)).

where_am_i :-
    player_location(X),
    write('You are in: '), write(X), nl.

write_object(Object) :- write(Object), nl.

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
    write('Inventory is empty'), nl,
    !.
inventory :-
    write('Inventory: '), nl,
    forall(in_inventory(Object), write_object(Object)).

io_get_object(Object) :-
    get_object(Object),
    write('You obtained '), write(Object), nl,
    !.
io_get_object(Object) :-
    player_location(CurrentLocation),
    not(object_location(Object, CurrentLocation)),
    write('You can\'t find '), write(Object), nl,
    !.
io_get_object(Object) :-
    in_inventory(Object),
    write('You already have one of these'), nl,
    !.

io_move_to(Destination) :-
    move_to(Destination),
  	write('You are now in the '), write(Destination), nl,
    !.
io_move_to(Destination) :-
    player_location(Player),
    not(is_connected(Player, Destination)),
  	write('You can\'t get there from here.'), nl,
    !.
io_move_to(Destination) :-
    player_location(Player),
    is_closed(Player, Destination),
    write('The door is closed'), nl,
    !.
io_move_to(Destination) :-
    creature_location(Creature, Destination),
    write('The '), write(Creature), write(' is blocking the entrance to the '), write(Destination), nl,
    !.

io_kill(Object, Target) :-
    kill(Object, Target),
    write('You killed the '), write(Target), nl,
    !.
io_kill(Object, _) :-
    not(in_inventory(Object)),
    write('Object not in inventory'),
    !.
io_kill(Object, _) :-
    not(can_kill_creature(Object)),
    write(Object), write(' can\'t kill anything'),
    !.
io_kill(_, Target) :-
    player_can_see_creature(Target),
    write('You can\'t find the '), write(Target),
    nl,
    !.

io_open(Destination, Object) :-
    open(Destination, Object),
    write('The '), write(Destination), write(' is now opened'), nl,
    !.
io_open(Destination, _) :-
    player_location(Current),
    not(is_connected(Current, Destination)),
    write('This door doesn\'t exist'),
    !.
io_open(Destination, _) :-
    player_location(Current),
    is_connected(Current, Destination),
    not(is_closed(Current, Destination)),
    write('This door is already opened'),
    !.
io_open(_, Object) :-
    not(in_inventory(Object)),
    write('Object not in inventory'),
    !.
io_open(_, Object) :-
    not(can_open_a_door(Object)),
    write(Object), write(' can\'t open a door'),
    !.

io_ask_to_leave(Creature) :-
    ask_to_leave(Creature),
    write('The '), write(Creature), write(' leaves, as requested'), nl,
    !.
io_ask_to_leave(Creature) :-
    write('You can\'t find the '), write(Creature), nl.

io_act([go, to, Where]) :- io_move_to(Where), !.
io_act([open, Door, with, Object]) :- io_open(Door, Object), !.
io_act([open, Door]) :- in_inventory(key), io_open(Door, key), !.
io_act([get, Object]) :- io_get_object(Object), !.
io_act([kill, Target, with, Object]) :- io_kill(Object, Target), !.
io_act([can, you, leave, Creature]) :- io_ask_to_leave(Creature), !.
io_act([show, inventory]) :- inventory, !.
io_act([stop]) :- stop, !.
io_act([where, am, i]) :- where_am_i, !.
io_act([look, around]) :- look_around, !.
io_act(_) :- write('Invalid action'), nl.

read_atoms(Atoms) :-
    read_line_to_codes(user_input, Cs),
    atom_codes(A, Cs),
    atomic_list_concat(Atoms, ' ', A).

loop :- stopped,!.
loop :- won, write('Congratulations, you won!'), nl, !.
loop :-
  	prompt(_, 'Type an action...'),
  	read_atoms(Action),
  	io_act(Action),
  	loop.

start :-
    write('Get the egg'), nl,
    loop.

stop :-
    assert(stopped),
    write('Goodbye!'),
    nl.

won :-
    in_inventory(egg).

repeatsAtEnd(List, Sublist) :-
    append(Aux, Sublist , List),
    append(_, Sublist, Aux).

anyRepeatsAtEnd(List) :-
    repeatsAtEnd(List, SubList),
    length(SubList, Length),
    Length > 0,
    !.

alwaysWin_(_, _) :-
    won,
    !.

alwaysWin_([Move|MoveSet], PreviousMoves) :-
    act(Move),
    append(PreviousMoves, [Move], CurrentMoves),
    not(anyRepeatsAtEnd(CurrentMoves)),
    alwaysWin_(MoveSet, CurrentMoves).
    
    
alwaysWin(MoveSet) :-
    alwaysWin_(MoveSet, []).





