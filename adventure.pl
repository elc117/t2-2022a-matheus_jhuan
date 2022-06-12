/*
 * Adventure game
 * Fonte de inspiração: https://www.cs.auckland.ac.nz/~j-hamer/07.363/explore.html
 * 
 */

/*Game state and rules*/

:- debug.

initial_state(State) :-
    State = state{
            	player: house,
                inventory: [],
                creatures: [(zombie, pen)],
                locked_doors: [(yard, pen), (yard, house), (yard, woods)],
                objects: [(egg, pen), (bazooka, woods), (key, house)]
            }.

connected(yard, pen).
connected(yard, house).
connected(yard, woods).

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

:- dynamic stopped/0.

creature_at_location((_, Location), Location).

any_creature_at_location(Creatures, Location) :-
    member(Creature, Creatures),
    creature_at_location(Creature, Location).

is_closed(A, B, LockedDoors) :-
    member((A, B), LockedDoors), !.
is_closed(A, B, LockedDoors) :-
    member((B, A), LockedDoors).

can_reach(A, B, LockedDoors) :-
    is_connected(A, B),
    not(is_closed(A, B, LockedDoors)).
    
/*Base action predicates*/

can_move_to(Destination, PlayerLocation, Creatures, LockedDoors) :-
  	can_reach(PlayerLocation, Destination, LockedDoors),
    not(any_creature_at_location(Creatures, Destination)).

can_open(Destination, PlayerLocation, Object, Inventory, LockedDoors) :-
    member(Object, Inventory),
    can_open_a_door(Object),
    is_connected(PlayerLocation, Destination),
    is_closed(PlayerLocation, Destination, LockedDoors).

can_get_object(Object, PlayerLocation, ObjectsInWorld) :-
    member((Object, PlayerLocation), ObjectsInWorld).

can_kill(Object, Inventory, Creature, Creatures, PlayerLocation, LockedDoors) :-
    member(Object, Inventory),
    can_kill_creature(Object),
    member((Creature, CreatureLocation), Creatures),
    can_reach(PlayerLocation, CreatureLocation, LockedDoors).

can_ask_to_leave(PlayerLocation, Creature, Creatures, LockedDoors) :-
    member((Creature, CreatureLocation), Creatures),
    can_reach(PlayerLocation, CreatureLocation, LockedDoors).

move_to(Destination, State, NextState) :-
    can_move_to(Destination, State.player, State.creatures, State.locked_doors),
    NextState = State.put(player, Destination).

remove_unordered(A, B, List, Result) :-
    member((A, B), List),
    delete(List, (A, B), Result),
    !.

remove_unordered(A, B, List, Result) :-
    delete(List, (B, A), Result).

do_open(Door, Object, State, NextState) :-
    can_open(Door, State.player, Object, State.inventory, State.locked_doors),
    remove_unordered(Door, State.player, State.locked_doors, NextLockedDoors),
    NextState = State.put(locked_doors, NextLockedDoors).

get_object(Object, State, NextState) :-
    can_get_object(Object, State.player, State.objects),
    delete(State.objects, (Object, State.player), NextObjects),
    append(State.inventory, [Object], NextInventory),
    NextState = State.put(objects, NextObjects).put(inventory, NextInventory).

kill(Object, Target, State, NextState) :-
    can_kill(Object, State.inventory, Target, State.creatures, State.player, State.locked_doors),
    delete(State.creatures, (Target, _), NextCreatures),
    NextState = State.put(creatures, NextCreatures).
    

ask_to_leave(Target, State, NextState) :-
    can_ask_to_leave(State.player, Target, State.creatures, State.locked_doors),
    delete(State.creatures, (Target, _), NextCreatures),
    NextState = State.put(creatures, NextCreatures).
    
act([go, to, Where], CurrentState, NextState) :- move_to(Where, CurrentState, NextState).
act([open, Door, with, Object], CurrentState, NextState) :- do_open(Door, Object, CurrentState, NextState).
act([get, Object], CurrentState, NextState) :- get_object(Object, CurrentState, NextState).
act([kill, Target, with, Object], CurrentState, NextState) :- kill(Object, Target, CurrentState, NextState).
act([can, you, leave, Creature], CurrentState, NextState) :- ask_to_leave(Creature, CurrentState, NextState).

/*I/O action predicates*/
write_area(Current, LocationToWrite, State) :-
    is_closed(Current, LocationToWrite, State.locked_doors),
    write(LocationToWrite), write(' [locked]'), nl,
    !.
write_area(Current, LocationToWrite, State) :-
    is_connected(Current, LocationToWrite),
    member((Creature, LocationToWrite), State.creatures),
    write('The '), write(Creature), write(' is blocking the entrance to the '), write(LocationToWrite), nl,
    !.
write_area(Current, LocationToWrite, _) :-
    is_connected(Current, LocationToWrite),
    write(LocationToWrite), nl,
    !.
write_area(_, _, _).

write_areas(State) :-
    write('Areas:'), nl,
    forall(is_connected(State.player, Area), write_area(State.player, Area, State)).

where_am_i(State) :-
    write('You are in: '), write(State.player), nl.

write_object(Object) :- write(Object), nl.

write_objects(State) :-
    findall(Object, member((Object, State.player), State.objects), []),
    !. 
write_objects(State) :-
    write('Objects: '),
    nl,
    forall(member((Object, State.player), State.objects), write_object(Object)).

look_around(State) :-
    where_am_i(State),
    write_areas(State),
    write_objects(State).

inventory(State) :-
    State.inventory == [],
    write('Inventory is empty'), nl,
    !.
inventory(State) :-
    write('Inventory: '), nl,
    forall(member(Object, State.inventory), write_object(Object)).

io_get_object(Object, State, NextState) :-
    get_object(Object, State, NextState),
    write('You obtained '), write(Object), nl,
    !.
io_get_object(Object, State, State) :-
    not(member((Object, State.player), State.objects)),
    write('You can\'t find '), write(Object), nl,
    !.
io_get_object(Object, State, State) :-
    member(Object, State.inventory),
    write('You already have one of these'), nl,
    !.

io_move_to(Destination, State, NextState) :-
    move_to(Destination, State, NextState),
  	write('You are now in the '), write(Destination), nl,
    !.
io_move_to(Destination, State, State) :-
    not(is_connected(State.player, Destination)),
  	write('You can\'t find this place.'), nl,
    !.
io_move_to(Destination, State, State) :-
    is_closed(State.player, Destination, State.locked_doors),
    write('The door is closed'), nl,
    !.
io_move_to(Destination, State, State) :-
    member((Creature, Destination), State.creatures),
    write('The '), write(Creature), write(' is blocking the entrance to the '), write(Destination), nl,
    !.

io_kill(Object, Target, State, NextState) :-
    kill(Object, Target, State, NextState),
    write('You killed the '), write(Target), nl,
    !.
io_kill(Object, _, State, State) :-
    not(member(Object, State.inventory)),
    write('Object not in inventory'),
    !.
io_kill(Object, _, State, State) :-
    not(can_kill_creature(Object)),
    write(Object), write(' can\'t kill anything'),
    !.
io_kill(_, Target, State, State) :-
    not(can_ask_to_leave(State.player, Target, State.creatures, State.locked_doors)),
    write('You can\'t find the '), write(Target),
    nl,
    !.

io_open(Destination, Object, State, NextState) :-
    do_open(Destination, Object, State, NextState),
    write('The '), write(Destination), write(' is now opened'), nl,
    !.
io_open(Destination, _, State, State) :-
    not(is_connected(State.player, Destination)),
    write('This door doesn\'t exist'),
    !.
io_open(Destination, _, State, State) :-
    not(is_closed(State.player, Destination, State.locked_doors)),
    write('This door is already opened'),
    !.
io_open(_, Object, State, State) :-
    not(member(Object, State.inventory)),
    write('Object not in inventory'),
    !.
io_open(_, Object, State, State) :-
    not(can_open_a_door(Object)),
    write(Object), write(' can\'t open a door'),
    !.

io_ask_to_leave(Creature, State, NextState) :-
    ask_to_leave(Creature, State, NextState),
    write('The '), write(Creature), write(' leaves, as requested'), nl,
    !.
io_ask_to_leave(Creature, State, State) :-
    write('You can\'t find the '), write(Creature), nl.

io_act([go, to, Where], State, NextState) :- io_move_to(Where, State, NextState), !.
io_act([open, Door, with, Object], State, NextState) :- io_open(Door, Object, State, NextState), !.
io_act([open, Door], State, NextState) :- member(key, State.inventory), io_open(Door, key, State, NextState), !.
io_act([get, Object], State, NextState) :- io_get_object(Object, State, NextState), !.
io_act([kill, Target, with, Object], State, NextState) :- io_kill(Object, Target, State, NextState), !.
io_act([can, you, leave, Creature], State, NextState) :- io_ask_to_leave(Creature, State, NextState), !.
io_act([show, inventory], State, State) :- inventory(State), !.
io_act([stop], State, NextState) :- stop, NextState = State, !.
io_act([where, am, i], State, State) :- where_am_i(State), !.
io_act([look, around], State, State) :- look_around(State), !.
io_act(_, State, State) :- write('Invalid action'), nl.

read_atoms(Atoms) :-
    read_line_to_codes(user_input, Cs),
    atom_codes(A, Cs),
    atomic_list_concat(Atoms, ' ', A).

loop(_) :- stopped,!.
loop(State) :- won(State), write('Congratulations, you won!'), nl, !.
loop(State) :-
  	prompt(_, 'Type an action...'),
  	read_atoms(Action),
  	io_act(Action, State, NextState),
  	loop(NextState).

start :-
    write('Get the egg'), nl,
    initial_state(State),
    loop(State).

stop :-
    assert(stopped),
    write('Goodbye!'),
    nl.

won(State) :-
    member(egg, State.inventory).

alwaysWin_([], _, State) :-
    won(State),
    !.

alwaysWin_([Move|MoveSet], PreviousStates, State) :-
    act(Move, State, NextState),
    append(PreviousStates, [State], States),
    not(member(NextState, States)),
    alwaysWin_(MoveSet, States, NextState).


alwaysWin(MoveSet) :-
    initial_state(State),
    alwaysWin_(MoveSet, [], State).
