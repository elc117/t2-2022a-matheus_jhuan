/*
 * Adventure game
 * Fonte: https://www.cs.auckland.ac.nz/~j-hamer/07.363/explore.html
 * 
 */

:- dynamic location/2.
location(egg, pen).
location(ducks, pen).
location(fox, woods).
location(you, house).

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

fox :-
  location(ducks, yard),
  location(you, house),
  write('The fox has taken a duck.'), nl.
fox.

move(Player, Destination) :-
    location(Player, Current),
  	can_reach(Current, Destination),
  	retract(location(Player, Current)),
  	assert(location(Player, Destination)).

goto(X) :-
    move(you, X),
  	write('You are in the '), write(X), nl.
goto(X) :-
    location(you, Y),
    is_connected(X, Y),
    write('The gate is closed').
goto(_) :-
  	write('You can\'t get there from here.'), nl.

do_open(A, B) :-
  	retract(closed(A, B)),
	write('Gate opened!').

open(house_gate) :-
  	do_open(yard, house).

open(pen_gate) :-
  	do_open(yard, pen).

open(house_gate) :-
  	do_open(yard, house).

open(_) :-
    write('This gate doesn\'t exist').

act(goto) :-
    prompt(_, 'Where?'),
    read(X),
    goto(X).

act(open) :-
    prompt(_, 'Which gate?'),
    read(X),
    open(X).

act(stop) :-
    done.

act(where_am_i) :-
    location(you, X),
    write(X).

act(_) :-
    write('There is no such action').

loop :-
  prompt(_, 'Type a command...'),
  read(Command),
  act(Command),
  fox,
  loop.

start :- loop.

done :-
  location(you, house),
  write('Thanks for getting the egg.'), nl.
