/*
 * Adventure game
 * Fonte: https://www.cs.auckland.ac.nz/~j-hamer/07.363/explore.html
 * 
 */
location(egg, duck_pen).
location(ducks, duck_pen).
location(fox, woods).
location(you, house).

connect(duck_pen, yard).
connect(yard, house).
connect(yard, woods).



goto(X) :-
  location(you, L),
  connect(L, X),
  retract( location(you, L) ),
  assert( location(you, X) ),
  write("You are in the"), write(X), nl.
goto(_) :-
  write("You can't get there from here."), nl.

fox :-
  location(ducks, yard),
  location(you, house),
  write("The fox has taken a duck."), nl.
fox.

go :- done.
go :-
  write(">> "),
  read(X),
  X,
  fox,
  go.


done :-
  location(you, house),
  write("Thanks for getting the egg."), nl.