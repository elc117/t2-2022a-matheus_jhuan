%%  eliza(+Stimuli, -Response) is det.
%   @param  Stimuli is a list of atoms (words).
%   @author Richard A. O'Keefe (The Craft of Prolog)
verbo([amo], [ama]).
verbo([amei], [amou]).
verbo([tem], [tenho]).
verbo([teve], [tinha]).
verbos(X, Y) :- verbo(X, Y).

:- dynamic nome/1.
nome(lindo).


jereusa([meu, nome, eh, NomeNovo], Response) :-
    nome(NomeAtual),
    retract(nome(NomeAtual)),
    assert(nome(NomeNovo)),
    Response = [oi, NomeNovo],
    !.

jereusa(Stimuli, Response) :-
    template(InternalStimuli, InternalResponse),
    match(InternalStimuli, Stimuli),
    match(InternalResponse, Response),
    !.

template([s([eu,estou]),s(X)], [s([porque, você, está]),s(X),w('?')]).
template([w(eu),s(X),w(você)], [s([porque,voce]),w(me), s(Y),w('?')]) :-
    verbos(X, Y).
template([w(eu),s(X),w(você), w(jereusa)], [s([eu]),s(X),w(você),w(tambem), w(NomeNovo)]) :-
    nome(NomeNovo).
    
template([s([quantos, anos, voce]),w(X), s(Z)], [w(eu),w(Y), w('18') ,w(anos), s(Z)]) :-
    verbos(X, Y).



match([],[]).
match([Item|Items],[Word|Words]) :-
    match(Item, Items, Word, Words).

match(w(Word), Items, Word, Words) :-
    match(Items, Words).
match(s([Word|Seg]), Items, Word, Words0) :-
    append(Seg, Words1, Words0),
    match(Items, Words1).


/** <examples>

?- eliza([i, am, very, hungry], Response).
?- eliza([i, love, you], Response).

*/
