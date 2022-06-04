%%  eliza(+Stimuli, -Response) is det.
%   @param  Stimuli is a list of atoms (words).
%   @author Richard A. O'Keefe (The Craft of Prolog)


/** <examplos>

?- jereusa([eu, amo, você, jereusa], Response).
?- jereusa([quais, sao, minhas, musicas, favoritas], Response).

*/


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



template([s([eu, estou]), s(X)], [s([porque, você, está]),s(X),w('?')]).
template([s([quantos, anos, voce]),s(X), s(_)], [w(eu),s(Y), w(18) ,w(anos)]) :-
    verbos(X, Y).
template([s([o, que, voce]),s(X), s(_)], [w(eu),s(Y),s(W)]) :-
    verbos(X,Y),
		respostas(X, W).
template([s(X),s(Z), s(_)], [s(Y),s(W),w(sim)]) :-
    verbos(X,Y),
    	respostas(Z,W).
template([w(eu),s(X),w(voce)], [s([porque,voce]),w(me), s(Y),w('?')]) :-
    verbos(X, Y).
template([w(quais),s(X),s(Y)], [s(X),s(Y), w(':'), s(W)]) :-
    respostas(Y,W).
template([w(_),s(X),s(Y), s(K)], [s(X),s(Y),s(K), w(':'), s(W)]) :-
    respostas(Y,W).
template([w(eu),s(X),w(voce), w(jereusa)], [s([eu]),s(X),w(você),w(tambem), w(NomeNovo)]) :-
    nome(NomeNovo). 

    
   

match([],[]).

match([Item|Items],[Word|Words]) :-
    match(Item, Items, Word, Words).

match(w(Word), Items, Word, Words) :-
    match(Items, Words).
match(s([Word|Seg]), Items, Word, Words0) :-
    append(Seg, Words1, Words0),
    match(Items, Words1).




% "banco de dados"
verbo([amo], [ama]).
verbo([acha], [acho]).
verbo([ama], [amo]).
verbo([odeio], [odeia]).
verbo([amei], [amou]).
verbo([tem], [tenho]).
verbo([teve], [tinha]).
verbo([tinha], [tinha]).
verbo([namora], [namoro]).
verbo([faz], [faço]).
verbo([estudou], [estudei]).
verbo([gosta], [gosto]).
verbo([odeia], [odeio]).
palavra([gosta], [de, voce]).
palavra([odeia], [humanos]).
palavra([faz], [amizade]).
palavra([ama], [voce]).
palavra([comigo], [com, voce]).
palavra([acha], [interessante]).
palavra([feliz], [se, voce, esta, feliz, eu, tambem, estou]).
palavra([estacoes], [verao, outono, inverno, primavera]).
palavra([musicas], [anitta, envolver,luisa, sonsa, sentadao]).
palavra([musica], [anitta, envolver]).
respostas(X, Y) :- palavra(X, Y).
verbos(X,Y) :- verbo(X,Y).



