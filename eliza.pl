%%  eliza(+Stimuli, -Response) is det.
%   @param  Stimuli is a list of atoms (words).
%   @author Richard A. O'Keefe (The Craft of Prolog)


/** <exemplos>

?- jereusa([eu, amo, voce, jereusa], Response).
?- jereusa([quais, sao, minhas, musicas, favoritas], Response).
?- jereusa([mora, comigo, jereusa], Response).
?- jereusa([o, que, voce, faz, jereusa], Response).
?- jereusa('o que voce odeia jereusa', Response).

*/


:- dynamic nome/1.
nome(lindo).


resposta([meu, nome, é, NomeNovo], Response) :-
    nome(NomeAtual),
    retract(nome(NomeAtual)),
    assert(nome(NomeNovo)),
    Response = [oi, NomeNovo, meu, nome, é, jereusa],
    !.


resposta(Stimuli, Response) :-
    template(InternalStimuli, InternalResponse),
    match(InternalStimuli, Stimuli),
    match(InternalResponse, ListResponse),
    atomic_list_concat(ListResponse, ' ', Response),
    !.
  

jereusa(Stimuli, Response) :-
    atomic_list_concat(Atomos, ' ', Stimuli),
    resposta(Atomos, Response).

		
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
template([w(_),s(X),s(Y), s(K)], [s(C),s(Y),s(K), w(':'), s(W)]) :-
    respostas(Y,W),
    	pronomes(X,C).
template([w(eu), s(Y), s(X)], [s([porque, você]), s(K), s(Z),w('?')]) :-
    pronomes(Y,K),
    	verbos(X,Z).
template([w(eu), s(X), s(Y)], [s([porque, você]), s(Z), s(Y),w('?')]) :-
    verbos(X,Z).
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
verbo([mora], [moro]).
verbo([estou], [esta]).
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
palavra([gosta], [de, fazer, amizade]).
palavra([odeia], [humanos]).
palavra([faz], [amizade]).
palavra([ama], [voce]).
palavra([comigo], [com, voce]).
palavra([acha], [interessante]).
palavra([lugar], [na, praia]).
palavra([estacoes], [verao, outono, inverno, primavera]).
palavra([musicas], [anitta, envolver,',', luisa, sonsa, sentadao]).
palavra([musica], [anitta, envolver]).
pronome([sua], [minha]).
pronome([suas], [minhas]).
pronome([minha], [sua]).
pronome([me], [se]).
pronome([te], [me]).

% "predicados"
respostas(X, Y) :- palavra(X, Y).
verbos(X,Y) :- verbo(X,Y).
pronomes(X,Y) :- pronome(X,Y).

