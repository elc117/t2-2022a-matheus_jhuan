# Tema: Outras aplicações de Prolog


## Objetivos
Teremos como objetivo incentivar o contato com outras aplicacões em Prolog.



## Links de inspiração
Eliza Chatbot: https://swish.swi-prolog.org/example/eliza.pl   
Adventure Game: https://www.cs.auckland.ac.nz/~j-hamer/07.363/explore.html



## Exercícios

1. De acordo com o que foi visto na [Eliza Chatbot](https://swish.swi-prolog.org/example/eliza.pl) e em [jereusa.pl](https://github.com/elc117/t2-2022a-matheus_jhuan), escreva um código em [Eliza Chatbot](https://swish.swi-prolog.org/example/eliza.pl) que retorne a seguinte saída (usando o mesmo template):

   ```
   > eliza('eu te amo', Response).
   ?- Response = ['eu também te amo'].
   ```
   e
   ```
   > eliza('eu te odeio', Response).
   ?- Response = ['eu também te odeio'].
   ```
   
2. A partir do código em [adventure.pl](https://github.com/elc117/t2-2022a-matheus_jhuan/blob/main/adventure.pl), faça um predicado que retorne uma sequência de ações que resultam em vitória. Por exemplo:
   ```
   > win(Actions).
   ?- Response = [
      [get, key],
      [open, yard, with, key],
      [go, to, yard],
      [open, woods, with, key],
      [go, to, woods],
      [get, bazooka],
      [go, to, yard],
      [open, pen, with, bazooka],
      [kill, zombie, with, bazooka],
      [go, to, pen],
      [get, egg]
   ].
   ```
   Predicados úteis que podem ser utilizados:
      - act(Action, State, NextState) -> verifica se a ação Action é válida para o estado State, e leva do estado State para o estado NextState.
      - won(State) -> verifica se no estado State o jogador venceu.
      - initial_state(State) -> unifica State com o estado inicial do jogo.

   Tome cuidado para que o programa não caia em um loop infinito, como ficar se movimentando apenas entre o jardim e a casa, por exemplo.
