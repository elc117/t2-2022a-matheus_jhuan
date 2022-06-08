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
   
2. A partir do código em [adventure.pl](https://github.com/elc117/t2-2022a-matheus_jhuan/blob/main/adventure.pl), faça com que seja possível que o jogador peça para que o zumbi abra caminho para o objetivo, ao invés de matá-lo. Foi criado um predicado 'ask_to_leave(Creature)' que pode ser utilizado.
