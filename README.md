# Bomberman

## Descrição

Este projeto tem como objetivo a implementação de um jogo Bomberman utilizando a linguagem Haskell.

## Requisitos para executar o projeto no Ubuntu

* Instalar o Cabal:

```sudo apt-get install cabal-install && cabal update```

* Necessário instalar a lib hspec, boxes e random:

```cabal update && cabal install hspec boxes-0.1.5 random```

* Rodar na pasta raiz o seguinte comando para testar:

```cabal test``` ou ```stack test```

* Rodar na pasta raiz o seguinte comando para executar a Main:

```stack ghci```

* Logo após isso basta digitar ```main``` para inicializar o jogo.

## Tabuleiro base para teste:

![Tabuleiro](images/tabuleiro.png)
