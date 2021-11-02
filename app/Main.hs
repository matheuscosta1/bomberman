{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Main where
import Bomberman
import System.Random.Shuffle ( shuffleM ) 
import Data.Maybe ( fromMaybe, isNothing, fromJust )

--ghci -o trabalho app/Main.hs src/Bomberman.hs -package random-shuffle
--main

ajustaJogadores:: Jogador -> [Jogador] -> [Jogador]
ajustaJogadores jogadorAtualizado (x:xs) = if identificadorX == identificadorListaJogadores then jogadorAtualizado:xs else x:ajustaJogadores jogadorAtualizado xs
    where
        identificadorX = identificador jogadorAtualizado
        identificadorListaJogadores = identificador x


looping :: Tabuleiro -> [Jogador] -> IO()
looping tabuleiro jogadores = do
    print $ "Jogadores: " ++ show jogadores
    --let jogador1 = (1, pegaLocalizacaoJogador tabuleiroVálido 0 JOGADOR_1, NADA, [(PRESENTE_PATINS, 0), (PRESENTE_ARREMESSO, 0)])

    --let jogador2 = (2, pegaLocalizacaoJogador tabuleiroVálido 0 JOGADOR_2, NADA, [(PRESENTE_PATINS, 0), (PRESENTE_ARREMESSO, 0)])

    jogador <- shuffleM jogadores

    print $ "Jogador: " ++ show jogador

    let identificacaoJogador = pegaQualÉOJogador (head jogador)

    print $ "Identificador: " ++ show identificacaoJogador

    move <- pegaMov [identificacaoJogador]

    let (j,op) = fromMaybe (ITEM_NAO_ENCONTRADO,NO_OP) move
    print $ "(Jogador,Ação)" ++ show (j,op)
    print $ "Operacao " ++ show op
    
    let direcao = converteAcaoEmDirecao op

    print $ "Direçao " ++ show direcao

    let localizacaoQueOJogadorEstá = pegaLocalizacaoJogador tabuleiro 0 identificacaoJogador

    let localizacaoQueOJogadorQuerIr = pegaLocalizacaoQueOJogadorQuerIrBaseadoNaDirecao localizacaoQueOJogadorEstá direcao

    let itensQueEstaoNaNovaPosicaoQueOJogadorQuerIr = dadoCoordenadaPegarOsItens tabuleiro localizacaoQueOJogadorQuerIr

    let novoTabuleiro = movimentaJogadorNoTabuleiro tabuleiro identificacaoJogador localizacaoQueOJogadorEstá localizacaoQueOJogadorQuerIr itensQueEstaoNaNovaPosicaoQueOJogadorQuerIr

    let jogadorAtualizado = atualizaCapacidadesDoJogadorDeAcordoComOsItensQueElePodePegarDaNovaCélula (head jogador) novoTabuleiro direcao itensQueEstaoNaNovaPosicaoQueOJogadorQuerIr

    let jogadoresAtualizados = ajustaJogadores jogadorAtualizado jogadores

    --let jogador1 = jogadorAtualizado

    print $ "Novo Tabuleiro: " ++ show novoTabuleiro
    print $ "Jogador Atualizado: " ++show jogadorAtualizado

    if op == SAIR then return () else looping novoTabuleiro jogadoresAtualizados

main :: IO ()
main = do

    looping tabuleiroVálido [jogador1, jogador2]
    

