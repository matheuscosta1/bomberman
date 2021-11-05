{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-deferred-out-of-scope-variables #-}
module Main where
----------------------------------------------------------------------------------------------------------------------------------------------------------
--Integrantes:

--Matheus José da Costa                   11711BCC008
--Gustavo Melo do Carmo                   11721BCC035
----------------------------------------------------------------------------------------------------------------------------------------------------------


import Bomberman
import System.Random.Shuffle ( shuffleM )
import Data.Maybe ( fromMaybe, isNothing, fromJust )
import Text.PrettyPrint.Boxes
import Data.List (transpose, intercalate)

print_table :: [[String]] -> IO ()
print_table rows = printBox $ hsep 2 left (map (vcat left . map text) (transpose rows))

ajustaJogadores:: Jogador -> [Jogador] -> [Jogador]
ajustaJogadores jogadorAtualizado (x:xs) = if identificadorX == identificadorListaJogadores then jogadorAtualizado:xs else x:ajustaJogadores jogadorAtualizado xs
    where
        identificadorX = identificador jogadorAtualizado
        identificadorListaJogadores = identificador x

mover:: Tabuleiro -> [Jogador] -> [Jogador] -> Maybe (Item, Ação) -> Item -> IO()
mover tabuleiro jogadores jogadorSorteado movimento identificacaoJogador = do

    let (j,operador) = fromMaybe (ITEM_NAO_ENCONTRADO,NO_OP) movimento
    putStr $ "\n\n(Jogador, Acao): " ++ show (j, operador)

    let direcao = converteAcaoEmDirecao operador

    let localizacaoQueOJogadorEstá = pegaLocalizacaoJogador tabuleiro 0 identificacaoJogador

    let localizacaoQueOJogadorQuerIr = pegaLocalizacaoQueOJogadorQuerIrBaseadoNaDirecao localizacaoQueOJogadorEstá direcao

    let itensQueEstaoNaNovaPosicaoQueOJogadorQuerIr = dadoCoordenadaPegarOsItens tabuleiro localizacaoQueOJogadorQuerIr

    let novoTabuleiro = movimentaJogadorNoTabuleiro tabuleiro identificacaoJogador localizacaoQueOJogadorEstá localizacaoQueOJogadorQuerIr itensQueEstaoNaNovaPosicaoQueOJogadorQuerIr

    let jogadorAtualizado = atualizaCapacidadesDoJogadorDeAcordoComOsItensQueElePodePegarDaNovaCélula (head jogadorSorteado) novoTabuleiro direcao itensQueEstaoNaNovaPosicaoQueOJogadorQuerIr

    let jogadoresAtualizados = ajustaJogadores jogadorAtualizado jogadores

    putStr $ "\n\n" ++ "Tabuleiro atualizado: \n\n"
    print_table (imprimeLinhas novoTabuleiro)

    putStr $ "\nJogador Atualizado: " ++show jogadorAtualizado

    if operador == SAIR then return () else loopingGame novoTabuleiro jogadoresAtualizados


bomba:: Tabuleiro -> [Jogador] -> [Jogador] -> Maybe (Item, Ação) -> Item -> IO()
bomba tabuleiro jogadores jogadorSorteado movimento identificacaoJogador = do

    let (j,operador) = fromMaybe (ITEM_NAO_ENCONTRADO,NO_OP) movimento
    putStr $ "\n\n(Jogador, Acao): " ++ show (j, operador)

    let direcaoJogador = direcao (head jogadorSorteado)

    let localizacaoQueOJogadorEstá = pegaLocalizacaoJogador tabuleiro 0 identificacaoJogador

    let localizacaoQueOJogadorQuerIr = pegaLocalizacaoQueOJogadorQuerIrBaseadoNaDirecao localizacaoQueOJogadorEstá direcaoJogador

    let itensQueEstaoNaNovaPosicaoQueOJogadorQuerIr = dadoCoordenadaPegarOsItens tabuleiro localizacaoQueOJogadorQuerIr

    let novoTabuleiro = colocarBomba tabuleiro localizacaoQueOJogadorQuerIr itensQueEstaoNaNovaPosicaoQueOJogadorQuerIr direcaoJogador

    let jogadorAtualizado = atualizaCapacidadesDoJogadorDeAcordoComOsItensQueElePodePegarDaNovaCélula (head jogadorSorteado) novoTabuleiro direcaoJogador itensQueEstaoNaNovaPosicaoQueOJogadorQuerIr

    let jogadoresAtualizados = ajustaJogadores jogadorAtualizado jogadores

    putStr $ "\n\n" ++ "Tabuleiro atualizado: \n\n"

    print_table (imprimeLinhas novoTabuleiro)

    putStr $ "\nJogador Atualizado: " ++show jogadorAtualizado

    loopingGame novoTabuleiro jogadoresAtualizados

loopingGame :: Tabuleiro -> [Jogador] -> IO()
loopingGame tabuleiro jogadores = do

    jogadorSorteado <- shuffleM jogadores

    let identificacaoJogador = pegaQualÉOJogador (head jogadorSorteado)

    putStr $ "\n" ++ show identificacaoJogador ++ " possui a configuracao: \n\n" ++ show jogadorSorteado
    putStr "\nMovimento: "

    movimento <- pegaMovimento [identificacaoJogador]

    let (j,operador) = fromMaybe (ITEM_NAO_ENCONTRADO,NO_OP) movimento

    if operador == SAIR || éFimDeJogo tabuleiro then return ()
    else
        case operador of
                        ColocarBomba   -> bomba tabuleiro jogadores jogadorSorteado movimento identificacaoJogador
                        Mover _        -> mover tabuleiro jogadores jogadorSorteado movimento identificacaoJogador
                        NO_OP          -> loopingGame tabuleiro jogadores
                        _              -> loopingGame tabuleiro jogadores

main :: IO ()
main = do
    putStr $ "\n" ++ "Tabuleiro inicial: \n\n"
    print_table (imprimeLinhas tabuleiroVálido)

    loopingGame tabuleiroVálido [jogador1, jogador2]