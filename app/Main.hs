{-# LANGUAGE OverloadedStrings #-}
module Main where


import Bomberman
import System.Random.Shuffle ( shuffleM ) 
import Data.Maybe ( fromMaybe, isNothing, fromJust )
import Text.PrettyPrint.Boxes
import Data.List (transpose, intercalate)

print_table rows = printBox $ hsep 2 left (map (vcat left . map text) (transpose rows))

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

    jogadorSorteado <- shuffleM jogadores

    print $ "Jogador: " ++ show jogadorSorteado

    let identificacaoJogador = pegaQualÉOJogador (head jogadorSorteado)

    print $ "Identificador: " ++ show identificacaoJogador

    movimento <- pegaMov [identificacaoJogador]

    let (j,operador) = fromMaybe (ITEM_NAO_ENCONTRADO,NO_OP) movimento
    print $ "(Jogador,Ação)" ++ show (j,operador)
    print $ "Operacao " ++ show operador
    
    let direcao = converteAcaoEmDirecao operador

    print $ "Direçao " ++ show direcao

    let localizacaoQueOJogadorEstá = pegaLocalizacaoJogador tabuleiro 0 identificacaoJogador

    let localizacaoQueOJogadorQuerIr = pegaLocalizacaoQueOJogadorQuerIrBaseadoNaDirecao localizacaoQueOJogadorEstá direcao

    let itensQueEstaoNaNovaPosicaoQueOJogadorQuerIr = dadoCoordenadaPegarOsItens tabuleiro localizacaoQueOJogadorQuerIr

    let novoTabuleiro = movimentaJogadorNoTabuleiro tabuleiro identificacaoJogador localizacaoQueOJogadorEstá localizacaoQueOJogadorQuerIr itensQueEstaoNaNovaPosicaoQueOJogadorQuerIr

    let jogadorAtualizado = atualizaCapacidadesDoJogadorDeAcordoComOsItensQueElePodePegarDaNovaCélula (head jogadorSorteado) novoTabuleiro direcao itensQueEstaoNaNovaPosicaoQueOJogadorQuerIr
    
    let jogadoresAtualizados = ajustaJogadores jogadorAtualizado jogadores

    print_table (imprimeLinhas novoTabuleiro)
    --let jogador1 = jogadorAtualizado

    --print $ "Novo Tabuleiro: " ++ show novoTabuleiro
    print $ "Jogador Atualizado: " ++show jogadorAtualizado

    if operador == SAIR then return () else looping novoTabuleiro jogadoresAtualizados


main :: IO ()
main = do
    print_table (imprimeLinhas tabuleiroVálido)
    looping tabuleiroVálido [jogador1, jogador2]
    {--print $ "Novo Tabuleiro: " ++ show tabuleiroVálido
    putStrLn $ tabl EnvAscii hdecor vdecor aligns cells 
    where
        hdecor = DecorUnion [DecorOuter, DecorOnly [1]]
        vdecor = DecorAll
        aligns = [AlignLeft, AlignLeft, AlignRight]
        cells  = [ ["Name", "SI Unit", "Value"]
                , ["Speed of light", "m/s", "299792458"]
                , ["Atmosphere", "Pa", "101325"]
                , ["Absolute zero", "C", "-273.15"] ]
    --}
    

    
