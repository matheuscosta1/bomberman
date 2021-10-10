module Main where
import Bomberman


main :: IO ()
main = do

    let identificacaoJogador = pegaQualÉOJogador jogador1

    let direcao = SUL

    let localizacaoQueOJogadorEstá = pegaLocalizacaoJogador tabuleiroVálido 0 identificacaoJogador

    let localizacaoQueOJogadorQuerIr = pegaLocalizacaoQueOJogadorQuerIrBaseadoNaDirecao localizacaoQueOJogadorEstá direcao

    let itensQueEstaoNaNovaPosicaoQueOJogadorQuerIr = dadoCoordenadaPegarOsItens tabuleiroVálido localizacaoQueOJogadorQuerIr

    let novoTabuleiro = movimentaJogadorNoTabuleiro tabuleiroVálido identificacaoJogador localizacaoQueOJogadorEstá localizacaoQueOJogadorQuerIr itensQueEstaoNaNovaPosicaoQueOJogadorQuerIr

    let jogadorAtualizado = atualizaCapacidadesDoJogadorDeAcordoComOsItensQueElePodePegarDaNovaCélula jogador1 novoTabuleiro direcao itensQueEstaoNaNovaPosicaoQueOJogadorQuerIr

    putStrLn"------------------------"
    putStrLn"Jogador atualizado: "
    print jogadorAtualizado
    putStrLn"------------------------"
    putStrLn"Novo Tabuleiro: "
    print novoTabuleiro
    putStrLn"------------------------"

