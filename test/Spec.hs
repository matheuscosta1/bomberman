import Bomberman
import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)

main:: IO ()
main = hspec spec

--jogadorNaNovaPosicao = 

spec::Spec
spec = do
        describe "Pega Linha 5" $ do
                it "Valida se a linha 5 do tabuleiro é ([PEDRA],[GRAMA,JOGADOR_1],[PEDRA],[GRAMA,PAREDE],[PEDRA],[GRAMA],[GRAMA],[PEDRA])" $
                        getLinha tabuleiroVálido 5 `shouldBe` ([PEDRA],[GRAMA,JOGADOR_1],[PEDRA],[GRAMA,PAREDE],[PEDRA],[GRAMA],[GRAMA],[PEDRA])
        describe "Cria tabuleiro" $ do
                it "Valida criação do tabuleiro" $
                        criaTabuleiro tabuleiroVálido `shouldBe` tabuleiroVálido
        describe "Cria tabuleiro inválido" $ do
                it "Lança exceção ao criar tabuleiro inválido" $
                        evaluate(criaTabuleiro tabuleiroInválido) `shouldThrow` errorCall "Tabuleiro inválido"
        describe "Pega posição do jogador 1" $ do
                it "Valida se a posição do jogador 1 é (5,2)" $
                        pegaLocalizacaoJogador tabuleiroVálido 0 JOGADOR_1 `shouldBe` (5,2)
        describe "Pega nova posição do jogador baseado na direção que ele quer ir" $ do
                it "Valida se quando o jogador 1 sair da posição (5,2) movendo para o NORTE se a posição dele será (4,2)" $
                        pegaLocalizacaoQueOJogadorQuerIrBaseadoNaDirecao (pegaLocalizacaoJogador tabuleiroVálido 0 JOGADOR_1) NORTE `shouldBe` (4,2)
        describe "Pega nova posição do jogador baseado na direção que ele quer ir" $ do
                it "Valida se quando o jogador 1 sair da posição (5,2) movendo para o SUL se a posição dele será (6,2)" $
                        pegaLocalizacaoQueOJogadorQuerIrBaseadoNaDirecao (pegaLocalizacaoJogador tabuleiroVálido 0 JOGADOR_1) SUL `shouldBe` (6,2)
        describe "Pega os itens de uma célula a partir de suas coordenadas" $ do
                it "Busca os itens [GRAMA, JOGADOR_1] na posição (5,2)" $
                        dadoCoordenadaPegarOsItens tabuleiroVálido (pegaLocalizacaoJogador tabuleiroVálido 0 JOGADOR_1) `shouldBe` [GRAMA, JOGADOR_1]
        describe "Valida se item existe na célula" $ do
                it "Verifica se o item JOGADOR_1 existe na célula [GRAMA, JOGADOR_1]" $
                        validaItemExisteNaCélula [GRAMA, JOGADOR_1] JOGADOR_1 `shouldBe` True
        --describe "Movimenta jogador" $ do
        --        it "Valida se quando o jogador 1 mover para o Norte ele deixa a sua posição (5,2) e assume uma nova (4,2)" $
                        