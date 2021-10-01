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
                it "Valida uma linha do tabuleiro" $
                        getLinha tabuleiroVálido 5 `shouldBe` ([PEDRA],[GRAMA,JOGADOR_1],[PEDRA],[GRAMA,PAREDE],[PEDRA],[GRAMA],[GRAMA],[PEDRA])
        describe "Cria tabuleiro" $ do
                it "Valida criação do tabuleiro" $
                        criaTabuleiro tabuleiroVálido `shouldBe` tabuleiroVálido
        describe "Cria tabuleiro inválido" $ do
                it "Lança exceção ao criar tabuleiro inválido" $
                        evaluate(criaTabuleiro tabuleiroInválido) `shouldThrow` errorCall "Tabuleiro inválido"
        describe "Pega posição do jogador 1" $ do
                it "Valida se a posição do jogador é (5,2)" $
                        pegaLocalizacaoJogador tabuleiroVálido 0 JOGADOR_1 `shouldBe` (5,2)
        --describe "Movimenta jogador" $ do
        --        it "Valida se quando o jogador 1 mover para o Norte ele deixa a sua posição (5,2) e assume uma nova (4,2)" $
                        