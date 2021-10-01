import Bomberman
import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)

main:: IO ()
main = hspec spec


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