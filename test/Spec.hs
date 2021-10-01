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
        describe "Valida se a célula é buraco" $ do
                it "É buraco" $
                        verificaSeÉBuraco [] `shouldBe` True
        describe "Valida grama" $ do
                it "Valida se a grama está na base da pilha" $
                        verificaGrama [GRAMA] `shouldBe` True
        describe "Valida grama" $ do
                it "Valida se a grama não está na base da pilha" $
                        verificaGrama [PEDRA, GRAMA] `shouldBe` False
        describe "Valida presente" $ do
                it "Valida se o presente arremesso está sobre grama" $
                        verificaPresente [GRAMA, PRESENTE_ARREMESSO] `shouldBe` True
        describe "Valida presente" $ do
                it "Valida se o presente patins está sobre grama" $
                        verificaPresente [GRAMA, PRESENTE_PATINS] `shouldBe` True
        describe "Valida presente" $ do
                it "Valida se o presente patins não está sobre grama" $
                        verificaPresente [PRESENTE_PATINS, GRAMA] `shouldBe` False
        describe "Valida pedra" $ do
                it "Valida se a pedra está na base da pilha" $
                        verificaPedra [PEDRA] `shouldBe` True
        describe "Valida pedra" $ do
                it "Valida se a pedra não está na base da pilha" $
                        verificaPedra [PRESENTE_PATINS, PEDRA] `shouldBe` False
        describe "Valida bomba" $ do
                it "Valida se bomba está sobre grama" $
                        verificaBomba [GRAMA, BOMBA] `shouldBe` True
        describe "Valida bomba" $ do
                it "Valida se bomba não está sobre grama" $
                        verificaBomba [BOMBA] `shouldBe` False
        describe "Valida jogador" $ do
                it "Valida se o jogador 1 está sobre grama" $
                        verificaJogador [GRAMA, JOGADOR_1] JOGADOR_1 `shouldBe` True
        describe "Valida jogador" $ do
                it "Valida se o jogador 1 não está sobre grama" $
                        verificaJogador [JOGADOR_1, GRAMA] JOGADOR_1 `shouldBe` False
        describe "Valida parede" $ do
                it "Valida se parede está na base" $
                        paredeEstáNaBase [PAREDE] `shouldBe` True
        describe "Valida pedra" $ do
                it "Valida se pedra está sobre grama" $
                        paredeEstáSobreGrama [GRAMA, PAREDE] `shouldBe` True
        describe "Valida pedra" $ do
                it "Valida se pedra está sobre presente" $
                        verificaParede [GRAMA, PRESENTE_PATINS, PAREDE] `shouldBe` True
        describe "Valida pedra" $ do
                it "Valida se pedra está sobre presente" $
                        verificaParede [GRAMA, PRESENTE_PATINS, PAREDE] `shouldBe` True
        describe "Valida célula" $ do
                it "Valida se uma célula [GRAMA, PRESENTE_PATINS, PAREDE] está nas regras do jogo" $
                        validaCélula [GRAMA, PRESENTE_PATINS, PAREDE] `shouldBe` True
        describe "Valida célula" $ do
                it "Valida se uma célula [PRESENTE_PATINS, GRAMA, PAREDE] está nas regras do jogo" $
                        validaCélula [PRESENTE_PATINS, GRAMA, PAREDE] `shouldBe` False
        describe "Valida colunas da linha 5 do tabuleiro" $ do
                it "Verifica se todas as colunas da linha 5 do tabuleiro são válidas" $
                        percorreCélulasParaValidarTabuleiro (getLinha tabuleiroVálido 5) 0 `shouldBe` [True, True, True, True, True, True, True, True]
        describe "Valida colunas da linha 2 de um tabuleiro inválido" $ do
                it "Verifica se há alguma célula inválida na linha 2 de um tabuleiro inválido" $
                        percorreCélulasParaValidarTabuleiro (getLinha tabuleiroInválido 2) 0 `shouldBe` [True, False, True, True, True, True, True, True]
        describe "Percorre todas as linhas de um tabuleiro e valida se suas colunas são válidas" $ do
                it "Valida se as células das linhas são válidas" $
                        percorreLinhasParaValidarTabuleiro tabuleiroVálido  0 `shouldBe` [True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True]
        describe "Percorre todas as linhas de um tabuleiro inválido e valida se suas colunas são válidas" $ do
                it "Valida se as células das linhas são válidas" $
                        percorreLinhasParaValidarTabuleiro tabuleiroInválido  0 `shouldBe` [True,True,True,True,True,True,True,True,True,False,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True]
        describe "Valida se no tabuleiro há alguma célula inválida" $ do
                it "Verifica se tem célula inválida em um tabuleiro inválido" $
                        validaTabuleiro tabuleiroInválido `shouldBe` False
        describe "Valida se no tabuleiro há alguma célula inválida" $ do
                it "Verifica se tem célula inválida em um tabuleiro válido" $
                        validaTabuleiro tabuleiroVálido `shouldBe` True 
        
        --describe "Movimenta jogador" $ do
        --        it "Valida se quando o jogador 1 mover para o Norte ele deixa a sua posição (5,2) e assume uma nova (4,2)" $
                        