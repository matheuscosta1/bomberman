import Bomberman
import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)

main:: IO ()
main = hspec spec

spec::Spec
spec = do
        describe "Pega Linha 5" $ do
                it "Valida se a linha 5 do tabuleiro é ([PEDRA],[GRAMA,JOGADOR_1],[PEDRA],[GRAMA,PAREDE],[PEDRA],[GRAMA],[GRAMA],[PEDRA])" $
                        getLinha tabuleiroVálido 5 `shouldBe` ([PEDRA],[GRAMA,JOGADOR_1],[PEDRA],[GRAMA,PAREDE],[PEDRA],[GRAMA],[GRAMA],[PEDRA])
        describe "Pega Célula 2 da linha 5" $ do
                it "Valida se a linha 5 do tabuleiro é ([PEDRA],[GRAMA,JOGADOR_1],[PEDRA],[GRAMA,PAREDE],[PEDRA],[GRAMA],[GRAMA],[PEDRA])" $
                        getCélula (getLinha tabuleiroVálido 5) 2 `shouldBe` [GRAMA,JOGADOR_1]
        describe "Cria tabuleiro" $ do
                it "Valida criação do tabuleiro" $
                        criaTabuleiro tabuleiroVálido `shouldBe` tabuleiroVálido
        describe "Cria tabuleiro inválido" $ do
                it "Lança exceção ao criar tabuleiro inválido" $
                        evaluate(criaTabuleiro tabuleiroInválido) `shouldThrow` errorCall "Tabuleiro inválido"
        describe "Varre colunas" $ do
                it "Pega as coordenadas de um item varrendo as colunas de uma linha" $
                        pegaCoordenadasDeUmItemVarrendoColuna (getLinha tabuleiroVálido 5) 0 5 JOGADOR_1 `shouldBe` (5,2)
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
        describe "Incrementa capacidades" $ do
                it "Dado as capacidades de um jogador e há um novo item para ele coletar, incrementa nas suas capacidades esse item" $
                        incrementaCapacidades [(PRESENTE_PATINS, 1), (PRESENTE_ARREMESSO, 3)] PRESENTE_PATINS `shouldBe` [(PRESENTE_PATINS, 2), (PRESENTE_ARREMESSO, 3)]
        describe "Decrementa capacidades" $ do
                it "Dado as capacidades de um jogador e há um item a ser arremessado, decrementa esse itme nas suas capacidades" $
                        decrementaCapacidades [(PRESENTE_ARREMESSO, 3), (PRESENTE_PATINS, 2)] PRESENTE_ARREMESSO `shouldBe` [(PRESENTE_ARREMESSO,2),(PRESENTE_PATINS,2)]
        describe "Decrementa capacidades" $ do
                it "Dado as capacidades de um jogador e há um item a ser arremessado, e o jogador possui apenas 1 capacidade desse item, remove-o das capacidades" $
                        decrementaCapacidades [(PRESENTE_ARREMESSO, 1), (PRESENTE_PATINS, 2)] PRESENTE_ARREMESSO `shouldBe` [(PRESENTE_PATINS,2)]
        describe "Atualiza capacidades do jogador" $ do
                it "Dado um jogador e os itens de uma nova célula, devolve o jogador com as capacidades atualizadas" $
                        atualizaCapacidadesDoJogadorDeAcordoComOsItensQueElePodePegarDaNovaCélula jogador1 tabuleiroVálido NORTE (dadoCoordenadaPegarOsItens tabuleiroVálido (4,2)) `shouldBe` (1,(5,2),NORTE,[(PRESENTE_PATINS,0),(PRESENTE_ARREMESSO,1)])
        
        describe "Move jogador 1" $ do
                it "Move jogador 1 para a nova célula e remove itens" $
                        adicionaJogadorNaNovaCelulaERemoveItens [GRAMA, PRESENTE_ARREMESSO] JOGADOR_1 `shouldBe` [GRAMA, JOGADOR_1]
        describe "Move jogador 1" $ do
                it "Remove jogador 1 da antiga posição" $
                        removeJogadorCélula [GRAMA, JOGADOR_1] JOGADOR_1 `shouldBe` [GRAMA]
        describe "Move jogador 1" $ do
                it "Atualiza célula 2 da linha 5 e retorna uma nova linha removendo o jogador da atual posição (5,2) e movendo par ao NORTE (4,2)" $
                        atualizaCélulaJogador (getLinha tabuleiroVálido 5) JOGADOR_1 2 `shouldBe` ([PEDRA],[GRAMA],[PEDRA],[GRAMA,PAREDE],[PEDRA],[GRAMA],[GRAMA],[PEDRA])
        describe "Move jogador 1" $ do
                it "Monta um novo tabuleiro baseado na célula da linha em que o jogador 1 irá sair" $
                        montaNovoTabuleiroBaseadoNaNovaLinha tabuleiroVálido ( atualizaCélulaJogador (getLinha tabuleiroVálido 5) JOGADOR_1 2 ) 5  `shouldBe` (([PEDRA],[PEDRA],[PEDRA],[PEDRA],[PEDRA],[PEDRA],[PEDRA],[PEDRA]),([PEDRA],[GRAMA],[GRAMA],[GRAMA,PRESENTE_PATINS],[GRAMA],[GRAMA],[GRAMA],[PEDRA]),([PEDRA],[GRAMA],[PEDRA],[GRAMA],[PEDRA],[GRAMA],[GRAMA,PAREDE],[PEDRA]),([PEDRA],[GRAMA,PRESENTE_ARREMESSO],[GRAMA],[GRAMA,BOMBA],[GRAMA,PAREDE],[GRAMA],[GRAMA,JOGADOR_2],[PEDRA]),([PEDRA],[GRAMA],[PEDRA],[GRAMA,PAREDE],[PEDRA],[GRAMA],[GRAMA],[PEDRA]),([PEDRA],[GRAMA,PRESENTE_PATINS],[GRAMA],[GRAMA],[GRAMA],[GRAMA,PAREDE],[GRAMA],[PEDRA]),([PEDRA],[GRAMA,BOMBA],[GRAMA],[GRAMA,PRESENTE_ARREMESSO,JOGADOR_4],[GRAMA,JOGADOR_3],[GRAMA],[GRAMA],[PEDRA]),([PEDRA],[PEDRA],[PEDRA],[PEDRA],[PEDRA],[PEDRA],[PEDRA],[PEDRA]))
        describe "Move jogador 1" $ do
                it "Atualiza célula com a nova posição do jogador (chegando na posição (4,2)) " $
                        atualizaCélulaNovaPosicaoDoJogador (getLinha tabuleiroVálido 4) JOGADOR_1 2 `shouldBe` ([PEDRA],[GRAMA,JOGADOR_1],[GRAMA],[GRAMA,BOMBA],[GRAMA,PAREDE],[GRAMA],[GRAMA,JOGADOR_2],[PEDRA])
        describe "Move jogador 1" $ do
                it "Monta um novo tabuleiro baseado na célula da linha em que o jogador 1 irá chegar (nova posição)" $
                        montaNovoTabuleiroBaseadoNaNovaLinha tabuleiroVálido (atualizaCélulaNovaPosicaoDoJogador (getLinha tabuleiroVálido 4) JOGADOR_1 2) 4 `shouldBe` (([PEDRA],[PEDRA],[PEDRA],[PEDRA],[PEDRA],[PEDRA],[PEDRA],[PEDRA]),([PEDRA],[GRAMA],[GRAMA],[GRAMA,PRESENTE_PATINS],[GRAMA],[GRAMA],[GRAMA],[PEDRA]),([PEDRA],[GRAMA],[PEDRA],[GRAMA],[PEDRA],[GRAMA],[GRAMA,PAREDE],[PEDRA]),([PEDRA],[GRAMA,JOGADOR_1],[GRAMA],[GRAMA,BOMBA],[GRAMA,PAREDE],[GRAMA],[GRAMA,JOGADOR_2],[PEDRA]),([PEDRA],[GRAMA,JOGADOR_1],[PEDRA],[GRAMA,PAREDE],[PEDRA],[GRAMA],[GRAMA],[PEDRA]),([PEDRA],[GRAMA,PRESENTE_PATINS],[GRAMA],[GRAMA],[GRAMA],[GRAMA,PAREDE],[GRAMA],[PEDRA]),([PEDRA],[GRAMA,BOMBA],[GRAMA],[GRAMA,PRESENTE_ARREMESSO,JOGADOR_4],[GRAMA,JOGADOR_3],[GRAMA],[GRAMA],[PEDRA]),([PEDRA],[PEDRA],[PEDRA],[PEDRA],[PEDRA],[PEDRA],[PEDRA],[PEDRA]))
        describe "Move jogador 1" $ do
                it "Valida se quando o jogador 1 mover para o Norte ele deixa a sua posição (5,2) e assume uma nova (4,2)" $
                        movimentaJogadorNoTabuleiro tabuleiroVálido JOGADOR_1 (5,2) (4,2) [GRAMA, PRESENTE_ARREMESSO] `shouldBe` (([PEDRA],[PEDRA],[PEDRA],[PEDRA],[PEDRA],[PEDRA],[PEDRA],[PEDRA]),([PEDRA],[GRAMA],[GRAMA],[GRAMA,PRESENTE_PATINS],[GRAMA],[GRAMA],[GRAMA],[PEDRA]),([PEDRA],[GRAMA],[PEDRA],[GRAMA],[PEDRA],[GRAMA],[GRAMA,PAREDE],[PEDRA]),([PEDRA],[GRAMA,JOGADOR_1],[GRAMA],[GRAMA,BOMBA],[GRAMA,PAREDE],[GRAMA],[GRAMA,JOGADOR_2],[PEDRA]),([PEDRA],[GRAMA],[PEDRA],[GRAMA,PAREDE],[PEDRA],[GRAMA],[GRAMA],[PEDRA]),([PEDRA],[GRAMA,PRESENTE_PATINS],[GRAMA],[GRAMA],[GRAMA],[GRAMA,PAREDE],[GRAMA],[PEDRA]),([PEDRA],[GRAMA,BOMBA],[GRAMA],[GRAMA,PRESENTE_ARREMESSO,JOGADOR_4],[GRAMA,JOGADOR_3],[GRAMA],[GRAMA],[PEDRA]),([PEDRA],[PEDRA],[PEDRA],[PEDRA],[PEDRA],[PEDRA],[PEDRA],[PEDRA]))
        describe "Move jogador 1" $ do
                it "Valida se quando o jogador 1 mover para o Leste ele deixa a sua posição (5,2) e assume uma nova (5,3) -- em que tem PEDRA -- dará erro que ele não pode se mover" $
                        evaluate(movimentaJogadorNoTabuleiro tabuleiroVálido JOGADOR_1 (5,2) (5,3) [PEDRA]) `shouldThrow` errorCall "Jogador não pode se mover para a posição desejada"
        describe "Atualiza jogador 1" $ do
                it "Atualiza capacidades do jogador 1" $
                        atualizaJogador jogador1 tabuleiroVálido NORTE [(PRESENTE_ARREMESSO, 1)] `shouldBe` (1, (5,2), NORTE, [(PRESENTE_ARREMESSO, 1)])
        describe "Fim de jogo" $ do
                it "Valida se é fim de jogo" $
                        éFimDeJogo tabuleiroVálido `shouldBe` False
        describe "Fim de jogo" $ do
                it "Valida se é fim de jogo" $
                        éFimDeJogo (([PEDRA],[PEDRA],[PEDRA],[PEDRA],[PEDRA],[PEDRA],[PEDRA],[PEDRA]),([PEDRA],[GRAMA],[GRAMA],[GRAMA,PRESENTE_PATINS],[GRAMA],[GRAMA],[GRAMA],[PEDRA]),([PEDRA],[GRAMA],[PEDRA],[GRAMA],[PEDRA],[GRAMA],[GRAMA,PAREDE],[PEDRA]),([PEDRA],[GRAMA,JOGADOR_1],[GRAMA],[GRAMA,BOMBA],[GRAMA,PAREDE],[GRAMA],[GRAMA],[PEDRA]),([PEDRA],[GRAMA],[PEDRA],[GRAMA,PAREDE],[PEDRA],[GRAMA],[GRAMA],[PEDRA]),([PEDRA],[GRAMA],[GRAMA],[GRAMA],[GRAMA],[GRAMA,PAREDE],[GRAMA],[PEDRA]),([PEDRA],[GRAMA,BOMBA],[GRAMA],[GRAMA,PRESENTE_ARREMESSO],[GRAMA],[GRAMA],[GRAMA],[PEDRA]),([PEDRA],[PEDRA],[PEDRA],[PEDRA],[PEDRA],[PEDRA],[PEDRA],[PEDRA])) `shouldBe` True


                        
