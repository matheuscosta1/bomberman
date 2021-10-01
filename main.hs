{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-typed-holes #-}
type Célula = [Item]
type Linha = (Célula, Célula, Célula, Célula, Célula, Célula, Célula, Célula)
type Tabuleiro = (Linha, Linha, Linha, Linha, Linha, Linha, Linha, Linha)
data Item = GRAMA | PAREDE | PEDRA | PRESENTE_PATINS | PRESENTE_ARREMESSO | BOMBA | JOGADOR_1 | JOGADOR_2 | JOGADOR_3 | JOGADOR_4 | JOGADOR_5 | JOGADOR_6 | ITEM_NAO_ENCONTRADO deriving(Eq, Show)

data Direcao = NORTE | SUL | LESTE | OESTE deriving(Eq, Show)
type Identificador = Int
type Localizacao = (Int, Int)
type Capacidades = [(Item, Int)]
type Jogador = (Identificador, Localizacao, Direcao, Capacidades)


linha1 = (coluna11, coluna12, coluna13, coluna14, coluna15, coluna16, coluna17, coluna18)

linha2 = (coluna21, coluna22, coluna23, coluna24, coluna25, coluna26, coluna27, coluna28)

linha3 = (coluna31, coluna32, coluna33, coluna34, coluna35, coluna36, coluna37, coluna38)

linha4 = (coluna41, coluna42, coluna43, coluna44, coluna45, coluna46, coluna47, coluna48)

linha5 = (coluna51, coluna52, coluna53, coluna54, coluna55, coluna56, coluna57, coluna58)

linha6 = (coluna61, coluna62, coluna63, coluna64, coluna65, coluna66, coluna67, coluna68)

linha7 = (coluna71, coluna72, coluna73, coluna74, coluna75, coluna76, coluna77, coluna78)

linha8 = (coluna81, coluna82, coluna83, coluna84, coluna85, coluna86, coluna87, coluna88)


--coluna 1
coluna11 = [PEDRA]
coluna12 = [PEDRA]
coluna13 = [PEDRA]
coluna14 = [PEDRA]
coluna15 = [PEDRA]
coluna16 = [PEDRA]
coluna17 = [PEDRA]
coluna18 = [PEDRA]

--coluna 2
coluna21 = [PEDRA]
coluna22 = [GRAMA]
coluna23 = [GRAMA]
coluna24 = [PRESENTE_PATINS]
coluna25 = [GRAMA]
coluna26 = [GRAMA]
coluna27 = [GRAMA]
coluna28 = [PEDRA]

--coluna 3
coluna31 = [PEDRA]
coluna32 = [GRAMA]
coluna33 = [PEDRA]
coluna34 = [GRAMA]
coluna35 = [PEDRA]
coluna36 = [GRAMA]
coluna37 = [GRAMA, PAREDE]
coluna38 = [PEDRA]

--coluna 4
coluna41 = [PEDRA]
coluna42 = [GRAMA, PRESENTE_ARREMESSO]
coluna43 = [GRAMA]
coluna44 = [GRAMA, BOMBA]

coluna45 = [GRAMA, PAREDE]
coluna46 = [GRAMA]
coluna47 = [GRAMA, JOGADOR_2]
coluna48 = [PEDRA]

--coluna 5
coluna51 = [PEDRA]
coluna52 = [GRAMA, JOGADOR_1]
coluna53 = [PEDRA]
coluna54 = [GRAMA, PAREDE]

coluna55 = [PEDRA]
coluna56 = [GRAMA]
coluna57 = [GRAMA]
coluna58 = [PEDRA]

--coluna 6
coluna61 = [PEDRA]
coluna62 = [GRAMA]
coluna63 = [GRAMA]
coluna64 = [GRAMA]
coluna65 = [GRAMA]
coluna66 = [GRAMA, PAREDE]
coluna67 = [GRAMA]
coluna68 = [PEDRA]

--coluna 7
coluna71 = [PEDRA]
coluna72 = [GRAMA, BOMBA]
coluna73 = [GRAMA]
coluna74 = [GRAMA, PRESENTE_ARREMESSO, JOGADOR_4]
coluna75 = [GRAMA, JOGADOR_3]
coluna76 = [GRAMA]
coluna77 = [GRAMA]
coluna78 = [PEDRA]

--coluna 8
coluna81 = [PEDRA]
coluna82 = [PEDRA]
coluna83 = [PEDRA]
coluna84 = [PEDRA]
coluna85 = [PEDRA]
coluna86 = [PEDRA]
coluna87 = [PEDRA]
coluna88 = [PEDRA]

identificador:: Jogador -> Identificador
identificador (identificador,_,_,_) = identificador

localizacao:: Jogador -> Localizacao
localizacao (_,localizacao,_,_) = localizacao

direcao:: Jogador -> Direcao
direcao (_,_,direcao,_) = direcao

capacidade:: Jogador -> Capacidades
capacidade (_,_,_,capacidade) = capacidade

verificaSeÉBuraco:: Célula -> Bool
verificaSeÉBuraco = null

verificaGrama:: Célula -> Bool
verificaGrama célula = head célula == GRAMA

verificaPresente:: Célula -> Bool
verificaPresente célula = resultado
    where
        caudaCélula = head(tail célula)
        resultado = verificaGrama célula && caudaCélula == PRESENTE_ARREMESSO || caudaCélula == PRESENTE_PATINS

verificaPedra:: Célula -> Bool
verificaPedra célula = head célula == PEDRA

verificaBomba:: Célula -> Bool
verificaBomba célula = resultado
    where
        caudaCélula = head(tail célula)
        resultado = verificaGrama célula && caudaCélula == BOMBA

verificaJogador:: Célula -> Item -> Bool
verificaJogador célula jogador = verificaGrama célula && head(tail célula) == jogador

paredeEstáNaBase:: Célula -> Bool
paredeEstáNaBase célula = head célula == PAREDE

paredeEstáSobreGrama:: Célula -> Bool
paredeEstáSobreGrama célula = verificaGrama célula && head(tail célula) == PAREDE

verificaParede:: Célula -> Bool
verificaParede célula = resultado
    where
    cabeca = head(tail célula)
    cauda = head(tail(tail célula))
    resultado = paredeEstáNaBase célula || paredeEstáSobreGrama célula || cabeca == PRESENTE_ARREMESSO || head(tail célula) == PRESENTE_PATINS && cauda == PAREDE

validaBomba:: Célula -> Bool
validaBomba célula = resultado
    where
        caudaCélula = head(tail célula)
        resultado = verificaGrama célula && caudaCélula == BOMBA

validaCélula:: Célula -> Bool
validaCélula célula
    | PRESENTE_PATINS `elem` célula || PRESENTE_ARREMESSO `elem` célula = verificaPresente célula
    | PAREDE `elem` célula = verificaParede célula
    | PEDRA `elem` célula = verificaPedra célula
    | BOMBA `elem` célula = verificaBomba célula
    | JOGADOR_1 `elem` célula = verificaJogador célula JOGADOR_1
    | JOGADOR_2 `elem` célula = verificaJogador célula JOGADOR_2
    | JOGADOR_3 `elem` célula = verificaJogador célula JOGADOR_3
    | JOGADOR_4 `elem` célula = verificaJogador célula JOGADOR_4
    | JOGADOR_5 `elem` célula = verificaJogador célula JOGADOR_5
    | JOGADOR_6 `elem` célula = verificaJogador célula JOGADOR_6
    | GRAMA `elem` célula = verificaGrama célula
    | otherwise = verificaSeÉBuraco célula
   

percorreCélulasParaValidarTabuleiro:: Linha -> Int -> [Bool]
percorreCélulasParaValidarTabuleiro linha iterador
    | iterador == 8 && not(validaCélula célula) = [False]
    | validaCélula célula = [True]
    | otherwise = percorreCélulasParaValidarTabuleiro linha (iterador+1)
    where
        célula = if iterador == 8 then getCélula linha (iterador-1) else getCélula linha (iterador+1)
        




splitTodos :: [Char] -> Char -> [[Char]]
splitTodos "" a = []
splitTodos lista caractere = takeWhile (/= caractere) lista : splitTodos (drop 1 (dropWhile (/= caractere) lista)) caractere

{--



criaTabuleiro:: Tabuleiro -> Tabuleiro
criaTabuleiro tabuleiro = if validaTabuleiro tabuleiro then tabuleiro else error "Tabuleiro inválido"


validaTabuleiro:: Tabuleiro -> Bool
validaTabuleiro tabuleiro = resultado
    where


percorreLinhasParaValidarTabuleiro:: Tabuleiro -> Int -> [Bool]
percorreLinhasParaValidarTabuleiro tabuleiro iterador
    | 
    | otherwise = percorreCélulasParaValidarTabuleiro tabuleiro (iterador+1)
    where
        linha = if iterador == 8 then getLinha tabuleiro (iterador-1) else getLinha tabuleiro (iterador+1)

--}


tabuleiro:: Tabuleiro
tabuleiro = (linha1, linha2, linha3, linha4, linha5, linha6, linha7, linha8)

getLinha:: Tabuleiro -> Int -> Linha
getLinha (linha,_,_,_,_,_,_,_) 1 = linha
getLinha (_,linha,_,_,_,_,_,_) 2 = linha
getLinha (_,_,linha,_,_,_,_,_) 3 = linha
getLinha (_,_,_,linha,_,_,_,_) 4 = linha
getLinha (_,_,_,_,linha,_,_,_) 5 = linha
getLinha (_,_,_,_,_,linha,_,_) 6 = linha
getLinha (_,_,_,_,_,_,linha,_) 7 = linha
getLinha (_,_,_,_,_,_,_,linha) 8 = linha

getCélula:: Linha -> Int -> Célula
getCélula (célula,_,_,_,_,_,_,_) 1 = célula
getCélula (_,célula,_,_,_,_,_,_) 2 = célula
getCélula (_,_,célula,_,_,_,_,_) 3 = célula
getCélula (_,_,_,célula,_,_,_,_) 4 = célula
getCélula (_,_,_,_,célula,_,_,_) 5 = célula
getCélula (_,_,_,_,_,célula,_,_) 6 = célula
getCélula (_,_,_,_,_,_,célula,_) 7 = célula
getCélula (_,_,_,_,_,_,_,célula) 8 = célula

atualizaCélulaJogador:: Linha -> Item -> Int ->  Linha
atualizaCélulaJogador (celula1, celula2, celula3, celula4, celula5, celula6, celula7, celula8) jogador 1 = (removeJogadorCélula celula1 jogador, celula2 , celula3, celula4, celula5, celula6, celula7, celula8)
atualizaCélulaJogador (celula1, celula2, celula3, celula4, celula5, celula6, celula7, celula8) jogador 2 = (celula1, removeJogadorCélula celula2 jogador, celula3, celula4, celula5, celula6, celula7, celula8)
atualizaCélulaJogador (celula1, celula2, celula3, celula4, celula5, celula6, celula7, celula8) jogador 3 = (celula1, celula2 , removeJogadorCélula celula3 jogador, celula4, celula5, celula6, celula7, celula8)
atualizaCélulaJogador (celula1, celula2, celula3, celula4, celula5, celula6, celula7, celula8) jogador 4 = (celula1, celula2 , celula3, removeJogadorCélula celula4 jogador, celula5, celula6, celula7, celula8)
atualizaCélulaJogador (celula1, celula2, celula3, celula4, celula5, celula6, celula7, celula8) jogador 5 = (celula1, celula2 , celula3, celula4, removeJogadorCélula celula5 jogador, celula6, celula7, celula8)
atualizaCélulaJogador (celula1, celula2, celula3, celula4, celula5, celula6, celula7, celula8) jogador 6 = (celula1, celula2 , celula3, celula4, celula5, removeJogadorCélula celula6 jogador, celula7, celula8)
atualizaCélulaJogador (celula1, celula2, celula3, celula4, celula5, celula6, celula7, celula8) jogador 7 = (celula1, celula2 , celula3, celula4, celula5, celula6, removeJogadorCélula celula7 jogador, celula8)
atualizaCélulaJogador (celula1, celula2, celula3, celula4, celula5, celula6, celula7, celula8) jogador 8 = (celula1, celula2 , celula3, celula4, celula5, celula6, celula7, removeJogadorCélula celula8 jogador)

montaNovoTabuleiroBaseadoNaNovaLinha:: Tabuleiro -> Linha -> Int -> Tabuleiro
montaNovoTabuleiroBaseadoNaNovaLinha (linha1, linha2, linha3, linha4, linha5, linha6, linha7, linha8) novaLinha 1 = (novaLinha, linha2, linha3, linha4, linha5, linha6, linha7, linha8)
montaNovoTabuleiroBaseadoNaNovaLinha (linha1, linha2, linha3, linha4, linha5, linha6, linha7, linha8) novaLinha 2 = (linha1, novaLinha, linha3, linha4, linha5, linha6, linha7, linha8)
montaNovoTabuleiroBaseadoNaNovaLinha (linha1, linha2, linha3, linha4, linha5, linha6, linha7, linha8) novaLinha 3 = (linha1, linha2, novaLinha, linha4, linha5, linha6, linha7, linha8)
montaNovoTabuleiroBaseadoNaNovaLinha (linha1, linha2, linha3, linha4, linha5, linha6, linha7, linha8) novaLinha 4 = (linha1, linha2, linha3, novaLinha, linha5, linha6, linha7, linha8)
montaNovoTabuleiroBaseadoNaNovaLinha (linha1, linha2, linha3, linha4, linha5, linha6, linha7, linha8) novaLinha 5 = (linha1, linha2, linha3, linha4, novaLinha, linha6, linha7, linha8)
montaNovoTabuleiroBaseadoNaNovaLinha (linha1, linha2, linha3, linha4, linha5, linha6, linha7, linha8) novaLinha 6 = (linha1, linha2, linha3, linha4, linha5, novaLinha, linha7, linha8)
montaNovoTabuleiroBaseadoNaNovaLinha (linha1, linha2, linha3, linha4, linha5, linha6, linha7, linha8) novaLinha 7 = (linha1, linha2, linha3, linha4, linha5, linha6, novaLinha, linha8)
montaNovoTabuleiroBaseadoNaNovaLinha (linha1, linha2, linha3, linha4, linha5, linha6, linha7, linha8) novaLinha 8 = (linha1, linha2, linha3, linha4, linha5, linha6, linha7, novaLinha)

adicionaJogadorNaNovaCelulaERemoveItens:: Célula -> Item -> Célula
adicionaJogadorNaNovaCelulaERemoveItens [] jogador = []
adicionaJogadorNaNovaCelulaERemoveItens (x:xs) jogador = x:[jogador]
    --TODO: validar se for algum item do tipo PRESENTE_PATINS ou PRESENTE_ARREMESSO, guardar no inventário do jogador
    --TODO: se a célula estiver vazia, o jogador morre

atualizaCélulaNovaPosicaoDoJogador:: Linha -> Item -> Int ->  Linha
atualizaCélulaNovaPosicaoDoJogador (celula1, celula2, celula3, celula4, celula5, celula6, celula7, celula8) jogador 1 = (adicionaJogadorNaNovaCelulaERemoveItens celula1 jogador, celula2 , celula3, celula4, celula5, celula6, celula7, celula8)
atualizaCélulaNovaPosicaoDoJogador (celula1, celula2, celula3, celula4, celula5, celula6, celula7, celula8) jogador 2 = (celula1, adicionaJogadorNaNovaCelulaERemoveItens celula2 jogador, celula3, celula4, celula5, celula6, celula7, celula8)
atualizaCélulaNovaPosicaoDoJogador (celula1, celula2, celula3, celula4, celula5, celula6, celula7, celula8) jogador 3 = (celula1, celula2 , adicionaJogadorNaNovaCelulaERemoveItens celula3 jogador, celula4, celula5, celula6, celula7, celula8)
atualizaCélulaNovaPosicaoDoJogador (celula1, celula2, celula3, celula4, celula5, celula6, celula7, celula8) jogador 4 = (celula1, celula2 , celula3, adicionaJogadorNaNovaCelulaERemoveItens celula4 jogador, celula5, celula6, celula7, celula8)
atualizaCélulaNovaPosicaoDoJogador (celula1, celula2, celula3, celula4, celula5, celula6, celula7, celula8) jogador 5 = (celula1, celula2 , celula3, celula4, adicionaJogadorNaNovaCelulaERemoveItens celula5 jogador, celula6, celula7, celula8)
atualizaCélulaNovaPosicaoDoJogador (celula1, celula2, celula3, celula4, celula5, celula6, celula7, celula8) jogador 6 = (celula1, celula2 , celula3, celula4, celula5, adicionaJogadorNaNovaCelulaERemoveItens celula6 jogador, celula7, celula8)
atualizaCélulaNovaPosicaoDoJogador (celula1, celula2, celula3, celula4, celula5, celula6, celula7, celula8) jogador 7 = (celula1, celula2 , celula3, celula4, celula5, celula6, adicionaJogadorNaNovaCelulaERemoveItens celula7 jogador, celula8)
atualizaCélulaNovaPosicaoDoJogador (celula1, celula2, celula3, celula4, celula5, celula6, celula7, celula8) jogador 8 = (celula1, celula2 , celula3, celula4, celula5, celula6, celula7, adicionaJogadorNaNovaCelulaERemoveItens celula8 jogador)

--atualizaTabuleiro:: Linha -> 
movimentaJogador:: Tabuleiro -> Item -> Direcao -> Tabuleiro
movimentaJogador tabuleiro jogador direcao = resultado
    where
        (linhaQueOJogadorEstá, colunaQueOJogadorEstá) = pegaLocalizacaoJogador tabuleiro 0 jogador

        (linhaQueOJogadorQuerIr, colunaQueOJogadorQuerIr) = pegaLocalizacaoQueOJogadorQuerIrBaseadoNaDirecao (linhaQueOJogadorEstá, colunaQueOJogadorEstá) direcao

        --TODO: validar se o jogador pode movimentar ou não. Se for bomba ou pedra, não pode mover
        itensQueEstaoNaNovaPosicaoQueOJogadorQuerIr = dadoCoordenadaPegarOsItens tabuleiro (linhaQueOJogadorQuerIr, colunaQueOJogadorQuerIr)

        linhaJogador = getLinha tabuleiro linhaQueOJogadorEstá
        novaLinha = atualizaCélulaJogador linhaJogador jogador colunaQueOJogadorEstá

        tabuleiroComAPosicaoAntigaDoJogadorAtualizada = montaNovoTabuleiroBaseadoNaNovaLinha tabuleiro novaLinha linhaQueOJogadorEstá

        linhaComANovaPosicaoJogador = getLinha tabuleiroComAPosicaoAntigaDoJogadorAtualizada linhaQueOJogadorQuerIr
        novaLinhaComANovaPosicaoDoJogador = atualizaCélulaNovaPosicaoDoJogador linhaComANovaPosicaoJogador jogador colunaQueOJogadorQuerIr

        tabuleiroComANovaPosicaoDoJogadorAtualizada = montaNovoTabuleiroBaseadoNaNovaLinha tabuleiroComAPosicaoAntigaDoJogadorAtualizada novaLinhaComANovaPosicaoDoJogador linhaQueOJogadorQuerIr

        --todo: falta atualizar a célula que o jogador irá chegar (nova posicao). já está funcionando atualizar (FEITO)
        resultado = tabuleiroComANovaPosicaoDoJogadorAtualizada

pegaLocalizacaoJogador:: Tabuleiro -> Int -> Item -> Localizacao
pegaLocalizacaoJogador tabuleiro iterador item
    | iterador == 8 && not(procuraItemNasColunas linha 0 item) = (-1,-1)
    | procuraItemNasColunas linha 0 item = coordenadas
    | otherwise = pegaLocalizacaoJogador tabuleiro (iterador+1) item
    where
        linha = if iterador == 8 then getLinha tabuleiro (iterador-1) else getLinha tabuleiro (iterador+1)
        coordenadas = pegaCoordenadasDeUmItemVarrendoColuna linha 0 (iterador+1) item

pegaCoordenadasDeUmItemVarrendoColuna:: Linha -> Int -> Int -> Item -> Localizacao
pegaCoordenadasDeUmItemVarrendoColuna linha iterador numeroLinha item
    | iterador == 8 && not(validaItemExisteNaCélula célula item) = (-1,-1)
    | validaItemExisteNaCélula célula item && numeroLinha == 0 = (numeroLinha+1, iterador+1)
    | validaItemExisteNaCélula célula item = (numeroLinha, iterador+1)
    | otherwise = pegaCoordenadasDeUmItemVarrendoColuna linha (iterador+1) numeroLinha item
    where
        célula = if iterador == 8 then getCélula linha (iterador-1) else getCélula linha (iterador+1)

validaItemExisteNaCélula:: Célula -> Item -> Bool
validaItemExisteNaCélula célula item = item `elem` célula

pegaItem:: Célula -> Item -> Item
pegaItem célula item
    | item `elem` célula = item
    | otherwise = ITEM_NAO_ENCONTRADO

jogador1:: Jogador
jogador1 = (1, pegaLocalizacaoJogador tabuleiro 0 JOGADOR_1, NORTE, [])

convertJogadorStringToItem:: String -> Item
convertJogadorStringToItem jogador
    | jogador == "JOGADOR_1" = JOGADOR_1
    | jogador == "JOGADOR_2" = JOGADOR_2
    | jogador == "JOGADOR_3" = JOGADOR_3
    | jogador == "JOGADOR_4" = JOGADOR_4
    | jogador == "JOGADOR_5" = JOGADOR_5
    | otherwise = JOGADOR_6

atualizaJogador:: Jogador -> Capacidades -> Jogador
atualizaJogador jogador capacidades = resultado
    where
        direcao1 = direcao jogador
        identificador1 = identificador jogador
        jogadorIdentificador = convertJogadorStringToItem ("JOGADOR_"++show identificador1)
        localizacaoJogador = pegaLocalizacaoJogador tabuleiro 0 jogadorIdentificador
        resultado = (identificador1, localizacaoJogador, direcao1, capacidades)

percorreLinhas:: Tabuleiro -> Int -> Item -> Item
percorreLinhas tabuleiro iterador item
    | iterador == 8 && not(procuraItemNasColunas linha 0 item) = ITEM_NAO_ENCONTRADO
    | procuraItemNasColunas linha 0 item = item
    | otherwise = percorreLinhas tabuleiro (iterador+1) item
    where
        linha = if iterador == 8 then getLinha tabuleiro (iterador-1) else getLinha tabuleiro (iterador+1)

procuraItemNasColunas:: Linha -> Int -> Item -> Bool
procuraItemNasColunas linha iterador item
    | iterador == 8 && not(validaItemExisteNaCélula célula item) = False
    | validaItemExisteNaCélula célula item = True
    | otherwise = procuraItemNasColunas linha (iterador+1) item
    where
        célula = if iterador == 8 then getCélula linha (iterador-1) else getCélula linha (iterador+1)

dadoCoordenadaPegarOsItens:: Tabuleiro -> Localizacao -> Célula
dadoCoordenadaPegarOsItens tabuleiro (coordenadaX, coordenadaY) = resultado
    where
        linha = getLinha tabuleiro coordenadaX
        célula = getCélula linha coordenadaY
        resultado = célula

pegaLocalizacaoQueOJogadorQuerIrBaseadoNaDirecao:: Localizacao -> Direcao -> Localizacao
pegaLocalizacaoQueOJogadorQuerIrBaseadoNaDirecao (linha, coluna) direcao
    | direcao == NORTE = (linha-1, coluna)
    | direcao == SUL = (linha+1, coluna)
    | direcao == LESTE = (linha, coluna+1)
    | otherwise = (linha, coluna-1)

removeJogadorCélula:: Célula -> Item -> Célula
removeJogadorCélula [] jogador = []
removeJogadorCélula (x:xs) jogador = if jogador == x then removeJogadorCélula xs jogador else x:removeJogadorCélula xs jogador

jogadores:: [Item]
jogadores = [JOGADOR_1, JOGADOR_2, JOGADOR_3, JOGADOR_4, JOGADOR_5, JOGADOR_6]

percorreJogadores:: Tabuleiro -> [Item] -> [Item]
percorreJogadores tabuleiro [] = []
percorreJogadores tabuleiro (x:xs) = resultado
    where
        jogador = percorreLinhas tabuleiro 0 x
        resultado = if jogador `elem` jogadores then x:percorreJogadores tabuleiro xs else percorreJogadores tabuleiro xs

éFimDeJogo:: Tabuleiro -> Bool
éFimDeJogo tabuleiro = length(percorreJogadores tabuleiro jogadores) == 1

{-

>>>getLinha tabuleiro 5
([PEDRA],[GRAMA,JOGADOR_1],[PEDRA],[GRAMA,PAREDE],[PEDRA],[GRAMA],[GRAMA],[PEDRA])

>>>procuraItemNasColunas (getLinha tabuleiro 5) 0 JOGADOR_1
True

>>>pegaLocalizacaoJogador tabuleiro 0 JOGADOR_1
(5,2)

>>>pegaLocalizacaoJogador tabuleiro 0 JOGADOR_4
(7,4)

>>>pegaItem (getCélula (getLinha tabuleiro 5) 2) JOGADOR_1
JOGADOR_1

>>>pegaItem (getCélula (getLinha tabuleiro 5) 5) JOGADOR_1
ITEM_NAO_ENCONTRADO

-}
