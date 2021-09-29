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


linha1 = (colunaQueOJogadorEstá11, colunaQueOJogadorEstá12, colunaQueOJogadorEstá13, colunaQueOJogadorEstá14, colunaQueOJogadorEstá15, colunaQueOJogadorEstá16, colunaQueOJogadorEstá17, colunaQueOJogadorEstá18)

linha2 = (colunaQueOJogadorEstá21, colunaQueOJogadorEstá22, colunaQueOJogadorEstá23, colunaQueOJogadorEstá24, colunaQueOJogadorEstá25, colunaQueOJogadorEstá26, colunaQueOJogadorEstá27, colunaQueOJogadorEstá28)

linha3 = (colunaQueOJogadorEstá31, colunaQueOJogadorEstá32, colunaQueOJogadorEstá33, colunaQueOJogadorEstá34, colunaQueOJogadorEstá35, colunaQueOJogadorEstá36, colunaQueOJogadorEstá37, colunaQueOJogadorEstá38)

linha4 = (colunaQueOJogadorEstá41, colunaQueOJogadorEstá42, colunaQueOJogadorEstá43, colunaQueOJogadorEstá44, colunaQueOJogadorEstá45, colunaQueOJogadorEstá46, colunaQueOJogadorEstá47, colunaQueOJogadorEstá48)

linha5 = (colunaQueOJogadorEstá51, colunaQueOJogadorEstá52, colunaQueOJogadorEstá53, colunaQueOJogadorEstá54, colunaQueOJogadorEstá55, colunaQueOJogadorEstá56, colunaQueOJogadorEstá57, colunaQueOJogadorEstá58)

linha6 = (colunaQueOJogadorEstá61, colunaQueOJogadorEstá62, colunaQueOJogadorEstá63, colunaQueOJogadorEstá64, colunaQueOJogadorEstá65, colunaQueOJogadorEstá66, colunaQueOJogadorEstá67, colunaQueOJogadorEstá68)

linha7 = (colunaQueOJogadorEstá71, colunaQueOJogadorEstá72, colunaQueOJogadorEstá73, colunaQueOJogadorEstá74, colunaQueOJogadorEstá75, colunaQueOJogadorEstá76, colunaQueOJogadorEstá77, colunaQueOJogadorEstá78)

linha8 = (colunaQueOJogadorEstá81, colunaQueOJogadorEstá82, colunaQueOJogadorEstá83, colunaQueOJogadorEstá84, colunaQueOJogadorEstá85, colunaQueOJogadorEstá86, colunaQueOJogadorEstá87, colunaQueOJogadorEstá88)


--colunaQueOJogadorEstá 1
colunaQueOJogadorEstá11 = [PEDRA]
colunaQueOJogadorEstá12 = [PEDRA]
colunaQueOJogadorEstá13 = [PEDRA]
colunaQueOJogadorEstá14 = [PEDRA]
colunaQueOJogadorEstá15 = [PEDRA]
colunaQueOJogadorEstá16 = [PEDRA]
colunaQueOJogadorEstá17 = [PEDRA]
colunaQueOJogadorEstá18 = [PEDRA]

--colunaQueOJogadorEstá 2
colunaQueOJogadorEstá21 = [PEDRA]
colunaQueOJogadorEstá22 = [GRAMA]
colunaQueOJogadorEstá23 = [GRAMA]
colunaQueOJogadorEstá24 = [PRESENTE_PATINS]
colunaQueOJogadorEstá25 = [GRAMA]
colunaQueOJogadorEstá26 = [GRAMA]
colunaQueOJogadorEstá27 = [GRAMA]
colunaQueOJogadorEstá28 = [PEDRA]

--colunaQueOJogadorEstá 3
colunaQueOJogadorEstá31 = [PEDRA]
colunaQueOJogadorEstá32 = [GRAMA]
colunaQueOJogadorEstá33 = [PEDRA]
colunaQueOJogadorEstá34 = [GRAMA]
colunaQueOJogadorEstá35 = [PEDRA]
colunaQueOJogadorEstá36 = [GRAMA]
colunaQueOJogadorEstá37 = [GRAMA, PAREDE]
colunaQueOJogadorEstá38 = [PEDRA]

--colunaQueOJogadorEstá 4
colunaQueOJogadorEstá41 = [PEDRA]
colunaQueOJogadorEstá42 = [GRAMA, PRESENTE_ARREMESSO]
colunaQueOJogadorEstá43 = [GRAMA]
colunaQueOJogadorEstá44 = [GRAMA, BOMBA]

colunaQueOJogadorEstá45 = [GRAMA, PAREDE]
colunaQueOJogadorEstá46 = [GRAMA]
colunaQueOJogadorEstá47 = [GRAMA, JOGADOR_2]
colunaQueOJogadorEstá48 = [PEDRA]

--colunaQueOJogadorEstá 5
colunaQueOJogadorEstá51 = [PEDRA]
colunaQueOJogadorEstá52 = [GRAMA, JOGADOR_1]
colunaQueOJogadorEstá53 = [PEDRA]
colunaQueOJogadorEstá54 = [GRAMA, PAREDE]

colunaQueOJogadorEstá55 = [PEDRA]
colunaQueOJogadorEstá56 = [GRAMA]
colunaQueOJogadorEstá57 = [GRAMA]
colunaQueOJogadorEstá58 = [PEDRA]

--colunaQueOJogadorEstá 6
colunaQueOJogadorEstá61 = [PEDRA]
colunaQueOJogadorEstá62 = [GRAMA]
colunaQueOJogadorEstá63 = [GRAMA]
colunaQueOJogadorEstá64 = [GRAMA]
colunaQueOJogadorEstá65 = [GRAMA]
colunaQueOJogadorEstá66 = [GRAMA, PAREDE]
colunaQueOJogadorEstá67 = [GRAMA]
colunaQueOJogadorEstá68 = [PEDRA]

--colunaQueOJogadorEstá 7
colunaQueOJogadorEstá71 = [PEDRA]
colunaQueOJogadorEstá72 = [GRAMA, BOMBA]
colunaQueOJogadorEstá73 = [GRAMA]
colunaQueOJogadorEstá74 = [GRAMA, PRESENTE_ARREMESSO, JOGADOR_4]
colunaQueOJogadorEstá75 = [GRAMA, JOGADOR_3]
colunaQueOJogadorEstá76 = [GRAMA]
colunaQueOJogadorEstá77 = [GRAMA]
colunaQueOJogadorEstá78 = [PEDRA]

--colunaQueOJogadorEstá 8
colunaQueOJogadorEstá81 = [PEDRA]
colunaQueOJogadorEstá82 = [PEDRA]
colunaQueOJogadorEstá83 = [PEDRA]
colunaQueOJogadorEstá84 = [PEDRA]
colunaQueOJogadorEstá85 = [PEDRA]
colunaQueOJogadorEstá86 = [PEDRA]
colunaQueOJogadorEstá87 = [PEDRA]
colunaQueOJogadorEstá88 = [PEDRA]

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

verificaPresente:: Célula -> Item -> Bool
verificaPresente célula presente = verificaGrama célula && head(tail célula) == presente

verificaJogador:: Célula -> Item -> Bool
verificaJogador célula jogador = verificaGrama célula && head(tail célula) == jogador

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
    | iterador == 8 && not(procuraItemNasColunaQueOJogadorEstá linha 0 item) = (-1,-1)
    | procuraItemNasColunaQueOJogadorEstá linha 0 item = coordenadas
    | otherwise = pegaLocalizacaoJogador tabuleiro (iterador+1) item
    where
        linha = if iterador == 8 then getLinha tabuleiro (iterador-1) else getLinha tabuleiro (iterador+1)
        coordenadas = pegaCoordenadasDeUmItemVarrendoColunaQueOJogadorEstá linha 0 (iterador+1) item

pegaCoordenadasDeUmItemVarrendoColunaQueOJogadorEstá:: Linha -> Int -> Int -> Item -> Localizacao
pegaCoordenadasDeUmItemVarrendoColunaQueOJogadorEstá linha iterador numeroLinha item
    | iterador == 8 && not(validaItemExisteNaCélula célula item) = (-1,-1)
    | validaItemExisteNaCélula célula item && numeroLinha == 0 = (numeroLinha+1, iterador+1)
    | validaItemExisteNaCélula célula item = (numeroLinha, iterador+1)
    | otherwise = pegaCoordenadasDeUmItemVarrendoColunaQueOJogadorEstá linha (iterador+1) numeroLinha item
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
    | iterador == 8 && not(procuraItemNasColunaQueOJogadorEstá linha 0 item) = ITEM_NAO_ENCONTRADO
    | procuraItemNasColunaQueOJogadorEstá linha 0 item = item
    | otherwise = percorreLinhas tabuleiro (iterador+1) item
    where
        linha = if iterador == 8 then getLinha tabuleiro (iterador-1) else getLinha tabuleiro (iterador+1)

procuraItemNasColunaQueOJogadorEstá:: Linha -> Int -> Item -> Bool
procuraItemNasColunaQueOJogadorEstá linha iterador item
    | iterador == 8 && not(validaItemExisteNaCélula célula item) = False
    | validaItemExisteNaCélula célula item = True
    | otherwise = procuraItemNasColunaQueOJogadorEstá linha (iterador+1) item
    where
        célula = if iterador == 8 then getCélula linha (iterador-1) else getCélula linha (iterador+1)

dadoCoordenadaPegarOsItens:: Tabuleiro -> Localizacao -> Célula
dadoCoordenadaPegarOsItens tabuleiro (coordenadaX, coordenadaY) = resultado
    where
        linha = getLinha tabuleiro coordenadaX
        célula = getCélula linha coordenadaY
        resultado = célula

pegaLocalizacaoQueOJogadorQuerIrBaseadoNaDirecao:: Localizacao -> Direcao -> Localizacao
pegaLocalizacaoQueOJogadorQuerIrBaseadoNaDirecao (linha, colunaQueOJogadorEstá) direcao
    | direcao == NORTE = (linha-1, colunaQueOJogadorEstá)
    | direcao == SUL = (linha+1, colunaQueOJogadorEstá)
    | direcao == LESTE = (linha, colunaQueOJogadorEstá+1)
    | otherwise = (linha, colunaQueOJogadorEstá-1)

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

>>>procuraItemNasColunaQueOJogadorEstá (getLinha tabuleiro 5) 0
No instance for (Show (Item -> Bool))
  arising from a use of ‘evalPrint’
  (maybe you haven't applied a function to enough arguments?)

>>>pegaLocalizacaoJogador tabuleiro 0 JOGADOR_1
(5,2)

>>>pegaLocalizacaoJogador tabuleiro 0 JOGADOR_4
(7,4)

>>>pegaItem (getCélula (getLinha tabuleiro 5) 2) JOGADOR_1
JOGADOR_1

>>>pegaItem (getCélula (getLinha tabuleiro 5) 5) JOGADOR_1
ITEM_NAO_ENCONTRADO

-}