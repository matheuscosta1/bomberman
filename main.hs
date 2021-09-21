{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
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


--Coluna 1
coluna11 = [PEDRA]
coluna12 = [PEDRA]
coluna13 = [PEDRA]
coluna14 = [PEDRA]
coluna15 = [PEDRA]
coluna16 = [PEDRA]
coluna17 = [PEDRA]
coluna18 = [PEDRA]

--Coluna 2
coluna21 = [PEDRA]
coluna22 = [GRAMA]
coluna23 = [GRAMA]
coluna24 = [PRESENTE_PATINS]
coluna25 = [GRAMA]
coluna26 = [GRAMA]
coluna27 = [GRAMA]
coluna28 = [PEDRA]

--Coluna 3
coluna31 = [PEDRA]
coluna32 = [GRAMA]
coluna33 = [PEDRA]
coluna34 = [GRAMA]
coluna35 = [PEDRA]
coluna36 = [GRAMA]
coluna37 = [GRAMA, PAREDE]
coluna38 = [PEDRA]

--Coluna 4
coluna41 = [PEDRA]
coluna42 = [GRAMA, PRESENTE_ARREMESSO]
coluna43 = [GRAMA]
coluna44 = [GRAMA, BOMBA]

coluna45 = [GRAMA, PAREDE]
coluna46 = [GRAMA]
coluna47 = [GRAMA, JOGADOR_2]
coluna48 = [PEDRA]

--Coluna 5
coluna51 = [PEDRA]
coluna52 = [GRAMA, JOGADOR_1]
coluna53 = [PEDRA]
coluna54 = [GRAMA, PAREDE]

coluna55 = [PEDRA]
coluna56 = [GRAMA]
coluna57 = [GRAMA]
coluna58 = [PEDRA]

--Coluna 6
coluna61 = [PEDRA]
coluna62 = [GRAMA]
coluna63 = [GRAMA]
coluna64 = [GRAMA]
coluna65 = [GRAMA]
coluna66 = [GRAMA, PAREDE]
coluna67 = [GRAMA]
coluna68 = [PEDRA]

--Coluna 7
coluna71 = [PEDRA]
coluna72 = [GRAMA, BOMBA]
coluna73 = [GRAMA]
coluna74 = [GRAMA, PRESENTE_ARREMESSO, JOGADOR_4]
coluna75 = [GRAMA, JOGADOR_3]
coluna76 = [GRAMA]
coluna77 = [GRAMA]
coluna78 = [PEDRA]

--Coluna 8
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


--atualizaCélula:: Tabuleiro -> Item -> Direcao -> Tabuleiro
--atualizaCélula tabuleiro jogador direcao = resultado
--    where 
--        localizacaoJogador = pegaLocalizacaoJogador tabuleiro 0 jogador
--        localizacaoQueOJogadorQuerIr = pegaLocalizacaoQueOJogadorQuerIrBaseadoNaDirecao localizacaoJogador direcao
--        itensDaPosicaoQueOJogadorQuerIr = dadoCoordenadaPegarOsItens tabuleiro localizacaoQueOJogadorQuerIr

--        resultado 


pegaLocalizacaoJogador:: Tabuleiro -> Int -> Item -> Localizacao
pegaLocalizacaoJogador tabuleiro iterador item
    | iterador == 8 && not(procuraItemNasColunas linha 0 item) = (-1,-1)
    | procuraItemNasColunas linha 0 item = coordenadas
    | otherwise = pegaLocalizacaoJogador tabuleiro (iterador+1) item
    where
        linha = if iterador == 8 then getLinha tabuleiro (iterador-1) else getLinha tabuleiro (iterador+1)
        coordenadas = pegaCoordenadasDeUmItemVarrendoColunas linha 0 (iterador+1) item

pegaCoordenadasDeUmItemVarrendoColunas:: Linha -> Int -> Int -> Item -> Localizacao
pegaCoordenadasDeUmItemVarrendoColunas linha iterador numeroLinha item
    | iterador == 8 && not(validaItemExisteNaCélula célula item) = (-1,-1)
    | validaItemExisteNaCélula célula item && numeroLinha == 0 = (numeroLinha+1, iterador+1)
    | validaItemExisteNaCélula célula item = (numeroLinha, iterador+1)
    | otherwise = pegaCoordenadasDeUmItemVarrendoColunas linha (iterador+1) numeroLinha item
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


{-

>>>getLinha tabuleiro 5
([PEDRA],[GRAMA,JOGADOR_1],[PEDRA],[GRAMA,PAREDE],[PEDRA],[GRAMA],[GRAMA],[PEDRA])

>>>procuraItemNasColunas (getLinha tabuleiro 5) 0
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



