--TESTE
type Célula = [Item]
type Linha = (Célula, Célula, Célula, Célula, Célula, Célula, Célula, Célula)
type Tabuleiro = (Linha, Linha, Linha, Linha, Linha, Linha, Linha, Linha)
data Item = GRAMA | PAREDE | PEDRA | PRESENTE_PATINS | PRESENTE_ARREMESSO | BOMBA | JOGADOR_1 | JOGADOR_2 | JOGADOR_3 | JOGADOR_4 | JOGADOR_5 | JOGADOR_6 deriving(Eq, Show)

data Direção = NORTE | SUL | LESTE | OESTE deriving(Eq, Show)
type Identificador = Int
type Localizacao = (Int, Int)
type Capacidade = (Item, Int)
type Jogador = (Identificador, Localizacao, Direção, Capacidade)

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
coluna74 = [GRAMA, PRESENTE_ARREMESSO]
coluna75 = [GRAMA]
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

