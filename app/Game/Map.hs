-- | Os países e regiões de 1962 (história alternativa) e o estado inicial.
--   Os contornos do mapa ficam em Game.Geografia.
module Game.Map
  ( paisesIniciais
  , initialState
  , turnoFinal
  ) where

import qualified Data.Map.Strict as Map
import Game.Types

-- ---------------------------------------------------------------------------
-- Os países em janeiro de 1962
--
-- Alinhamento: positivo = pró-EUA, negativo = pró-Reich.
-- A partir de +50 o país é controlado pelos EUA; a partir de -50, pelo Reich.
-- Algumas regiões juntam vários países atuais (em 1962 muitos eram colônias).

paisesIniciais :: [Pais]
paisesIniciais =
  --    código nome                                  sigla  região          estab. estratégico núcleo      alinhamento
  [ Pais 'U' "Estados Unidos"                        "EUA" AmericaDoNorte 4 True  (Just EUA)    100
  , Pais 'C' "Canadá"                                "CAN" AmericaDoNorte 4 False Nothing       85
  , Pais 'M' "México"                                "MEX" AmericaCentral 2 True  Nothing       35
  , Pais 'k' "Cuba"                                  "CUB" AmericaCentral 2 True  Nothing       (-10)
  , Pais 'a' "América Central e Caribe"              "CAM" AmericaCentral 1 False Nothing       30
  , Pais 'o' "Colômbia e Equador"                    "COL" AmericaDoSul   2 False Nothing       20
  , Pais 'v' "Venezuela e Guianas"                   "VEN" AmericaDoSul   2 True  Nothing       10
  , Pais 'p' "Peru e Bolívia"                        "PER" AmericaDoSul   1 False Nothing       0
  , Pais 'B' "Brasil"                                "BRA" AmericaDoSul   2 True  Nothing       5
  , Pais 'h' "Chile"                                 "CHL" AmericaDoSul   3 False Nothing       15
  , Pais 'A' "Argentina, Uruguai e Paraguai"         "ARG" AmericaDoSul   2 True  Nothing       (-30)
  , Pais 'R' "Reino Unido e Irlanda"                 "GBR" Europa         4 True  Nothing       80
  , Pais 'D' "Grande Reich Alemão"                   "REI" Europa         4 True  (Just Reich)  (-100)
  , Pais 'F' "França (Estado Francês)"               "FRA" Europa         2 True  Nothing       (-70)
  , Pais 'E' "Espanha"                               "ESP" Europa         2 True  Nothing       (-40)
  , Pais 't' "Portugal"                              "POR" Europa         2 False Nothing       (-15)
  , Pais 'L' "Itália e Líbia"                        "ITA" Europa         3 True  Nothing       (-75)
  , Pais 'x' "Bálcãs e Danúbio"                      "BAL" Europa         1 False Nothing       (-40)
  , Pais 'n' "Noruega"                               "NOR" Europa         2 False Nothing       (-60)
  , Pais 'S' "Suécia e Finlândia"                    "SUE" Europa         3 False Nothing       0
  , Pais 'w' "Ostland (Rússia ocupada)"              "OST" Europa         1 False Nothing       (-80)
  , Pais 'K' "Ucrânia (ocupada)"                     "UCR" Europa         1 False Nothing       (-70)
  , Pais 'Z' "Rússia Livre (Sibéria e Ásia Central)" "RUS" Asia           1 True  Nothing       10
  , Pais 'Y' "Turquia"                               "TUR" OrienteMedio   2 True  Nothing       0
  , Pais 'J' "Egito e Sudão"                         "EGI" OrienteMedio   2 True  Nothing       0
  , Pais 's' "Arábia"                                "ARA" OrienteMedio   2 True  Nothing       30
  , Pais 'i' "Iraque e Levante"                      "IRQ" OrienteMedio   1 False Nothing       (-20)
  , Pais 'r' "Irã e Afeganistão"                     "IRA" OrienteMedio   2 True  Nothing       0
  , Pais 'g' "Argélia, Marrocos e Tunísia"           "ALG" Africa         1 False Nothing       (-40)
  , Pais 'b' "África Ocidental"                      "AOC" Africa         1 False Nothing       (-30)
  , Pais 'z' "Mittelafrika (África Central)"         "MIT" Africa         1 False Nothing       (-40)
  , Pais 'e' "Etiópia e África Oriental"             "ETI" Africa         1 False Nothing       10
  , Pais 'u' "África Austral"                        "AFS" Africa         3 True  Nothing       (-10)
  , Pais 'I' "Índia (subcontinente)"                 "IND" Asia           2 True  Nothing       20
  , Pais 'c' "China"                                 "CHN" Asia           1 True  Nothing       15
  , Pais 'm' "Manchukuo"                             "MAN" Asia           1 False Nothing       (-70)
  , Pais '3' "Coreia"                                "COR" Asia           2 False Nothing       (-60)
  , Pais 'j' "Japão"                                 "JAP" Asia           4 True  Nothing       (-85)
  , Pais 'd' "Indochina, Tailândia e Birmânia"       "INC" Asia           1 False Nothing       (-40)
  , Pais '2' "Filipinas"                             "FIL" Asia           2 False Nothing       70
  , Pais '1' "Indonésia"                             "IDN" Asia           1 True  Nothing       0
  , Pais 'O' "Austrália e Nova Zelândia"             "AUS" Oceania        4 False Nothing       90
  ]

-- | Último turno: outubro de 1966 (20 trimestres).
turnoFinal :: Int
turnoFinal = 20

-- | Estado antes de o jogador escolher um lado (tela inicial).
initialState :: GameState
initialState = GameState
  { gsTurno      = 1
  , gsJogador    = EUA
  , gsIniciado   = False
  , gsPaises     = Map.fromList [ (paisCodigo p, p) | p <- paisesIniciais ]
  , gsOrcamento  = Map.fromList [ (EUA, 10), (Reich, 12) ]
  , gsEspacial   = Map.fromList [ (EUA, 0), (Reich, 0) ]
  , gsTensao     = 1
  , gsSemente    = 1962
  , gsFim        = Nothing
  , gsLog        = []
  , gsComandante = ""
  , gsPartidaId  = Nothing
  }
