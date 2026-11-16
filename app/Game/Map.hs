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
  --    código nome                                      prep.  sigla região          estab. estratégico núcleo      alinhamento
  [ Pais 'U' "Estados Unidos"                          "nos"  "EUA" AmericaDoNorte 4 True  (Just EUA)    100
  , Pais 'C' "Canadá"                                  "no"   "CAN" AmericaDoNorte 4 False Nothing       85
  , Pais 'M' "México"                                  "no"   "MEX" AmericaCentral 2 True  Nothing       35
  , Pais 'k' "Cuba"                                    "em"   "CUB" AmericaCentral 2 True  Nothing       (-10)
  , Pais 'a' "América Central e Caribe"                "na"   "CAM" AmericaCentral 1 False Nothing       30
  , Pais 'o' "Colômbia e Equador"                      "na"   "COL" AmericaDoSul   2 False Nothing       20
  , Pais 'v' "Venezuela e Guianas"                     "na"   "VEN" AmericaDoSul   2 True  Nothing       10
  , Pais 'p' "Peru e Bolívia"                          "no"   "PER" AmericaDoSul   1 False Nothing       0
  , Pais 'B' "Brasil"                                  "no"   "BRA" AmericaDoSul   2 True  Nothing       5
  , Pais 'h' "Chile"                                   "no"   "CHL" AmericaDoSul   3 False Nothing       15
  , Pais 'A' "Argentina, Uruguai e Paraguai"           "na"   "ARG" AmericaDoSul   2 True  Nothing       (-30)
  , Pais 'R' "Reino Unido e Irlanda"                   "no"   "GBR" Europa         4 True  Nothing       80
  , Pais 'D' "Grande Reich Alemão"                     "no"   "REI" Europa         4 True  (Just Reich)  (-100)
  , Pais 'F' "França (Estado Francês)"                 "na"   "FRA" Europa         2 True  Nothing       (-70)
  , Pais 'E' "Espanha"                                 "na"   "ESP" Europa         2 True  Nothing       (-40)
  , Pais 't' "Portugal"                                "em"   "POR" Europa         2 False Nothing       (-15)
  , Pais 'L' "Itália e Líbia"                          "na"   "ITA" Europa         3 True  Nothing       (-75)
  , Pais 'x' "Bálcãs e Danúbio"                        "nos"  "BAL" Europa         1 False Nothing       (-40)
  , Pais 'n' "Noruega"                                 "na"   "NOR" Europa         2 False Nothing       (-60)
  , Pais 'S' "Suécia e Finlândia"                      "na"   "SUE" Europa         3 False Nothing       0
  , Pais 'w' "Ostland (Rússia ocupada)"                "no"   "OST" Europa         1 False Nothing       (-80)
  , Pais 'K' "Ucrânia (ocupada)"                       "na"   "UCR" Europa         1 False Nothing       (-70)
  , Pais 'Z' "Rússia Livre (Sibéria e Ásia Central)"   "na"   "RUS" Asia           1 True  Nothing       10
  , Pais 'Y' "Turquia"                                 "na"   "TUR" OrienteMedio   2 True  Nothing       0
  , Pais 'J' "Egito e Sudão"                           "no"   "EGI" OrienteMedio   2 True  Nothing       0
  , Pais 's' "Arábia"                                  "na"   "ARA" OrienteMedio   2 True  Nothing       30
  , Pais 'i' "Iraque e Levante"                        "no"   "IRQ" OrienteMedio   1 False Nothing       (-20)
  , Pais 'r' "Irã e Afeganistão"                       "no"   "IRA" OrienteMedio   2 True  Nothing       0
  , Pais 'g' "Argélia, Marrocos e Tunísia"             "na"   "ALG" Africa         1 False Nothing       (-40)
  , Pais 'b' "África Ocidental"                        "na"   "AOC" Africa         1 False Nothing       (-30)
  , Pais 'z' "Mittelafrika (África Central)"           "na"   "MIT" Africa         1 False Nothing       (-40)
  , Pais 'e' "Etiópia e África Oriental"               "na"   "ETI" Africa         1 False Nothing       10
  , Pais 'u' "África Austral"                          "na"   "AFS" Africa         3 True  Nothing       (-10)
  , Pais 'I' "Índia (subcontinente)"                   "na"   "IND" Asia           2 True  Nothing       20
  , Pais 'c' "China"                                   "na"   "CHN" Asia           1 True  Nothing       15
  , Pais 'm' "Manchukuo"                               "em"   "MAN" Asia           1 False Nothing       (-70)
  , Pais '3' "Coreia"                                  "na"   "COR" Asia           2 False Nothing       (-60)
  , Pais 'j' "Japão"                                   "no"   "JAP" Asia           4 True  Nothing       (-85)
  , Pais 'd' "Indochina, Tailândia e Birmânia"         "na"   "INC" Asia           1 False Nothing       (-40)
  , Pais '2' "Filipinas"                               "nas"  "FIL" Asia           2 False Nothing       70
  , Pais '1' "Indonésia"                               "na"   "IDN" Asia           1 True  Nothing       0
  , Pais 'O' "Austrália e Nova Zelândia"               "na"   "AUS" Oceania        4 False Nothing       90
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
  , gsCrise        = Nothing
  , gsCrisesVistas = []
  , gsJornal       = False
  , gsResumo       = []
  , gsMudaram      = []
  , gsSons         = []
  , gsDado         = Nothing
  , gsCenaFinal    = False
  }
