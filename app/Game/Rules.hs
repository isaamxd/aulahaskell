-- | Regras do jogo. Todas as funções são puras: recebem um estado e
--   devolvem um novo estado (ou um erro explicando por que a ação falhou).
--   Até o dado é puro: a "sorte" vem de uma semente guardada no estado,
--   então a mesma semente sempre dá o mesmo resultado (bom pra testar).
module Game.Rules
  ( limiteControle
  , tensaoMaxima
  , pontosVitoria
  , controlador
  , alinhamentoPara
  , paisesControlados
  , pontos
  , renda
  , orcamento
  , nivelEspacial
  , custo
  , chanceSucesso
  , tensaoDaAcao
  , rolarDado
  , aplicarAcao
  , jogarIA
  , encerrarTurno
  , iniciarPartida
  , dataDoTurno
  , mensagemFim
  ) where

import Control.Monad (when)
import Data.List (sortOn)
import qualified Data.Map.Strict as Map
import Data.Maybe (isJust, isNothing)
import Data.Text (Text)
import qualified Data.Text as T
import Game.Map (initialState, turnoFinal)
import Game.Types

-- ---------------------------------------------------------------------------
-- Constantes

-- | A partir de +50 (ou -50) o país passa a ser controlado.
limiteControle :: Int
limiteControle = 50

-- | Tensão 5 = guerra nuclear.
tensaoMaxima :: Int
tensaoMaxima = 5

-- | Quem chegar a esta pontuação vence na hora.
pontosVitoria :: Int
pontosVitoria = 36

-- ---------------------------------------------------------------------------
-- Consultas

controlador :: Pais -> Maybe Lado
controlador p
  | Just lado <- paisNucleo p                       = Just lado
  | paisAlinhamento p >= limiteControle             = Just EUA
  | paisAlinhamento p <= negate limiteControle      = Just Reich
  | otherwise                                       = Nothing

-- | Alinhamento do ponto de vista de um lado (positivo = a favor dele).
alinhamentoPara :: Lado -> Pais -> Int
alinhamentoPara EUA p   = paisAlinhamento p
alinhamentoPara Reich p = negate (paisAlinhamento p)

paisesControlados :: Lado -> GameState -> [Pais]
paisesControlados lado st = filter ((== Just lado) . controlador) (Map.elems (gsPaises st))

-- | Países estratégicos valem 2 pontos; os outros, 1.
valor :: Pais -> Int
valor p = if paisEstrategico p then 2 else 1

pontos :: Lado -> GameState -> Int
pontos lado st = sum (map valor (paisesControlados lado st))

orcamento :: Lado -> GameState -> Int
orcamento lado st = Map.findWithDefault 0 lado (gsOrcamento st)

nivelEspacial :: Lado -> GameState -> Int
nivelEspacial lado st = Map.findWithDefault 0 lado (gsEspacial st)

-- | Renda por turno: base + 1 a cada 2 países controlados + corrida espacial.
renda :: Lado -> GameState -> Int
renda lado st = rendaBase lado + length (paisesControlados lado st) `div` 2 + nivelEspacial lado st
  where
    rendaBase EUA   = 7 -- economia americana intacta
    rendaBase Reich = 5 -- império grande, mas caro de manter
    -- (valores equilibrados simulando 600 partidas IA contra IA)

custo :: Acao -> GameState -> Int
custo (Influenciar c) st = maybe 99 (\p -> 1 + paisEstabilidade p) (Map.lookup c (gsPaises st))
custo (Golpe _) _            = 4
custo (GuerraProcuracao _) _ = 6
custo CorridaEspacial _      = 8

-- | Chance de sucesso em %, pras ações que dependem do dado.
chanceSucesso :: Acao -> GameState -> Maybe Int
chanceSucesso (Golpe c) st =
  fmap (\p -> (6 - min 5 (paisEstabilidade p)) * 100 `div` 6) (Map.lookup c (gsPaises st))
chanceSucesso (GuerraProcuracao _) _ = Just 66
chanceSucesso _ _                    = Nothing

-- | Quanto a ação aumenta a tensão nuclear.
tensaoDaAcao :: Acao -> GameState -> Int
tensaoDaAcao (Golpe c) st =
  maybe 1 (\p -> if paisEstrategico p then 2 else 1) (Map.lookup c (gsPaises st))
tensaoDaAcao (GuerraProcuracao _) _ = 2
tensaoDaAcao _ _                    = 0

-- | Data de um turno: cada turno é um trimestre a partir de janeiro de 1962.
dataDoTurno :: Int -> Text
dataDoTurno t = mes <> " de " <> T.pack (show ano)
  where
    (q, r) = (t - 1) `divMod` 4
    ano    = 1962 + q :: Int
    mes    = ["Janeiro", "Abril", "Julho", "Outubro"] !! r

-- ---------------------------------------------------------------------------
-- Aleatoriedade pura (gerador congruencial linear)

-- | Rola um dado de 6 faces e devolve o estado com a semente avançada.
rolarDado :: GameState -> (Int, GameState)
rolarDado st = (dado, st { gsSemente = semente' })
  where
    semente' = (gsSemente st * 1103515245 + 12345) `mod` 2147483648
    dado     = (semente' `div` 65536) `mod` 6 + 1

-- ---------------------------------------------------------------------------
-- Ações

-- | Começa uma partida com o lado escolhido.
iniciarPartida :: Lado -> Int -> Text -> GameState
iniciarPartida lado semente nome =
  registrar ("Você assume o comando: " <> ladoNome lado <> ".") $
  registrar "O mundo está dividido entre Washington e Berlim." $
    initialState
      { gsJogador    = lado
      , gsIniciado   = True
      , gsSemente    = semente `mod` 2147483648
      , gsComandante = nome
      }

-- | Aplica uma ação de um dos lados. Falha se não houver orçamento,
--   se o alvo for inválido ou se a partida já tiver terminado.
aplicarAcao :: Lado -> Acao -> GameState -> Either Text GameState
aplicarAcao lado acao st = do
  when (isJust (gsFim st)) $
    Left "A partida já terminou."
  let preco = custo acao st
  when (orcamento lado st < preco) $
    Left "Orçamento insuficiente para essa ação."
  st' <- executar lado acao (gastar lado preco st)
  pure (verificarTensao lado st')

executar :: Lado -> Acao -> GameState -> Either Text GameState
executar lado (Influenciar c) st = do
  p <- alvo c st
  when (alinhamentoPara lado p >= 100) $
    Left "Esse país já está totalmente do seu lado."
  let p' = mover lado 15 p
  pure (registrar (ladoNome lado <> " amplia sua influência em " <> paisNome p <> ".")
                  (atualizarPais p' st))

executar lado (Golpe c) st = do
  p <- alvo c st
  when (controlador p == Just lado) $
    Left "Esse país já está sob seu controle."
  let (dado, st1) = rolarDado st
      sucesso     = dado > paisEstabilidade p
      p'          = if sucesso then mover lado 40 p else mover (adversario lado) 10 p
      msg
        | sucesso   = "Golpe apoiado por " <> ladoNome lado <> " derruba o governo de " <> paisNome p <> "!"
        | otherwise = "Golpe apoiado por " <> ladoNome lado <> " fracassa em " <> paisNome p <> "."
  pure (registrar msg (aumentarTensao (tensaoDaAcao (Golpe c) st) (atualizarPais p' st1)))

executar lado (GuerraProcuracao c) st = do
  p <- alvo c st
  when (isJust (controlador p)) $
    Left "Guerras por procuração só acontecem em países em disputa."
  let (dado, st1) = rolarDado st
      sucesso     = dado >= 3
      p'
        | sucesso   = p { paisAlinhamento = paraLado lado 60 }
        | otherwise = mover (adversario lado) 15 p
      msg
        | sucesso   = "Guerra civil em " <> paisNome p <> ": vencem os aliados de " <> ladoNome lado <> "!"
        | otherwise = "Guerra civil em " <> paisNome p <> ": os aliados de " <> ladoNome lado <> " são derrotados."
  pure (registrar msg (aumentarTensao 2 (atualizarPais p' st1)))

executar lado CorridaEspacial st = do
  let nivel = nivelEspacial lado st
  when (nivel >= 4) $
    Left "Sua corrida espacial já está completa."
  let marcos = [ "lança o primeiro satélite artificial"
               , "coloca o primeiro homem em órbita"
               , "monta uma estação orbital"
               , "pousa uma tripulação na Lua"
               ]
  pure (registrar (ladoNome lado <> " " <> (marcos !! nivel) <> "!")
                  st { gsEspacial = Map.insert lado (nivel + 1) (gsEspacial st) })

-- | O país alvo de uma ação (não pode ser uma superpotência).
alvo :: Char -> GameState -> Either Text Pais
alvo c st = case Map.lookup c (gsPaises st) of
  Nothing -> Left "Esse país não existe."
  Just p
    | isJust (paisNucleo p) -> Left "Não é possível agir dentro de uma superpotência."
    | otherwise             -> Right p

-- ---------------------------------------------------------------------------
-- Inteligência artificial do lado oposto

-- | A IA gasta seu orçamento em até 8 ações, sem nunca arriscar uma guerra nuclear.
jogarIA :: GameState -> GameState
jogarIA st0 = go (8 :: Int) st0
  where
    lado = adversario (gsJogador st0)
    go 0 s = s
    go n s
      | isJust (gsFim s) = s
      | otherwise = case escolherAcaoIA lado s of
          Nothing   -> s
          Just acao -> case aplicarAcao lado acao s of
            Left _   -> s
            Right s' -> go (n - 1) s'

escolherAcaoIA :: Lado -> GameState -> Maybe Acao
escolherAcaoIA lado st
  | podeEspacial                = Just CorridaEspacial
  | (p : _) <- alvosGolpe       = Just (Golpe (paisCodigo p))
  | (p : _) <- alvosInfluencia  = Just (Influenciar (paisCodigo p))
  | otherwise                   = Nothing
  where
    dinheiro     = orcamento lado st
    podeEspacial = nivelEspacial lado st < 4 && dinheiro >= 14
    disputaveis  =
      [ p | p <- Map.elems (gsPaises st), isNothing (paisNucleo p), controlador p /= Just lado ]
    -- golpes só em países estratégicos instáveis, e só se a tensão permitir
    alvosGolpe
      | dinheiro < 4 = []
      | otherwise =
          sortOn paisEstabilidade
            [ p | p <- disputaveis
                , paisEstrategico p
                , paisEstabilidade p <= 2
                , alinhamentoPara lado p > negate limiteControle
                , gsTensao st + tensaoDaAcao (Golpe (paisCodigo p)) st <= 3 ]
    -- influência: prioriza estratégicos e os mais perto de virar
    alvosInfluencia =
      sortOn prioridade
        [ p | p <- disputaveis, custo (Influenciar (paisCodigo p)) st <= dinheiro ]
    prioridade p = (negate (valor p), limiteControle - alinhamentoPara lado p)

-- ---------------------------------------------------------------------------
-- Turnos e fim de jogo

-- | Encerra o turno: a IA joga, todos recebem renda, a tensão baixa um
--   pouco e verificamos se alguém venceu.
encerrarTurno :: GameState -> GameState
encerrarTurno st
  | isJust (gsFim st) = st
  | otherwise =
      let depoisIA = jogarIA st
      in if isJust (gsFim depoisIA) then depoisIA else verificarFim (proximoTurno depoisIA)

proximoTurno :: GameState -> GameState
proximoTurno st = registrar "Novo trimestre: orçamentos recebidos." st'
  where
    st' = st
      { gsTurno     = gsTurno st + 1
      , gsOrcamento = Map.mapWithKey (\lado v -> v + renda lado st) (gsOrcamento st)
      , gsTensao    = max 0 (gsTensao st - 1)
      }

verificarFim :: GameState -> GameState
verificarFim st
  | pontos jogador st >= pontosVitoria = terminar (Vitoria jogador)
  | pontos ia st >= pontosVitoria      = terminar (Vitoria ia)
  | gsTurno st > turnoFinal            = terminar porPontos
  | otherwise                          = st
  where
    jogador = gsJogador st
    ia      = adversario jogador
    porPontos = case compare (pontos jogador st) (pontos ia st) of
      GT -> Vitoria jogador
      LT -> Vitoria ia
      EQ -> Empate
    terminar f = registrar (mensagemFim f) st { gsFim = Just f }

mensagemFim :: Fim -> Text
mensagemFim (Vitoria lado)       = ladoNome lado <> " vence a Guerra Fria!"
mensagemFim Empate               = "A Guerra Fria termina num impasse."
mensagemFim (GuerraNuclear lado) = "GUERRA NUCLEAR. " <> ladoNome lado <> " levou o mundo ao abismo."

-- ---------------------------------------------------------------------------
-- Auxiliares

paraLado :: Lado -> Int -> Int
paraLado EUA v   = v
paraLado Reich v = negate v

limitar :: Int -> Int
limitar = max (-100) . min 100

-- | Move o alinhamento de um país em direção a um lado.
mover :: Lado -> Int -> Pais -> Pais
mover lado v p = p { paisAlinhamento = limitar (paisAlinhamento p + paraLado lado v) }

atualizarPais :: Pais -> GameState -> GameState
atualizarPais p st = st { gsPaises = Map.insert (paisCodigo p) p (gsPaises st) }

gastar :: Lado -> Int -> GameState -> GameState
gastar lado v st = st { gsOrcamento = Map.adjust (subtract v) lado (gsOrcamento st) }

aumentarTensao :: Int -> GameState -> GameState
aumentarTensao n st = st { gsTensao = min tensaoMaxima (gsTensao st + n) }

-- | Se a tensão chegou ao máximo, quem agiu por último causou a guerra nuclear.
verificarTensao :: Lado -> GameState -> GameState
verificarTensao lado st
  | gsTensao st >= tensaoMaxima && isNothing (gsFim st) =
      let f = GuerraNuclear lado in registrar (mensagemFim f) st { gsFim = Just f }
  | otherwise = st

registrar :: Text -> GameState -> GameState
registrar msg st = st { gsLog = take 60 ((dataDoTurno (gsTurno st) <> ": " <> msg) : gsLog st) }
