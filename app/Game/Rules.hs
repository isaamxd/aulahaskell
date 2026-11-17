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
  , crisesDisponiveis
  , resolverCrise
  , fecharJornal
  , tensaoDaOpcao
  , dataDoTurno
  , mensagemFim
  , nomeDoLado
  , quemAge
  , libertados
  , metaLibertacao
  ) where

import Control.Monad (when)
import Data.List (sortOn)
import qualified Data.Map.Strict as Map
import Data.Maybe (isJust, isNothing)
import Data.Text (Text)
import qualified Data.Text as T
import Game.Crises
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

-- | Modo Resistência: quantos países europeus é preciso libertar.
metaLibertacao :: Int
metaLibertacao = 6

-- ---------------------------------------------------------------------------
-- Nomes de cada lado, conforme o modo

-- | Nome de um lado nesta partida (no modo Resistência, o lado aliado é a Resistência).
nomeDoLado :: GameState -> Lado -> Text
nomeDoLado st lado
  | gsModo st == Resistencia && lado == EUA = "Resistência Europeia"
  | otherwise                               = ladoNome lado

-- | Quem age, nas manchetes: "Washington", "Berlim" ou "A Resistência".
quemAge :: GameState -> Lado -> Text
quemAge st lado
  | gsModo st == Resistencia && lado == EUA = "A Resistência"
  | otherwise                               = ladoCapital lado

-- | É uma ação da Resistência (e não dos EUA ou do Reich)?
ehResistencia :: GameState -> Lado -> Bool
ehResistencia st lado = gsModo st == Resistencia && lado == EUA

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

-- | Países europeus (fora do Reich) já libertados pela Resistência.
libertados :: GameState -> Int
libertados st = length
  [ p | p <- Map.elems (gsPaises st)
      , paisRegiao p == Europa
      , isNothing (paisNucleo p)
      , controlador p == Just EUA ]

-- | Renda por turno: base + 1 a cada 2 países controlados + corrida espacial
--   (ou rotas de fuga, no modo Resistência).
renda :: Lado -> GameState -> Int
renda lado st = rendaBase + length (paisesControlados lado st) `div` 2 + nivelEspacial lado st
  where
    rendaBase = case (gsModo st, lado) of
      (_, EUA)             -> 7 -- economia americana intacta / apoio aliado à Resistência
      (GuerraFria, Reich)  -> 5 -- império grande, mas caro de manter
      (Resistencia, Reich) -> 2 -- a repressão só gasta na Europa
    -- (valores equilibrados simulando partidas em Python: Guerra Fria IA x IA
    --  e Resistência com um jogador simples vencendo cerca de 2 em 3)

custo :: Acao -> GameState -> Int
custo (Influenciar c) st = maybe 99 (\p -> 1 + paisEstabilidade p) (Map.lookup c (gsPaises st))
custo (Golpe _) _            = 4
custo (GuerraProcuracao _) _ = 6
custo CorridaEspacial st     = if gsModo st == Resistencia then 6 else 8

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

-- | Quanto uma resposta de crise mexe na tensão.
tensaoDaOpcao :: Opcao -> Int
tensaoDaOpcao op = sum [ n | Tensao n <- opcaoEfeitos op ]

-- | Data de um turno: cada turno é um trimestre a partir de janeiro de 1962.
dataDoTurno :: Int -> Text
dataDoTurno t = mes <> " de " <> T.pack (show ano)
  where
    (q, r) = (t - 1) `divMod` 4
    ano    = 1962 + q :: Int
    mes    = ["Janeiro", "Abril", "Julho", "Outubro"] !! r

-- ---------------------------------------------------------------------------
-- Aleatoriedade pura (gerador congruencial linear)

-- | Sorteia um número de 0 a n-1 e devolve o estado com a semente avançada.
sortear :: Int -> GameState -> (Int, GameState)
sortear n st = (resultado, st { gsSemente = semente' })
  where
    semente'  = (gsSemente st * 1103515245 + 12345) `mod` 2147483648
    resultado = (semente' `div` 65536) `mod` max 1 n

-- | Rola um dado de 6 faces.
rolarDado :: GameState -> (Int, GameState)
rolarDado st = let (n, st') = sortear 6 st in (n + 1, st')

-- ---------------------------------------------------------------------------
-- Ações

-- | Começa uma partida no modo escolhido. O jogador sempre fica com o lado
--   aliado (EUA ou Resistência); o Reich é controlado pelo computador.
iniciarPartida :: Modo -> Int -> Text -> GameState
iniciarPartida modo semente nome =
  registrar segunda $
  registrar primeira $
    initialState
      { gsModo       = modo
      , gsJogador    = EUA
      , gsIniciado   = True
      , gsSemente    = semente `mod` 2147483648
      , gsComandante = nome
      }
  where
    (primeira, segunda) = case modo of
      GuerraFria  -> ( "O mundo está dividido entre Washington e Berlim."
                     , "Você assume o comando em Washington." )
      Resistencia -> ( "A Europa vive sob ocupação. Num porão de Paris, um rádio é ligado."
                     , "Você assume o comando da Resistência Europeia." )

-- | Aplica uma ação de um dos lados. Falha se não houver orçamento,
--   se o alvo for inválido, se houver uma crise esperando decisão
--   ou se a partida já tiver terminado.
aplicarAcao :: Lado -> Acao -> GameState -> Either Text GameState
aplicarAcao lado acao st = do
  when (isJust (gsFim st)) $
    Left "A partida já terminou."
  when (isJust (gsCrise st)) $
    Left "Decida a crise do jornal antes de agir."
  let preco = custo acao st
  when (orcamento lado st < preco) $
    Left "Orçamento insuficiente para essa ação."
  st' <- executar lado acao (gastar lado preco st)
  pure (comMudancas st (verificarTensao lado st'))

executar :: Lado -> Acao -> GameState -> Either Text GameState
executar lado (Influenciar c) st = do
  p <- alvo c st
  when (alinhamentoPara lado p >= 100) $
    Left "Esse país já está totalmente do seu lado."
  let p' = mover lado 15 p
      msg
        | ehResistencia st lado = "O rádio clandestino da Resistência é ouvido " <> noPais p <> "."
        | gsModo st == Resistencia = "Berlim reforça a ocupação " <> noPais p <> "."
        | otherwise = ladoCapital lado <> " amplia sua influência " <> noPais p <> "."
  pure $ comSom "influencia" $ registrar msg (atualizarPais p' st)

executar lado (Golpe c) st = do
  p <- alvo c st
  when (controlador p == Just lado) $
    Left "Esse país já está sob seu controle."
  let (dado, st1) = rolarDado st
      sucesso     = dado > paisEstabilidade p
      sabotagem   = ehResistencia st lado
      p1          = if sucesso then mover lado 40 p else mover (adversario lado) 10 p
      -- a sabotagem bem-sucedida também deixa a ocupação mais instável
      p'          = if sabotagem && sucesso
                      then p1 { paisEstabilidade = max 1 (paisEstabilidade p1 - 1) }
                      else p1
      inicio      = "🎲 " <> tshow dado <> " · "
      msg
        | sabotagem && sucesso = inicio <> "Sabotagem " <> noPais p <> ": trens descarrilam e a ocupação enfraquece!"
        | sabotagem            = inicio <> "A sabotagem " <> noPais p <> " fracassa, e agentes são presos."
        | sucesso   = inicio <> "Golpe apoiado por " <> quemAge st lado <> " derruba o governo " <> noPais p <> "!"
        | otherwise = inicio <> "Golpe apoiado por " <> quemAge st lado <> " fracassa " <> noPais p <> "."
  pure $ comDado dado $ comSom (if sucesso then "sucesso" else "fracasso") $
    registrar msg (aumentarTensao (tensaoDaAcao (Golpe c) st) (atualizarPais p' st1))

executar lado (GuerraProcuracao c) st = do
  p <- alvo c st
  when (isJust (controlador p)) $
    Left "Guerras por procuração só acontecem em países em disputa."
  let (dado, st1) = rolarDado st
      sucesso     = dado >= 3
      p'
        | sucesso   = p { paisAlinhamento = paraLado lado 60 }
        | otherwise = mover (adversario lado) 15 p
      inicio = "🎲 " <> tshow dado <> " · "
      msg
        | ehResistencia st lado && sucesso = inicio <> "Levante " <> noPais p <> ": o povo expulsa os ocupantes!"
        | ehResistencia st lado            = inicio <> "O levante " <> noPais p <> " é esmagado."
        | sucesso   = inicio <> "Guerra civil " <> noPais p <> ": vencem os aliados de " <> quemAge st lado <> "!"
        | otherwise = inicio <> "Guerra civil " <> noPais p <> ": os aliados de " <> quemAge st lado <> " são derrotados."
  pure $ comDado dado $ comSom (if sucesso then "sucesso" else "fracasso") $
    registrar msg (aumentarTensao 2 (atualizarPais p' st1))

executar lado CorridaEspacial st
  | ehResistencia st lado = do
      let nivel = nivelEspacial lado st
          salvos = 1500 * (nivel + 1)
      when (nivel >= 4) $
        Left "Todas as rotas de fuga já estão abertas."
      pure $ comSom "influencia" $
        registrar ("A Resistência abre " <> rotaDeFuga nivel <> ": " <> tshow salvos <> " refugiados chegam em segurança.")
          (avancarEspacial lado st) { gsRefugiados = gsRefugiados st + salvos }
  | otherwise = do
      let nivel = nivelEspacial lado st
      when (nivel >= 4) $
        Left "Sua corrida espacial já está completa."
      pure $ comSom "foguete" $
        registrar (ladoCapital lado <> " " <> marcoEspacial nivel <> "!") (avancarEspacial lado st)

-- | As rotas de fuga da Resistência, na ordem em que são abertas.
rotaDeFuga :: Int -> Text
rotaDeFuga nivel = case nivel of
  0 -> "uma rota pelos Pireneus até Lisboa"
  1 -> "uma rota de barco até a Suécia"
  2 -> "uma rota pelo Mediterrâneo"
  _ -> "uma rede de esconderijos em toda a Europa"

marcoEspacial :: Int -> Text
marcoEspacial nivel = case nivel of
  0 -> "lança o primeiro satélite artificial"
  1 -> "coloca o primeiro homem em órbita"
  2 -> "monta uma estação orbital"
  _ -> "pousa uma tripulação na Lua"

avancarEspacial :: Lado -> GameState -> GameState
avancarEspacial lado st
  | nivel >= 4 = st
  | otherwise  = st { gsEspacial = Map.insert lado (nivel + 1) (gsEspacial st) }
  where
    nivel = nivelEspacial lado st

-- | O país alvo de uma ação (não pode ser uma superpotência).
alvo :: Char -> GameState -> Either Text Pais
alvo c st = case Map.lookup c (gsPaises st) of
  Nothing -> Left "Esse país não existe."
  Just p
    | isJust (paisNucleo p) && gsModo st == Resistencia ->
        Left "O coração do Reich é vigiado demais. A Resistência atua nos países ocupados."
    | isJust (paisNucleo p) -> Left "Não é possível agir dentro de uma superpotência."
    | gsModo st == Resistencia && paisRegiao p /= Europa ->
        Left "A Resistência só atua na Europa."
    | otherwise             -> Right p

-- ---------------------------------------------------------------------------
-- Crises do jornal

-- | Crises que ainda podem acontecer agora.
crisesDisponiveis :: GameState -> [Crise]
crisesDisponiveis st =
  [ c | c <- crises
      , criseId c `notElem` gsCrisesVistas st
      , ehDesteModo c
      , all (condicaoOk st) (criseCondicoes c) ]
  where
    -- crises marcadas com SoNoModo Resistencia só aparecem nesse modo;
    -- as outras só aparecem na Guerra Fria
    ehDesteModo c = (SoNoModo Resistencia `elem` criseCondicoes c) == (gsModo st == Resistencia)

condicaoOk :: GameState -> Condicao -> Bool
condicaoOk st cond = case cond of
  SoPara lado      -> gsJogador st == lado
  SoNoModo modo    -> gsModo st == modo
  APartirDoTurno t -> gsTurno st >= t
  AteOTurno t      -> gsTurno st <= t
  TensaoMinima n   -> gsTensao st >= n
  NaoEhDoJogador c -> case Map.lookup c (gsPaises st) of
    Just p  -> controlador p /= Just (gsJogador st)
    Nothing -> False

-- | Sorteia a crise do trimestre (se ainda houver alguma).
sortearCrise :: GameState -> GameState
sortearCrise st = case crisesDisponiveis st of
  []    -> st
  lista ->
    let (i, st') = sortear (length lista) st
    in st' { gsCrise = Just (criseId (lista !! i)) }

-- | O jogador escolhe a resposta A (True) ou B (False) da crise pendente.
resolverCrise :: Bool -> GameState -> Either Text GameState
resolverCrise escolheuA st = do
  crise <- maybe (Left "Não há nenhuma crise esperando decisão.") Right (gsCrise st >>= buscarCrise)
  let jogador = gsJogador st
      opcao   = if escolheuA then criseA crise else criseB crise
      st1     = foldl (flip (aplicarEfeito jogador)) st (opcaoEfeitos opcao)
      st2     = st1 { gsCrise        = Nothing
                    , gsCrisesVistas = criseId crise : gsCrisesVistas st1
                    , gsJornal       = False }
  pure (comMudancas st (verificarTensao jogador (comSom "manchete" (registrar (opcaoDesfecho opcao) st2))))

-- | Fecha o jornal (só quando não há crise esperando decisão).
fecharJornal :: GameState -> Either Text GameState
fecharJornal st
  | isJust (gsCrise st) = Left "Decida a crise antes de fechar o jornal."
  | otherwise           = Right st { gsJornal = False }

-- | Aplica um efeito de crise, do ponto de vista do jogador.
aplicarEfeito :: Lado -> Efeito -> GameState -> GameState
aplicarEfeito lado efeito st = case efeito of
  Influencia c v -> case Map.lookup c (gsPaises st) of
    Just p | isNothing (paisNucleo p) -> atualizarPais (mover lado v p) st
    _                                 -> st
  Estabilidade c n -> case Map.lookup c (gsPaises st) of
    Just p -> atualizarPais p { paisEstabilidade = max 1 (min 4 (paisEstabilidade p + n)) } st
    _      -> st
  Tensao n    -> st { gsTensao = max 0 (min tensaoMaxima (gsTensao st + n)) }
  Orcamento n -> st { gsOrcamento = Map.adjust (\v -> max 0 (v + n)) lado (gsOrcamento st) }
  Espacial    -> avancarEspacial lado st
  Refugiados n -> st { gsRefugiados = gsRefugiados st + n }

-- ---------------------------------------------------------------------------
-- Inteligência artificial do lado oposto

-- | A IA gasta seu orçamento em até 8 ações, sem nunca arriscar uma guerra nuclear.
jogarIA :: GameState -> GameState
jogarIA st0 = go limite st0
  where
    limite :: Int
    limite = if gsModo st0 == Resistencia then 3 else 8
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
    podeEspacial = gsModo st == GuerraFria && nivelEspacial lado st < 4 && dinheiro >= 14
    disputaveis  =
      [ p | p <- Map.elems (gsPaises st), isNothing (paisNucleo p), controlador p /= Just lado
          , gsModo st == GuerraFria || paisRegiao p == Europa ]
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
--   pouco, verificamos se alguém venceu e sorteamos a crise do jornal.
--   Não faz nada se a partida acabou ou se há uma crise esperando decisão.
encerrarTurno :: GameState -> GameState
encerrarTurno st
  | isJust (gsFim st) || isJust (gsCrise st) = st
  | otherwise =
      let depoisIA = jogarIA st { gsSons = [] }
          fechado
            | isJust (gsFim depoisIA) = depoisIA
            | otherwise               = verificarFim (proximoTurno depoisIA)
          comCrise
            | isJust (gsFim fechado) = fechado
            | otherwise              = sortearCrise fechado
          final = comCrise
            { gsJornal = isNothing (gsFim comCrise)
            , gsResumo = novidades st comCrise
            -- dos sons da IA, só mantém os de fim de jogo; o jornal chega pelo telégrafo
            , gsSons   = [ s | s <- gsSons comCrise, s `elem` ["nuclear", "fim"] ] ++ ["telegrafo"]
            , gsDado   = Nothing }
      in comMudancas st final

-- | Os relatórios novos entre dois estados, em ordem cronológica.
novidades :: GameState -> GameState -> [Text]
novidades antes depois =
  reverse (filter (not . T.isInfixOf "Novo trimestre") novos)
  where
    novos = case gsLog antes of
      []         -> gsLog depois
      (topo : _) -> takeWhile (/= topo) (gsLog depois)

proximoTurno :: GameState -> GameState
proximoTurno st = registrar "Novo trimestre: orçamentos recebidos." st'
  where
    st' = st
      { gsTurno     = gsTurno st + 1
      , gsOrcamento = Map.mapWithKey (\lado v -> v + renda lado st) (gsOrcamento st)
      , gsTensao    = max 0 (gsTensao st - 1)
      }

verificarFim :: GameState -> GameState
verificarFim st = case gsModo st of
  Resistencia
    | libertados st >= metaLibertacao -> terminar (Vitoria EUA) st
    | gsTurno st > turnoFinal         -> terminar (Vitoria Reich) st
    | otherwise                       -> st
  GuerraFria
    | pontos jogador st >= pontosVitoria -> terminar (Vitoria jogador) st
    | pontos ia st >= pontosVitoria      -> terminar (Vitoria ia) st
    | gsTurno st > turnoFinal            -> terminar porPontos st
    | otherwise                          -> st
  where
    jogador = gsJogador st
    ia      = adversario jogador
    porPontos = case compare (pontos jogador st) (pontos ia st) of
      GT -> Vitoria jogador
      LT -> Vitoria ia
      EQ -> Empate

terminar :: Fim -> GameState -> GameState
terminar f st = comSom som (registrar (mensagemFim st f) st { gsFim = Just f, gsCenaFinal = True })
  where
    som = case f of
      GuerraNuclear _
        | gsModo st == Resistencia -> "alarme"
        | otherwise                -> "nuclear"
      _ -> "fim"

mensagemFim :: GameState -> Fim -> Text
mensagemFim st f = case (gsModo st, f) of
  (Resistencia, Vitoria EUA)     -> "A Europa se levanta: a Resistência liberta o continente!"
  (Resistencia, Vitoria Reich)   -> "A ocupação continua. A luta seguirá na clandestinidade."
  (Resistencia, Empate)          -> "Nem libertação, nem derrota: a luta continua."
  (Resistencia, GuerraNuclear _) -> "A Gestapo descobre a rede. A Resistência é desmantelada."
  (GuerraFria, Vitoria lado)       -> ladoCapital lado <> " vence a Guerra Fria!"
  (GuerraFria, Empate)             -> "A Guerra Fria termina num impasse."
  (GuerraFria, GuerraNuclear lado) -> "GUERRA NUCLEAR. " <> ladoCapital lado <> " levou o mundo ao abismo."

-- ---------------------------------------------------------------------------
-- Auxiliares

paraLado :: Lado -> Int -> Int
paraLado EUA v   = v
paraLado Reich v = negate v

limitar :: Int -> Int
limitar = max (-100) . min 100

-- | Move o alinhamento de um país em direção a um lado (v negativo = pro outro lado).
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
  | gsTensao st >= tensaoMaxima && isNothing (gsFim st) = terminar (GuerraNuclear lado) st
  | otherwise = st

-- | Marca quais países trocaram de dono entre dois estados (pra tela piscar).
comMudancas :: GameState -> GameState -> GameState
comMudancas antes depois =
  depois { gsMudaram = mudaram
         , gsSons    = gsSons depois ++ [ "virada" | not (null mudaram) ] }
  where
    mudaram =
      [ c | (c, p) <- Map.toList (gsPaises depois)
          , fmap controlador (Map.lookup c (gsPaises antes)) /= Just (controlador p) ]

comSom :: Text -> GameState -> GameState
comSom som st = st { gsSons = gsSons st ++ [som] }

comDado :: Int -> GameState -> GameState
comDado d st = st { gsDado = Just d }

registrar :: Text -> GameState -> GameState
registrar msg st = st { gsLog = take 60 ((dataDoTurno (gsTurno st) <> ": " <> msg) : gsLog st) }

tshow :: Show a => a -> Text
tshow = T.pack . show
