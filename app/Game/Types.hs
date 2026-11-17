-- | Tipos centrais do jogo. Tudo aqui é dado puro: nenhum IO.
--
--   Cenário: 1962, numa história alternativa em que o Eixo venceu a
--   Segunda Guerra na Europa. Os Estados Unidos e o Reich Alemão são as
--   duas superpotências nucleares e disputam o resto do mundo sem se
--   enfrentar diretamente: é uma Guerra Fria.
module Game.Types
  ( Lado (..)
  , Modo (..)
  , Regiao (..)
  , Pais (..)
  , Acao (..)
  , Fim (..)
  , GameState (..)
  , Interceptacao (..)
  , ladoNome
  , ladoCapital
  , ladoSlug
  , ladoCor
  , adversario
  , regiaoNome
  , noPais
  ) where

import Data.Map.Strict (Map)
import Data.Text (Text)

-- | Os dois lados do conflito. No modo Resistência, o lado "EUA" é a
--   Resistência Europeia (os aliados), e o Reich continua sendo o inimigo.
data Lado = EUA | Reich
  deriving (Eq, Ord, Show, Enum, Bounded)

-- | Os modos de jogo.
data Modo
  = GuerraFria   -- ^ você comanda os Estados Unidos e disputa o mundo
  | Resistencia  -- ^ você comanda a Resistência e liberta a Europa ocupada
  deriving (Eq, Show)

ladoNome :: Lado -> Text
ladoNome EUA   = "Estados Unidos"
ladoNome Reich = "Reich Alemão"

-- | A capital, usada como sujeito nas manchetes ("Washington amplia...").
ladoCapital :: Lado -> Text
ladoCapital EUA   = "Washington"
ladoCapital Reich = "Berlim"

-- | Nome usado nas URLs (/novo/eua, /novo/reich).
ladoSlug :: Lado -> Text
ladoSlug EUA   = "eua"
ladoSlug Reich = "reich"

ladoCor :: Lado -> Text
ladoCor EUA   = "#3d6a9e"
ladoCor Reich = "#8c2f2f"

adversario :: Lado -> Lado
adversario EUA   = Reich
adversario Reich = EUA

data Regiao
  = AmericaDoNorte
  | AmericaCentral
  | AmericaDoSul
  | Europa
  | OrienteMedio
  | Africa
  | Asia
  | Oceania
  deriving (Eq, Ord, Show, Enum, Bounded)

regiaoNome :: Regiao -> Text
regiaoNome AmericaDoNorte = "América do Norte"
regiaoNome AmericaCentral = "América Central e Caribe"
regiaoNome AmericaDoSul   = "América do Sul"
regiaoNome Europa         = "Europa"
regiaoNome OrienteMedio   = "Oriente Médio"
regiaoNome Africa         = "África"
regiaoNome Asia           = "Ásia"
regiaoNome Oceania        = "Oceania"

data Pais = Pais
  { paisCodigo       :: Char        -- ^ letra que identifica o país
  , paisNome         :: Text
  , paisPrep         :: Text        -- ^ "no", "na", "nos", "em"... (pra frase ficar certa)
  , paisSigla        :: Text        -- ^ três letras mostradas no mapa
  , paisRegiao       :: Regiao
  , paisEstabilidade :: Int         -- ^ 1 (instável) a 4 (estável): quanto maior, mais difícil virar
  , paisEstrategico  :: Bool        -- ^ países-chave valem pontos em dobro
  , paisNucleo       :: Maybe Lado  -- ^ território de uma superpotência (não pode ser disputado)
  , paisAlinhamento  :: Int         -- ^ -100 (totalmente pró-Reich) a +100 (totalmente pró-EUA)
  } deriving (Eq, Show)

-- | "no Brasil", "na Argentina", "em Cuba"...
noPais :: Pais -> Text
noPais p = paisPrep p <> " " <> paisNome p

-- | O que uma superpotência pode fazer num turno.
data Acao
  = Influenciar Char       -- ^ propaganda, ajuda econômica, diplomacia
  | Golpe Char             -- ^ derrubar o governo (depende do dado)
  | GuerraProcuracao Char  -- ^ financiar uma guerra num país em disputa
  | CorridaEspacial        -- ^ prestígio: aumenta a renda
  deriving (Eq, Show)

-- | Como a partida terminou.
data Fim
  = Vitoria Lado
  | Empate
  | GuerraNuclear Lado     -- ^ quem levou a tensão ao limite (e perdeu)
  deriving (Eq, Show)

-- | O estado completo de uma partida. Cada ação do jogo é uma função
--   pura @GameState -> GameState@ (ou @Either Text GameState@ se puder falhar).
data GameState = GameState
  { gsTurno        :: Int              -- ^ cada turno é um trimestre, a partir de jan/1962
  , gsModo         :: Modo
  , gsJogador      :: Lado             -- ^ o lado do jogador (o outro é a IA)
  , gsIniciado     :: Bool             -- ^ False enquanto o jogador não escolheu um lado
  , gsPaises       :: Map Char Pais
  , gsOrcamento    :: Map Lado Int
  , gsEspacial     :: Map Lado Int     -- ^ nível na corrida espacial (0 a 4)
  , gsTensao       :: Int              -- ^ 0 (calma) a 5 (guerra nuclear)
  , gsSemente      :: Int              -- ^ semente do gerador pseudoaleatório (puro!)
  , gsFim          :: Maybe Fim
  , gsLog          :: [Text]           -- ^ relatórios, do mais novo pro mais antigo
  , gsComandante   :: Text             -- ^ nome do jogador, usado no histórico
  , gsPartidaId    :: Maybe Int        -- ^ ID no banco, se a partida já foi salva
  , gsRefugiados   :: Int              -- ^ pessoas salvas pelas rotas de fuga (modo Resistência)
    -- crises e jornal
  , gsCrise        :: Maybe Int        -- ^ crise esperando a decisão do jogador
  , gsCrisesVistas :: [Int]            -- ^ crises que já aconteceram (não repetem)
  , gsJornal       :: Bool             -- ^ mostrar o jornal do trimestre
  , gsResumo       :: [Text]           -- ^ o que aconteceu no último fim de trimestre
    -- efeitos da última ação (a tela usa e depois o servidor limpa)
  , gsMudaram      :: [Char]           -- ^ países que trocaram de dono
  , gsSons         :: [Text]           -- ^ sons a tocar
  , gsDado         :: Maybe Int        -- ^ último dado rolado
  , gsCenaFinal    :: Bool             -- ^ mostrar a cutscene de fim de jogo
    -- minijogo da Enigma
  , gsEnigma       :: Maybe Interceptacao -- ^ mensagem do Reich esperando ser decifrada
  } deriving (Eq, Show)

-- | Uma mensagem cifrada do Reich interceptada pelo jogador (minijogo da Enigma).
data Interceptacao = Interceptacao
  { intCifrada    :: Text   -- ^ o texto cifrado que aparece na tela
  , intChave      :: Text   -- ^ a posição certa dos 3 rotores, ex.: "KDR"
  , intAlvo       :: Char   -- ^ o país que o Reich planejava atacar
  , intTentativas :: Int    -- ^ tentativas que ainda restam
  } deriving (Eq, Show)
