-- | Minijogo da Enigma: de vez em quando o jogador intercepta uma mensagem
--   cifrada do Reich e precisa descobrir a posição dos 3 rotores.
--
--   A cifra é uma versão simplificada da máquina: cada letra é deslocada no
--   alfabeto pelo valor de um rotor (A = 0, B = 1, ...), e os rotores se
--   revezam a cada letra (1º, 2º, 3º, 1º, ...). Tudo aqui é função pura.
module Game.Enigma
  ( cifrar
  , decifrar
  , normalizar
  , mensagemClara
  , interceptar
  , tentarChave
  , desistirEnigma
  ) where

import Data.Char (chr, isAsciiUpper, ord)
import Data.List (mapAccumL)
import qualified Data.Map.Strict as Map
import Data.Maybe (isNothing)
import Data.Text (Text)
import qualified Data.Text as T
import Game.Rules (controlador, dataDoTurno, quemAge, rolarDado, tensaoMaxima)
import Game.Types

-- ---------------------------------------------------------------------------
-- A cifra

-- | Desloca uma letra maiúscula n posições no alfabeto (o resto não muda).
deslocar :: Int -> Char -> Char
deslocar n c
  | isAsciiUpper c = chr (ord 'A' + (ord c - ord 'A' + n) `mod` 26)
  | otherwise      = c

-- | Valor de cada rotor da chave: "KDR" -> [10, 3, 17].
rotores :: Text -> [Int]
rotores chave = [ ord c - ord 'A' | c <- T.unpack (T.toUpper chave), isAsciiUpper c ]

-- | Passa o texto pelos rotores. Espaços não giram os rotores.
aplicar :: Int -> Text -> Text -> Text
aplicar sentido chave texto
  | null ks   = texto
  | otherwise = T.pack (snd (mapAccumL passo 0 (T.unpack texto)))
  where
    ks = rotores chave
    passo :: Int -> Char -> (Int, Char)
    passo i c
      | isAsciiUpper c = (i + 1, deslocar (sentido * (ks !! (i `mod` length ks))) c)
      | otherwise      = (i, c)

cifrar :: Text -> Text -> Text
cifrar = aplicar 1

-- | Desfaz a cifra: decifrar k (cifrar k t) == t
decifrar :: Text -> Text -> Text
decifrar = aplicar (-1)

-- | Deixa um texto só com letras maiúsculas sem acento e espaços.
normalizar :: Text -> Text
normalizar = T.unwords . T.words . T.map semAcento . T.toUpper . T.strip . fst . T.breakOn "("
  where
    semAcento :: Char -> Char
    semAcento c = case c of
      'Á' -> 'A'; 'À' -> 'A'; 'Â' -> 'A'; 'Ã' -> 'A'
      'É' -> 'E'; 'Ê' -> 'E'; 'Í' -> 'I'
      'Ó' -> 'O'; 'Ô' -> 'O'; 'Õ' -> 'O'; 'Ú' -> 'U'
      'Ç' -> 'C'; ',' -> ' '
      _   -> c

-- | A mensagem do Reich antes de cifrar. Sempre termina com "BERLIM":
--   é essa pista que permite descobrir os rotores.
mensagemClara :: Pais -> Text
mensagemClara p = T.unwords ["ALVO", normalizar (paisNome p), "ASSINADO", "BERLIM"]

-- ---------------------------------------------------------------------------
-- O minijogo dentro da partida

-- | A cada três trimestres chega uma mensagem interceptada (se não houver outra).
interceptar :: GameState -> GameState
interceptar st
  | isNothing (gsFim st), isNothing (gsEnigma st), gsTurno st `mod` 3 == 1, not (null alvos) =
      registrar "📻 Uma mensagem do Reich foi interceptada! Decifre-a no memorando." $
        st4 { gsEnigma = Just Interceptacao
                { intCifrada    = cifrar chave (mensagemClara alvo)
                , intChave      = chave
                , intAlvo       = paisCodigo alvo
                , intTentativas = 3 }
            , gsSons = gsSons st4 ++ ["telegrafo"] }
  | otherwise = st
  where
    alvos :: [Pais]
    alvos = [ p | p <- Map.elems (gsPaises st)
                , isNothing (paisNucleo p)
                , controlador p /= Just (gsJogador st)
                , gsModo st /= Resistencia || paisRegiao p == Europa ]
    (d1, st1) = rolarDado st
    (d2, st2) = rolarDado st1
    (d3, st3) = rolarDado st2
    (d4, st4) = rolarDado st3
    letra :: Int -> Char
    letra n = chr (ord 'A' + n `mod` 26)
    chave :: Text
    chave = T.pack [letra (d1 * 5 + d2), letra (d2 * 7 + d3 * 3), letra (d3 * 11 + d4)]
    alvo :: Pais
    alvo = alvos !! ((d1 + d2 + d3 + d4) `mod` length alvos)

-- | O jogador tenta uma posição dos rotores. Acertando, descobre o alvo do
--   Reich e se antecipa; errando três vezes, a Gestapo percebe.
tentarChave :: Text -> GameState -> Either Text GameState
tentarChave tentativa st = case gsEnigma st of
  Nothing -> Left "Não há nenhuma mensagem para decifrar."
  Just e
    | T.toUpper tentativa == intChave e -> Right (acertou e)
    | intTentativas e <= 1 ->
        Right $ registrar "📻 Chave errada de novo: o Reich percebeu e trocou o código. O alerta sobe." $
          st { gsEnigma = Nothing
             , gsTensao = min (tensaoMaxima - 1) (gsTensao st + 1)
             , gsSons   = ["alarme"] }
    | otherwise ->
        Right $ registrar ("📻 Chave errada. Restam " <> tshow (intTentativas e - 1) <> " tentativas.") $
          st { gsEnigma = Just e { intTentativas = intTentativas e - 1 }, gsSons = ["fracasso"] }
  where
    acertou :: Interceptacao -> GameState
    acertou e = case Map.lookup (intAlvo e) (gsPaises st) of
      Nothing -> st { gsEnigma = Nothing }
      Just p ->
        let p' = p { paisAlinhamento = min 100 (paisAlinhamento p + 15) }
        in registrar ("📻 Mensagem decifrada! O Reich planejava agir " <> noPais p <> ". "
                      <> quemAge st (gsJogador st) <> " se antecipa (+15 de influência).") $
             st { gsEnigma   = Nothing
                , gsPaises   = Map.insert (paisCodigo p) p' (gsPaises st)
                , gsTensao   = max 0 (gsTensao st - 1)
                , gsMudaram  = [paisCodigo p]
                , gsSons     = ["sucesso"] }

-- | Joga a mensagem fora sem tentar.
desistirEnigma :: GameState -> Either Text GameState
desistirEnigma st = case gsEnigma st of
  Nothing -> Left "Não há nenhuma mensagem para decifrar."
  Just _  -> Right (registrar "📻 A mensagem interceptada foi descartada." st { gsEnigma = Nothing })

registrar :: Text -> GameState -> GameState
registrar msg st = st { gsLog = take 60 ((dataDoTurno (gsTurno st) <> ": " <> msg) : gsLog st) }

tshow :: Show a => a -> Text
tshow = T.pack . show
