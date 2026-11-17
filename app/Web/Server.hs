{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

-- | Servidor Servant: junta o exemplo "hello" da aula, a API JSON de
--   partidas (Api.Partidas), as rotas do jogo (que devolvem HTML) e os
--   arquivos de static/.
module Web.Server (runServer) where

import Api.Partidas
import Control.Monad.IO.Class (liftIO)
import Data.IORef
import Data.Maybe (isJust)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Clock.POSIX (getPOSIXTime)
import Database.PostgreSQL.Simple (Connection)
import Game.Enigma (desistirEnigma, interceptar, tentarChave)
import Game.Map (initialState)
import Game.Rules
import Game.Types
import Lucid (Html, renderBS)
import Model
import Network.HTTP.Media ((//), (/:))
import Network.Wai.Handler.Warp (run)
import Servant
import Web.Views

-- ---------------------------------------------------------------------------
-- Tipo de conteúdo HTML (as rotas do jogo devolvem páginas, não JSON)

data HTML

instance Accept HTML where
  contentType _ = "text" // "html" /: ("charset", "utf-8")

instance MimeRender HTML (Html ()) where
  mimeRender _ = renderBS

-- ---------------------------------------------------------------------------
-- Rotas

type JogoAPI =
       Get '[HTML] (Html ())                                                    -- GET  /
  :<|> "pais"        :> Capture "codigo" String :> Get  '[HTML] (Html ())         -- GET  /pais/B
  :<|> "novo"        :> Capture "modo" String   :> Post '[HTML] (Html ())         -- POST /novo/eua
  :<|> "influenciar" :> Capture "codigo" String :> Post '[HTML] (Html ())         -- POST /influenciar/B
  :<|> "golpe"       :> Capture "codigo" String :> Post '[HTML] (Html ())         -- POST /golpe/B
  :<|> "guerra"      :> Capture "codigo" String :> Post '[HTML] (Html ())         -- POST /guerra/B
  :<|> "espacial"    :> Post '[HTML] (Html ())                                   -- POST /espacial
  :<|> "turno"       :> Post '[HTML] (Html ())                                   -- POST /turno
  :<|> "crise"       :> Capture "opcao" String  :> Post '[HTML] (Html ())         -- POST /crise/a
  :<|> "jornal"      :> "fechar" :> Post '[HTML] (Html ())                       -- POST /jornal/fechar
  :<|> "reiniciar"   :> Post '[HTML] (Html ())                                   -- POST /reiniciar
  :<|> "enigma"      :> "desistir" :> Post '[HTML] (Html ())                     -- POST /enigma/desistir
  :<|> "enigma"      :> Capture "chave" String  :> Post '[HTML] (Html ())         -- POST /enigma/KDR
  :<|> "salvar"      :> ReqBody '[FormUrlEncoded] FormSalvar :> Post '[HTML] (Html ())

-- | A API completa. A primeira rota é o exemplo da aula ("hello"), mantido.
type API =
       "hello" :> Get '[JSON] String
  :<|> PartidasAPI
  :<|> JogoAPI
  :<|> Raw

api :: Proxy API
api = Proxy

helloFunc :: Handler String
helloFunc = pure "Hello, world!"

server :: Connection -> IORef GameState -> Server API
server conn ref =
       helloFunc
  :<|> servidorPartidas conn
  :<|> servidorJogo conn ref
  :<|> serveDirectoryWebApp "static"

servidorJogo :: Connection -> IORef GameState -> Server JogoAPI
servidorJogo conn ref =
       inicio
  :<|> verPais
  :<|> novo
  :<|> acaoEmPais Influenciar
  :<|> acaoEmPais Golpe
  :<|> acaoEmPais GuerraProcuracao
  :<|> executar Nothing (aplicarAcaoDoJogador CorridaEspacial)
  :<|> turno
  :<|> crise
  :<|> fechar
  :<|> reiniciar
  :<|> executar Nothing desistirEnigma
  :<|> (\chave -> executar Nothing (tentarChave (T.pack chave)))
  :<|> salvar
  where
    inicio :: Handler (Html ())
    inicio = liftIO (readIORef ref) >>= responder . page

    verPais :: String -> Handler (Html ())
    verPais codigo = do
      c  <- codigoPais codigo
      st <- liftIO (readIORef ref)
      responder (gameView st (Just c) Nothing)

    novo :: String -> Handler (Html ())
    novo modoTxt = do
      modo <- case modoTxt of
        "eua"         -> pure GuerraFria
        "resistencia" -> pure Resistencia
        _             -> throwError err404
      semente <- liftIO (floor <$> getPOSIXTime)
      st <- atualizar (\s -> iniciarPartida modo semente (gsComandante s))
      responder (gameView st Nothing Nothing)

    acaoEmPais :: (Char -> Acao) -> String -> Handler (Html ())
    acaoEmPais construtor codigo = do
      c <- codigoPais codigo
      executar (Just c) (aplicarAcaoDoJogador (construtor c))

    turno :: Handler (Html ())
    -- Fim do trimestre; se o trimestre mudou, pode chegar uma mensagem cifrada.
    turno = executar Nothing $ \s ->
      let s' = encerrarTurno s
      in Right (if gsTurno s' /= gsTurno s then interceptar s' else s')

    crise :: String -> Handler (Html ())
    crise opcao = case opcao of
      "a" -> executar Nothing (resolverCrise True)
      "b" -> executar Nothing (resolverCrise False)
      _   -> throwError err404

    fechar :: Handler (Html ())
    fechar = executar Nothing fecharJornal

    reiniciar :: Handler (Html ())
    reiniciar = do
      st <- atualizar (\s -> initialState { gsComandante = gsComandante s })
      responder (gameView st Nothing Nothing)

    salvar :: FormSalvar -> Handler (Html ())
    salvar form = do
      let nome = T.strip (nomeComandante form)
      _  <- atualizar (\s -> s { gsComandante = if T.null nome then "Anônimo" else nome })
      st <- liftIO (readIORef ref) >>= salvarNoBanco False
      let msg = "Partida salva no histórico (#" <> maybe "?" (T.pack . show) (gsPartidaId st) <> ")."
      responder (gameView st Nothing (Just (Info msg)))

    -- Aplica uma regra pura ao estado e devolve a tela atualizada.
    -- Se a regra falhar, o estado não muda e aparece o aviso de erro.
    executar :: Maybe Char -> (GameState -> Either Text GameState) -> Handler (Html ())
    executar sel regra = do
      (st, notice) <- liftIO $ atomicModifyIORef' ref $ \s ->
        case regra s of
          Left err -> (s,  (s,  Just (Erro err)))
          Right s' -> (s', (s', avisoDe s s'))
      st' <- salvarSeTerminou st
      responder (gameView st' sel notice)

    -- Depois de montar a tela, limpa os efeitos de uma vez só
    -- (países piscando, sons, dado), pra não repetirem no próximo clique.
    responder :: Html () -> Handler (Html ())
    responder html = do
      _ <- atualizar (\s -> s { gsMudaram = [], gsSons = [], gsDado = Nothing, gsCenaFinal = False })
      pure html

    -- Salva no banco usando os mesmos handlers da API:
    -- na primeira vez faz POST; depois faz PUT na mesma partida.
    -- Com manterNome = True (salvamento automático), o nome que estiver no
    -- banco é preservado (caso tenha sido renomeado no histórico).
    salvarNoBanco :: Bool -> GameState -> Handler GameState
    salvarNoBanco manterNome st = do
      let partida = partidaDoJogo st
      pid <- case gsPartidaId st of
        Nothing -> inserir partida
        Just existente -> do
          antiga <- liftIO (runHandler (handlerPartidaPorId conn existente))
          let partida' = case antiga of
                Right p | manterNome -> partida { comandante = comandante p }
                _                    -> partida
          r <- liftIO (runHandler (handlerPutPartida conn existente partida'))
          case r of
            Right _ -> pure existente
            Left _  -> inserir partida -- foi apagada no histórico: cria de novo
      atualizar (\s -> s { gsPartidaId = Just pid })

    -- No fim da partida o resultado vai pro histórico automaticamente.
    -- Se o banco falhar, o jogo continua mesmo assim.
    salvarSeTerminou :: GameState -> Handler GameState
    salvarSeTerminou st
      | isJust (gsFim st) && gsCenaFinal st = do
          r <- liftIO (runHandler (salvarNoBanco True st))
          pure (either (const st) id r)
      | otherwise = pure st

    inserir :: Partida -> Handler Int
    inserir partida = resultado <$> handlerPostPartida conn partida

    atualizar :: (GameState -> GameState) -> Handler GameState
    atualizar f = liftIO $ atomicModifyIORef' ref $ \s -> let s' = f s in (s', s')

-- | A ação do jogador humano (o lado dele está no próprio estado).
aplicarAcaoDoJogador :: Acao -> GameState -> Either Text GameState
aplicarAcaoDoJogador acao s = aplicarAcao (gsJogador s) acao s

-- | Aviso verde com o último relatório, quando a ação gerou um.
avisoDe :: GameState -> GameState -> Maybe Notice
avisoDe antes depois
  | gsJornal depois                      = Nothing   -- o jornal já mostra tudo
  | take 1 (gsLog depois) == take 1 (gsLog antes) = Nothing
  | otherwise = case gsLog depois of
      (m : _) -> Just (Info m)
      []      -> Nothing

-- | O código do país vem na URL como uma letra só.
codigoPais :: String -> Handler Char
codigoPais [c] = pure c
codigoPais _   = throwError err404

-- | Converte o estado do jogo no registro que vai pro banco.
partidaDoJogo :: GameState -> Partida
partidaDoJogo st = Partida
  { idPartida  = 0 -- ignorado: o banco gera o ID
  , comandante = T.unpack (if T.null (gsComandante st) then "Anônimo" else gsComandante st)
  , nacao      = T.unpack (nomeDoLado st jogador)
  , turnos     = gsTurno st
  , paises     = length (paisesControlados jogador st)
  , situacao   = case gsFim st of
      Nothing -> "Em andamento"
      Just (Vitoria lado)
        | lado == jogador -> "Vitória"
        | otherwise       -> "Derrota"
      Just Empate            -> "Empate"
      Just (GuerraNuclear _) -> "Guerra nuclear"
  }
  where
    jogador = gsJogador st

runServer :: Int -> Connection -> IO ()
runServer port conn = do
  ref <- newIORef initialState
  run port (serve api (server conn ref))
