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
import qualified Data.Text as T
import Data.Time.Clock.POSIX (getPOSIXTime)
import Database.PostgreSQL.Simple (Connection)
import Game.Map (initialState)
import Game.Rules (aplicarAcao, encerrarTurno, iniciarPartida, paisesControlados)
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
  :<|> "novo"        :> Capture "lado" String   :> Post '[HTML] (Html ())         -- POST /novo/eua
  :<|> "influenciar" :> Capture "codigo" String :> Post '[HTML] (Html ())         -- POST /influenciar/B
  :<|> "golpe"       :> Capture "codigo" String :> Post '[HTML] (Html ())         -- POST /golpe/B
  :<|> "guerra"      :> Capture "codigo" String :> Post '[HTML] (Html ())         -- POST /guerra/B
  :<|> "espacial"    :> Post '[HTML] (Html ())                                   -- POST /espacial
  :<|> "turno"       :> Post '[HTML] (Html ())                                   -- POST /turno
  :<|> "reiniciar"   :> Post '[HTML] (Html ())                                   -- POST /reiniciar
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
  :<|> executarAcao Nothing CorridaEspacial
  :<|> turno
  :<|> reiniciar
  :<|> salvar
  where
    inicio :: Handler (Html ())
    inicio = do
      st <- liftIO (readIORef ref)
      pure (page st)

    verPais :: String -> Handler (Html ())
    verPais codigo = do
      c  <- codigoPais codigo
      st <- liftIO (readIORef ref)
      pure (gameView st (Just c) Nothing)

    novo :: String -> Handler (Html ())
    novo ladoTxt = do
      lado <- case ladoTxt of
        "eua"   -> pure EUA
        "reich" -> pure Reich
        _       -> throwError err404
      semente <- liftIO (floor <$> getPOSIXTime)
      st <- atualizar (\s -> iniciarPartida lado semente (gsComandante s))
      pure (gameView st Nothing Nothing)

    acaoEmPais :: (Char -> Acao) -> String -> Handler (Html ())
    acaoEmPais construtor codigo = do
      c <- codigoPais codigo
      executarAcao (Just c) (construtor c)

    executarAcao :: Maybe Char -> Acao -> Handler (Html ())
    executarAcao sel acao = do
      (st, notice) <- liftIO $ atomicModifyIORef' ref $ \s ->
        case aplicarAcao (gsJogador s) acao s of
          Left err -> (s,  (s,  Erro err))
          Right s' -> (s', (s', Info (ultimoRelatorio s')))
      st' <- salvarSeTerminou st
      pure (gameView st' sel (Just notice))

    turno :: Handler (Html ())
    turno = do
      st  <- atualizar encerrarTurno
      st' <- salvarSeTerminou st
      pure (gameView st' Nothing Nothing)

    reiniciar :: Handler (Html ())
    reiniciar = do
      st <- atualizar (\s -> initialState { gsComandante = gsComandante s })
      pure (gameView st Nothing Nothing)

    salvar :: FormSalvar -> Handler (Html ())
    salvar form = do
      let nome = T.strip (nomeComandante form)
      _  <- atualizar (\s -> s { gsComandante = if T.null nome then "Anônimo" else nome })
      st <- liftIO (readIORef ref) >>= salvarNoBanco
      let msg = "Partida salva no histórico (#" <> maybe "?" (T.pack . show) (gsPartidaId st) <> ")."
      pure (gameView st Nothing (Just (Info msg)))

    -- Salva no banco usando os mesmos handlers da API:
    -- na primeira vez faz POST; depois faz PUT na mesma partida.
    salvarNoBanco :: GameState -> Handler GameState
    salvarNoBanco st = do
      let partida = partidaDoJogo st
      pid <- case gsPartidaId st of
        Nothing -> inserir partida
        Just existente -> do
          r <- liftIO (runHandler (handlerPutPartida conn existente partida))
          case r of
            Right _ -> pure existente
            Left _  -> inserir partida -- foi apagada no histórico: cria de novo
      atualizar (\s -> s { gsPartidaId = Just pid })

    -- No fim da partida o resultado vai pro histórico automaticamente.
    -- Se o banco falhar, o jogo continua mesmo assim.
    salvarSeTerminou :: GameState -> Handler GameState
    salvarSeTerminou st
      | isJust (gsFim st) = do
          r <- liftIO (runHandler (salvarNoBanco st))
          pure (either (const st) id r)
      | otherwise = pure st

    inserir :: Partida -> Handler Int
    inserir partida = resultado <$> handlerPostPartida conn partida

    atualizar :: (GameState -> GameState) -> Handler GameState
    atualizar f = liftIO $ atomicModifyIORef' ref $ \s -> let s' = f s in (s', s')

-- | O código do país vem na URL como uma letra só.
codigoPais :: String -> Handler Char
codigoPais [c] = pure c
codigoPais _   = throwError err404

ultimoRelatorio :: GameState -> T.Text
ultimoRelatorio st = case gsLog st of
  (m : _) -> m
  []      -> "Feito."

-- | Converte o estado do jogo no registro que vai pro banco.
partidaDoJogo :: GameState -> Partida
partidaDoJogo st = Partida
  { idPartida  = 0 -- ignorado: o banco gera o ID
  , comandante = T.unpack (if T.null (gsComandante st) then "Anônimo" else gsComandante st)
  , nacao      = T.unpack (ladoNome jogador)
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
