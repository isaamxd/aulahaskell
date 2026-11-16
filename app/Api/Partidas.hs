{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

-- | API REST de partidas (CRUD), no mesmo padrão das aulas:
--   cada handler recebe a conexão com o banco e responde JSON.
module Api.Partidas
  ( PartidasAPI
  , servidorPartidas
  , handlerPostPartida
  , handlerPutPartida
  ) where

import Control.Exception (try)
import Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString.Lazy as BL
import Database.PostgreSQL.Simple
  (Connection, Only (..), SqlError (..), execute, query, query_)
import Model
import Servant

type PartidasAPI =
       "api" :> "partidas" :> Get '[JSON] PartidasResponse                                       -- lista todas
  :<|> "api" :> "partidas" :> Capture "id" Int :> Get '[JSON] Partida                            -- busca por ID
  :<|> "api" :> "partidas" :> ReqBody '[JSON] Partida :> Post '[JSON] ResultadoResponse           -- cadastra
  :<|> "api" :> "partidas" :> Capture "id" Int :> "nome" :> ReqBody '[JSON] PartidaNome
                           :> Patch '[JSON] NoContent                                            -- muda o nome
  :<|> "api" :> "partidas" :> Capture "id" Int :> ReqBody '[JSON] Partida :> Put '[JSON] NoContent -- atualiza tudo
  :<|> "api" :> "partidas" :> Capture "id" Int :> Delete '[JSON] NoContent                       -- apaga

servidorPartidas :: Connection -> Server PartidasAPI
servidorPartidas conn =
       handlerPartidasTodas conn
  :<|> handlerPartidaPorId conn
  :<|> handlerPostPartida conn
  :<|> handlerPatchPartidaNome conn
  :<|> handlerPutPartida conn
  :<|> handlerDeletePartida conn

-- ---------------------------------------------------------------------------
-- Handlers

-- | GET /api/partidas  (ordenado como ranking: mais países primeiro)
handlerPartidasTodas :: Connection -> Handler PartidasResponse
handlerPartidasTodas conn = do
  linhas <- rodarSql $ query_ conn
    "SELECT id, comandante, nacao, turnos, paises, situacao FROM partida \
    \ORDER BY paises DESC, turnos ASC"
  pure (PartidasResponse (map paraPartida linhas))

-- | GET /api/partidas/:id  (404 se não existir)
handlerPartidaPorId :: Connection -> Int -> Handler Partida
handlerPartidaPorId conn pid = do
  linhas <- rodarSql $ query conn
    "SELECT id, comandante, nacao, turnos, paises, situacao FROM partida WHERE id = ?"
    (Only pid)
  case linhas of
    [linha] -> pure (paraPartida linha)
    _       -> throwError err404

-- | POST /api/partidas  (devolve o ID criado; o idPartida enviado é ignorado)
handlerPostPartida :: Connection -> Partida -> Handler ResultadoResponse
handlerPostPartida conn p = do
  linhas <- rodarSql $ query conn
    "INSERT INTO partida (comandante, nacao, turnos, paises, situacao) \
    \VALUES (?, ?, ?, ?, ?) RETURNING id"
    (comandante p, nacao p, turnos p, paises p, situacao p)
  case linhas of
    [Only novoId] -> pure (ResultadoResponse novoId)
    _             -> throwError err500

-- | PATCH /api/partidas/:id/nome  (muda só o comandante)
handlerPatchPartidaNome :: Connection -> Int -> PartidaNome -> Handler NoContent
handlerPatchPartidaNome conn pid corpo = do
  n <- rodarSql $ execute conn
    "UPDATE partida SET comandante = ? WHERE id = ?"
    (novoNome corpo, pid)
  if n == 1 then pure NoContent else throwError err404

-- | PUT /api/partidas/:id  (atualiza o registro inteiro)
handlerPutPartida :: Connection -> Int -> Partida -> Handler NoContent
handlerPutPartida conn pid p = do
  n <- rodarSql $ execute conn
    "UPDATE partida SET comandante = ?, nacao = ?, turnos = ?, paises = ?, situacao = ? \
    \WHERE id = ?"
    (comandante p, nacao p, turnos p, paises p, situacao p, pid)
  if n == 1 then pure NoContent else throwError err404

-- | DELETE /api/partidas/:id
handlerDeletePartida :: Connection -> Int -> Handler NoContent
handlerDeletePartida conn pid = do
  n <- rodarSql $ execute conn "DELETE FROM partida WHERE id = ?" (Only pid)
  if n == 1 then pure NoContent else throwError err404

-- ---------------------------------------------------------------------------
-- Auxiliares

type Linha = (Int, String, String, Int, Int, String)

paraPartida :: Linha -> Partida
paraPartida (i, c, n, t, r, s) = Partida i c n t r s

-- | Roda uma operação no banco. Se o banco der erro, responde 500 com a
--   mensagem, em vez de derrubar o servidor (o "try" sugerido na aula).
rodarSql :: IO a -> Handler a
rodarSql acao = do
  r <- liftIO (try acao)
  case r of
    Left e  -> throwError err500 { errBody = "Erro no banco de dados: " <> BL.fromStrict (sqlErrorMsg e) }
    Right x -> pure x
