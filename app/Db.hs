-- | Conexão com o PostgreSQL e migração automática.
module Db (urlPadrao, conectar, migrar) where

import Control.Exception (SomeException, try)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import Database.PostgreSQL.Simple (Connection, connectPostgreSQL, execute_)
import Database.PostgreSQL.Simple.Types (Query (..))

-- | Roda o arquivo de migração ao ligar o servidor. Como o SQL usa
--   "CREATE TABLE IF NOT EXISTS", rodar de novo não estraga nada.
migrar :: Connection -> IO ()
migrar conn = do
  sql <- BS.readFile "migrations/001_criar_partida.sql"
  _ <- execute_ conn (Query sql)
  pure ()

-- | Configuração criada pelo scripts/iniciar.sh.
urlPadrao :: String
urlPadrao = "host=localhost port=5432 dbname=fronteiras user=postgres password=postgres"

-- | Tenta conectar. Devolve o erro como texto se o banco não estiver rodando.
conectar :: String -> IO (Either String Connection)
conectar url = do
  r <- try (connectPostgreSQL (BS8.pack url))
  pure $ case r of
    Left e     -> Left (show (e :: SomeException))
    Right conn -> Right conn
