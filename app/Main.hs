-- Back-end: Servant
--canal haskellfp
module Main where

import Data.Maybe (fromMaybe)
import Db (conectar, migrar, urlPadrao)
import System.Environment (lookupEnv)
import System.Exit (exitFailure)
import System.IO (hSetEncoding, stdout, utf8)
import Text.Read (readMaybe)
import Web.Server (runServer)

main :: IO ()
main = do
  hSetEncoding stdout utf8
  port <- fromMaybe 8080 . (>>= readMaybe) <$> lookupEnv "PORT"
  url  <- fromMaybe urlPadrao <$> lookupEnv "DATABASE_URL"
  resultadoConexao <- conectar url
  case resultadoConexao of
    Left erro -> do
      putStrLn "ERRO: nao consegui conectar ao banco de dados (PostgreSQL)."
      putStrLn "  Rode o jogo com:  bash scripts/iniciar.sh"
      putStrLn ("  Detalhe: " ++ erro)
      exitFailure
    Right conn -> do
      migrar conn
      putStrLn ("Serving on port " ++ show port)
      putStrLn ("Guerra Fria 1962 rodando em http://localhost:" ++ show port)
      runServer port conn
