{-# LANGUAGE DeriveGeneric #-}

-- | Model: as estruturas que viram JSON (entrada e saída da API)
--   e a estrutura do formulário de salvar partida.
module Model
  ( Partida (..)
  , PartidasResponse (..)
  , PartidaNome (..)
  , ResultadoResponse (..)
  , FormSalvar (..)
  ) where

import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
import GHC.Generics (Generic)
import Web.FormUrlEncoded (FromForm)

-- | Uma partida salva no banco (tabela "partida").
--   O campo se chama idPartida (e não "id") pra não esconder a função
--   id que já existe no Haskell.
data Partida = Partida
  { idPartida  :: Int
  , comandante :: String
  , nacao      :: String
  , turnos     :: Int
  , paises     :: Int   -- ^ países controlados pelo jogador
  , situacao   :: String
  } deriving (Show, Generic)

instance ToJSON Partida    -- saída: estrutura -> JSON (GET)
instance FromJSON Partida  -- entrada: JSON -> estrutura (POST, PUT)

-- | Wrapper pra lista de partidas virar { "partidas": [...] }.
newtype PartidasResponse = PartidasResponse
  { partidas :: [Partida]
  } deriving (Show, Generic)

instance ToJSON PartidasResponse

-- | Corpo do PATCH: só o campo que vai mudar.
newtype PartidaNome = PartidaNome
  { novoNome :: String
  } deriving (Show, Generic)

instance FromJSON PartidaNome

-- | Resposta do POST: o ID que o banco gerou.
newtype ResultadoResponse = ResultadoResponse
  { resultado :: Int
  } deriving (Show, Generic)

instance ToJSON ResultadoResponse

-- | Formulário "Salvar partida" da tela do jogo.
newtype FormSalvar = FormSalvar
  { nomeComandante :: Text
  } deriving (Show, Generic)

instance FromForm FormSalvar
