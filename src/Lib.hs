{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators   #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Lib
    ( startApp
    , app
    ) where

import Data.Aeson
import Data.Aeson.Types
import GHC.Generics
import Network.Wai
import Network.Wai.Handler.Warp
import Servant
import Control.Monad.Reader
import Control.Monad.Except
import System.Environment
import Data.Maybe

data HandlerEnvironment = HandlerEnvironment
    { potato :: String
    , potatoo :: String }

newtype PotatoHandler a = PotatoHandler {
    unPotato :: ReaderT HandlerEnvironment (ExceptT ServerError IO) a
} deriving (Monad, Functor, Applicative, MonadReader HandlerEnvironment, MonadIO, MonadError ServerError)

-- TODO: use envy
potatoToHandler :: PotatoHandler a -> Handler a
potatoToHandler ph = do
  potato <- liftIO $ lookupEnv "POTATO"
  potatoo <- liftIO $ lookupEnv "POTATOO"
  r <- liftIO $ runExceptT (runReaderT (unPotato ph) (HandlerEnvironment (fromMaybe "unknown" potato) (fromMaybe "unknown" potatoo)))
  liftEither r

data Ledger = Ledger
  { ledgerId        :: Int
  , ledgerName :: String
  } deriving (Eq, Show, Generic)

instance ToJSON Ledger

data Account = Account
  { accountId        :: Int
  , accountName :: String
  } deriving (Eq, Show, Generic)

instance ToJSON Account

type LedgerAPI = "ledger" :> Get '[JSON] [Ledger]
type AccountAPI = "account" :>
  ( Get '[JSON] [Account]
  :<|> DeleteNoContent '[JSON] NoContent
  )

type API = LedgerAPI
  :<|> AccountAPI

startApp :: IO ()
startApp = run 3000 app

app :: Application
app = serve api server

api :: Proxy API
api = Proxy

ledgerServer :: ServerT LedgerAPI PotatoHandler
ledgerServer = return ledgers

accountServer :: ServerT AccountAPI PotatoHandler
accountServer = get :<|> delete where
  get :: PotatoHandler [Account]
  get = return accounts

  delete :: PotatoHandler NoContent
  delete = return NoContent

potatoServer :: ServerT API PotatoHandler
potatoServer = ledgerServer :<|> accountServer

server :: Server API
server = hoistServer api potatoToHandler potatoServer

ledgers :: [Ledger]
ledgers = [Ledger 1 "Test"]

accounts :: [Account]
accounts = [Account 1 "Test"]
