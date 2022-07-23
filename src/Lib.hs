{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TypeOperators   #-}
module Lib
    ( startApp
    , app
    ) where

import Network.Wai
import Network.Wai.Handler.Warp
import Servant
import Potato
import Ledger
import Account

api :: Proxy API
api = Proxy

type API = LedgerAPI
  :<|> AccountAPI

server :: Server API
server = hoistServer api potatoToHandler endpoints
  where
    endpoints = ledgerServer :<|> accountServer

app :: Application
app = serve api server

startApp :: IO ()
startApp = run 3000 app
