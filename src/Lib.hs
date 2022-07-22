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

potatoServer :: ServerT API (PotatoHandler e)
potatoServer = ledgerServer :<|> accountServer

server :: Server API
server = hoistServer api potatoToHandler potatoServer

app :: Application
app = serve api server

startApp :: IO ()
startApp = run 3000 app
