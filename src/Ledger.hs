{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators   #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Ledger
    ( Ledger
    , LedgerAPI
    , ledgerServer
    ) where

import Data.Aeson
import Control.Monad.Reader
import GHC.Generics
import Servant
import Data.Has
import Data.Bifunctor (first)
import Control.Monad.Error.Class (liftEither)
import Witch.From
import Potato
import Fauna
    ( HasFaunaClient(getFaunaClient), FaunaClient(getLedgers) )

data Ledger = Ledger
  { ledgerId        :: Int
  , ledgerName :: String
  } deriving (Eq, Show, Generic)
instance ToJSON Ledger


type LedgerAPI = "ledger" :> Get '[JSON] [Ledger]

ledgerServer :: (Has FaunaClient e) => ServerT LedgerAPI (PotatoHandler e)
ledgerServer = getLedgersHandler

getLedgersHandler :: (Has FaunaClient e) => ServerT LedgerAPI (PotatoHandler e)
getLedgersHandler = do
  client <- asks getFaunaClient
  l2 <- liftIO (getLedgers client) >>= (liftEither . mapErrTo500)
  return ledgers
  where
    mapErrTo500 = first (const err500)

ledgers :: [Ledger]
ledgers = [Ledger 1 "Test"]