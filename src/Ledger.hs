{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators   #-}
module Ledger
    ( Ledger
    , LedgerAPI
    , ledgerServer
    ) where

import Data.Aeson
import GHC.Generics
import Servant
import Potato

data Ledger = Ledger
  { ledgerId        :: Int
  , ledgerName :: String
  } deriving (Eq, Show, Generic)
instance ToJSON Ledger

type LedgerAPI = "ledger" :> Get '[JSON] [Ledger]

ledgerServer :: ServerT LedgerAPI PotatoHandler
ledgerServer = return ledgers

ledgers :: [Ledger]
ledgers = [Ledger 1 "Test"]