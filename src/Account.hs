{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators   #-}
module Account
    ( Account
    , AccountAPI
    , accountServer
    ) where

import Data.Aeson
import GHC.Generics
import Servant
import Potato


data Account = Account
  { accountId        :: Int
  , accountName :: String
  } deriving (Eq, Show, Generic)
instance ToJSON Account


type AccountAPI = "account" :>
  ( Get '[JSON] [Account]
  :<|> DeleteNoContent
  )

accountServer :: ServerT AccountAPI (PotatoHandler e)
accountServer = get :<|> delete where
  get :: PotatoHandler e [Account]
  get = return accounts

  delete :: PotatoHandler e NoContent
  delete = return NoContent

accounts :: [Account]
accounts = [Account 1 "Test"]
