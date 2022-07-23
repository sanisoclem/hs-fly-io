{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE UndecidableInstances #-}

module Fauna
  ( createClient
  , FaunaConfig (..)
  , FaunaClient (..)
  , HasFaunaClient (..)
  , Ledgers (..)
  , Accounts
  )
where

import Data.Has
import Data.ByteString.Lazy.Char8 (ByteString)
import Data.Text.Encoding (encodeUtf8)
import Data.Morpheus.Client
  ( Fetch (..),
    FetchError,
    declareLocalTypesInline,
    declareGlobalTypes,
    raw,
  )
import Data.Text (Text)
import Network.HTTP.Req
  ( POST (..),
    ReqBodyLbs (..),
    defaultHttpConfig,
    header,
    https,
    lbsResponse,
    req,
    responseBody,
    runReq,
    Url,
    Scheme (Https),
  )

declareGlobalTypes "assets/schema.gql"

declareLocalTypesInline
  "assets/schema.gql"
  [raw|
    query Ledgers {
      allLedgers {
        ledgerId: id
        ledgerName: name
      }
    }
  |]

declareLocalTypesInline
  "assets/schema.gql"
  [raw|
    query Accounts ($allAccountsLedgerId: String!) {
      allAccounts (ledgerId: $allAccountsLedgerId) {
        accountId: id
        accountName: name
      }
    }
  |]


data FaunaConfig = FaunaConfig
  { faunaSecret :: Text
  , faunaGraphqlEndpoint :: Url 'Https
  }

data FaunaClient = FaunaClient
  { getLedgers :: !(IO (Either (FetchError Ledgers) Ledgers)) -- use MonadUnliftIO ?
  , getAccounts :: !(Text -> IO (Either (FetchError Accounts) Accounts))
  }

class HasFaunaClient env where
  getFaunaClient :: env -> FaunaClient

instance Has FaunaClient m => HasFaunaClient m where
  getFaunaClient = getter

getResolver :: Url 'Https -> Text -> ByteString -> IO ByteString
getResolver url secret b = runReq defaultHttpConfig $ do
  let headers = header "Content-Type" "application/json"
                <> header "Authorization" (encodeUtf8 ("Bearer %s" <> secret))
  responseBody
    <$> req
      POST
      url
      (ReqBodyLbs b)
      lbsResponse
      headers

createClient :: FaunaConfig -> FaunaClient
createClient config = FaunaClient fetchLedgers fetchAccounts
  where
    fetchLedgers = fetch resolver ()
    fetchAccounts x = fetch resolver (AccountsArgs { allAccountsLedgerId = x } )
    resolver = getResolver url secret
    url = faunaGraphqlEndpoint config
    secret = faunaSecret config


