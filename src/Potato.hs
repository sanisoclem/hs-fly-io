{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators   #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Potato
    ( PotatoHandler
    , potatoToHandler
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
import Data.Text (pack)
import Fauna
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
    (/:)
  )

newtype PotatoHandler e a = PotatoHandler {
    unPotato :: ReaderT e (ExceptT ServerError IO) a
} deriving (Monad, Functor, Applicative, MonadReader e, MonadIO, MonadError ServerError)

-- TODO: use envy
potatoToHandler :: PotatoHandler FaunaClient a -> Handler a
potatoToHandler ph = do
  let endpoint = https "graphql.fauna.com" /: "graphql"
  secret <- liftIO $ lookupEnv "FAUNA_SECRET"
  secret <- pack <$> maybe (throwError err500) return secret
  let config = FaunaConfig secret endpoint
  let client = createClient config

  r <- liftIO $ runExceptT (runReaderT (unPotato ph) client)
  liftEither r