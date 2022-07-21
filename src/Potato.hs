{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators   #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Potato
    ( HandlerEnvironment
    , PotatoHandler
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