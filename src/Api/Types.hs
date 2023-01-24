{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module Api.Types (API, Token (..)) where

import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
import Database.Types (Example)
import GHC.Generics (Generic)
import Servant (Capture, Get, JSON, PlainText, Post, ReqBody, (:<|>), (:>))
import Servant.Auth.JWT (FromJWT, ToJWT)
import Servant.Auth.Server (Auth, JWT)

type API =
    "login" :> Get '[PlainText] Text
        :<|> Auth '[JWT] Token :> "example" :> Capture "dat" Text :> Post '[PlainText] Text
        :<|> Auth '[JWT] Token :> "example" :> Get '[JSON] [Example]

data Token = Token
    { someId :: Integer
    , someData :: Text
    }
    deriving (Show, Generic)

instance FromJSON Token

instance FromJWT Token

instance ToJSON Token

instance ToJWT Token
