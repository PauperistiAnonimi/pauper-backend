{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}

module Api.Handler (login, examplePOST, exampleGET) where

import Api.Types (Token (..))
import App.AppM
import App.Config
import Database.Types (DB (..), Example (..))
import Logging.Types

import Control.Monad.Except (MonadError, throwError)
import Control.Monad.Reader (MonadIO, MonadReader, ReaderT, ask, asks, liftIO, runReaderT)
import Crypto.JOSE.JWA.JWS (Alg (HS512))
import Data.ByteString.Lazy (toStrict)
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8)
import Database.PostgreSQL.Simple (Connection)
import Debug.Trace (trace)
import Servant (ServerError, err401, err500)
import Servant.Auth.Server (AuthResult (..), defaultJWTSettings, fromSecret, jwtAlg, makeJWT)
import Text.InterpolatedString.QM (qms)

login :: (MonadReader (AppCtx Connection) m, MonadIO m, MonadError ServerError m, HasLogger m) => m Text
login = do
    logInfo "generating jwt token"
    AppCtx{..} <- ask
    etkn <- liftIO $ makeJWT (Token 1234 "pippo") (jwtSettings config) Nothing
    case etkn of
        Left err -> do
            logError "Failed to generate token"
            liftIO $ print err
            throwError err500
        Right tkn -> pure . decodeUtf8 . toStrict $ tkn

examplePOST :: (MonadReader (AppCtx Connection) m, MonadIO m, MonadError ServerError m, HasLogger m, DB m) => AuthResult Token -> Text -> m Text
examplePOST (Authenticated (Token _ sd)) dat = do
    logInfo "Authenticated example request!"
    saveExample sd dat
    pure [qms|{dat} - {sd}|]
examplePOST Servant.Auth.Server.BadPassword _ = trace "BadPassword" $ throwError err401
examplePOST Servant.Auth.Server.NoSuchUser _ = trace "NoSuchUser" $ throwError err401
examplePOST Servant.Auth.Server.Indefinite _ = trace "Indefinite" $ throwError err401

exampleGET :: (MonadReader (AppCtx Connection) m, MonadIO m, MonadError ServerError m, HasLogger m, DB m) => AuthResult Token -> m [Example]
exampleGET (Authenticated (Token _ sd)) = do
    logInfo "Authenticated example request!"
    getExamples sd
exampleGET Servant.Auth.Server.BadPassword = trace "BadPassword" $ throwError err401
exampleGET Servant.Auth.Server.NoSuchUser = trace "NoSuchUser" $ throwError err401
exampleGET Servant.Auth.Server.Indefinite = trace "Indefinite" $ throwError err401
