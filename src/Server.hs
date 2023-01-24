{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Server (runServer, makeJWTSettings) where

import Api.Handler (exampleGET, examplePOST, login)
import Api.Types (API)
import App.AppM
import App.Config
import Database.PostgreSQL.Simple (Connection)
import Database.Types (DB (..))
import Logging.Types

import Control.Monad.Except (MonadError, runExceptT, throwError)
import Control.Monad.Reader (MonadIO, MonadReader, ReaderT, ask, asks, liftIO, runReaderT)
import Crypto.JOSE.JWA.JWS (Alg (HS512))
import Data.Either
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Logging.Logger (logGeneric)
import Network.Wai.Handler.Warp
import Servant (
    Application,
    Context (EmptyContext, (:.)),
    Handler,
    Proxy (..),
    ServerError (ServerError),
    ServerT,
    err500,
    hoistServerWithContext,
    serveWithContext,
    throwError,
    (:<|>) ((:<|>)),
 )
import Servant.Auth.Server
import Servant.Server (
    Handler,
    HasServer (ServerT),
    hoistServer,
    serve,
 )
import System.Log.FastLogger

api :: Proxy API
api = Proxy

server :: (MonadIO m, MonadError ServerError m) => ServerT API (AppM Connection ServerError m)
server = login :<|> examplePOST :<|> exampleGET

servantProxy :: Proxy '[JWTSettings, CookieSettings]
servantProxy = Proxy

runServer :: AppCtx Connection -> IO ()
runServer ctx@AppCtx{..} = do
    let Config{..} = config
        servantCtx = defaultCookieSettings :. jwtSettings :. EmptyContext
    logGeneric logger "INFO" config "Starting server"
    run serverPort $ serveWithContext api servantCtx $ hoistServerWithContext api servantProxy (r ctx) server
  where
    r ctx app = do
        res <- runExceptT $ runReaderT (runAppM app) ctx
        case res of
            Left err -> throwError err
            Right x -> pure x

makeJWTSettings :: Text -> JWTSettings
makeJWTSettings secret = let jwk = fromSecret (TE.encodeUtf8 secret) in (defaultJWTSettings jwk){jwtAlg = Just HS512}
