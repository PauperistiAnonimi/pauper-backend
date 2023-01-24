{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module App.AppM.Logging where

import App.AppM.Type (AppM)
import App.Config (
    AppCtx (AppCtx),
    Config (Config, appName, appVersion, env, logLevel),
 )
import Control.Monad.Reader (MonadIO, ask, liftIO)
import Logging.Logger (logAction)
import Logging.Types (HasLogger (..), LogLevel (DEBUG, ERROR, INFO))
import System.Log.FastLogger (
    LoggerSet,
    defaultBufSize,
    newStdoutLoggerSet,
 )

getLoggerSet :: IO LoggerSet
getLoggerSet = newStdoutLoggerSet defaultBufSize

instance (MonadIO m) => HasLogger (AppM a e m) where
    logInfo msg = do
        AppCtx cfg@Config{..} logger _ <- ask
        liftIO $ logAction logger cfg logLevel INFO msg
    logError msg = do
        AppCtx cfg@Config{..} logger _ <- ask
        liftIO $ liftIO $ logAction logger cfg logLevel ERROR msg
    logDebug msg = do
        AppCtx cfg@Config{..} logger _ <- ask
        liftIO $ liftIO $ logAction logger cfg logLevel DEBUG msg
