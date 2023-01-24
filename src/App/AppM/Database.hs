{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}

module App.AppM.Database where

import App.AppM.Type (AppM)
import App.Config (DatabaseConfig (..), db)
import Control.Monad (void)
import Control.Monad.Reader (MonadIO, asks, liftIO)
import Data.Functor ((<&>))
import Data.Maybe (listToMaybe)
import qualified Data.Text as T
import Data.Time (getCurrentTime)
import Database.PostgreSQL.Simple (
    ConnectInfo (..),
    Connection,
    In (In),
    Only (fromOnly),
    connect,
    execute,
    executeMany,
    execute_,
    query,
    query_,
 )
import Database.PostgreSQL.Simple.SqlQQ (sql)
import Database.Types (DB (..), Example (..))

getConnection :: DatabaseConfig -> IO Connection
getConnection DatabaseConfig{..} = connect $ ConnectInfo (T.unpack pgHost) 5432 (T.unpack pgUser) (T.unpack pgPass) (T.unpack pgDb)

instance (MonadIO m) => DB (AppM Connection e m) where
    saveExample usr dat = do
        conn <- asks db
        void $
            liftIO $
                execute
                    conn
                    [sql|INSERT INTO public.example (dat, user_id) VALUES (?, ?)|]            (dat, usr)

    getExamples userId = do
        conn <- asks db
        liftIO $ query conn "SELECT id, dat, user_id FROM public.example WHERE user_id = ?" [userId]
