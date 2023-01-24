module App.Config (Config (..), AppCtx (..), DatabaseConfig (..)) where

import Data.Text (Text)
import Servant.Auth.Server (JWTSettings)
import System.Log.FastLogger (LoggerSet)

data AppCtx a = AppCtx
    { config :: Config
    , logger :: LoggerSet
    , db :: a
    }

data Config = Config
    { env :: Text
    , appName :: Text
    , appVersion :: Text
    , logLevel :: Text
    , serverPort :: Int
    , databaseCfg :: DatabaseConfig
    , jwtSettings :: JWTSettings
    }

data DatabaseConfig = DatabaseConfig
    { pgHost :: Text
    , pgPort :: Integer
    , pgUser :: Text
    , pgPass :: Text
    , pgDb :: Text
    }
    deriving (Show)
