{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Database.Types where

import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
import Data.Time (UTCTime)
import Database.PostgreSQL.Simple (FromRow, ToRow)
import GHC.Generics (Generic)

data Example = Example
    { eId :: Integer
    , eData :: Text
    , eUserId :: Text
    }
    deriving (Generic, FromRow, ToRow, Show)

instance ToJSON Example
instance FromJSON Example

class (Monad m) => DB m where
    saveExample :: Text -> Text -> m ()
    getExamples :: Text -> m [Example]
