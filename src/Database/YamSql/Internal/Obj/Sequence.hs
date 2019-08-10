module Database.YamSql.Internal.Obj.Sequence where

import Database.YamSql.Internal.Basic

data Sequence =
  Sequence
    { sequenceName :: SqlName
    , sequenceDescription :: Text
    , sequenceIncrement :: Maybe Int
    , sequenceMinValue :: Maybe Int
    , sequenceMaxValue :: Maybe Int
    , sequenceStartValue :: Maybe Int
    , sequenceCache :: Maybe Int
    , sequenceCycle :: Maybe Bool
    -- PostgreSQL extension
    , sequenceOwnedByColumn :: Maybe SqlName
    }
  deriving (Generic, Show, Data)

instance FromJSON Sequence where
  parseJSON = parseYamSql

instance ToJSON Sequence where
  toJSON = toYamSqlJson

data SQL_SEQUENCE =
  SQL_SEQUENCE
  deriving (SqlObjType, Show)

instance ToSqlCode SQL_SEQUENCE where
  toSqlCode = const "SEQUENCE"
