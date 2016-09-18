{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}

module Database.YamSql.Internal.Sequence where

import Database.YamSql.Internal.Basic

data Sequence = Sequence
  { sequenceName :: SqlName
  , sequenceIncrement :: Maybe Int
  , sequenceMinValue :: Maybe Int
  , sequenceMaxValue :: Maybe Int
  , sequenceStartValue :: Maybe Int
  , sequenceCache :: Maybe Int
  , sequenceCycle :: Maybe Bool
    -- PostgreSQL extension
  , sequenceOwnedByColumn :: Maybe SqlName
  } deriving (Generic, Show, Data)

instance FromJSON Sequence where
  parseJSON = parseYamSql

instance ToJSON Sequence where
  toJSON = toYamSqlJson

instance ToSqlIdPart Sequence where
  sqlIdPart = sequenceName
  sqlIdPartType _ = "SEQUENCE"
