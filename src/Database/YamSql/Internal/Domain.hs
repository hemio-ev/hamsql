{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}

module Database.YamSql.Internal.Domain where

import Database.YamSql.Internal.Basic
import Database.YamSql.Internal.Check

-- | Domains are aliases of an existing SQL types, possibly with checks
data Domain = Domain
  { domainName        :: SqlName
  , domainDescription :: Text
  , domainType        :: SqlType
  , domainDefault     :: Maybe Text
  , domainChecks      :: Maybe [Check]
  } deriving (Generic, Show, Data)

instance FromJSON Domain where
  parseJSON = parseYamSql

instance ToJSON Domain where
  toJSON = toYamSqlJson

instance ToSqlIdPart Domain where
  sqlIdPart = domainName
  sqlIdPartType = const "DOMAIN"
