{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}

module Database.YamSql.Internal.Domain where

import Database.YamSql.Internal.Basic
import Database.YamSql.Internal.Check

-- Domains --
data Domain = Domain
  { domainName :: SqlName
  , domainDescription :: Text
  , domainType :: SqlType
  , domainDefault :: Maybe Text
  , domainChecks :: Maybe [Check]
  } deriving (Generic, Show, Data, Typeable)

instance FromJSON Domain where
  parseJSON = strictParseYaml

instance ToJSON Domain where
  toJSON = genericToJSON myOpt

instance ToSqlIdPart Domain where
  sqlIdPart = domainName
  sqlIdPartType _ = "DOMAIN"
