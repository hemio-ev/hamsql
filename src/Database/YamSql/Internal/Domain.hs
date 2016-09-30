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

data SQL_DOMAIN =
  SQL_DOMAIN
  deriving (SqlObjType, Show)

instance ToSqlCode SQL_DOMAIN where
  toSqlCode = const "DOMAIN"

data SQL_DOMAIN_CONSTRAINT =
  SQL_DOMAIN_CONSTRAINT
  deriving (SqlObjType, Show)

instance ToSqlCode SQL_DOMAIN_CONSTRAINT where
  toSqlCode = const "DOMAIN_CONSTRAINT"
