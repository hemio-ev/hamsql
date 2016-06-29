{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}

module Database.YamSql.Internal.Domain where

import Database.YamSql.Internal.Basic
import Database.YamSql.Internal.Check
import Utils

-- Domains --

data Domain = Domain {
    domainName        :: SqlName,
    domainDescription :: Text,
    domainType        :: SqlType,
    domainDefault     :: Maybe Text,
    domainChecks      :: Maybe [Check]
} deriving (Generic, Show, Data, Typeable)
instance FromJSON Domain where parseJSON = strictParseYaml
instance ToJSON Domain where toJSON = genericToJSON myOpt

