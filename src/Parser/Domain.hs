{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Parser.Domain where

import Data.Typeable
import Data.Aeson.Types
import Data.Data
import GHC.Generics

import Parser.Basic
import Parser.Check
import Utils

-- Domains --

data Domain = Domain {
    domainName :: SqlName,
    domainDescription :: String,
    domainType  :: SqlType,
    domainDefault :: Maybe String,
    domainChecks :: Maybe [Check]
} deriving (Generic, Show, Data, Typeable)
instance FromJSON Domain where parseJSON = strictParseYaml
instance ToJSON Domain where toJSON = genericToJSON myOpt

