{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}

module Parser.Domain where

import Data.Aeson.Types
import Data.Data
import Data.Typeable
import GHC.Generics

import Parser.Basic
import Parser.Check
import Utils

-- Domains --

data Domain = Domain {
    domainName        :: SqlName,
    domainDescription :: String,
    domainType        :: SqlType,
    domainDefault     :: Maybe String,
    domainChecks      :: Maybe [Check]
} deriving (Generic, Show, Data, Typeable)
instance FromJSON Domain where parseJSON = strictParseYaml
instance ToJSON Domain where toJSON = genericToJSON myOpt

